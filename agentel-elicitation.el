;;; agentel-elicitation.el --- Form elicitation for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; Answers `elicitation/create' requests in form mode.  Claude asks
;; AskUserQuestion this way, and without the capability the tool is
;; disabled.  The form is read from its JSON schema, so other forms,
;; such as those of MCP servers, work the same way.
;;
;; The form is shown in the transcript and filled in from the
;; minibuffer, one field at a time.  A free text field that claude-agent-acp
;; marks as the custom answer of a choice is not asked separately:
;; text typed at the choice that matches no option goes there.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'crm)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)

(defface agentel-elicitation-face
  '((t :inherit warning))
  "Face of questions from the agent."
  :group 'agentel)

(defun agentel-elicitation--capabilities ()
  "Return the client capability of answering forms."
  '((elicitation . ((form . nil)))))

;;;; Schema

(defun agentel-elicitation--fields (item)
  "Return the fields of the form ITEM as (KEY . SCHEMA) in order."
  (alist-get 'properties
             (alist-get 'requestedSchema (plist-get item :params))))

(defun agentel-elicitation--custom-for (field)
  "Return the key of the choice whose custom answer FIELD is, or nil."
  (when-let* ((meta (alist-get '_askUserQuestionCustomAnswer
                               (alist-get '_meta (cdr field))))
              (question (alist-get 'questionId meta)))
    (intern question)))

(defun agentel-elicitation--custom-field (fields key)
  "Return the key of the custom answer field of the choice KEY in FIELDS."
  (car (seq-find (lambda (f) (eq (agentel-elicitation--custom-for f) key))
                 fields)))

(defun agentel-elicitation--options (schema)
  "Return the options of the choice SCHEMA as (TITLE VALUE DESCRIPTION)."
  (let ((items (or (alist-get 'oneOf schema)
                   (alist-get 'anyOf (alist-get 'items schema))))
        (plain (or (alist-get 'enum schema)
                   (alist-get 'enum (alist-get 'items schema)))))
    (if items
        (mapcar (lambda (o)
                  (list (or (alist-get 'title o) (format "%s" (alist-get 'const o)))
                        (alist-get 'const o)
                        (alist-get 'description o)))
                items)
      (mapcar (lambda (v) (list (format "%s" v) v nil)) plain))))

(defun agentel-elicitation--label (key schema)
  "Return the label of the field KEY with SCHEMA."
  (or (alist-get 'title schema) (symbol-name key)))

;;;; Display

(defun agentel-elicitation--render-field (field)
  "Render FIELD of a form."
  (let* ((schema (cdr field))
         (description (alist-get 'description schema))
         (multiple (equal (alist-get 'type schema) "array")))
    (concat
     "\n  "
     (propertize (agentel-elicitation--label (car field) schema) 'face 'bold)
     (if description (concat " — " description) "")
     (if multiple " (any number)" "")
     (mapconcat (lambda (option)
                  (concat "\n    • " (car option)
                          (if (nth 2 option) (concat " — " (nth 2 option)) "")))
                (agentel-elicitation--options schema) ""))))

(defun agentel-elicitation--summary (item)
  "Return the answers of ITEM as one line."
  (let ((content (plist-get item :content))
        (fields (agentel-elicitation--fields item)))
    (mapconcat
     (lambda (answer)
       (let* ((key (or (agentel-elicitation--custom-for
                        (assq (car answer) fields))
                       (car answer)))
              (value (cdr answer)))
         (format "%s: %s"
                 (agentel-elicitation--label key (alist-get key fields))
                 (if (vectorp value)
                     (mapconcat (lambda (v) (format "%s" v)) value ", ")
                   value))))
     content "; ")))

(defun agentel-elicitation--render (entry)
  "Render the form ENTRY."
  (let* ((item (agentel-chat-entry-get entry 'item))
         (message (or (alist-get 'message (plist-get item :params)) "Question"))
         (outcome (plist-get item :outcome)))
    (if outcome
        (propertize (format "? %s → %s" message
                            (if (eq outcome 'accept)
                                (agentel-elicitation--summary item)
                              outcome))
                    'face 'agentel-chat-notice-face)
      (concat
       (propertize (concat "? " message) 'face 'agentel-elicitation-face)
       (mapconcat #'agentel-elicitation--render-field
                  (seq-remove #'agentel-elicitation--custom-for
                              (agentel-elicitation--fields item))
                  "")
       "\n  "
       (buttonize "[Answer]" (lambda (_) (agentel-elicitation--ask item)))
       " "
       (buttonize "[Decline]" (lambda (_) (agentel-elicitation-decline item)))))))

(defun agentel-elicitation--show (item)
  "Show the state of the form ITEM in its session buffer."
  (let ((buffer (agentel-session-buffer (plist-get item :session)))
        (key (cons 'elicitation (plist-get item :id))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if-let* ((entry (agentel-chat-find key)))
            (agentel-chat-refresh entry)
          (agentel-chat-add key 'elicitation #'agentel-elicitation--render
                            `((item . ,item))))))))

;;;; Answering

(defun agentel-elicitation--close (item outcome)
  "Stop waiting for ITEM, recording OUTCOME."
  (plist-put item :outcome outcome)
  (agentel-session-remove-pending (plist-get item :session) item)
  (agentel-elicitation--show item))

(defun agentel-elicitation--reply (item result outcome)
  "Answer the form ITEM with RESULT and record OUTCOME."
  (unless (plist-get item :outcome)
    (agentel-connection-respond (plist-get item :connection) (plist-get item :id)
                                result)
    (agentel-elicitation--close item outcome)))

(defun agentel-elicitation-decline (item)
  "Decline to answer the form ITEM."
  (agentel-elicitation--reply item '((action . "decline")) 'declined))

(defun agentel-elicitation--read-choice (prompt schema)
  "Read a choice of SCHEMA with PROMPT.
Return (VALUE . nil) for an option, (nil . TEXT) for other text and
nil when skipped."
  (let* ((options (agentel-elicitation--options schema))
         (answer (string-trim
                  (completing-read prompt
                                   (agentel-chat-ordered-completion
                                    (mapcar #'car options))))))
    (cond ((string-empty-p answer) nil)
          ((assoc answer options) (cons (nth 1 (assoc answer options)) nil))
          (t (cons nil answer)))))

(defun agentel-elicitation--read-choices (prompt schema)
  "Read any number of choices of SCHEMA with PROMPT.
Return (VALUES . TEXT) where TEXT joins answers matching no option."
  (let* ((options (agentel-elicitation--options schema))
         (answers (completing-read-multiple prompt (mapcar #'car options)))
         values others)
    (dolist (answer answers)
      (if-let* ((option (assoc answer options)))
          (push (nth 1 option) values)
        (push answer others)))
    (cons (nreverse values)
          (and others (string-join (nreverse others) ", ")))))

(defun agentel-elicitation--read-field (key schema)
  "Read the value of the plain field KEY with SCHEMA, or nil to skip it."
  (let ((prompt (format "%s: " (agentel-elicitation--label key schema))))
    (pcase (alist-get 'type schema)
      ("boolean" (if (y-or-n-p prompt) t :false))
      ((or "number" "integer") (read-number prompt))
      (_ (let ((text (string-trim (read-string prompt))))
           (unless (string-empty-p text) text))))))

(defun agentel-elicitation--read (item)
  "Read the answers to the form ITEM and return the content alist."
  (let ((fields (agentel-elicitation--fields item))
        content)
    (dolist (field fields)
      (let* ((key (car field))
             (schema (cdr field))
             (custom (agentel-elicitation--custom-field fields key))
             (prompt (format "%s%s: "
                             (agentel-elicitation--label key schema)
                             (if-let* ((d (alist-get 'description schema)))
                                 (format " (%s)" d) ""))))
        (cond
         ((agentel-elicitation--custom-for field))
         ((agentel-elicitation--options schema)
          (let ((answer (if (equal (alist-get 'type schema) "array")
                            (agentel-elicitation--read-choices prompt schema)
                          (agentel-elicitation--read-choice prompt schema))))
            (when (car answer)
              (push (cons key (if (listp (car answer)) (vconcat (car answer))
                                (car answer)))
                    content))
            (when (cdr answer)
              (push (cons (or custom key) (cdr answer)) content))))
         (t (when-let* ((value (agentel-elicitation--read-field key schema)))
              (push (cons key value) content))))))
    (nreverse content)))

(defun agentel-elicitation--ask (item)
  "Fill in the form ITEM from the minibuffer and send the answers."
  (let ((content (agentel-elicitation--read item)))
    (plist-put item :content content)
    (agentel-elicitation--reply
     item `((action . "accept") (content . ,(or content (make-hash-table))))
     'accept)))

;;;; Protocol

(defun agentel-elicitation--session (connection params)
  "Return the session of CONNECTION that PARAMS ask in."
  (or (agentel-session-get (alist-get 'sessionId params))
      (seq-find (lambda (s) (eq (agentel-session-connection s) connection))
                (agentel-session-roots))))

(defun agentel-elicitation--handle (connection id params)
  "Handle the form request ID with PARAMS on CONNECTION."
  (let ((session (agentel-elicitation--session connection params)))
    (if (and session (equal (alist-get 'mode params) "form"))
        (let ((item (list :kind 'elicitation :id id :params params
                          :session session :connection connection)))
          (plist-put item :answer (lambda () (agentel-elicitation--ask item)))
          (agentel-session-add-pending session item)
          (agentel-elicitation--show item))
      (agentel-connection-respond connection id '((action . "decline"))))))

(defun agentel-elicitation--withdraw (connection method params)
  "Drop the form of CONNECTION that the agent withdrew.
METHOD and PARAMS are those of the notification."
  (when (equal method "$/cancel_request")
    (dolist (session (agentel-session-list))
      (dolist (item (agentel-session-pending session))
        (when (and (eq (plist-get item :kind) 'elicitation)
                   (eq (plist-get item :connection) connection)
                   (equal (plist-get item :id) (alist-get 'requestId params)))
          (agentel-elicitation--close item 'withdrawn))))))

(add-hook 'agentel-connection-capability-functions #'agentel-elicitation--capabilities)
(setf (alist-get "elicitation/create" agentel-connection-request-handlers
                 nil nil #'equal)
      #'agentel-elicitation--handle)
(add-hook 'agentel-connection-notification-functions #'agentel-elicitation--withdraw)

(provide 'agentel-elicitation)
;;; agentel-elicitation.el ends here
