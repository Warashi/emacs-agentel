;;; agentel-elicitation.el --- Form elicitation for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'agentel-conversation)
(require 'agentel-chat)

(defface agentel-elicitation-face
  '((t :inherit warning))
  "Face of questions from the agent."
  :group 'agentel)

(defun agentel-elicitation--capabilities ()
  "Return the client capability of answering forms."
  '((elicitation . ((form . nil)))))

;;;; Form

(defvar agentel-elicitation--requests (make-hash-table :test 'eq)
  "Requests waiting for an answer by the items that show them.
Each is a plist of the :connection and :id to answer, the :custom keys
under which to send free text by the keys of their fields, and the
:session whose conversation holds the item.")

(defun agentel-elicitation--render-field (field)
  "Render FIELD of a form."
  (let-alist field
    (concat
     "\n  "
     (propertize .label 'face 'bold)
     (if .description (concat " — " .description) "")
     (if .multiple " (any number)" "")
     (mapconcat (lambda (option)
                  (let-alist option
                    (concat "\n    • " .label
                            (if .description (concat " — " .description) ""))))
                .options ""))))

(defun agentel-elicitation--summary (fields values others)
  "Return the answers to a form of FIELDS as one line.
VALUES and OTHERS are the answers as in `agentel-elicitation--update'."
  (string-join
   (mapcan
    (lambda (field)
      (let-alist field
        (delq nil
              (list
               (when-let* ((value (assq .key values)))
                 (format "%s: %s" .label
                         (if (consp (cdr value))
                             (mapconcat (lambda (v) (format "%s" v)) (cdr value) ", ")
                           (cdr value))))
               (when-let* ((text (alist-get .key others)))
                 (format "%s: %s" .label text))))))
    fields)
   "; "))

(defun agentel-elicitation--render (entry _options)
  "Render the form ENTRY."
  (let-alist (agentel-store-model-data entry)
    (if (or .answered .declined .withdrawn)
        (propertize (format "? %s → %s" .question
                            (cond (.answered (agentel-elicitation--summary
                                              .fields .values .others))
                                  (.declined "declined")
                                  (t "withdrawn")))
                    'face 'agentel-chat-notice-face)
      (concat
       (propertize (concat "? " .question) 'face 'agentel-elicitation-face)
       (mapconcat #'agentel-elicitation--render-field .fields "")
       "\n  "
       (buttonize "[Answer]" (lambda (_) (agentel-elicitation--ask entry)))
       " "
       (buttonize "[Decline]" (lambda (_) (agentel-elicitation-decline entry)))))))

(defun agentel-elicitation--update (message data)
  "Return the DATA of a form changed by MESSAGE.
MESSAGE is (ask QUESTION FIELDS) when the agent asks, (answer VALUES
OTHERS) when the user answers, (decline) when the user declines and
\(withdraw) when the agent no longer asks.

Each field has its `key', its `label', maybe a `description', its
`kind', one of `choice', `boolean', `number' and `text', `multiple'
when it takes any number of answers, and the `options' of a choice,
each with its `label', its `value' and maybe a `description'.
VALUES maps the keys of fields to their answers, a list of values for
a multiple choice, and OTHERS maps the keys of choices to text typed
that matches no option."
  (pcase message
    (`(ask ,question ,fields)
     `((question . ,question) (fields . ,fields) (waiting . t)))
    (`(answer ,values ,others)
     (let-alist data
       `((question . ,.question) (fields . ,.fields)
         (answered . t) (values . ,values) (others . ,others))))
    ('(decline)
     (let-alist data `((question . ,.question) (fields . ,.fields) (declined . t))))
    ('(withdraw)
     (let-alist data `((question . ,.question) (fields . ,.fields) (withdrawn . t))))))

(agentel-conversation-define 'elicitation #'agentel-elicitation--update)
(agentel-ui-define-view 'elicitation #'agentel-elicitation--render)

;;;; Answering

(defun agentel-elicitation--read-choice (prompt options)
  "Read one of OPTIONS with PROMPT.
Return (VALUE . nil) for an option, (nil . TEXT) for other text and
nil when skipped."
  (let* ((labels (mapcar (lambda (o) (alist-get 'label o)) options))
         (answer (string-trim
                  (completing-read prompt
                                   (agentel-chat-ordered-completion labels)))))
    (cond ((string-empty-p answer) nil)
          ((member answer labels)
           (cons (alist-get 'value (nth (seq-position labels answer) options)) nil))
          (t (cons nil answer)))))

(defun agentel-elicitation--read-choices (prompt options)
  "Read any number of OPTIONS with PROMPT.
Return (VALUES . TEXT) where TEXT joins answers matching no option."
  (let* ((labels (mapcar (lambda (o) (alist-get 'label o)) options))
         (answers (completing-read-multiple prompt labels))
         values others)
    (dolist (answer answers)
      (if (member answer labels)
          (push (alist-get 'value (nth (seq-position labels answer) options)) values)
        (push answer others)))
    (cons (nreverse values)
          (and others (string-join (nreverse others) ", ")))))

(defun agentel-elicitation--read-field (field)
  "Read the value of the plain FIELD, or nil to skip it."
  (let ((prompt (format "%s: " (alist-get 'label field))))
    (pcase (alist-get 'kind field)
      ('boolean (if (y-or-n-p prompt) t :false))
      ('number (read-number prompt))
      (_ (let ((text (string-trim (read-string prompt))))
           (unless (string-empty-p text) text))))))

(defun agentel-elicitation--read (fields)
  "Read the answers to FIELDS and return (VALUES . OTHERS).
VALUES and OTHERS are as in `agentel-elicitation--update'."
  (let (values others)
    (dolist (field fields)
      (let-alist field
        (if (eq .kind 'choice)
            (let* ((prompt (format "%s%s: " .label
                                   (if .description (format " (%s)" .description) "")))
                   (answer (if .multiple
                               (agentel-elicitation--read-choices prompt .options)
                             (agentel-elicitation--read-choice prompt .options))))
              (when (car answer) (push (cons .key (car answer)) values))
              (when (cdr answer) (push (cons .key (cdr answer)) others)))
          (when-let* ((value (agentel-elicitation--read-field field)))
            (push (cons .key value) values)))))
    (cons (nreverse values) (nreverse others))))

(defun agentel-elicitation--ask (item)
  "Fill in the form ITEM from the minibuffer and send the answers."
  (let ((answers (agentel-elicitation--read (agentel-store-get item 'fields))))
    (agentel-elicitation-answer item (car answers) (cdr answers))))

(agentel-chat-define-answer 'elicitation #'agentel-elicitation--ask)

;;;; Protocol

(defun agentel-elicitation--custom-for (property)
  "Return the key of the choice whose custom answer PROPERTY is, or nil."
  (when-let* ((meta (alist-get '_askUserQuestionCustomAnswer
                               (alist-get '_meta (cdr property))))
              (question (alist-get 'questionId meta)))
    (intern question)))

(defun agentel-elicitation--options (schema)
  "Return the options of the choice SCHEMA."
  (let ((items (or (alist-get 'oneOf schema)
                   (alist-get 'anyOf (alist-get 'items schema))))
        (plain (or (alist-get 'enum schema)
                   (alist-get 'enum (alist-get 'items schema)))))
    (if items
        (mapcar (lambda (o)
                  (seq-filter
                   #'cdr
                   `((label . ,(or (alist-get 'title o)
                                   (format "%s" (alist-get 'const o))))
                     (value . ,(alist-get 'const o))
                     (description . ,(alist-get 'description o)))))
                items)
      (mapcar (lambda (v) `((label . ,(format "%s" v)) (value . ,v))) plain))))

(defun agentel-elicitation--field (property)
  "Return the field asked by the schema PROPERTY, a (KEY . SCHEMA)."
  (let* ((schema (cdr property))
         (options (agentel-elicitation--options schema)))
    (seq-filter
     #'cdr
     `((key . ,(car property))
       (label . ,(or (alist-get 'title schema) (symbol-name (car property))))
       (description . ,(alist-get 'description schema))
       (kind . ,(if options 'choice
                  (pcase (alist-get 'type schema)
                    ("boolean" 'boolean)
                    ((or "number" "integer") 'number)
                    (_ 'text))))
       (multiple . ,(equal (alist-get 'type schema) "array"))
       (options . ,options)))))

(defun agentel-elicitation--content (request fields values others)
  "Return the content of the answer to REQUEST with FIELDS.
VALUES and OTHERS are as in `agentel-elicitation--update'."
  (let (content)
    (dolist (field fields)
      (let ((key (alist-get 'key field)))
        (when-let* ((value (assq key values)))
          (push (cons key (if (consp (cdr value)) (vconcat (cdr value)) (cdr value)))
                content))
        (when-let* ((text (alist-get key others)))
          (push (cons (or (alist-get key (plist-get request :custom)) key) text)
                content))))
    (or (nreverse content) (make-hash-table))))

(defun agentel-elicitation--close (item message)
  "Stop waiting for an answer to ITEM and send MESSAGE to it."
  (let ((request (gethash item agentel-elicitation--requests)))
    (remhash item agentel-elicitation--requests)
    (agentel-conversation-send (plist-get request :session)
                               (agentel-store-model-key item)
                               'elicitation message)))

(defun agentel-elicitation--reply (item result message)
  "Answer the form ITEM with RESULT and send MESSAGE to it."
  (when-let* ((request (gethash item agentel-elicitation--requests)))
    (agentel-connection-respond (plist-get request :connection)
                                (plist-get request :id)
                                result)
    (agentel-elicitation--close item message)))

(defun agentel-elicitation-answer (item values others)
  "Answer the form ITEM with VALUES and OTHERS.
They are as in `agentel-elicitation--update'."
  (when-let* ((request (gethash item agentel-elicitation--requests)))
    (agentel-elicitation--reply
     item
     `((action . "accept")
       (content . ,(agentel-elicitation--content
                    request (agentel-store-get item 'fields) values others)))
     `(answer ,values ,others))))

(defun agentel-elicitation-decline (item)
  "Decline to answer the form ITEM."
  (agentel-elicitation--reply item '((action . "decline")) '(decline)))

(defun agentel-elicitation--session (connection params)
  "Return the session of CONNECTION that PARAMS ask in."
  (or (agentel-session-get (alist-get 'sessionId params) connection)
      (seq-find (lambda (s) (eq (agentel-session-connection s) connection))
                (agentel-session-roots))))

(defun agentel-elicitation--handle (connection id params)
  "Handle the form request ID with PARAMS on CONNECTION."
  (let ((session (agentel-elicitation--session connection params)))
    (if (and session (equal (alist-get 'mode params) "form"))
        (let* ((properties (alist-get 'properties (alist-get 'requestedSchema params)))
               (custom (delq nil (mapcar (lambda (p)
                                           (when-let* ((key (agentel-elicitation--custom-for p)))
                                             (cons key (car p))))
                                         properties)))
               (item (agentel-conversation-send
                      session (cons 'elicitation id) 'elicitation
                      `(ask ,(or (alist-get 'message params) "Question")
                            ,(mapcar #'agentel-elicitation--field
                                     (seq-remove #'agentel-elicitation--custom-for
                                                 properties)))))
               (request (list :connection connection :id id :session session
                              :custom custom)))
          (puthash item request agentel-elicitation--requests))
      (agentel-connection-respond connection id '((action . "decline"))))))

(defun agentel-elicitation--withdraw (connection method params)
  "Drop the form of CONNECTION that the agent withdrew.
METHOD and PARAMS are those of the notification."
  (when (equal method "$/cancel_request")
    (let (withdrawn)
      (maphash (lambda (item request)
                 (when (and (eq (plist-get request :connection) connection)
                            (equal (plist-get request :id)
                                   (alist-get 'requestId params)))
                   (push item withdrawn)))
               agentel-elicitation--requests)
      (dolist (item withdrawn)
        (agentel-elicitation--close item '(withdraw))))))

(defun agentel-elicitation--forget (session)
  "Forget the forms of SESSION once it is removed from the registry."
  (unless (memq session (agentel-session-list))
    (let (gone)
      (maphash (lambda (item request)
                 (when (eq (plist-get request :session) session)
                   (push item gone)))
               agentel-elicitation--requests)
      (dolist (item gone)
        (remhash item agentel-elicitation--requests)))))

(add-hook 'agentel-connection-capability-functions #'agentel-elicitation--capabilities)
(setf (alist-get "elicitation/create" agentel-connection-request-handlers
                 nil nil #'equal)
      #'agentel-elicitation--handle)
(add-hook 'agentel-connection-notification-functions #'agentel-elicitation--withdraw)
(add-hook 'agentel-session-changed-functions #'agentel-elicitation--forget)

(provide 'agentel-elicitation)
;;; agentel-elicitation.el ends here
