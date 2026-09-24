;;; agentel-chat.el --- Session buffers for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; Each session is shown in its own buffer: a read-only transcript
;; followed by an input area where the prompt is edited like any other
;; text.
;;
;; The transcript is a sequence of entries (messages, thoughts, tool
;; calls, ...).  An entry owns the text carrying its object in the
;; `agentel-chat-entry' property, starting at its start marker.  An
;; entry is changed by rendering it again and replacing exactly that
;; text, and new entries are always inserted at the end of the
;; transcript, so updating an older entry never moves text that belongs
;; to another one.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)

(defgroup agentel nil
  "Agent Client Protocol client."
  :group 'tools
  :prefix "agentel-")

(defface agentel-chat-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of messages written by the user.")

(defface agentel-chat-thought-face
  '((t :inherit shadow :slant italic))
  "Face of the agent's thoughts.")

(defface agentel-chat-tool-face
  '((t :inherit font-lock-function-name-face))
  "Face of tool call titles.")

(defface agentel-chat-tool-body-face
  '((t :inherit shadow))
  "Face of tool call output.")

(defface agentel-chat-notice-face
  '((t :inherit font-lock-comment-face))
  "Face of notices from agentel.")

(defface agentel-chat-error-face
  '((t :inherit error))
  "Face of errors.")

(defface agentel-chat-prompt-face
  '((t :inherit minibuffer-prompt))
  "Face of the prompt in front of the input area.")

(defvar agentel-chat-header-functions nil
  "Functions returning segments of the header line.
Each is called with the session and returns a string or nil.")

(defvar agentel-chat-send-functions nil
  "Abnormal hook run with the session and the input when it is sent.
If a function returns non-nil, it handled the input and it is not
sent to the agent as a prompt.")

(defvar agentel-chat-format-message-function nil
  "Function returning a finished agent message text formatted for display.
It is called with the text once the message is complete, so it never
sees half of a construct, such as a code block, that spans chunks.")

(defvar agentel-chat-prompt-string "❯ "
  "String in front of the input area.")

(defvar-local agentel-chat--session nil
  "Session shown in this buffer.")

(defvar-local agentel-chat--entries nil
  "Hash table of the entries of this buffer by key.")

(defvar-local agentel-chat--last nil
  "Entry at the end of the transcript.")

(defvar-local agentel-chat--transcript-end nil
  "Marker at the end of the transcript.")

(defvar-local agentel-chat--input-start nil
  "Marker at the start of the input area, or nil without one.")

(defvar-local agentel-chat--history nil
  "Inputs sent from this buffer, newest first.")

(defvar-local agentel-chat--history-index nil
  "Position in `agentel-chat--history' while browsing it.")

(cl-defstruct (agentel-chat-entry (:constructor agentel-chat-entry--make)
                                  (:copier nil))
  "A piece of the transcript."
  key type data render start collapsed)

;;;; Keymaps

(defvar-keymap agentel-chat-entry-map
  :doc "Keymap on foldable transcript entries."
  "TAB" #'agentel-chat-toggle
  "RET" #'agentel-chat-toggle
  "<mouse-1>" #'agentel-chat-toggle)

(defvar-keymap agentel-chat-mode-map
  :doc "Keymap of `agentel-chat-mode'."
  "TAB" #'completion-at-point
  "C-c C-c" #'agentel-chat-send
  "C-c C-k" #'agentel-chat-cancel
  "C-c C-a" #'agentel-chat-answer
  "C-c C-i" #'agentel-chat-goto-input
  "M-p" #'agentel-chat-previous-input
  "M-n" #'agentel-chat-next-input)

;;;; Entries

(defmacro agentel-chat--with-transcript (&rest body)
  "Run BODY, which changes the read-only transcript.
The change is not recorded for undo.  Undo records positions, and
recorded input edits would point at the wrong text after the
transcript grows above them, so the undo history is dropped."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     (prog1 (let ((buffer-undo-list t)) ,@body)
       (unless (eq buffer-undo-list t)
         (setq buffer-undo-list nil)))))

(defun agentel-chat--entry-end (entry)
  "Return the position after the text of ENTRY."
  (next-single-property-change (agentel-chat-entry-start entry)
                               'agentel-chat-entry nil
                               (marker-position agentel-chat--transcript-end)))

(defun agentel-chat--propertize (entry string)
  "Return STRING marked as text of ENTRY."
  (let ((string (copy-sequence string)))
    (add-text-properties 0 (length string)
                         (list 'agentel-chat-entry entry
                               'read-only t
                               'rear-nonsticky t
                               'front-sticky '(read-only))
                         string)
    string))

(defun agentel-chat--entry-string (entry at)
  "Return the text of ENTRY when it starts at position AT."
  (agentel-chat--propertize
   entry
   (concat (if (> at (point-min)) "\n\n" "")
           (funcall (agentel-chat-entry-render entry) entry))))

(defun agentel-chat-entry-get (entry key)
  "Return the value ENTRY stores under KEY."
  (alist-get key (agentel-chat-entry-data entry)))

(gv-define-setter agentel-chat-entry-get (value entry key)
  `(setf (alist-get ,key (agentel-chat-entry-data ,entry)) ,value))

(defun agentel-chat-find (key)
  "Return the entry stored under KEY in this buffer."
  (gethash key agentel-chat--entries))

(defun agentel-chat-add (key type render &optional data collapsed)
  "Append an entry to the transcript of this buffer and return it.
KEY identifies the entry for `agentel-chat-find', TYPE is a symbol
naming its kind, RENDER a function returning its text from the entry,
DATA its initial alist and COLLAPSED its initial folding."
  (agentel-chat-finish-message)
  (let ((entry (agentel-chat-entry--make :key key :type type :render render
                                         :data data :collapsed collapsed)))
    (agentel-chat--with-transcript
      (save-excursion
        (goto-char agentel-chat--transcript-end)
        ;; The gap in front of the prompt is hidden while nothing was said.
        (when (and agentel-chat--input-start (= (point) (point-min)))
          (remove-text-properties (point) (+ (point) 2) '(invisible nil)))
        (setf (agentel-chat-entry-start entry) (copy-marker (point)))
        (insert (agentel-chat--entry-string entry (point)))))
    (when key (puthash key entry agentel-chat--entries))
    (setq agentel-chat--last entry)
    entry))

(defun agentel-chat-refresh (entry)
  "Render ENTRY again in place."
  (let* ((start (marker-position (agentel-chat-entry-start entry)))
         (end (agentel-chat--entry-end entry))
         (offset (and (<= start (point)) (< (point) end) (- (point) start))))
    (agentel-chat--with-transcript
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert (agentel-chat--entry-string entry start))))
    (when offset
      (goto-char (min (+ start offset) (agentel-chat--entry-end entry))))))

(defun agentel-chat--append (entry text)
  "Append TEXT to ENTRY, which must be the last entry."
  (agentel-chat--with-transcript
    (save-excursion
      (goto-char agentel-chat--transcript-end)
      (insert (agentel-chat--propertize
               entry
               (propertize text 'face (agentel-chat--text-face entry)))))))

(defun agentel-chat-entry-at (&optional pos)
  "Return the entry at POS, which defaults to point."
  (get-text-property (or pos (point)) 'agentel-chat-entry))

(defun agentel-chat-toggle ()
  "Fold or unfold the entry at point."
  (interactive)
  (when-let* ((entry (agentel-chat-entry-at)))
    (setf (agentel-chat-entry-collapsed entry)
          (not (agentel-chat-entry-collapsed entry)))
    (agentel-chat-refresh entry)))

(defun agentel-chat-notice (session text &optional type)
  "Add the notice TEXT to the buffer of SESSION.
TYPE is `error' for errors."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (agentel-chat-add nil (or type 'notice) #'agentel-chat--render-notice
                        `((text . ,text))))))

;;;; Rendering

(defun agentel-chat--text-face (entry)
  "Return the face of streamed text in ENTRY."
  (pcase (agentel-chat-entry-type entry)
    ('user 'agentel-chat-user-face)
    ('thought 'agentel-chat-thought-face)))

(defun agentel-chat--render-text (entry)
  "Render the text message ENTRY."
  (let ((text (agentel-chat-entry-get entry 'text)))
    (pcase (agentel-chat-entry-type entry)
      ('user (propertize (concat agentel-chat-prompt-string text)
                         'face 'agentel-chat-user-face))
      ('agent (if (and (agentel-chat-entry-get entry 'finished)
                       agentel-chat-format-message-function)
                  (funcall agentel-chat-format-message-function text)
                text))
      (_ (propertize text 'face (agentel-chat--text-face entry))))))

(defun agentel-chat-finish-message ()
  "Mark the agent message at the end of this buffer as complete."
  (let ((entry agentel-chat--last))
    (when (and entry (eq (agentel-chat-entry-type entry) 'agent)
               (not (agentel-chat-entry-get entry 'finished)))
      (setf (agentel-chat-entry-get entry 'finished) t)
      (when agentel-chat-format-message-function
        (agentel-chat-refresh entry)))))

(defun agentel-chat--render-thought (entry)
  "Render the thought ENTRY, folded to its first line when collapsed."
  (let ((text (agentel-chat-entry-get entry 'text)))
    (propertize
     (if (agentel-chat-entry-collapsed entry)
         (concat "▸ Thinking: "
                 (truncate-string-to-width
                  (car (split-string (string-trim text) "\n")) 60 nil nil "…"))
       (concat "▾ Thinking\n" text))
     'face 'agentel-chat-thought-face
     'keymap agentel-chat-entry-map)))

(defun agentel-chat--render-notice (entry)
  "Render the notice ENTRY."
  (propertize (agentel-chat-entry-get entry 'text)
              'face (if (eq (agentel-chat-entry-type entry) 'error)
                        'agentel-chat-error-face
                      'agentel-chat-notice-face)))

(defconst agentel-chat--status-icons
  '(("pending" . "…") ("in_progress" . "⟳") ("completed" . "✓") ("failed" . "✗"))
  "Icons of tool call statuses.")

(defun agentel-chat--content-text (content)
  "Return the displayable text of the tool call CONTENT list."
  (string-join
   (delq nil
         (mapcar
          (lambda (item)
            (pcase (alist-get 'type item)
              ("content" (alist-get 'text (alist-get 'content item)))
              ("diff" (format "%s %s" (if (alist-get 'oldText item) "Edit" "Write")
                              (alist-get 'path item)))
              ("terminal" nil)))
          content))
   "\n"))

(defun agentel-chat--indent (text)
  "Return TEXT with every line indented."
  (replace-regexp-in-string "^" "    " text))

(defun agentel-chat--render-tool (entry)
  "Render the tool call ENTRY."
  (let* ((status (agentel-chat-entry-get entry 'status))
         (icon (or (cdr (assoc status agentel-chat--status-icons)) "…"))
         (body (agentel-chat--content-text (agentel-chat-entry-get entry 'content)))
         (foldable (not (string-empty-p body)))
         (header (concat (cond ((not foldable) "  ")
                               ((agentel-chat-entry-collapsed entry) "▸ ")
                               (t "▾ "))
                         icon " "
                         (propertize (or (agentel-chat-entry-get entry 'title) "Tool")
                                     'face 'agentel-chat-tool-face))))
    (propertize
     (if (or (not foldable) (agentel-chat-entry-collapsed entry))
         header
       (concat header "\n"
               (propertize (agentel-chat--indent (string-trim-right body))
                           'face 'agentel-chat-tool-body-face)))
     'keymap agentel-chat-entry-map)))

(defconst agentel-chat--plan-marks
  '(("completed" . "[x]") ("in_progress" . "[-]") ("pending" . "[ ]"))
  "Check boxes of plan entry statuses.")

(defun agentel-chat--render-plan (entry)
  "Render the plan ENTRY."
  (concat
   (propertize "Plan" 'face 'bold)
   (mapconcat (lambda (item)
                (format "\n  %s %s"
                        (or (cdr (assoc (alist-get 'status item)
                                        agentel-chat--plan-marks))
                            "[ ]")
                        (alist-get 'content item)))
              (agentel-chat-entry-get entry 'entries) "")))

;;;; Updates from the agent

(defun agentel-chat--text-chunk (type update)
  "Show the text chunk UPDATE as part of a message of TYPE."
  (let ((text (alist-get 'text (alist-get 'content update))))
    (when (and text (not (string-empty-p text)))
      (if (and agentel-chat--last
               (eq (agentel-chat-entry-type agentel-chat--last) type))
          (let ((entry agentel-chat--last))
            (setf (agentel-chat-entry-get entry 'text)
                  (concat (agentel-chat-entry-get entry 'text) text))
            (cond ((eq type 'thought) (agentel-chat-refresh entry))
                  ((agentel-chat-entry-get entry 'finished)
                   ;; Show the whole message unformatted until it ends again.
                   (setf (agentel-chat-entry-get entry 'finished) nil)
                   (agentel-chat-refresh entry))
                  (t (agentel-chat--append entry text))))
        (agentel-chat-add nil type
                          (if (eq type 'thought)
                              #'agentel-chat--render-thought
                            #'agentel-chat--render-text)
                          `((text . ,text))
                          (eq type 'thought))))))

(defun agentel-chat--tool (update)
  "Show or update the tool call described by UPDATE."
  (let* ((key (cons 'tool (alist-get 'toolCallId update)))
         (entry (or (agentel-chat-find key)
                    (agentel-chat-add key 'tool #'agentel-chat--render-tool nil t))))
    (dolist (field '(title kind status content rawInput locations))
      (when-let* ((value (alist-get field update)))
        (setf (agentel-chat-entry-get entry field) value)))
    (agentel-chat-refresh entry)))

(defun agentel-chat--plan (update)
  "Show the plan in UPDATE, replacing the previous one."
  (let ((entry (or (agentel-chat-find 'plan)
                   (agentel-chat-add 'plan 'plan #'agentel-chat--render-plan))))
    (setf (agentel-chat-entry-get entry 'entries) (alist-get 'entries update))
    (agentel-chat-refresh entry)))

(defun agentel-chat--on-update (session update)
  "Show UPDATE of SESSION in its buffer."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (pcase (alist-get 'sessionUpdate update)
        ("agent_message_chunk" (agentel-chat--text-chunk 'agent update))
        ("agent_thought_chunk" (agentel-chat--text-chunk 'thought update))
        ("user_message_chunk" (agentel-chat--text-chunk 'user update))
        ((or "tool_call" "tool_call_update") (agentel-chat--tool update))
        ("plan" (agentel-chat--plan update))))))

(add-hook 'agentel-session-update-functions #'agentel-chat--on-update)

;;;; Input

(defun agentel-chat-input ()
  "Return the text of the input area."
  (if agentel-chat--input-start
      (buffer-substring-no-properties agentel-chat--input-start (point-max))
    ""))

(defun agentel-chat--set-input (text)
  "Replace the input area with TEXT."
  (delete-region agentel-chat--input-start (point-max))
  (goto-char (point-max))
  (insert text))

(defun agentel-chat--insert-prompt ()
  "Insert the prompt and the input area after the transcript."
  (agentel-chat--with-transcript
    (goto-char (point-max))
    (let ((end (point)))
      (insert (propertize (concat (propertize "\n\n" 'invisible (= end (point-min)))
                                  agentel-chat-prompt-string)
                          'face 'agentel-chat-prompt-face
                          'read-only t
                          'front-sticky '(read-only)
                          'rear-nonsticky t
                          'field 'agentel-chat-prompt))
      ;; The transcript grows in front of the prompt, not after it.
      (set-marker agentel-chat--transcript-end end))
    (setq agentel-chat--input-start (point-marker))))

(defun agentel-chat--finish-turn (session)
  "Record that the turn of SESSION ended."
  (agentel-session-set-busy session nil)
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (agentel-chat-finish-message))))

(defun agentel-chat--prompt (session text)
  "Send TEXT to the agent as a prompt of SESSION."
  (agentel-session-set-busy session t)
  (agentel-connection-request
   (agentel-session-connection session) "session/prompt"
   `((sessionId . ,(agentel-session-id session))
     (prompt . [((type . "text") (text . ,text))]))
   :on-success
   (lambda (result)
     (agentel-chat--finish-turn session)
     (let ((reason (alist-get 'stopReason result)))
       (unless (member reason '("end_turn" nil))
         (agentel-chat-notice session (format "Turn ended: %s" reason)))))
   :on-failure
   (lambda (error)
     (agentel-chat--finish-turn session)
     (agentel-chat-notice session
                          (format "Prompt failed: %s" (alist-get 'message error))
                          'error))))

(defun agentel-chat-send ()
  "Send the input to the agent."
  (interactive)
  (let ((session agentel-chat--session)
        (text (string-trim (agentel-chat-input))))
    (unless agentel-chat--input-start
      (user-error "This session does not take input"))
    (when (agentel-session-ended session)
      (user-error "This session has ended"))
    (unless (string-empty-p text)
      (push text agentel-chat--history)
      (setq agentel-chat--history-index nil)
      (agentel-chat--set-input "")
      (unless (run-hook-with-args-until-success 'agentel-chat-send-functions
                                                session text)
        (agentel-chat-add nil 'user #'agentel-chat--render-text `((text . ,text)))
        (agentel-chat--prompt session text)))))

(defun agentel-chat-cancel ()
  "Ask the agent to stop the current turn."
  (interactive)
  (let ((session agentel-chat--session))
    (agentel-connection-notify (agentel-session-connection session)
                               "session/cancel"
                               `((sessionId . ,(agentel-session-id session))))))

(defun agentel-chat-previous-input (n)
  "Replace the input with the Nth previous sent input."
  (interactive "p")
  (when agentel-chat--history
    (setq agentel-chat--history-index
          (max 0 (min (1- (length agentel-chat--history))
                      (+ (or agentel-chat--history-index -1) n))))
    (agentel-chat--set-input (nth agentel-chat--history-index
                                  agentel-chat--history))))

(defun agentel-chat-next-input (n)
  "Replace the input with the Nth next sent input."
  (interactive "p")
  (if (and agentel-chat--history-index
           (>= (- agentel-chat--history-index n) 0))
      (agentel-chat-previous-input (- n))
    (setq agentel-chat--history-index nil)
    (agentel-chat--set-input "")))

(defun agentel-chat-ordered-completion (candidates)
  "Return a completion table of CANDIDATES that keeps their order."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string predicate))))

(defun agentel-chat-answer ()
  "Answer the oldest question of this session or of its subagents.
Questions are the pending items of sessions, plists whose :answer is a
command that asks the user and replies to the agent."
  (interactive)
  (let ((pending (agentel-session-pending-items agentel-chat--session)))
    (unless pending
      (user-error "No question is waiting for an answer"))
    (funcall (plist-get (cdar pending) :answer))))

(defun agentel-chat-goto-input ()
  "Move point to the end of the input area."
  (interactive)
  (goto-char (point-max)))

;;;; Mode

(defun agentel-chat--header-line ()
  "Return the header line of this buffer."
  (let ((session agentel-chat--session))
    (string-join
     (delq nil (cons (format "%s [%s]" (agentel-session-name session)
                             (agentel-session-state session))
                     (mapcar (lambda (f) (funcall f session))
                             agentel-chat-header-functions)))
     "  │  ")))

(define-derived-mode agentel-chat-mode text-mode "agentel"
  "Major mode of agentel session buffers.
The transcript is read-only; type the prompt at the end of the buffer
and send it with \\[agentel-chat-send]."
  (setq-local agentel-chat--entries (make-hash-table :test 'equal))
  (setq-local agentel-chat--transcript-end (point-min-marker))
  (set-marker-insertion-type agentel-chat--transcript-end t)
  ;; The result of :eval is itself a mode line format, where % is special.
  (setq-local header-line-format
              '(:eval (string-replace "%" "%%" (agentel-chat--header-line)))))

(defun agentel-chat--on-changed (session)
  "Redisplay the header line of SESSION's buffer."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (force-mode-line-update))))

(add-hook 'agentel-session-changed-functions #'agentel-chat--on-changed)

(defun agentel-chat--buffer-name (session)
  "Return a buffer name for SESSION."
  (generate-new-buffer-name
   (format "*agentel: %s*" (agentel-session-name session))))

(cl-defun agentel-chat-open (session &key input)
  "Create and return the buffer of SESSION.
With INPUT, the buffer has an input area for prompts."
  (let ((buffer (get-buffer-create (agentel-chat--buffer-name session))))
    (with-current-buffer buffer
      (agentel-chat-mode)
      (setq agentel-chat--session session)
      (when input (agentel-chat--insert-prompt))
      (unless input (setq buffer-read-only t)))
    (setf (agentel-session-buffer session) buffer)
    buffer))

(provide 'agentel-chat)
;;; agentel-chat.el ends here
