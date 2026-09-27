;;; agentel-chat.el --- Session buffers for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each session is shown in its own buffer: a read-only transcript
;; followed by an input area where the prompt is edited like any other
;; text.
;;
;; The transcript shows the conversation of the session, whose items
;; (messages, thoughts, tool calls, ...) are its entries; see
;; `agentel-conversation'.  The conversation outlives the buffer showing
;; it, and the buffer subscribes to it.  The entries are drawn in a
;; region of `agentel-ui-region' in front of the prompt, which changes
;; only the text of the entries that changed.  The text of an entry
;; carries its item in the `agentel-chat-entry' property.
;;
;; The header line shows the models of the session store whose views
;; (`agentel-ui-define-view') have the :header property, a number
;; ordering them, after the state, project and name of the session.
;; The lines pinned above the prompt show those with the :pin property
;; the same way.  Both are made again when the store changes, and the
;; views are shown with the options of `agentel-chat--view-options' for
;; what no model keeps.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-turn)
(require 'agentel-conversation)
(require 'agentel-store)
(require 'agentel-ui)
(require 'agentel-ui-region)

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

(defvar agentel-chat-send-functions nil
  "Abnormal hook run with the session and the input when it is sent.
If a function returns non-nil, it handled the input and it is not
sent to the agent as a prompt.")

(defvar agentel-chat-format-message-function nil
  "Function returning a finished agent message text formatted for display.
It is called with the text once the message is complete, so it never
sees half of a construct, such as a code block, that spans chunks.")

(defvar-local agentel-chat-transcript-function #'identity
  "Function choosing the entries the transcript of this buffer shows.
It is called with the items of the conversation, oldest first, and
returns those to show in the order to show them.  After changing it,
call `agentel-chat-render'.")

(defvar agentel-chat-prompt-string "❯ "
  "String in front of the input area.")

(defvar-local agentel-chat--session nil
  "Session shown in this buffer.")

(defvar-local agentel-chat--region nil
  "Region of `agentel-ui-region' showing the transcript.")

(defvar-local agentel-chat--transcript-end nil
  "Marker at the end of the transcript.")

(defvar-local agentel-chat--input-start nil
  "Marker at the start of the input area, or nil without one.")

(defvar-local agentel-chat--header-line ""
  "Header line of this buffer, with the % signs of the mode line escaped.")

(defvar-local agentel-chat--pin nil
  "Overlay on the prompt that shows the pinned lines, or nil without one.")

(defvar-local agentel-chat--history nil
  "Inputs sent from this buffer, newest first.")

(defvar-local agentel-chat--history-index nil
  "Position in `agentel-chat--history' while browsing it.")

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

(defun agentel-chat--fit-gap ()
  "Hide the gap in front of the prompt while the transcript is empty."
  (when agentel-chat--input-start
    (let* ((end (marker-position agentel-chat--transcript-end))
           (empty (= end (point-min))))
      (unless (eq (get-text-property end 'invisible) empty)
        (agentel-chat--with-transcript
          (put-text-property end (+ end 2) 'invisible empty))))))

(defun agentel-chat-render ()
  "Show the conversation of the session of this buffer in its transcript.
The entries shown are those `agentel-chat-transcript-function' chooses.
Before the buffer shows a session, nothing is shown."
  (when agentel-chat--session
    (agentel-ui-region-render agentel-chat--region
                              (funcall agentel-chat-transcript-function
                                       (agentel-conversation-items agentel-chat--session)))
    (agentel-chat--fit-gap)))

(defun agentel-chat--show (session)
  "Show SESSION in this buffer and follow its changes.
The transcript follows its conversation, and the header line and the
pinned lines its models."
  (agentel-chat-render)
  (agentel-chat--draw)
  (let* ((buffer (current-buffer))
         (store (agentel-session-store session))
         (subscriber (lambda (_entry _added)
                       (with-current-buffer buffer
                         (agentel-chat-render))))
         (draw (lambda (_model _added)
                 (with-current-buffer buffer
                   (agentel-chat--draw))))
         (unsubscribe (lambda ()
                        (agentel-conversation-unsubscribe session subscriber)
                        (agentel-store-unsubscribe store draw))))
    (agentel-conversation-subscribe session subscriber)
    (agentel-store-subscribe store draw)
    ;; Changing the major mode forgets the entries of the buffer.
    (add-hook 'change-major-mode-hook unsubscribe nil t)
    (add-hook 'kill-buffer-hook unsubscribe nil t)))

(defun agentel-chat-entry-at (&optional pos)
  "Return the entry at POS, which defaults to point."
  (agentel-ui-region-model-at agentel-chat--region (or pos (point))))

(defun agentel-chat-toggle ()
  "Fold or unfold the entry at point."
  (interactive)
  (when-let* ((entry (agentel-chat-entry-at)))
    (agentel-ui-region-toggle agentel-chat--region entry)))

;;;; Rendering

(defun agentel-chat--render-text (entry _options)
  "Render the text message ENTRY."
  (let ((text (agentel-store-get entry 'text)))
    (pcase (agentel-store-model-type entry)
      ('user (propertize (concat agentel-chat-prompt-string text)
                         'face 'agentel-chat-user-face))
      ('agent (if (and (agentel-store-get entry 'finished)
                       agentel-chat-format-message-function)
                  (funcall agentel-chat-format-message-function text)
                text)))))

(agentel-ui-define-view 'user #'agentel-chat--render-text)

(agentel-ui-define-view 'agent #'agentel-chat--render-text)

(defun agentel-chat--render-thought (entry options)
  "Render the thought ENTRY, folded to its first line when OPTIONS say so.
The folded line fits in the :width of OPTIONS."
  (let ((text (agentel-store-get entry 'text)))
    (propertize
     (if (plist-get options :collapsed)
         (agentel-ui-one-line "▸ Thinking: " text (plist-get options :width))
       (concat "▾ Thinking\n" text))
     'face 'agentel-chat-thought-face
     'keymap agentel-chat-entry-map)))

(agentel-ui-define-view 'thought #'agentel-chat--render-thought :collapsed t)

(defun agentel-chat--render-notice (entry _options)
  "Render the notice ENTRY."
  (propertize (agentel-store-get entry 'text)
              'face (if (eq (agentel-store-model-type entry) 'error)
                        'agentel-chat-error-face
                      'agentel-chat-notice-face)))

(dolist (type '(notice error stop))
  (agentel-ui-define-view type #'agentel-chat--render-notice))

(defconst agentel-chat--status-icons
  '((pending . "…") (running . "⟳") (done . "✓") (failed . "✗"))
  "Icons of tool call statuses.")

(defun agentel-chat--indent (text)
  "Return TEXT with every line indented."
  (replace-regexp-in-string "^" "    " text))

(defun agentel-chat--render-tool (entry options)
  "Render the tool call ENTRY, folded when OPTIONS say so.
The header is one line in the :width of OPTIONS, led by why the tool
is called when the agent tells it; a title cut short there shows in
full on top of the output."
  (let* ((status (agentel-store-get entry 'status))
         (collapsed (plist-get options :collapsed))
         (icon (or (alist-get status agentel-chat--status-icons) "…"))
         (title (propertize (or (agentel-store-get entry 'title) "Tool")
                            'face 'agentel-chat-tool-face))
         (why (agentel-store-get entry 'why))
         (summary (if (and why (not (equal why title)))
                      (concat (propertize (concat why " — ") 'face 'agentel-chat-tool-face)
                              title)
                    title))
         (line (lambda (marker)
                 (agentel-ui-one-line (concat marker icon " ") summary
                                      (plist-get options :width))))
         (cut (not (equal (funcall line "  ")
                          (concat "  " icon " " (string-trim summary)))))
         (output (or (agentel-store-get entry 'output) ""))
         (body (string-join (delete "" (list (if cut title "") output)) "\n"))
         (foldable (not (string-empty-p body)))
         (header (funcall line (cond ((not foldable) "  ")
                                     (collapsed "▸ ")
                                     (t "▾ ")))))
    (propertize
     (if (or (not foldable) collapsed)
         header
       (concat header "\n"
               (propertize (agentel-chat--indent (string-trim-right body))
                           'face 'agentel-chat-tool-body-face)))
     'keymap agentel-chat-entry-map)))

(agentel-ui-define-view 'tool #'agentel-chat--render-tool :collapsed t)

(defconst agentel-chat--plan-marks
  '((done . "[x]") (running . "[-]") (pending . "[ ]"))
  "Check boxes of plan step statuses.")

(defun agentel-chat--render-plan (entry _options)
  "Render the plan ENTRY."
  (concat
   (propertize "Plan" 'face 'bold)
   (mapconcat (lambda (step)
                (format "\n  %s %s"
                        (or (alist-get (alist-get 'status step)
                                       agentel-chat--plan-marks)
                            "[ ]")
                        (alist-get 'content step)))
              (agentel-store-get entry 'steps) "")))

(agentel-ui-define-view 'plan #'agentel-chat--render-plan)

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
      (insert (propertize (concat "\n\n" agentel-chat-prompt-string)
                          'face 'agentel-chat-prompt-face
                          'read-only t
                          'front-sticky '(read-only)
                          'rear-nonsticky t
                          'field 'agentel-chat-prompt))
      ;; The transcript grows in front of the prompt, not after it.
      (set-marker agentel-chat--transcript-end end))
    ;; Nothing is ever inserted into the prompt, so an overlay on it
    ;; stays between the transcript and the input.
    (setq agentel-chat--pin
          (make-overlay (- (point) (length agentel-chat-prompt-string)) (point)))
    (setq agentel-chat--input-start (point-marker))))

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
        (agentel-turn-prompt session text)))))

(defun agentel-chat-cancel ()
  "Ask the agent to stop the current turn."
  (interactive)
  (agentel-turn-cancel agentel-chat--session))

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

(defvar agentel-chat--answers nil
  "Alist of item types and the functions that answer them.")

(defun agentel-chat-define-answer (type function)
  "Answer the items of TYPE with FUNCTION when the user asks to.
FUNCTION is called with the item and asks the user how to answer it."
  (declare (indent 1))
  (setf (alist-get type agentel-chat--answers) function))

(defun agentel-chat--questions (session)
  "Return the questions of SESSION and of its subagents, oldest session first.
They are the items waiting for an answer that can be answered."
  (append (seq-filter (lambda (item)
                        (and (agentel-store-get item 'waiting)
                             (alist-get (agentel-store-model-type item)
                                        agentel-chat--answers)))
                      (agentel-conversation-items session))
          (mapcan #'agentel-chat--questions (agentel-session-children session))))

(defun agentel-chat-answer ()
  "Answer the oldest question of this session or of its subagents."
  (interactive)
  (let ((question (car (agentel-chat--questions agentel-chat--session))))
    (unless question
      (user-error "No question is waiting for an answer"))
    (funcall (alist-get (agentel-store-model-type question) agentel-chat--answers)
             question)))

(defun agentel-chat-goto-input ()
  "Move point to the end of the input area."
  (interactive)
  (goto-char (point-max)))

;;;; Mode

(defun agentel-chat--view-options (session)
  "Return the options the models of SESSION are shown with in its buffer.
They carry what no model keeps: the :state of SESSION, the :project it
works in, its directory :cwd and the line :width of this buffer."
  (list :state (agentel-session-state session)
        :project (agentel-session-project-name session)
        :cwd (agentel-session-cwd session)
        :width (agentel-ui-line-width)))

(defun agentel-chat--header-lead (model options)
  "Return the start of the header line of a session whose model is MODEL.
It shows the :state and :project of OPTIONS and the name of the
session from the title of MODEL, which is nil before the session has
one, or from the :cwd of OPTIONS."
  (format "%s %s  │  %s"
          (agentel-ui-state (plist-get options :state))
          (plist-get options :project)
          (agentel-session-name-from (and model (agentel-store-get model 'title))
                                     (plist-get options :cwd))))

(defun agentel-chat--placed (store property)
  "Return the models of STORE whose views have PROPERTY, ordered by it."
  (let ((place (lambda (model)
                 (agentel-ui-view-property (agentel-store-model-type model) property))))
    (seq-sort-by place #'< (seq-filter place (agentel-store-models store)))))

(defun agentel-chat--header (store options)
  "Return the header line of a session with the models of STORE.
It leads with the state, project and name of the session, followed by
the models whose views have the :header property, lowest first, shown
with OPTIONS.  A view returning nil is left out."
  (string-join
   (delq nil (cons (agentel-chat--header-lead (agentel-store-find store 'state) options)
                   (mapcar (lambda (model) (apply #'agentel-ui-view model options))
                           (agentel-chat--placed store :header))))
   "  │  "))

(defun agentel-chat--pinned (store options)
  "Return the lines pinned above the prompt of a session with the models of STORE.
They are the texts of the models whose views have the :pin property,
lowest first, shown with OPTIONS, each line ending in a newline, or
nil without any.  A view returning nil or an empty text is left out."
  (when-let* ((texts (delete "" (delq nil (mapcar (lambda (model)
                                                    (apply #'agentel-ui-view model options))
                                                  (agentel-chat--placed store :pin))))))
    (concat (string-join texts "\n") "\n")))

(defun agentel-chat--draw-header (options)
  "Make the header line of this buffer again, its models shown with OPTIONS."
  (let ((line (string-replace "%" "%%"
                              (agentel-chat--header
                               (agentel-session-store agentel-chat--session) options))))
    (unless (equal-including-properties line agentel-chat--header-line)
      (setq agentel-chat--header-line line)
      (force-mode-line-update))))

(defun agentel-chat--draw-pin (options)
  "Make the pinned lines of this buffer again, their models shown with OPTIONS."
  (when agentel-chat--pin
    (let ((string (agentel-chat--pinned (agentel-session-store agentel-chat--session)
                                        options)))
      ;; Setting an equal string still makes the window redisplay.
      (unless (equal string (overlay-get agentel-chat--pin 'before-string))
        (overlay-put agentel-chat--pin 'before-string string)))))

(defun agentel-chat--draw ()
  "Make the header line and the pinned lines of this buffer again."
  (when agentel-chat--session
    (let ((options (agentel-chat--view-options agentel-chat--session)))
      (agentel-chat--draw-header options)
      (agentel-chat--draw-pin options))))

(define-derived-mode agentel-chat-mode text-mode "agentel"
  "Major mode of agentel session buffers.
The transcript is read-only; type the prompt at the end of the buffer
and send it with \\[agentel-chat-send]."
  (setq-local agentel-chat--region (agentel-ui-region-create (point-min) 'agentel-chat-entry))
  (setq-local agentel-chat--transcript-end (agentel-ui-region-end agentel-chat--region))
  ;; The header line is made when the session changes, not on every
  ;; redisplay.  The result of :eval is itself a mode line format, where
  ;; % is special.
  (setq-local header-line-format '(:eval agentel-chat--header-line))
  (agentel-ui-follow-width #'agentel-chat--fit-width))

(defun agentel-chat--fit-width ()
  "Render the entries and pinned lines that fit the line width again."
  (agentel-chat-render)
  (agentel-chat--draw))

(defun agentel-chat--on-changed (session)
  "Show the state of SESSION again in the header line of its buffer.
The state also changes when the session starts or stops waiting,
which no model keeps."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when agentel-chat--session
        (agentel-chat--draw-header (agentel-chat--view-options session))))))

(add-hook 'agentel-session-changed-functions #'agentel-chat--on-changed)

(defun agentel-chat--buffer-name (session)
  "Return a buffer name for SESSION, from its project if it has one."
  (generate-new-buffer-name
   (format "*agentel: %s*" (or (agentel-session-project session)
                               (agentel-session-name session)))))

(cl-defun agentel-chat-open (session &key input)
  "Create and return the buffer of SESSION.
With INPUT, the buffer has an input area for prompts."
  (let ((buffer (get-buffer-create (agentel-chat--buffer-name session))))
    (with-current-buffer buffer
      (agentel-chat-mode)
      (setq agentel-chat--session session)
      (when input (agentel-chat--insert-prompt))
      (agentel-chat--show session)
      (unless input (setq buffer-read-only t)))
    (setf (agentel-session-buffer session) buffer)
    buffer))

(defun agentel-chat-buffer (session)
  "Return the live buffer of SESSION, making a new one if it was killed.
Only a subagent outlives its buffer, so the new one has no input area."
  (let ((buffer (agentel-session-buffer session)))
    (if (buffer-live-p buffer)
        buffer
      (agentel-chat-open session :input (not (agentel-session-parent session))))))

(provide 'agentel-chat)
;;; agentel-chat.el ends here
