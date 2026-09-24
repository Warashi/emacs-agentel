;;; agentel-mock-agent.el --- A scripted stand-in for claude-agent-acp  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `emacs --batch -Q -l test/agentel-mock-agent.el'.  It speaks
;; ACP over stdio and mimics the message shapes of claude-agent-acp
;; 0.81.0.  What it does for a prompt depends on words in the prompt:
;;
;;   tool        a finished Read tool call
;;   permission  a Bash tool call that asks for permission first
;;   ask         an AskUserQuestion form elicitation
;;   subagent    a subagent that works in its own session
;;   slow        waits until the client cancels the turn
;;   fail        fails the prompt request
;;
;; Anything else is echoed back.  The initialize response carries the
;; client capabilities it received under `_meta.receivedCapabilities'
;; so tests can inspect them.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar agentel-mock--next-id 1000)
(defvar agentel-mock--waiting (make-hash-table :test 'equal)
  "Callbacks for requests sent to the client, by request id.")
(defvar agentel-mock--sessions (make-hash-table :test 'equal)
  "Session state by session id.")
(defvar agentel-mock--prompt-id nil
  "Request id of the prompt that waits for a cancel.")
(defvar agentel-mock--asking nil
  "Id of the request to the client that a cancel withdraws.")
(defvar agentel-mock--counter 0)
(defvar agentel-mock--delay 0.02
  "Seconds between streamed chunks.")

(defun agentel-mock--send (object)
  "Write OBJECT as one JSON line."
  ;; `json-serialize' returns unibyte UTF-8, which `princ' would print
  ;; byte by byte as raw characters.
  (princ (concat (decode-coding-string (json-serialize object) 'utf-8) "\n")))

(defun agentel-mock--respond (id result)
  "Answer request ID with RESULT."
  (agentel-mock--send `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun agentel-mock--error (id code message)
  "Fail request ID with CODE and MESSAGE."
  (agentel-mock--send `((jsonrpc . "2.0") (id . ,id)
                        (error . ((code . ,code) (message . ,message))))))

(defun agentel-mock--update (session-id update)
  "Send UPDATE for SESSION-ID."
  (sleep-for agentel-mock--delay)
  (agentel-mock--send `((jsonrpc . "2.0") (method . "session/update")
                        (params . ((sessionId . ,session-id)
                                   (update . ,update))))))

(defun agentel-mock--request (method params callback)
  "Send a request METHOD with PARAMS and call CALLBACK with its result."
  (let ((id (cl-incf agentel-mock--next-id)))
    (puthash id callback agentel-mock--waiting)
    (setq agentel-mock--asking id)
    (agentel-mock--send `((jsonrpc . "2.0") (id . ,id) (method . ,method)
                          (params . ,params)))))

(defun agentel-mock--say (session-id text &optional kind)
  "Stream TEXT to SESSION-ID in word sized chunks.
KIND is the session update name and defaults to an agent message."
  (dolist (chunk (split-string text "\\b" t))
    (agentel-mock--update
     session-id `((sessionUpdate . ,(or kind "agent_message_chunk"))
                  (content . ((type . "text") (text . ,chunk)))))))

(defun agentel-mock--config-options (state)
  "Return the configOptions of session STATE."
  (let* ((model (alist-get 'model state))
         (options
         (list
          `((id . "mode") (name . "Mode") (category . "mode") (type . "select")
            (currentValue . ,(alist-get 'mode state))
            (options . [((value . "default") (name . "Manual"))
                        ((value . "acceptEdits") (name . "Accept edits"))
                        ((value . "plan") (name . "Plan"))
                        ((value . "auto") (name . "Auto"))]))
          `((id . "model") (name . "Model") (category . "model") (type . "select")
            (currentValue . ,model)
            (options . [((value . "default") (name . "Default (recommended)"))
                        ((value . "opus[1m]") (name . "Opus 5.5"))
                        ((value . "sonnet") (name . "Sonnet 5"))
                        ((value . "haiku") (name . "Haiku 4.5"))])))))
    ;; Like the real adapter, a model without effort levels drops the option.
    (unless (equal model "haiku")
      (setq options
            (append options
                    (list `((id . "effort") (name . "Effort")
                            (category . "thought_level") (type . "select")
                            (currentValue . ,(alist-get 'effort state))
                            (options . [((value . "low") (name . "Low"))
                                        ((value . "medium") (name . "Medium"))
                                        ((value . "high") (name . "High"))
                                        ((value . "max") (name . "Max"))]))))))
    (vconcat options)))

(defun agentel-mock--new-session (id)
  "Create session state for ID and announce its commands."
  (puthash id (list (cons 'mode "default") (cons 'model "sonnet")
                    (cons 'effort "medium") (cons 'used 0))
           agentel-mock--sessions)
  id)

(defun agentel-mock--announce-commands (session-id)
  "Send the available commands of SESSION-ID."
  (agentel-mock--update
   session-id
   '((sessionUpdate . "available_commands_update")
     (availableCommands . [((name . "compact")
                            (description . "Clear history but keep a summary")
                            (input . ((hint . "<optional instructions>"))))
                           ((name . "context")
                            (description . "Show current context usage")
                            (input . nil))
                           ((name . "review")
                            (description . "Review a pull request")
                            (input . nil))]))))

(defun agentel-mock--usage (session-id)
  "Report grown usage for SESSION-ID."
  (let ((state (gethash session-id agentel-mock--sessions)))
    (setf (alist-get 'used state) (+ (alist-get 'used state) 12000))
    (puthash session-id state agentel-mock--sessions)
    (agentel-mock--update
     session-id `((sessionUpdate . "usage_update")
                  (used . ,(alist-get 'used state))
                  (size . 200000)
                  (cost . ((amount . ,(* 0.00001 (alist-get 'used state)))
                           (currency . "USD")))))))

(defun agentel-mock--finish (id session-id &optional stop-reason)
  "End prompt ID of SESSION-ID with STOP-REASON."
  (setq agentel-mock--prompt-id nil)
  (agentel-mock--usage session-id)
  (agentel-mock--respond id `((stopReason . ,(or stop-reason "end_turn")))))

(defun agentel-mock--tool (session-id)
  "Show a finished Read tool call in SESSION-ID."
  (let ((tool-id (format "toolu_%d" (cl-incf agentel-mock--counter))))
    (agentel-mock--update
     session-id `((sessionUpdate . "tool_call") (toolCallId . ,tool-id)
                  (title . "Read README.org") (kind . "read") (status . "pending")
                  (rawInput . ((file_path . "/tmp/README.org")))
                  (content . [])
                  (locations . [((path . "/tmp/README.org") (line . 1))])
                  (_meta . ((claudeCode . ((toolName . "Read")))))))
    (agentel-mock--update
     session-id `((sessionUpdate . "tool_call_update") (toolCallId . ,tool-id)
                  (status . "completed")
                  (content . [((type . "content")
                               (content . ((type . "text")
                                           (text . "#+title: agentel\nline 2\nline 3"))))])))))

(defun agentel-mock--permission (id session-id)
  "Ask for permission to run a command in SESSION-ID, then end prompt ID."
  (let ((tool-id (format "toolu_%d" (cl-incf agentel-mock--counter))))
    (agentel-mock--update
     session-id `((sessionUpdate . "tool_call") (toolCallId . ,tool-id)
                  (title . "rm -rf build") (kind . "execute") (status . "pending")
                  (rawInput . ((command . "rm -rf build")
                               (description . "Remove the build directory")))
                  (content . [((type . "content")
                               (content . ((type . "text")
                                           (text . "Remove the build directory"))))])
                  (_meta . ((claudeCode . ((toolName . "Bash")))))))
    (setq agentel-mock--prompt-id (cons id session-id))
    (agentel-mock--request
     "session/request_permission"
     `((sessionId . ,session-id)
       (toolCall . ((toolCallId . ,tool-id) (title . "rm -rf build")
                    (kind . "execute")
                    (rawInput . ((command . "rm -rf build")))))
       (options . [((optionId . "allow-once") (name . "Yes") (kind . "allow_once"))
                   ((optionId . "allow-with-updates")
                    (name . "Yes, and don't ask again for rm commands")
                    (kind . "allow_always"))
                   ((optionId . "reject") (name . "No") (kind . "reject_once"))]))
     (lambda (result)
       (let* ((outcome (alist-get 'outcome result))
              (option (alist-get 'optionId outcome))
              (allowed (member option '("allow-once" "allow-with-updates"))))
         (agentel-mock--update
          session-id `((sessionUpdate . "tool_call_update") (toolCallId . ,tool-id)
                       (status . ,(if allowed "completed" "failed"))
                       (content . [((type . "content")
                                    (content . ((type . "text")
                                                (text . ,(if allowed "removed"
                                                           "denied by user")))))])))
         (agentel-mock--say session-id
                            (format "Permission outcome: %s."
                                    (or option (alist-get 'outcome outcome))))
         (agentel-mock--finish id session-id))))))

(defun agentel-mock--ask (id session-id)
  "Ask the user a question in SESSION-ID, then end prompt ID."
  (setq agentel-mock--prompt-id (cons id session-id))
  (agentel-mock--request
   "elicitation/create"
   `((mode . "form") (sessionId . ,session-id)
     (message . "Please answer the following questions.")
     (requestedSchema
      . ((type . "object")
         (properties
          . ((question_0 . ((type . "string") (title . "Color")
                            (description . "Which color do you prefer?")
                            (oneOf . [((const . "Red") (title . "Red")
                                       (description . "Warm"))
                                      ((const . "Blue") (title . "Blue")
                                       (description . "Cool"))])))
             (question_0_custom . ((type . "string") (title . "Other")
                                   (description . "Type your own answer (optional).")
                                   (_meta . ((_askUserQuestionCustomAnswer
                                              . ((questionId . "question_0")
                                                 (isCustomAnswer . t)))))))
             (question_1 . ((type . "array") (title . "Fruits")
                            (description . "Which fruits do you like?")
                            (items . ((anyOf . [((const . "Apple") (title . "Apple"))
                                                ((const . "Banana") (title . "Banana"))
                                                ((const . "Cherry") (title . "Cherry"))]))))))))))
   (lambda (result)
     (agentel-mock--say session-id
                        (format "Elicitation result: %s."
                                (json-serialize result)))
     (agentel-mock--finish id session-id))))

(defun agentel-mock--subagent (id session-id)
  "Run a subagent under SESSION-ID, then end prompt ID."
  (let ((child (format "task-%d" (cl-incf agentel-mock--counter))))
    (agentel-mock--say session-id "Delegating to a subagent.")
    (agentel-mock--update
     session-id `((sessionUpdate . "subagent_spawned")
                  (subagentSessionId . ,child)
                  (name . "Explore the repository")
                  (task . "List the files and summarize them.")
                  (capabilities . ,(make-hash-table))))
    (agentel-mock--say child "I will look at the files." )
    (agentel-mock--tool child)
    (agentel-mock--say child "The repository has a README.")
    (agentel-mock--update
     session-id `((sessionUpdate . "subagent_state_update")
                  (subagentSessionId . ,child)
                  (state . "completed")))
    (agentel-mock--say session-id "The subagent reported a README.")
    (agentel-mock--finish id session-id)))

(defun agentel-mock--prompt (id params)
  "Handle prompt request ID with PARAMS."
  (let* ((session-id (alist-get 'sessionId params))
         (text (mapconcat (lambda (block) (or (alist-get 'text block) ""))
                          (alist-get 'prompt params) "")))
    (agentel-mock--update
     session-id `((sessionUpdate . "session_info_update")
                  (title . ,(truncate-string-to-width text 40))))
    (agentel-mock--say session-id "Thinking about it." "agent_thought_chunk")
    (cond
     ((string-match-p "fail" text)
      (agentel-mock--error id -32000 "Authentication required"))
     ((string-match-p "slow" text)
      (agentel-mock--say session-id "Working slowly")
      (setq agentel-mock--prompt-id (cons id session-id)))
     ((string-match-p "permission" text) (agentel-mock--permission id session-id))
     ((string-match-p "ask" text) (agentel-mock--ask id session-id))
     ((string-match-p "subagent" text) (agentel-mock--subagent id session-id))
     (t
      (when (string-match-p "tool" text)
        (agentel-mock--tool session-id))
      (agentel-mock--say session-id (concat "Echo: " text))
      (agentel-mock--finish id session-id)))))

(defun agentel-mock--set-config (id params)
  "Handle `session/set_config_option' request ID with PARAMS."
  (let* ((session-id (alist-get 'sessionId params))
         (state (gethash session-id agentel-mock--sessions))
         (key (intern (alist-get 'configId params))))
    (setf (alist-get key state) (alist-get 'value params))
    (puthash session-id state agentel-mock--sessions)
    (agentel-mock--respond id `((configOptions . ,(agentel-mock--config-options state))))))

(defun agentel-mock--load (id params)
  "Handle `session/load' request ID with PARAMS by replaying history."
  (let ((session-id (agentel-mock--new-session (alist-get 'sessionId params))))
    (agentel-mock--say session-id "What was the plan?" "user_message_chunk")
    (agentel-mock--say session-id "The plan was to write tests first.")
    (agentel-mock--say (concat session-id ":replay-subagent:toolu_1")
                       "Replayed subagent text.")
    (agentel-mock--respond
     id `((configOptions . ,(agentel-mock--config-options
                             (gethash session-id agentel-mock--sessions)))))
    (agentel-mock--announce-commands session-id)))

(defun agentel-mock--handle-request (id method params)
  "Handle the client request ID calling METHOD with PARAMS."
  (pcase method
    ("initialize"
     (agentel-mock--respond
      id `((protocolVersion . 1)
           (agentCapabilities . ((loadSession . t)
                                 (sessionCapabilities . ((list . ,(make-hash-table))
                                                         (resume . ,(make-hash-table))))))
           (agentInfo . ((name . "agentel-mock") (version . "0")))
           (authMethods . [])
           (_meta . ((receivedCapabilities
                      . ,(or (alist-get 'clientCapabilities params)
                             (make-hash-table))))))))
    ("session/new"
     (let ((session-id (agentel-mock--new-session
                        (format "mock-%d" (cl-incf agentel-mock--counter)))))
       (agentel-mock--respond
        id `((sessionId . ,session-id)
             (configOptions . ,(agentel-mock--config-options
                                (gethash session-id agentel-mock--sessions)))))
       (agentel-mock--announce-commands session-id)))
    ("session/load" (agentel-mock--load id params))
    ("session/list"
     (agentel-mock--respond
      id `((sessions . [((sessionId . "old-1") (cwd . ,(alist-get 'cwd params))
                         (title . "Write the parser")
                         (updatedAt . "2026-09-24T10:00:00Z"))
                        ((sessionId . "old-2") (cwd . ,(alist-get 'cwd params))
                         (title . "Fix the renderer")
                         (updatedAt . "2026-09-23T09:00:00Z"))]))))
    ("session/set_config_option" (agentel-mock--set-config id params))
    ("session/prompt" (agentel-mock--prompt id params))
    (_ (agentel-mock--error id -32601 (format "Method not found: %s" method)))))

(defun agentel-mock--handle (message)
  "Handle one parsed JSON-RPC MESSAGE."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message)))
    (cond
     ((and id (not method))
      (when-let* ((callback (gethash id agentel-mock--waiting)))
        (remhash id agentel-mock--waiting)
        (funcall callback (alist-get 'result message))))
     (id (agentel-mock--handle-request id method (alist-get 'params message)))
     ((equal method "session/cancel")
      ;; Like the SDK, withdraw the question the client has not answered.
      (when (and agentel-mock--asking
                 (gethash agentel-mock--asking agentel-mock--waiting))
        (remhash agentel-mock--asking agentel-mock--waiting)
        (agentel-mock--send `((jsonrpc . "2.0") (method . "$/cancel_request")
                              (params . ((requestId . ,agentel-mock--asking))))))
      (when agentel-mock--prompt-id
        (agentel-mock--finish (car agentel-mock--prompt-id)
                              (cdr agentel-mock--prompt-id) "cancelled")
        (setq agentel-mock--prompt-id nil))))))

(defun agentel-mock-main ()
  "Serve ACP on stdio until stdin closes."
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (condition-case nil
      (while t
        (let ((line (read-from-minibuffer "")))
          (unless (string-empty-p line)
            (agentel-mock--handle
             (json-parse-string line :object-type 'alist
                                :null-object nil :false-object nil)))))
    (end-of-file nil)))

(when noninteractive
  (agentel-mock-main))

(provide 'agentel-mock-agent)
;;; agentel-mock-agent.el ends here
