;;; agentel-async-task-test.el --- Tests for agentel-async-task  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-async-task)
(require 'agentel-test-helper)

(defun agentel-async-task-test-pin ()
  "Return the lines pinned above the prompt of the current buffer."
  (when-let* ((pin (overlay-get agentel-chat--pin 'before-string)))
    (substring-no-properties pin)))

(ert-deftest agentel-async-task-advertises-terminal-output ()
  (agentel-test-with-started session nil
    (let* ((received (alist-get 'receivedCapabilities
                                (alist-get '_meta (agentel-connection-info
                                                   (agentel-session-connection session)))))
           (meta (alist-get '_meta received)))
      (should (eq (alist-get 'terminal_output meta) t))
      (should-not (alist-get 'jetbrains meta)))))

(ert-deftest agentel-async-task-running-is-pinned-above-the-prompt ()
  (agentel-test-with-started session nil
    (agentel-test-send "async please")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should (equal (agentel-async-task-test-pin)
                   "⚙ npm run dev 🏃 ↳ Listening on port 3000\n"))
    (agentel-test-send "hello")
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should-not (agentel-async-task-test-pin))))

(ert-deftest agentel-async-task-keeps-only-the-latest-progress ()
  (let* ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (session (agentel-session-create)))
    (agentel-async-task--on-update
     session '((sessionUpdate . "tool_call") (toolCallId . "b1")
               (title . "dev") (rawInput . ((run_in_background . t)))
               (_meta . ((terminal_info . ((terminal_id . "b1")))))))
    (dotimes (i 100)
      (agentel-async-task--on-update
       session `((sessionUpdate . "tool_call_update") (toolCallId . "b1")
                 (_meta . ((terminal_output . ((terminal_id . "b1")
                                               (data . ,(format "line %d" i)))))))))
    (let* ((model (agentel-store-find (agentel-session-store session) 'async-tasks))
           (task (cdr (assoc "b1" (agentel-store-get model 'tasks)))))
      (should (equal (alist-get 'progress task) "line 99"))
      (should (= (length task) 5)))))

(defun agentel-async-task-test-tasks (&rest messages)
  "Return the tasks after MESSAGES."
  (let (data)
    (dolist (message messages)
      (setq data (agentel-async-task--update message data)))
    (alist-get 'tasks data)))

(ert-deftest agentel-async-task-is-forgotten-once-it-stops ()
  (should (equal (mapcar (lambda (task) (list (car task) (alist-get 'state (cdr task))))
                         (agentel-async-task-test-tasks
                          '(open "b1" "tool1") '(open "b2" "tool2")
                          '(close "b1")))
                 '(("b2" running)))))

(ert-deftest agentel-async-task-ignores-news-of-an-unknown-task ()
  (should-not (agentel-async-task-test-tasks
               '(update "b1" ((output . "Compiling"))) '(close "b1"))))

(ert-deftest agentel-async-task-shows-the-progress-on-one-line ()
  (let* ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (session (agentel-session-create)))
    (agentel-async-task--on-update
     session '((sessionUpdate . "tool_call") (toolCallId . "b1")
               (title . "dev") (rawInput . ((run_in_background . t)))
               (_meta . ((terminal_info . ((terminal_id . "b1")))))))
    (agentel-async-task--on-update
     session '((sessionUpdate . "tool_call_update") (toolCallId . "b1")
               (_meta . ((terminal_output . ((terminal_id . "b1")
                                             (data . "Compiling\nerror: missing semicolon")))))))
    (should (equal (substring-no-properties
                    (agentel-ui-view
                     (agentel-store-find (agentel-session-store session) 'async-tasks)
                     :width 80))
                   "⚙ dev 🏃 ↳ Compiling…"))))

(ert-deftest agentel-async-task-shows-each-task-on-a-line-of-the-width-oldest-first ()
  (let ((tasks (agentel-store-model--make
                :type 'agentel-async-tasks
                :data `((tasks . ,(agentel-async-task-test-tasks
                                   '(open "b1" "t1")
                                   '(update "b1" ((name . "development") (background . t)))
                                   '(open "b2" "t2")
                                   '(update "b2" ((name . "test") (background . t)))))))))
    (should (equal (substring-no-properties (agentel-ui-view tasks :width 9))
                   "⚙ develo…\n⚙ test 🏃"))))

(ert-deftest agentel-async-task-is-not-sent-without-the-capability ()
  (let ((agentel-connection-capability-functions
         (remq #'agentel-async-task--capabilities agentel-connection-capability-functions)))
    (agentel-test-with-started session nil
      (agentel-test-send "async please")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (agentel-async-task-test-pin)))))

(ert-deftest agentel-async-task-rejects-an-unknown-message ()
  (let ((data (agentel-async-task--update '(open "t1" "Build") nil)))
    (should-error (agentel-async-task--update '(pause "t1") data)
                  :type 'agentel-store-unknown-message)
    (should-error (agentel-async-task--update '(pause "t2") data)
                  :type 'agentel-store-unknown-message)))

(ert-deftest agentel-async-task-terminal-lives-until-exit ()
  (let* ((agentel-session--registry nil)
         (session (agentel-session-create)))
    (agentel-session-register session "session")
    (dolist (update
             '(((sessionUpdate . "tool_call") (toolCallId . "bash")
                (title . "Terminal") (status . "pending")
                (content . [((type . "terminal") (terminalId . "bash"))])
                (_meta . ((terminal_info . ((terminal_id . "bash"))))))
               ((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                (title . "sleep 8"))
               ((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                (status . "completed"))))
      (agentel-session-dispatch
       `((sessionId . "session") (update . ,update))))
    (let ((model (agentel-store-find (agentel-session-store session) 'async-tasks)))
      (should model)
      (should (string-match-p "sleep 8" (agentel-ui-view model :width 80)))
      (agentel-session-send session '(finish-turn))
      (should (string-match-p "sleep 8" (agentel-ui-view model :width 80)))
      (agentel-async-task--on-update
       session '((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                 (_meta . ((terminal_exit . ((terminal_id . "bash")
                                             (exit_code) (signal)))))))
      (should (equal (agentel-ui-view model :width 80) "")))))

(ert-deftest agentel-async-task-foreground-output-survives-terminal-content ()
  (let* ((agentel-session--registry nil)
         (session (agentel-session-create)))
    (agentel-session-register session "session")
    (dolist (update
             '(((sessionUpdate . "tool_call") (toolCallId . "bash")
                (title . "Terminal") (status . "pending")
                (content . [((type . "terminal") (terminalId . "bash"))])
                (_meta . ((terminal_info . ((terminal_id . "bash"))))))
               ((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                (title . "printf marker") (rawInput . ((description . "Show marker"))))
               ((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                (_meta . ((terminal_output . ((terminal_id . "bash") (data . "marker"))))))
               ((sessionUpdate . "tool_call_update") (toolCallId . "bash")
                (status . "completed")
                (content . [((type . "terminal") (terminalId . "bash"))])
                (_meta . ((terminal_info . ((terminal_id . "bash")))
                          (terminal_exit . ((terminal_id . "bash")
                                            (exit_code . 0) (signal))))))))
      (agentel-session-dispatch `((sessionId . "session") (update . ,update)))
      (let ((model (agentel-store-find (agentel-session-store session) 'async-tasks)))
        (should (equal (agentel-ui-view model :width 80) ""))))
    (let ((tool (agentel-conversation-find session '(tool . "bash")))
          (model (agentel-store-find (agentel-session-store session) 'async-tasks)))
      (should (equal (agentel-store-get tool 'output) "marker"))
      (should (equal (agentel-store-get tool 'title) "printf marker"))
      (should (equal (agentel-store-get tool 'why) "Show marker"))
      (should (eq (agentel-store-get tool 'status) 'done))
      (should (equal (agentel-ui-view model :width 80) "")))))

(ert-deftest agentel-async-task-model-leaves-earlier-data-alone ()
  (let* ((before (agentel-async-task--update '(open "b1" "t1") nil))
         (copy (copy-tree before)))
    (agentel-async-task--update '(update "b1" ((name . "dev") (background . t))) before)
    (agentel-async-task--update '(close "b1") before)
    (should (equal before copy))))

(provide 'agentel-async-task-test)
;;; agentel-async-task-test.el ends here
