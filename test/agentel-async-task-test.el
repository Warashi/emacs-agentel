;;; agentel-async-task-test.el --- Tests for agentel-async-task  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-async-task)
(require 'agentel-test-helper)

(defun agentel-async-task-test-pin ()
  "Return the lines pinned above the prompt of the current buffer."
  (when-let* ((pin (overlay-get agentel-chat--pin 'before-string)))
    (substring-no-properties pin)))

(ert-deftest agentel-async-task-advertises-the-air-capability ()
  (agentel-test-with-started session nil
    (let* ((received (alist-get 'receivedCapabilities
                                (alist-get '_meta (agentel-connection-info
                                                   (agentel-session-connection session)))))
           (air (alist-get 'air (alist-get 'jetbrains (alist-get '_meta received)))))
      (should (equal (alist-get 'version air) 1))
      (should (equal (alist-get 'capabilities air) ["asyncTasks"])))))

(ert-deftest agentel-async-task-running-is-pinned-above-the-prompt ()
  (agentel-test-with-started session nil
    (agentel-test-send "async please")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should (equal (agentel-async-task-test-pin)
                   "⚙ npm run dev [running] ↳ Listening on port 3000\n"))
    (agentel-test-send "hello")
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should-not (agentel-async-task-test-pin))))

(ert-deftest agentel-async-task-is-not-sent-without-the-capability ()
  (let ((agentel-connection-capability-functions
         (remq #'agentel-async-task--capabilities agentel-connection-capability-functions)))
    (agentel-test-with-started session nil
      (agentel-test-send "async please")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (agentel-async-task-test-pin)))))

(provide 'agentel-async-task-test)
;;; agentel-async-task-test.el ends here
