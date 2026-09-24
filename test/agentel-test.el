;;; agentel-test.el --- End to end tests for agentel  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel)
(require 'agentel-test-helper)

(defmacro agentel-test-with-started (var options &rest body)
  "Start a session on the mock agent with OPTIONS, bind it to VAR, run BODY."
  (declare (indent 2))
  `(let* ((agentel-session--registry nil)
          (command (agentel-test-mock-command))
          (agentel-command (car command))
          (agentel-command-args (cdr command))
          (,var (apply #'agentel-start :cwd temporary-file-directory
                       :display nil ,options)))
     (unwind-protect
         (progn
           (agentel-test-wait-until
            (lambda () (eq (agentel-session-state ,var) 'idle)))
           (with-current-buffer (agentel-session-buffer ,var) ,@body))
       (when (buffer-live-p (agentel-session-buffer ,var))
         (kill-buffer (agentel-session-buffer ,var))))))

(defun agentel-test-send (text)
  "Type TEXT into the input area and send it."
  (goto-char (point-max))
  (insert text)
  (agentel-chat-send))

(defun agentel-test-wait-for-text (regexp)
  "Wait until the current buffer matches REGEXP."
  (agentel-test-wait-until
   (lambda () (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t)))))

(ert-deftest agentel-start-opens-a-session-buffer ()
  (agentel-test-with-started session nil
    (should (string-prefix-p "mock-" (agentel-session-id session)))
    (should (eq major-mode 'agentel-chat-mode))))

(ert-deftest agentel-prompt-shows-reply-and-returns-to-idle ()
  (agentel-test-with-started session nil
    (agentel-test-send "hello")
    (should (eq (agentel-session-state session) 'running))
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should (equal (agentel-chat-input) ""))))

(ert-deftest agentel-prompt-failure-is-shown ()
  (agentel-test-with-started session nil
    (agentel-test-send "fail")
    (agentel-test-wait-for-text "Prompt failed: Authentication required")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))))

(ert-deftest agentel-cancel-ends-the-turn ()
  (agentel-test-with-started session nil
    (agentel-test-send "slow")
    (agentel-test-wait-for-text "Working slowly")
    (agentel-chat-cancel)
    (agentel-test-wait-for-text "Turn ended: cancelled")))

(ert-deftest agentel-killing-the-buffer-stops-the-agent ()
  (agentel-test-with-started session nil
    (let ((process (agentel-connection-process (agentel-session-connection session))))
      (kill-buffer (current-buffer))
      (agentel-test-wait-until (lambda () (not (process-live-p process))))
      (should-not (memq session (agentel-session-list))))))

(ert-deftest agentel-start-loads-a-previous-session ()
  (agentel-test-with-started session '(:session-id "old-1")
    (should (equal (agentel-session-id session) "old-1"))
    (agentel-test-wait-for-text "The plan was to write tests first.")
    (should-not (save-excursion (goto-char (point-min))
                                (search-forward "Replayed subagent" nil t)))))

(provide 'agentel-test)
;;; agentel-test.el ends here
