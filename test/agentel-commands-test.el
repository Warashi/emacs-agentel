;;; agentel-commands-test.el --- Tests for agentel-commands  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-commands)

(defmacro agentel-commands-test-with-session (&rest body)
  "Run BODY in the chat buffer of a session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-commands--on-update))
         (agentel-commands-client-commands nil))
     (let* ((session (agentel-session-create :cwd "/tmp/"))
            (buffer (agentel-chat-open session :input t)))
       (agentel-session-register session "s1")
       (agentel-session-dispatch
        '((sessionId . "s1")
          (update . ((sessionUpdate . "available_commands_update")
                     (availableCommands
                      . [((name . "compact")
                          (description . "Clear history but keep a summary")
                          (input . ((hint . "<instructions>"))))
                         ((name . "context")
                          (description . "Show current context usage"))])))))
       (unwind-protect
           (with-current-buffer buffer
             (goto-char (point-max))
             ,@body)
         (kill-buffer buffer)))))

(defun agentel-commands-test-candidates ()
  "Return the completion candidates at point."
  (when-let* ((capf (agentel-commands-completion-at-point)))
    (all-completions (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                     (nth 2 capf))))

(ert-deftest agentel-commands-completes-agent-commands ()
  (agentel-commands-test-with-session
    (insert "/co")
    (should (equal (sort (agentel-commands-test-candidates) #'string<)
                   '("compact" "context")))))

(ert-deftest agentel-commands-completes-client-commands ()
  (agentel-commands-test-with-session
    (agentel-commands-define "resume" "Resume a previous session" #'ignore)
    (insert "/re")
    (should (equal (agentel-commands-test-candidates) '("resume")))))

(ert-deftest agentel-commands-annotates-with-the-description ()
  (agentel-commands-test-with-session
    (insert "/")
    (let* ((capf (agentel-commands-completion-at-point))
           (annotate (plist-get (nthcdr 3 capf) :annotation-function)))
      (should (string-match-p "keep a summary" (funcall annotate "compact")))
      (should (string-match-p "<instructions>" (funcall annotate "compact"))))))

(ert-deftest agentel-commands-completes-only-at-the-start-of-the-input ()
  (agentel-commands-test-with-session
    (insert "please /co")
    (should-not (agentel-commands-completion-at-point))))

(ert-deftest agentel-commands-runs-client-commands-instead-of-sending ()
  (agentel-commands-test-with-session
    (let (called)
      (agentel-commands-define "resume" "Resume" (lambda (s args) (setq called (list s args))))
      (should (agentel-commands--run session "/resume  old one "))
      (should (equal called (list session "old one"))))))

(ert-deftest agentel-commands-leaves-agent-commands-to-the-agent ()
  (agentel-commands-test-with-session
    (should-not (agentel-commands--run session "/compact now"))))

(provide 'agentel-commands-test)
;;; agentel-commands-test.el ends here
