;;; agentel-subagent-test.el --- Tests for agentel-subagent  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-subagent)
(require 'agentel-test-helper)

(defun agentel-subagent-test-run (session)
  "Let the mock agent of SESSION run a subagent and return the child."
  (agentel-test-send "subagent please")
  (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
  (car (agentel-session-children session)))

(defun agentel-subagent-test-text (buffer)
  "Return the text of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest agentel-subagent-is-advertised ()
  (should (equal (agentel-subagent--capabilities) '((subagents . nil)))))

(ert-deftest agentel-subagent-gets-its-own-session-and-buffer ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (should child)
      (should (equal (agentel-session-name child) "Explore the repository"))
      (should (eq (agentel-session-state child) 'completed))
      (let ((text (agentel-subagent-test-text (agentel-session-buffer child))))
        (should (string-match-p "List the files and summarize them" text))
        (should (string-match-p "I will look at the files\\." text))
        (should (string-match-p "Read README\\.org" text))))))

(ert-deftest agentel-subagent-output-stays-out-of-the-parent ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (let ((text (agentel-subagent-test-text (current-buffer))))
      (should-not (string-match-p "I will look at the files" text))
      (should (string-match-p "Delegating to a subagent\\." text))
      (should (string-match-p "The subagent reported a README\\." text)))))

(ert-deftest agentel-subagent-is-one-item-in-the-parent ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (agentel-test-wait-for-text "⎇ Explore the repository \\[completed\\]")
    (agentel-test-wait-for-text "Read README\\.org")))

(ert-deftest agentel-subagent-item-opens-the-child-buffer ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (goto-char (point-min))
      (search-forward "Explore the repository")
      (agentel-subagent-open)
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child))))))

(ert-deftest agentel-subagent-buffer-has-no-input ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (with-current-buffer (agentel-session-buffer child)
        (should-not agentel-chat--input-start)
        (should-error (agentel-chat-send) :type 'user-error)))))

(ert-deftest agentel-subagent-buffers-go-with-the-parent ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-run session))
           (buffer (agentel-session-buffer child)))
      (kill-buffer (current-buffer))
      (should-not (buffer-live-p buffer))
      (should-not (memq child (agentel-session-list))))))

(provide 'agentel-subagent-test)
;;; agentel-subagent-test.el ends here
