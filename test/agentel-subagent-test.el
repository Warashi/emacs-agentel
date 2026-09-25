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

(ert-deftest agentel-subagent-history-is-replayed-into-its-buffer ()
  (agentel-test-with-started session '(:session-id "old-1")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (let ((child (car (agentel-session-children session))))
      (should (equal (agentel-session-name child) "Review the parser"))
      (should (eq (agentel-session-state child) 'completed))
      (should (string-match-p "Replayed subagent text\\."
                              (agentel-subagent-test-text (agentel-session-buffer child)))))))

(ert-deftest agentel-subagent-buffer-comes-back-after-being-killed ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (kill-buffer (agentel-session-buffer child))
      (agentel-subagent-open child)
      (should (buffer-live-p (agentel-session-buffer child)))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (should (memq child (agentel-session-list)))
      (should (string-match-p "Earlier output"
                              (agentel-subagent-test-text (agentel-session-buffer child)))))))

(ert-deftest agentel-subagent-buffer-has-no-input ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (with-current-buffer (agentel-session-buffer child)
        (should-not agentel-chat--input-start)
        (should-error (agentel-chat-send) :type 'user-error)))))

(defun agentel-subagent-test-pin ()
  "Return the lines pinned above the prompt of the current buffer."
  (overlay-get agentel-chat--pin 'before-string))

(defun agentel-subagent-test-background (session)
  "Let the mock agent of SESSION start a lingering subagent and return it."
  (agentel-test-send "background please")
  (agentel-test-wait-until
   (lambda () (string-match-p "make watch" (or (agentel-subagent-test-pin) ""))))
  (car (agentel-session-children session)))

(ert-deftest agentel-subagent-running-is-pinned-above-the-prompt ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-background session)
    (should (string-match-p "\\`⎇ Watch the build \\[running\\] ↳ make watch\n\\'"
                            (substring-no-properties (agentel-subagent-test-pin))))
    (agentel-chat-cancel)
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should-not (agentel-subagent-test-pin))))

(ert-deftest agentel-subagent-finished-is-not-pinned ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (should-not (agentel-subagent-test-pin))))

(ert-deftest agentel-subagent-pin-opens-the-child-buffer ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-background session))
           (map (get-text-property 0 'keymap (agentel-subagent-test-pin))))
      (funcall (keymap-lookup map "<mouse-1>"))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (agentel-chat-cancel))))

(ert-deftest agentel-subagent-open-offers-the-running-children ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-background session)))
      (goto-char (point-max))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (should (equal (all-completions "" collection)
                                  '("Watch the build")))
                   "Watch the build")))
        (agentel-subagent-open))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (agentel-chat-cancel))))

(ert-deftest agentel-subagent-open-without-running-children-is-a-user-error ()
  (agentel-test-with-started _session nil
    (goto-char (point-max))
    (should-error (agentel-subagent-open) :type 'user-error)))

(ert-deftest agentel-subagent-open-is-bound-in-the-session-buffer ()
  (should (eq (keymap-lookup agentel-chat-mode-map "C-c C-j") #'agentel-subagent-open)))

(ert-deftest agentel-subagent-buffers-go-with-the-parent ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-run session))
           (buffer (agentel-session-buffer child)))
      (kill-buffer (current-buffer))
      (should-not (buffer-live-p buffer))
      (should-not (memq child (agentel-session-list))))))

(provide 'agentel-subagent-test)
;;; agentel-subagent-test.el ends here
