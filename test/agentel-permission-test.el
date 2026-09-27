;;; agentel-permission-test.el --- Tests for agentel-permission  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-permission)
(require 'agentel-test-helper)

(defun agentel-permission-test-ask ()
  "Send a prompt that makes the mock agent ask for permission."
  (agentel-test-send "permission please")
  (agentel-test-wait-until
   (lambda () (eq (agentel-session-state agentel-chat--session) 'waiting))))

(ert-deftest agentel-permission-shows-the-tool-and-its-options ()
  (agentel-test-with-started _session nil
    (agentel-permission-test-ask)
    (agentel-test-wait-for-text "Allow rm -rf build\\?")
    (agentel-test-wait-for-text "\\[Yes\\] \\[Yes, and don't ask again for rm commands\\] \\[No\\]")))

(ert-deftest agentel-permission-button-answers-the-agent ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (goto-char (point-min))
    (search-forward "[No]")
    (push-button (1- (point)))
    (agentel-test-wait-for-text "Permission outcome: reject")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (agentel-test-wait-for-text "Allow rm -rf build\\? → No")))

(ert-deftest agentel-permission-can-be-answered-from-the-minibuffer ()
  (agentel-test-with-started _session nil
    (agentel-permission-test-ask)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "Yes" (all-completions "" collection)))
                 "Yes")))
      (agentel-chat-answer))
    (agentel-test-wait-for-text "Permission outcome: allow-once")))

(ert-deftest agentel-permission-stays-unanswered-when-nothing-is-chosen ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (agentel-chat-answer))
    (should (eq (agentel-session-state session) 'waiting))
    (should (agentel-store-get
             (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                       (agentel-conversation-items session))
             'waiting))))

(ert-deftest agentel-permission-item-holds-the-question-and-its-choices ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                          (agentel-conversation-items session))))
      (should (equal (agentel-store-model-data item)
                     '((question . "Allow rm -rf build?")
                       (choices "Yes" "Yes, and don't ask again for rm commands" "No")
                       (waiting . t)))))))

(ert-deftest agentel-permission-item-waits-until-it-is-answered ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                          (agentel-conversation-items session))))
      (should (agentel-store-get item 'waiting))
      (agentel-chat-cancel)
      (agentel-test-wait-until (lambda () (not (agentel-store-get item 'waiting)))))))

(ert-deftest agentel-permission-is-forgotten-with-its-session ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                          (agentel-conversation-items session))))
      (agentel-session-remove session)
      (should-not (gethash item agentel-permission--requests)))))

(ert-deftest agentel-permission-is-withdrawn-when-the-agent-exits ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                          (agentel-conversation-items session))))
      (delete-process (agentel-connection-process (agentel-session-connection session)))
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'exited)))
      (agentel-test-wait-for-text "Allow rm -rf build\\? → withdrawn")
      (should-not (agentel-session-waiting-p session))
      (cl-letf (((symbol-function 'agentel-connection-respond)
                 (lambda (&rest _) (ert-fail "Answered over an ended connection"))))
        (agentel-permission-choose item 0)))))

(ert-deftest agentel-permission-is-cancelled-when-its-session-ends-while-the-agent-runs ()
  (agentel-test-with-started session nil
    (let* ((connection (agentel-session-connection session))
           (child (agentel-session-create :connection connection :parent session))
           responses)
      (agentel-session-register child "child")
      (cl-letf (((symbol-function 'agentel-connection-respond)
                 (lambda (_connection id result) (push (cons id result) responses))))
        (agentel-permission--handle
         connection 99 '((sessionId . "child")
                         (toolCall (title . "ls"))
                         (options . [((optionId . "allow") (name . "Yes"))])))
        (agentel-session-send child '(end completed)))
      (should (equal responses '((99 (outcome (outcome . "cancelled"))))))
      (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                            (agentel-conversation-items child))))
        (should (agentel-store-get item 'withdrawn))
        (should-not (agentel-store-get item 'waiting))))))

(ert-deftest agentel-permission-is-withdrawn-when-the-turn-is-cancelled ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (agentel-chat-cancel)
    (agentel-test-wait-until (lambda () (not (eq (agentel-session-state session) 'waiting))))
    (agentel-test-wait-for-text "Allow rm -rf build\\? → withdrawn")))

(defun agentel-permission-test-view (&rest messages)
  "Return the text of a permission request after MESSAGES."
  (let ((data nil))
    (dolist (message messages)
      (setq data (agentel-permission--update message data)))
    (substring-no-properties
     (agentel-ui-view (agentel-store-model--make :type 'permission :data data)))))

(defconst agentel-permission-test-ask
  '(ask "Allow ls?" ("Yes" "No"))
  "The message asking whether ls may run.")

(ert-deftest agentel-permission-view-shows-what-it-was-asked ()
  (should (equal (agentel-permission-test-view agentel-permission-test-ask)
                 "⚠ Allow ls?\n  [Yes] [No]")))

(ert-deftest agentel-permission-view-shows-the-answer ()
  (should (equal (agentel-permission-test-view agentel-permission-test-ask
                                               '(answer "No"))
                 "Allow ls? → No")))

(ert-deftest agentel-permission-view-shows-that-it-was-withdrawn ()
  (should (equal (agentel-permission-test-view agentel-permission-test-ask
                                               '(withdraw))
                 "Allow ls? → withdrawn")))

(ert-deftest agentel-permission-rejects-an-unknown-message ()
  (should-error (agentel-permission--update '(forget) nil)
                :type 'agentel-store-unknown-message))

(provide 'agentel-permission-test)
;;; agentel-permission-test.el ends here
