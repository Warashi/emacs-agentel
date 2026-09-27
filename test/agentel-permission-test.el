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

(ert-deftest agentel-permission-item-waits-until-it-is-answered ()
  (agentel-test-with-started session nil
    (agentel-permission-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'permission))
                          (agentel-conversation-items session))))
      (should (agentel-store-get item 'waiting))
      (agentel-chat-cancel)
      (agentel-test-wait-until (lambda () (not (agentel-store-get item 'waiting)))))))

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
  '(ask item "Allow ls?" [((name . "Yes") (optionId . "y"))
                          ((name . "No") (optionId . "n"))])
  "The message asking whether ls may run.")

(ert-deftest agentel-permission-view-shows-what-it-was-asked ()
  (should (equal (agentel-permission-test-view agentel-permission-test-ask)
                 "⚠ Allow ls?\n  [Yes] [No]")))

(ert-deftest agentel-permission-view-shows-the-answer ()
  (should (equal (agentel-permission-test-view agentel-permission-test-ask
                                               '(close "No"))
                 "Allow ls? → No")))

(provide 'agentel-permission-test)
;;; agentel-permission-test.el ends here
