;;; agentel-config-test.el --- Tests for agentel-config  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-config)
(require 'agentel-test-helper)

(defun agentel-config-test-value (session id)
  "Return the current value of the option ID of SESSION."
  (alist-get 'currentValue (agentel-config-option session id)))

(defun agentel-config-test-settled (session)
  "Wait until SESSION finished applying its start options."
  (agentel-test-wait-until
   (lambda () (and (eq (agentel-session-state session) 'idle)
                   (agentel-session-data session 'config-options)))))

(ert-deftest agentel-config-applies-start-options-in-order ()
  (agentel-test-with-started session '(:model "opus[1m]" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-test-value session "model") "opus[1m]"))
    (should (equal (agentel-config-test-value session "effort") "high"))
    (should (equal (agentel-config-test-value session "mode") "plan"))))

(ert-deftest agentel-config-shows-the-settings-in-the-header ()
  (agentel-test-with-started session '(:model "opus[1m]" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (string-match-p "Opus 5\\.5 · High · Plan" (agentel-chat--header-line)))))

(ert-deftest agentel-config-names-other-options-in-the-header ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil))
    (let ((session (agentel-session-create)))
      (setf (agentel-session-data session 'config-options)
            '(((id . "fast") (name . "Fast mode") (currentValue . "off")
               (options . [((value . "off") (name . "Off"))]))
              ((id . "model") (name . "Model") (currentValue . "sonnet")
               (options . [((value . "sonnet") (name . "Sonnet 5"))]))))
      (should (equal (agentel-config--header session) "Sonnet 5 · Fast mode: Off")))))

(ert-deftest agentel-config-reports-unavailable-options ()
  ;; The mock, like the real adapter, has no effort levels for haiku.
  (agentel-test-with-started session '(:model "haiku" :effort "high" :mode "nonsense")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-test-value session "model") "haiku"))
    (agentel-test-wait-for-text "effort is not available")
    (agentel-test-wait-for-text
     "Could not set mode: Invalid value for config option mode: nonsense")))

(ert-deftest agentel-config-leaves-aliases-to-the-agent ()
  (agentel-test-with-started session '(:model "opus")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-test-value session "model") "opus[1m]"))))

(ert-deftest agentel-config-can-be-changed-interactively ()
  (agentel-test-with-started session nil
    (agentel-config-test-settled session)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (should (string-match-p "Mode" prompt))
                 (should (member "Accept edits" (all-completions "" collection)))
                 "Accept edits")))
      (agentel-config-set-mode))
    (agentel-test-wait-until
     (lambda () (equal (agentel-config-test-value session "mode") "acceptEdits")))))

(ert-deftest agentel-config-follows-mode-changes-by-the-agent ()
  (agentel-test-with-started session nil
    (agentel-config-test-settled session)
    (agentel-session-dispatch
     `((sessionId . ,(agentel-session-id session))
       (update . ((sessionUpdate . "current_mode_update")
                  (currentModeId . "acceptEdits")))))
    (should (equal (agentel-config-test-value session "mode") "acceptEdits"))))

(ert-deftest agentel-config-restarts-with-the-current-settings ()
  (agentel-test-with-started session '(:model "sonnet")
    (agentel-config-test-settled session)
    (agentel-config-set session "mode" "plan")
    (agentel-test-wait-until
     (lambda () (equal (agentel-config-test-value session "mode") "plan")))
    (should (equal (agentel-config--restart-options session)
                   '(:model "sonnet" :effort "medium" :mode "plan")))))

(ert-deftest agentel-config-restarts-without-options-the-model-lacks ()
  (agentel-test-with-started session '(:model "haiku")
    (agentel-config-test-settled session)
    (should-not (plist-member (agentel-config--restart-options session) :effort))))

(provide 'agentel-config-test)
;;; agentel-config-test.el ends here
