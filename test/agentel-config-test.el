;;; agentel-config-test.el --- Tests for agentel-config  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-config)
(require 'agentel-test-helper)

(defconst agentel-config-test-copilot-mode
  "https://agentclientprotocol.com/protocol/session-modes#"
  "Prefix of the mode values of Copilot CLI.")

(defmacro agentel-config-test-with-copilot (var options &rest body)
  "Start a session on the mock of Copilot with OPTIONS, bind it to VAR, run BODY."
  (declare (indent 2))
  `(let ((agentel-agents (list (cons 'copilot (agentel-test-mock-command 'copilot)))))
     (agentel-test-with-started ,var (append '(:agent copilot) ,options) ,@body)))

(defun agentel-config-test-settled (session)
  "Wait until SESSION finished applying its start options."
  (agentel-test-wait-until
   (lambda () (and (eq (agentel-session-state session) 'idle)
                   (agentel-config-option session "model")))))

(ert-deftest agentel-config-applies-start-options-in-order ()
  (agentel-test-with-started session '(:model "opus[1m]" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-value session "model") "opus[1m]"))
    (should (equal (agentel-config-value session "thought_level") "high"))
    (should (equal (agentel-config-value session "mode") "plan"))))

(ert-deftest agentel-config-runs-the-session-until-start-options-are-applied ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil)
        answer)
    (cl-letf (((symbol-function 'agentel-config-set)
               (lambda (_session _id _value callback) (setq answer callback))))
      (let ((session (agentel-session-create)))
        (agentel-session-register session "s1")
        (agentel-config--on-started
         session '((configOptions . [((id . "model") (category . "model"))]))
         '(:model "opus"))
        (should (eq (agentel-session-state session) 'running))
        (funcall answer)
        (should (eq (agentel-session-state session) 'idle))))))

(ert-deftest agentel-config-applies-start-options-to-the-options-of-their-category ()
  (agentel-config-test-with-copilot session '(:model "gpt-5.5" :effort "high")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-value session "model") "gpt-5.5"))
    (should (equal (alist-get 'id (agentel-config-option session "thought_level"))
                   "reasoning_effort"))
    (should (equal (agentel-config-value session "thought_level") "high"))))

(ert-deftest agentel-config-takes-the-name-of-a-value ()
  (agentel-config-test-with-copilot session '(:mode "plan")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-value session "mode")
                   (concat agentel-config-test-copilot-mode "plan")))))

(ert-deftest agentel-config-shows-the-settings-in-the-header ()
  (agentel-test-with-started session '(:model "opus[1m]" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (string-match-p "Opus 5\\.5 · High · Plan" agentel-chat--header-line))))

(ert-deftest agentel-config-shows-the-settings-of-other-agents-in-the-same-order ()
  (agentel-config-test-with-copilot session '(:model "gpt-5.5" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (string-match-p "GPT-5\\.5 · high · Plan" agentel-chat--header-line))))

(ert-deftest agentel-config-names-other-options-in-the-header ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil))
    (let ((session (agentel-session-create)))
      (agentel-config--on-update
       session '((sessionUpdate . "config_option_update")
                 (configOptions
                  . [((id . "fast") (name . "Fast mode") (currentValue . "off")
                      (options . [((value . "off") (name . "Off"))]))
                     ((id . "model") (name . "Model") (category . "model")
                      (currentValue . "sonnet")
                      (options . [((value . "sonnet") (name . "Sonnet 5"))]))])))
      (should (equal (agentel-ui-view
                      (agentel-store-find (agentel-session-store session) 'config))
                     "Sonnet 5 · Fast mode: Off")))))

(ert-deftest agentel-config-applies-start-options-until-told-it-finished ()
  (let ((data (agentel-config--update '(start-applying) '((options)))))
    (should (alist-get 'applying data))
    (should-not (alist-get 'applying (agentel-config--update '(finish-applying) data)))))

(ert-deftest agentel-config-selecting-a-value-changes-only-its-option ()
  (should (equal (alist-get 'options
                            (agentel-config--update
                             '(select "mode" "plan")
                             '((options ((id . "model") (category . "model") (value . "opus"))
                                        ((id . "mode") (category . "mode") (value . "default"))))))
                 '(((id . "model") (category . "model") (value . "opus"))
                   ((id . "mode") (category . "mode") (value . "plan"))))))

(ert-deftest agentel-config-reports-unavailable-options ()
  ;; The mock, like the real adapter, has no effort levels for haiku.
  (agentel-test-with-started session '(:model "haiku" :effort "high" :mode "nonsense")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-value session "model") "haiku"))
    (agentel-test-wait-for-text "effort is not available")
    (agentel-test-wait-for-text
     "Could not set mode: Invalid value for config option mode: nonsense")))

(ert-deftest agentel-config-leaves-aliases-to-the-agent ()
  (agentel-test-with-started session '(:model "opus")
    (agentel-config-test-settled session)
    (should (equal (agentel-config-value session "model") "opus[1m]"))))

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
     (lambda () (equal (agentel-config-value session "mode") "acceptEdits")))))

(ert-deftest agentel-config-follows-mode-changes-by-the-agent ()
  (agentel-test-with-started session nil
    (agentel-config-test-settled session)
    (agentel-session-dispatch
     `((sessionId . ,(agentel-session-id session))
       (update . ((sessionUpdate . "current_mode_update")
                  (currentModeId . "acceptEdits")))))
    (should (equal (agentel-config-value session "mode") "acceptEdits"))))

(ert-deftest agentel-config-restarts-with-the-current-settings ()
  (agentel-test-with-started session '(:model "sonnet")
    (agentel-config-test-settled session)
    (agentel-config-set session "mode" "plan")
    (agentel-test-wait-until
     (lambda () (equal (agentel-config-value session "mode") "plan")))
    (should (equal (agentel-config--restart-options session)
                   '(:model "sonnet" :effort "medium" :mode "plan")))))

(ert-deftest agentel-config-restarts-other-agents-with-the-current-settings ()
  (agentel-config-test-with-copilot session '(:model "gpt-5.5" :effort "high" :mode "plan")
    (agentel-config-test-settled session)
    (should (equal (agentel-config--restart-options session)
                   `(:model "gpt-5.5" :effort "high"
                            :mode ,(concat agentel-config-test-copilot-mode "plan"))))))

(ert-deftest agentel-config-changes-the-effort-of-other-agents-interactively ()
  (agentel-config-test-with-copilot session nil
    (agentel-config-test-settled session)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _collection &rest _)
                 (should (string-match-p "Reasoning Effort" prompt))
                 "high")))
      (agentel-config-set-effort))
    (agentel-test-wait-until
     (lambda () (equal (agentel-config-value session "thought_level") "high")))))

(ert-deftest agentel-config-restarts-without-options-the-model-lacks ()
  (agentel-test-with-started session '(:model "haiku")
    (agentel-config-test-settled session)
    (should-not (plist-member (agentel-config--restart-options session) :effort))))

(ert-deftest agentel-config-rejects-an-unknown-message ()
  (should-error (agentel-config--update '(forget) nil)
                :type 'agentel-store-unknown-message))

(provide 'agentel-config-test)
;;; agentel-config-test.el ends here
