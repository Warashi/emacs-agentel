;;; agentel-summary-test.el --- Tests for agentel-summary  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-summary)
(require 'agentel-test-helper)

(defmacro agentel-summary-test-with-mock (&rest body)
  "Run BODY with `agentel-summary-agent' on the mock agent."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-command-prefix nil)
         (agentel-environment nil)
         (agentel-agents (list (cons 'mock (agentel-test-mock-command))))
         (agentel-summary-agent '(:agent mock :model "haiku"))
         ;; The mock agent acts on words such as "ask" in the prompt.
         (agentel-summary--instructions "Sum up: "))
     ,@body))

(defun agentel-summary-test-say (session type text)
  "Add a finished message of TYPE saying TEXT to SESSION."
  (agentel-conversation-send session nil type `(chunk ,text))
  (agentel-conversation-finish-message session))

;;;; Excerpt

(ert-deftest agentel-summary-excerpt-has-the-title-the-first-prompt-and-the-last-items ()
  (let ((agentel-session--registry nil)
        (agentel-summary-excerpt-items 2))
    (let ((session (agentel-session-create :cwd "/tmp/one/")))
      (agentel-session-send session '(retitle "Fix the parser"))
      (agentel-conversation-prompt session "first prompt")
      (agentel-summary-test-say session 'agent "middle reply")
      (agentel-conversation-prompt session "second prompt")
      (agentel-summary-test-say session 'agent "last reply")
      (let ((excerpt (agentel-summary-excerpt session)))
        (should (string-match-p "Fix the parser" excerpt))
        (should (string-match-p "first prompt" excerpt))
        (should (string-match-p "second prompt" excerpt))
        (should (string-match-p "last reply" excerpt))
        (should-not (string-match-p "middle reply" excerpt))))))

(ert-deftest agentel-summary-excerpt-has-what-a-subagent-asks ()
  (let ((agentel-session--registry nil))
    (let* ((parent (agentel-session-create :cwd "/tmp/one/"))
           (child (agentel-session-create :parent parent :cwd "/tmp/one/")))
      (agentel-conversation-prompt parent "do it")
      (agentel-conversation-send child '(permission . 1) 'permission
                                 '(ask "Allow rm -rf build?" ("Allow" "Reject")))
      (should (string-match-p "Allow rm -rf build\\?"
                              (agentel-summary-excerpt parent))))))

(ert-deftest agentel-summary-excerpt-cuts-a-long-item ()
  (let ((agentel-session--registry nil)
        (agentel-summary-item-length 10))
    (let ((session (agentel-session-create :cwd "/tmp/one/")))
      (agentel-summary-test-say session 'agent (make-string 50 ?x))
      (should-not (string-match-p (make-string 11 ?x)
                                  (agentel-summary-excerpt session))))))

;;;; Running the summarizer

(ert-deftest agentel-summary-run-answers-with-the-reply-of-the-agent ()
  (agentel-summary-test-with-mock
    (let (done reply)
      (agentel-summary-run temporary-file-directory "the excerpt"
                           (lambda (text) (setq done t reply text)))
      (agentel-test-wait-until (lambda () done))
      (should (string-match-p "the excerpt" reply)))))

(ert-deftest agentel-summary-run-stops-its-agent-once-it-answered ()
  (agentel-summary-test-with-mock
    (let* (done
           (connection (agentel-summary-run temporary-file-directory "x"
                                            (lambda (_text) (setq done t)))))
      (agentel-test-wait-until (lambda () done))
      (agentel-test-wait-until (lambda () (not (agentel-connection-live-p connection)))))))

(ert-deftest agentel-summary-run-keeps-its-conversation-out-of-the-sessions ()
  (agentel-summary-test-with-mock
    (let (done)
      (agentel-summary-run temporary-file-directory "x"
                           (lambda (_text) (setq done t)))
      (agentel-test-wait-until (lambda () done))
      (should-not (agentel-session-list)))))

(ert-deftest agentel-summary-run-answers-nil-for-a-model-the-agent-rejects ()
  (agentel-summary-test-with-mock
    (let ((agentel-summary-agent '(:agent mock :model "no-such-model"))
          (done nil) (reply 'unset))
      (agentel-summary-run temporary-file-directory "x"
                           (lambda (text) (setq done t reply text)))
      (agentel-test-wait-until (lambda () done))
      (should-not reply))))

(ert-deftest agentel-summary-run-does-not-answer-once-cancelled ()
  (agentel-summary-test-with-mock
    (let* (answered
           (connection (agentel-summary-run temporary-file-directory "x"
                                            (lambda (_text) (setq answered t)))))
      (agentel-connection-shutdown connection)
      (accept-process-output nil 0.5)
      (should-not answered))))

(ert-deftest agentel-summary-run-needs-an-agent-and-a-model ()
  (let ((agentel-summary-agent '(:agent claude)))
    (should-error (agentel-summary-run temporary-file-directory "x" #'ignore)
                  :type 'user-error)))

;;; agentel-summary-test.el ends here
