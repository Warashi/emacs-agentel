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

(ert-deftest agentel-summary-run-speaks-the-version-chosen-for-its-agent ()
  (agentel-summary-test-with-mock
    (let* ((agentel-acp-v2-agents '(mock))
           (connection (agentel-summary-run temporary-file-directory "x" #'ignore)))
      (unwind-protect
          (should (equal (agentel-test-wait-until
                          (lambda () (agentel-connection-version connection)))
                         2))
        (agentel-connection-shutdown connection)))))

(ert-deftest agentel-summary-run-v2-answers-once-the-agent-becomes-idle ()
  (agentel-summary-test-with-mock
    (let ((agentel-acp-v2-agents '(mock))
          done reply)
      (agentel-summary-run temporary-file-directory "the excerpt"
                           (lambda (text) (setq done t reply text)))
      (agentel-test-wait-until (lambda () done))
      (should (string-match-p "the excerpt" reply)))))

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

(ert-deftest agentel-summary-run-answers-nil-when-the-agent-does-not-start ()
  (agentel-summary-test-with-mock
    (let ((agentel-agents '((mock "agentel-no-such-program")))
          (done nil) (reply 'unset))
      (agentel-summary-run temporary-file-directory "x"
                           (lambda (text) (setq done t reply text)))
      (agentel-test-wait-until (lambda () done))
      (should-not reply))))

(ert-deftest agentel-summary-run-needs-an-agent-and-a-model ()
  (let ((agentel-summary-agent '(:agent claude)))
    (should-error (agentel-summary-run temporary-file-directory "x" #'ignore)
                  :type 'user-error)))

;;;; Summing up the sessions that need the user

(defvar agentel-summary-test-runs nil
  "Summaries asked of the fake summarizer, newest first.
Each is a plist with the :excerpt, the :callback and :cancelled.")

(defun agentel-summary-test-start (_cwd excerpt callback)
  "Record a summary of EXCERPT to answer with CALLBACK and return its cancel."
  (let ((run (list :excerpt excerpt :callback callback :cancelled nil)))
    (push run agentel-summary-test-runs)
    (lambda () (plist-put run :cancelled t))))

(defun agentel-summary-test-answer (run text)
  "Answer the summary RUN with TEXT."
  (funcall (plist-get run :callback) text))

(defmacro agentel-summary-test-with-sessions (&rest body)
  "Run BODY with the sessions `one' and `two', none shown, summaries faked."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-list--store (agentel-store-create))
         (agentel-session-changed-functions (list #'agentel-list--on-changed))
         (agentel-summary-agent '(:agent mock :model "haiku"))
         (agentel-summary--start-function #'agentel-summary-test-start)
         (agentel-summary--job nil)
         (agentel-summary-test-runs nil))
     (let ((one (agentel-session-create :cwd "/tmp/one/"))
           (two (agentel-session-create :cwd "/tmp/two/")))
       (agentel-session-register one "1")
       (agentel-session-register two "2")
       (agentel-chat-open one :input t)
       (agentel-chat-open two :input t)
       (agentel-conversation-prompt one "work on one")
       (agentel-conversation-prompt two "work on two")
       (unwind-protect
           (save-window-excursion
             (with-temp-buffer
               (switch-to-buffer (current-buffer))
               ,@body))
         (kill-buffer (agentel-session-buffer one))
         (kill-buffer (agentel-session-buffer two))))))

(ert-deftest agentel-summary-is-off-by-default ()
  (agentel-summary-test-with-sessions
    (let ((agentel-summary-agent nil))
      (agentel-test-ask one)
      (agentel-summary--reconcile)
      (should-not agentel-summary-test-runs))))

(ert-deftest agentel-summary-sums-up-a-session-that-waits-for-the-user ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (should (= (length agentel-summary-test-runs) 1))
    (should (string-match-p "work on one" (plist-get (car agentel-summary-test-runs)
                                                     :excerpt)))
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: one")
    (should (equal (agentel-summary-text one) "Goal: one"))))

(ert-deftest agentel-summary-sums-up-a-session-whose-turn-ended-out-of-sight ()
  (agentel-summary-test-with-sessions
    (agentel-session-send two '(start-turn))
    (agentel-session-send two '(finish-turn))
    (agentel-summary--reconcile)
    (should (string-match-p "work on two" (plist-get (car agentel-summary-test-runs)
                                                     :excerpt)))))

(ert-deftest agentel-summary-sums-up-one-session-at-a-time-in-the-order-listed ()
  (agentel-summary-test-with-sessions
    (agentel-session-send one '(start-turn))
    (agentel-session-send one '(finish-turn))
    (agentel-test-ask two)
    (agentel-summary--reconcile)
    (should (= (length agentel-summary-test-runs) 1))
    (should (string-match-p "work on two" (plist-get (car agentel-summary-test-runs)
                                                     :excerpt)))
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: two")
    (should (= (length agentel-summary-test-runs) 2))
    (should (string-match-p "work on one" (plist-get (car agentel-summary-test-runs)
                                                     :excerpt)))))

(ert-deftest agentel-summary-keeps-the-summary-once-the-session-is-shown ()
  (agentel-summary-test-with-sessions
    (agentel-session-send two '(start-turn))
    (agentel-session-send two '(finish-turn))
    (agentel-summary--reconcile)
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: two")
    (switch-to-buffer (agentel-session-buffer two))
    (agentel-list--forget-shown (selected-frame))
    (agentel-summary--reconcile)
    (should (equal (agentel-summary-text two) "Goal: two"))))

(ert-deftest agentel-summary-forgets-the-summary-once-the-session-moves-on ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: one")
    (agentel-test-answer one)
    (agentel-summary--reconcile)
    (should-not (agentel-summary-text one))))

(ert-deftest agentel-summary-cancels-the-summary-of-a-session-that-moved-on ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (let ((run (car agentel-summary-test-runs)))
      (agentel-test-answer one)
      (agentel-summary--reconcile)
      (should (plist-get run :cancelled))
      (agentel-summary-test-answer run "Goal: stale")
      (should-not (agentel-summary-text one)))))

(ert-deftest agentel-summary-sums-up-a-session-again-when-it-waits-again ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: first")
    (agentel-test-answer one)
    (agentel-summary--reconcile)
    (agentel-conversation-send one 'second 'agentel-test-question '(ask))
    (agentel-summary--reconcile)
    (should (= (length agentel-summary-test-runs) 2))))

(ert-deftest agentel-summary-does-not-sum-up-again-after-it-failed ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (agentel-summary-test-answer (car agentel-summary-test-runs) nil)
    (agentel-summary--reconcile)
    (should (= (length agentel-summary-test-runs) 1))
    (should-not (agentel-summary-text one))))

(ert-deftest agentel-summary-goes-on-to-the-next-session-when-one-fails-at-once ()
  (agentel-summary-test-with-sessions
    (let* ((cancelled nil)
           (agentel-summary--start-function
            (lambda (cwd excerpt callback)
              (if (equal cwd "/tmp/one/")
                  (progn (funcall callback nil) (lambda () (setq cancelled t)))
                (agentel-summary-test-start cwd excerpt callback)))))
      (agentel-test-ask one)
      (agentel-test-ask two)
      (agentel-summary--reconcile)
      (should (= (length agentel-summary-test-runs) 1))
      (should (eq (car agentel-summary--job) two))
      (should-not cancelled))))

(ert-deftest agentel-summary-is-pinned-above-the-prompt ()
  (agentel-summary-test-with-sessions
    (agentel-test-ask one)
    (agentel-summary--reconcile)
    (agentel-summary-test-answer (car agentel-summary-test-runs) "Goal: one")
    (should (string-match-p "Goal: one"
                            (agentel-chat--pinned (agentel-session-store one)
                                                  (agentel-chat--view-options one))))))

;;; agentel-summary-test.el ends here
