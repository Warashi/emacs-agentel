;;; agentel-sleep-test.el --- Tests for agentel-sleep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agentel-sleep)

(defmacro agentel-sleep-test-with-sessions (&rest body)
  "Run BODY with no session, binding `blocks' to the sleep blocks held."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-update-functions nil)
         (agentel-session-waiting-functions nil)
         (agentel-session-running-functions nil)
         (agentel-session-changed-functions (list #'agentel-sleep--on-changed))
         (agentel-sleep--supported t)
         (agentel-sleep--token nil)
         (blocks nil)
         (count 0))
     (cl-letf (((symbol-function 'system-sleep-block-sleep)
                (lambda (_why allow-display-sleep)
                  (should allow-display-sleep)
                  (car (push (cl-incf count) blocks))))
               ((symbol-function 'system-sleep-unblock-sleep)
                (lambda (token) (setq blocks (delq token blocks)) t)))
       ,@body)))

(defun agentel-sleep-test-session (id)
  "Return a session registered as ID."
  (let ((session (agentel-session-create :cwd "/tmp/project/")))
    (agentel-session-register session id)
    session))

(ert-deftest agentel-sleep-blocks-sleep-during-a-turn ()
  (agentel-sleep-test-with-sessions
    (let ((session (agentel-sleep-test-session "s1")))
      (should-not blocks)
      (agentel-session-send session '(start-turn))
      (should (= (length blocks) 1))
      (agentel-session-send session '(finish-turn))
      (should-not blocks))))

(ert-deftest agentel-sleep-lets-sleep-while-waiting-for-the-user ()
  (agentel-sleep-test-with-sessions
    (let ((session (agentel-sleep-test-session "s1"))
          waiting)
      (add-hook 'agentel-session-waiting-functions (lambda (_session) waiting))
      (agentel-session-send session '(start-turn))
      (setq waiting t)
      (agentel-session-waiting-changed session)
      (should-not blocks)
      (setq waiting nil)
      (agentel-session-waiting-changed session)
      (should (= (length blocks) 1)))))

(ert-deftest agentel-sleep-holds-one-block-while-any-session-runs ()
  (agentel-sleep-test-with-sessions
    (let ((s1 (agentel-sleep-test-session "s1"))
          (s2 (agentel-sleep-test-session "s2")))
      (agentel-session-send s1 '(start-turn))
      (agentel-session-send s2 '(start-turn))
      (should (= (length blocks) 1))
      (agentel-session-send s1 '(finish-turn))
      (should (= (length blocks) 1))
      (agentel-session-send s2 '(end exited))
      (should-not blocks))))

(ert-deftest agentel-sleep-lets-sleep-once-a-running-session-is-gone ()
  (agentel-sleep-test-with-sessions
    (let ((session (agentel-sleep-test-session "s1")))
      (agentel-session-send session '(start-turn))
      (agentel-session-remove session)
      (should-not blocks))))

(ert-deftest agentel-sleep-does-nothing-where-unsupported ()
  (agentel-sleep-test-with-sessions
    (let ((agentel-sleep--supported nil)
          (session (agentel-sleep-test-session "s1")))
      (agentel-session-send session '(start-turn))
      (should-not blocks))))

(provide 'agentel-sleep-test)
;;; agentel-sleep-test.el ends here
