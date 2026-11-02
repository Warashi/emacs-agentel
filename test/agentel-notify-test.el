;;; agentel-notify-test.el --- Tests for agentel-notify  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agentel-notify)

(defmacro agentel-notify-test-with-sessions (&rest body)
  "Run BODY with no session, binding `notified' to the notifications sent.
`notified' holds the title and the body of each, newest first."
  (declare (indent 0))
  `(let* ((agentel-session--registry nil)
          (agentel-session-update-functions nil)
          (agentel-session-waiting-functions nil)
          (agentel-session-running-functions nil)
          (agentel-session-changed-functions (list #'agentel-notify--on-changed))
          (agentel-notify--states (make-hash-table :test 'eq :weakness 'key))
          (notified nil)
          (agentel-notify-function (lambda (title body) (push (list title body) notified))))
     ,@body))

(defun agentel-notify-test-session (id &optional parent)
  "Return a session registered as ID, spawned by PARENT if given."
  (let ((session (agentel-session-create :cwd "/tmp/project/" :parent parent)))
    (agentel-session-register session id)
    session))

(ert-deftest agentel-notify-tells-each-state-the-user-acts-in ()
  (agentel-notify-test-with-sessions
    (let ((session (agentel-notify-test-session "s1"))
          waiting)
      (should (equal notified '(("project" "Ready"))))
      (agentel-session-send session '(start-turn))
      (should (= (length notified) 1))
      (add-hook 'agentel-session-waiting-functions (lambda (_session) waiting))
      (setq waiting t)
      (agentel-session-waiting-changed session)
      (should (equal (car notified) '("project" "Waiting for you")))
      (setq waiting nil)
      (agentel-session-waiting-changed session)
      (agentel-session-send session '(finish-turn))
      (should (equal (car notified) '("project" "Ready")))
      (agentel-session-send session '(end failed))
      (should (equal (car notified) '("project" "Failed")))
      (should (= (length notified) 4)))))

(ert-deftest agentel-notify-tells-a-state-once ()
  (agentel-notify-test-with-sessions
    (let ((session (agentel-notify-test-session "s1")))
      (agentel-session-changed session)
      (should (= (length notified) 1)))))

(ert-deftest agentel-notify-tells-an-earlier-session-loaded ()
  (agentel-notify-test-with-sessions
    (let ((session (agentel-session-create :cwd "/tmp/project/")))
      (agentel-session-register session "s1")
      (setq notified nil)
      (agentel-session-send session '(start-loading))
      (should-not notified)
      (agentel-session-send session '(finish-loading))
      (should (equal notified '(("project" "Ready")))))))

(ert-deftest agentel-notify-tells-an-exited-session ()
  (agentel-notify-test-with-sessions
    (let ((session (agentel-notify-test-session "s1")))
      (agentel-session-send session '(end exited))
      (should (equal (car notified) '("project" "Exited"))))))

(ert-deftest agentel-notify-tells-only-the-states-chosen ()
  (agentel-notify-test-with-sessions
    (let ((agentel-notify-states '(waiting)))
      (agentel-notify-test-session "s1")
      (should-not notified))))

(ert-deftest agentel-notify-tells-the-wait-of-a-subagent-by-its-root ()
  (agentel-notify-test-with-sessions
    (let* ((parent (agentel-notify-test-session "s1"))
           (child (agentel-notify-test-session "c1" parent)))
      (setq notified nil)
      (add-hook 'agentel-session-waiting-functions (lambda (s) (eq s child)))
      (agentel-session-waiting-changed child)
      (agentel-session-send child '(end exited))
      (should (equal notified '(("project" "Waiting for you")))))))

(ert-deftest agentel-notify-forgets-a-removed-session ()
  (agentel-notify-test-with-sessions
    (let ((session (agentel-notify-test-session "s1")))
      (agentel-session-remove session)
      (should-not (gethash session agentel-notify--states))
      (should (= (length notified) 1)))))

(ert-deftest agentel-notify-writes-an-osc-777-notification ()
  (should (equal (agentel-notify--osc "a;b\ec" "d;e\af")
                 "\e]777;notify;a b c;d;e f\e\\")))

(ert-deftest agentel-notify-sends-to-each-text-terminal ()
  (let (sent)
    (cl-letf (((symbol-function 'terminal-list) (lambda () '(initial tty x)))
              ((symbol-function 'frame-initial-p) (lambda (terminal) (eq terminal 'initial)))
              ((symbol-function 'terminal-live-p) (lambda (terminal)
                                                    (if (eq terminal 'x) 'x t)))
              ((symbol-function 'send-string-to-terminal)
               (lambda (string terminal) (push (list string terminal) sent))))
      (agentel-notify-send "title" "body"))
    (should (equal sent `((,(agentel-notify--osc "title" "body") tty))))))

(ert-deftest agentel-notify-asks-macos-once-for-its-graphic-frames ()
  (let (started)
    (cl-letf (((symbol-function 'terminal-list) (lambda () '(ns ns)))
              ((symbol-function 'frame-initial-p) #'ignore)
              ((symbol-function 'terminal-live-p) (lambda (_terminal) 'ns))
              ((symbol-function 'start-process)
               (lambda (_name _buffer program &rest args) (push (cons program args) started))))
      (agentel-notify-send "a \"b\"" "c\\d"))
    (should (= (length started) 1))
    (should (equal (car (car started)) "osascript"))
    (should (equal (last (car started) 2) '("a \"b\"" "c\\d")))))

(provide 'agentel-notify-test)
;;; agentel-notify-test.el ends here
