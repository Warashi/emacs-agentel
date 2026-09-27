;;; agentel-async-task-test.el --- Tests for agentel-async-task  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-async-task)
(require 'agentel-test-helper)

(defun agentel-async-task-test-pin ()
  "Return the lines pinned above the prompt of the current buffer."
  (when-let* ((pin (overlay-get agentel-chat--pin 'before-string)))
    (substring-no-properties pin)))

(ert-deftest agentel-async-task-advertises-the-air-capability ()
  (agentel-test-with-started session nil
    (let* ((received (alist-get 'receivedCapabilities
                                (alist-get '_meta (agentel-connection-info
                                                   (agentel-session-connection session)))))
           (air (alist-get 'air (alist-get 'jetbrains (alist-get '_meta received)))))
      (should (equal (alist-get 'version air) 1))
      (should (seq-contains-p (alist-get 'capabilities air) "asyncTasks")))))

(ert-deftest agentel-async-task-running-is-pinned-above-the-prompt ()
  (agentel-test-with-started session nil
    (agentel-test-send "async please")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should (equal (agentel-async-task-test-pin)
                   "⚙ npm run dev 🏃 ↳ Listening on port 3000\n"))
    (agentel-test-send "hello")
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should-not (agentel-async-task-test-pin))))

(ert-deftest agentel-async-task-keeps-only-the-latest-progress ()
  (let* ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (session (agentel-session-create)))
    (agentel-async-task--on-update
     session '((sessionUpdate . "async_task_spawned") (asyncTaskId . "b1") (name . "dev")))
    (dotimes (i 100)
      (agentel-async-task--on-update
       session `((sessionUpdate . "async_task_progress") (asyncTaskId . "b1")
                 (summary . ,(format "line %d" i)))))
    (let ((task (cdr (assoc "b1" (agentel-async-task--tasks session)))))
      (should (equal (alist-get 'progress task) "line 99"))
      (should (= (length task) 3)))))

(defun agentel-async-task-test-tasks (&rest messages)
  "Return the tasks after MESSAGES."
  (let (data)
    (dolist (message messages)
      (setq data (agentel-async-task--update message data)))
    (alist-get 'tasks data)))

(ert-deftest agentel-async-task-is-forgotten-once-it-stops ()
  (should (equal (mapcar (lambda (task) (list (car task) (alist-get 'state (cdr task))))
                         (agentel-async-task-test-tasks
                          '(spawn "b1" "dev") '(spawn "b2" "test")
                          '(change-state "b2" paused) '(change-state "b1" completed)))
                 '(("b2" paused)))))

(ert-deftest agentel-async-task-ignores-news-of-an-unknown-task ()
  (should-not (agentel-async-task-test-tasks
               '(progress "b1" "Compiling") '(change-state "b1" running))))

(ert-deftest agentel-async-task-shows-the-progress-on-one-line ()
  (let* ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (session (agentel-session-create)))
    (agentel-async-task--on-update
     session '((sessionUpdate . "async_task_spawned") (asyncTaskId . "b1") (name . "dev")))
    (agentel-async-task--on-update
     session '((sessionUpdate . "async_task_progress") (asyncTaskId . "b1")
               (summary . "Compiling\nerror: missing semicolon")))
    (should (equal (mapcar #'substring-no-properties (agentel-async-task--pin session))
                   '("⚙ dev 🏃 ↳ Compiling…")))))

(ert-deftest agentel-async-task-is-not-sent-without-the-capability ()
  (let ((agentel-connection-capability-functions
         (remq #'agentel-async-task--capabilities agentel-connection-capability-functions)))
    (agentel-test-with-started session nil
      (agentel-test-send "async please")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (agentel-async-task-test-pin)))))

(provide 'agentel-async-task-test)
;;; agentel-async-task-test.el ends here
