;;; agentel-title-test.el --- Tests for agentel-title  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-title)

(defmacro agentel-title-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-title--on-update)))
     (let ((session (agentel-session-create)))
       (agentel-session-register session "s1")
       ,@body)))

(ert-deftest agentel-title-follows-the-title-the-agent-gives ()
  (agentel-title-test-with-session
    (agentel-session-dispatch
     '((sessionId . "s1")
       (update . ((sessionUpdate . "session_info_update")
                  (title . "Fix the bug")))))
    (should (equal (agentel-session-title session) "Fix the bug"))))

(ert-deftest agentel-title-stays-when-the-agent-gives-none ()
  (agentel-title-test-with-session
    (agentel-session-send session '(retitle "Fix the bug"))
    (agentel-session-dispatch
     '((sessionId . "s1")
       (update . ((sessionUpdate . "session_info_update")
                  (updatedAt . "2026-01-01T00:00:00Z")))))
    (should (equal (agentel-session-title session) "Fix the bug"))))

(provide 'agentel-title-test)
;;; agentel-title-test.el ends here
