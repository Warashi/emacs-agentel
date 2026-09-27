;;; agentel-session-test.el --- Tests for agentel-session  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-session)

(defmacro agentel-session-test-with-registry (&rest body)
  "Run BODY with an empty session registry and no hooks."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-update-functions nil)
         (agentel-session-changed-functions nil)
         (agentel-session-waiting-functions nil)
         (agentel-session-running-functions nil))
     ,@body))

(ert-deftest agentel-session-register-makes-session-findable-by-id ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create :cwd "/tmp")))
      (should-not (agentel-session-get "s1"))
      (agentel-session-register session "s1")
      (should (eq (agentel-session-get "s1") session))
      (should (equal (agentel-session-id session) "s1")))))

(ert-deftest agentel-session-list-keeps-creation-order ()
  (agentel-session-test-with-registry
    (let ((a (agentel-session-create))
          (b (agentel-session-create)))
      (should (equal (agentel-session-list) (list a b))))))

(ert-deftest agentel-session-remove-forgets-session ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (agentel-session-register session "s1")
      (agentel-session-remove session)
      (should-not (agentel-session-get "s1"))
      (should-not (agentel-session-list)))))

(ert-deftest agentel-session-dispatch-passes-update-to-hook ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create))
          seen)
      (agentel-session-register session "s1")
      (add-hook 'agentel-session-update-functions
                (lambda (s update) (push (cons s update) seen)))
      (agentel-session-dispatch
       '((sessionId . "s1")
         (update . ((sessionUpdate . "agent_message_chunk")))))
      (should (equal seen
                     (list (cons session
                                 '((sessionUpdate . "agent_message_chunk")))))))))

(ert-deftest agentel-session-dispatch-withholds-update-a-feature-rejects ()
  (agentel-session-test-with-registry
    (let ((agentel-session-withhold-functions nil)
          (session (agentel-session-create))
          seen)
      (agentel-session-register session "s1")
      (add-hook 'agentel-session-withhold-functions
                (lambda (s update)
                  (and (eq s session)
                       (equal (alist-get 'sessionUpdate update) "plan"))))
      (add-hook 'agentel-session-update-functions
                (lambda (_s update) (push (alist-get 'sessionUpdate update) seen)))
      (dolist (kind '("plan" "agent_message_chunk"))
        (agentel-session-dispatch
         `((sessionId . "s1") (update . ((sessionUpdate . ,kind))))))
      (should (equal seen '("agent_message_chunk"))))))

(ert-deftest agentel-session-dispatch-tells-connections-apart ()
  (agentel-session-test-with-registry
    (let ((a (agentel-session-create :connection 'conn-a))
          (b (agentel-session-create :connection 'conn-b))
          seen)
      (agentel-session-register a "same")
      (agentel-session-register b "same")
      (add-hook 'agentel-session-update-functions
                (lambda (s _update) (push s seen)))
      (agentel-session-dispatch
       '((sessionId . "same") (update . ((sessionUpdate . "plan"))))
       'conn-b)
      (should (equal seen (list b)))
      (should (eq (agentel-session-get "same" 'conn-a) a)))))

(ert-deftest agentel-session-dispatch-ignores-unknown-session ()
  (agentel-session-test-with-registry
    (let (seen)
      (add-hook 'agentel-session-update-functions
                (lambda (s update) (push (cons s update) seen)))
      (agentel-session-dispatch
       '((sessionId . "s1:replay-subagent:t1")
         (update . ((sessionUpdate . "agent_message_chunk")))))
      (should-not seen))))

(ert-deftest agentel-session-info-update-sets-title ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (agentel-session-register session "s1")
      (agentel-session-dispatch
       '((sessionId . "s1")
         (update . ((sessionUpdate . "session_info_update")
                    (title . "Fix the bug")))))
      (should (equal (agentel-session-title session) "Fix the bug")))))

(ert-deftest agentel-session-data-change-notifies ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create))
          changed)
      (add-hook 'agentel-session-changed-functions
                (lambda (s) (push s changed)))
      (setf (agentel-session-data session 'usage) 42)
      (should (equal (agentel-session-data session 'usage) 42))
      (should (equal changed (list session))))))

(ert-deftest agentel-session-update-follows-the-life-of-a-session ()
  (let ((data nil))
    (dolist (message '((register "s1") (retitle "Fix the bug")
                       (start-loading) (start-turn)))
      (setq data (agentel-session--update message data)))
    (should (equal (alist-get 'id data) "s1"))
    (should (equal (alist-get 'title data) "Fix the bug"))
    (should (alist-get 'loading data))
    (should (alist-get 'in-turn data))
    (dolist (message '((finish-loading) (finish-turn) (end exited)))
      (setq data (agentel-session--update message data)))
    (should-not (alist-get 'loading data))
    (should-not (alist-get 'in-turn data))
    (should (eq (alist-get 'ended data) 'exited))
    (should (equal (alist-get 'title data) "Fix the bug"))))

(ert-deftest agentel-session-send-changes-the-session-and-tells-listeners ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create))
          changed)
      (add-hook 'agentel-session-changed-functions (lambda (s) (push s changed)))
      (agentel-session-send session '(retitle "Fix the bug"))
      (should (equal (agentel-session-title session) "Fix the bug"))
      (should (equal changed (list session))))))

(ert-deftest agentel-session-state-reflects-activity ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (should (eq (agentel-session-state session) 'starting))
      (agentel-session-register session "s1")
      (should (eq (agentel-session-state session) 'idle))
      (agentel-session-send session '(start-turn))
      (should (eq (agentel-session-state session) 'running))
      (let ((waiting t))
        (add-hook 'agentel-session-waiting-functions (lambda (_session) waiting))
        (should (eq (agentel-session-state session) 'waiting))
        (setq waiting nil)
        (should (eq (agentel-session-state session) 'running))))))

(ert-deftest agentel-session-runs-while-a-feature-says-it-works ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create))
          (working t))
      (agentel-session-register session "s1")
      (add-hook 'agentel-session-running-functions (lambda (_session) working))
      (should (eq (agentel-session-state session) 'running))
      (setq working nil)
      (should (eq (agentel-session-state session) 'idle)))))

(ert-deftest agentel-session-stops-running-once-ended ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (agentel-session-register session "s1")
      (add-hook 'agentel-session-running-functions #'always)
      (agentel-session-send session '(end exited))
      (should-not (agentel-session-running-p session)))))

(ert-deftest agentel-session-state-shows-an-earlier-session-loading ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create))
          (waiting nil))
      (agentel-session-register session "s1")
      (add-hook 'agentel-session-waiting-functions (lambda (_session) waiting))
      (agentel-session-send session '(start-loading))
      (should (eq (agentel-session-state session) 'loading))
      (agentel-session-send session '(start-turn))
      (should (eq (agentel-session-state session) 'loading))
      (setq waiting t)
      (should (eq (agentel-session-state session) 'waiting))
      (setq waiting nil)
      (agentel-session-send session '(finish-loading))
      (should (eq (agentel-session-state session) 'running)))))

(ert-deftest agentel-session-state-of-ended-session ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (agentel-session-register session "s1")
      (agentel-session-send session '(end failed))
      (should (eq (agentel-session-state session) 'failed)))))

(ert-deftest agentel-session-children-are-listed-under-parent ()
  (agentel-session-test-with-registry
    (let* ((parent (agentel-session-create))
           (child (agentel-session-create :parent parent)))
      (should (equal (agentel-session-children parent) (list child)))
      (should (equal (agentel-session-roots) (list parent))))))

(ert-deftest agentel-session-waits-while-a-subagent-waits ()
  (agentel-session-test-with-registry
    (let* ((parent (agentel-session-create))
           (child (agentel-session-create :parent parent)))
      (agentel-session-register parent "p")
      (agentel-session-register child "c")
      (add-hook 'agentel-session-waiting-functions (lambda (s) (eq s child)))
      (should (eq (agentel-session-state parent) 'waiting))
      (should (agentel-session-waiting-p parent)))))

(ert-deftest agentel-session-tells-the-sessions-above-when-waiting-changes ()
  (agentel-session-test-with-registry
    (let* ((parent (agentel-session-create))
           (child (agentel-session-create :parent parent))
           changed)
      (add-hook 'agentel-session-changed-functions (lambda (s) (push s changed)))
      (agentel-session-waiting-changed child)
      (should (equal changed (list parent child))))))

(ert-deftest agentel-session-name-is-one-line ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create :cwd "/tmp/project/")))
      (should (equal (agentel-session-name session) "project"))
      (agentel-session-send session '(retitle "first\nsecond"))
      (should (equal (agentel-session-name session) "first second")))))

(ert-deftest agentel-session-project-name-is-the-project-of-the-top-level-session ()
  (agentel-session-test-with-registry
    (let* ((parent (agentel-session-create :cwd "/tmp/one/" :project "repo-one"))
           (child (agentel-session-create :parent parent :cwd "/tmp/one/")))
      (should (equal (agentel-session-project-name parent) "repo-one"))
      (should (equal (agentel-session-project-name child) "repo-one")))))

(ert-deftest agentel-session-project-name-is-the-directory-outside-a-project ()
  (agentel-session-test-with-registry
    (should (equal (agentel-session-project-name
                    (agentel-session-create :cwd "/tmp/two/"))
                   "two"))
    (should (equal (agentel-session-project-name (agentel-session-create))
                   "agent"))))

(provide 'agentel-session-test)
;;; agentel-session-test.el ends here
