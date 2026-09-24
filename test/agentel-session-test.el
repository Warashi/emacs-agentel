;;; agentel-session-test.el --- Tests for agentel-session  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-session)

(defmacro agentel-session-test-with-registry (&rest body)
  "Run BODY with an empty session registry and no hooks."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-update-functions nil)
         (agentel-session-changed-functions nil))
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

(ert-deftest agentel-session-state-reflects-activity ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (should (eq (agentel-session-state session) 'starting))
      (agentel-session-register session "s1")
      (should (eq (agentel-session-state session) 'idle))
      (agentel-session-set-busy session t)
      (should (eq (agentel-session-state session) 'running))
      (agentel-session-add-pending session 'question)
      (should (eq (agentel-session-state session) 'waiting))
      (agentel-session-remove-pending session 'question)
      (should (eq (agentel-session-state session) 'running)))))

(ert-deftest agentel-session-state-of-ended-session ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create)))
      (agentel-session-register session "s1")
      (agentel-session-set-ended session 'failed)
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
      (agentel-session-add-pending child 'question)
      (should (eq (agentel-session-state parent) 'waiting))
      (should (equal (agentel-session-pending-items parent)
                     (list (cons child 'question)))))))

(ert-deftest agentel-session-name-is-one-line ()
  (agentel-session-test-with-registry
    (let ((session (agentel-session-create :cwd "/tmp/project/")))
      (should (equal (agentel-session-name session) "project"))
      (setf (agentel-session-title session) "first\nsecond")
      (should (equal (agentel-session-name session) "first second")))))

(provide 'agentel-session-test)
;;; agentel-session-test.el ends here
