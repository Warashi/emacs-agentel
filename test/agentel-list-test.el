;;; agentel-list-test.el --- Tests for agentel-list  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-list)

(defmacro agentel-list-test-with-sessions (&rest body)
  "Run BODY with a parent `parent', its child `child' and another `other'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions (list #'agentel-list--schedule-refresh))
         (agentel-chat-header-functions (list (lambda (_s) "Opus · High"))))
     (let* ((parent (agentel-session-create :cwd "/tmp/one/"))
            (other (agentel-session-create :cwd "/tmp/two/"))
            (child (agentel-session-create :parent parent :cwd "/tmp/one/")))
       (agentel-session-register parent "p")
       (agentel-session-register other "o")
       (agentel-session-register child "c")
       (setf (agentel-session-title child) "Explore")
       (agentel-chat-open parent :input t)
       (agentel-chat-open child)
       (agentel-chat-open other :input t)
       (unwind-protect
           (with-current-buffer (agentel-list-noselect)
             ,@body)
         (dolist (s (list parent child other))
           (kill-buffer (agentel-session-buffer s)))
         (kill-buffer agentel-list-buffer-name)))))

(defun agentel-list-test-names ()
  "Return the name column of every row."
  (mapcar (lambda (entry) (substring-no-properties (aref (cadr entry) 1)))
          (funcall tabulated-list-entries)))

(ert-deftest agentel-list-shows-subagents-under-their-parent ()
  (agentel-list-test-with-sessions
    (should (equal (agentel-list-test-names) '("one" "  └ Explore" "two")))))

(ert-deftest agentel-list-shows-state-and-details ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending child 'question)
    (let ((row (cadr (assq parent (funcall tabulated-list-entries)))))
      (should (equal (substring-no-properties (aref row 0)) "waiting"))
      (should (equal (aref row 2) "Opus · High"))
      (should (equal (aref row 3) "/tmp/one/")))))

(ert-deftest agentel-list-follows-changes ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-list--refresh)
    (goto-char (point-min))
    (should (search-forward "running" nil t))))

(ert-deftest agentel-list-visits-the-session-at-point ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "Explore")
    (agentel-list-visit)
    (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))))

(provide 'agentel-list-test)
;;; agentel-list-test.el ends here
