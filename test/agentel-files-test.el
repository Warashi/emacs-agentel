;;; agentel-files-test.el --- Tests for agentel-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agentel-files)

(defun agentel-files-test-touch (dir &rest names)
  "Create the files NAMES, with their directories, under DIR."
  (dolist (name names)
    (let ((file (expand-file-name name dir)))
      (make-directory (file-name-directory file) t)
      (write-region "" nil file nil 'silent))))

(defmacro agentel-files-test-with-dir (git &rest body)
  "Run BODY with `dir' bound to a new directory, a git repository if GIT."
  (declare (indent 1))
  `(let ((dir (file-name-as-directory (make-temp-file "agentel-files" t))))
     (unwind-protect
         (progn
           (when ,git
             (let ((default-directory dir))
               (should (zerop (call-process "git" nil nil nil "init" "-q")))))
           ,@body)
       (delete-directory dir t))))

(defmacro agentel-files-test-with-chat (dir &rest body)
  "Run BODY in the chat buffer of a session whose cwd is DIR."
  (declare (indent 1))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil))
     (let* ((session (agentel-session-create :cwd ,dir))
            (buffer (agentel-chat-open session :input t)))
       (unwind-protect
           (with-current-buffer buffer
             (goto-char (point-max))
             ,@body)
         (kill-buffer buffer)))))

(defun agentel-files-test-candidates ()
  "Return the completion candidates at point."
  (when-let* ((capf (agentel-files-completion-at-point)))
    (sort (all-completions (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                           (nth 2 capf))
          #'string<)))

(ert-deftest agentel-files-lists-tracked-and-untracked-files-of-a-repository ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el" "sub/b.el" "ignored.log")
    (write-region "*.log\n" nil (expand-file-name ".gitignore" dir) nil 'silent)
    (should (equal (sort (agentel-files--list dir) #'string<)
                   '(".gitignore" "a.el" "sub/b.el")))))

(ert-deftest agentel-files-lists-files-outside-a-repository-but-not-hidden-ones ()
  (agentel-files-test-with-dir nil
    (agentel-files-test-touch dir "a.el" "sub/b.el" ".hidden/c.el")
    (should (equal (sort (agentel-files--list dir) #'string<)
                   '("a.el" "sub/b.el")))))

(ert-deftest agentel-files-lists-nothing-for-a-repository-that-ignores-all-files ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.log")
    (write-region "*.log\n.gitignore\n" nil (expand-file-name ".gitignore" dir) nil 'silent)
    (should-not (agentel-files--list dir))))

(ert-deftest agentel-files-does-not-take-the-optional-locks-of-git ()
  (agentel-files-test-with-dir t
    (let ((calls nil)
          (call-process (symbol-function 'call-process)))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program &rest args)
                   (push (nthcdr 3 args) calls)
                   (apply call-process program args))))
        (agentel-files--list dir))
      (should (equal (car calls) '("--no-optional-locks" "ls-files" "-z"
                                   "--cached" "--others" "--exclude-standard"))))))

(ert-deftest agentel-files-completes-a-path-after-an-at-sign ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el" "sub/b.el")
    (agentel-files-test-with-chat dir
      (insert "look at @su")
      (should (equal (agentel-files-test-candidates) '("sub/b.el")))
      (let ((capf (agentel-files-completion-at-point)))
        (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                       "su"))))))

(ert-deftest agentel-files-ignores-text-that-is-not-an-at-mention ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el")
    (agentel-files-test-with-chat dir
      (insert "su")
      (should-not (agentel-files-completion-at-point))
      (insert " me@a")
      (should-not (agentel-files-completion-at-point)))))

(provide 'agentel-files-test)
;;; agentel-files-test.el ends here
