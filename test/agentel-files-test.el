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
  "Run BODY with `dir' bound to a new directory, a git repository if GIT.
Each test has caches of its own."
  (declare (indent 1))
  `(let ((dir (file-name-as-directory (make-temp-file "agentel-files" t)))
         (agentel-files--caches (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (when ,git
             (let ((default-directory dir))
               (should (zerop (call-process "git" nil nil nil "init" "-q")))))
           ,@body)
       (delete-directory dir t))))

(defun agentel-files-test-listed (dir)
  "Return the files under DIR once they are listed again, in order."
  (let* ((cache (agentel-files--cache dir))
         (before (agentel-files--cache-time cache)))
    (agentel-files--refresh dir)
    (with-timeout (5 (error "Listing took too long"))
      (while (equal before (agentel-files--cache-time cache))
        (accept-process-output nil 0.05)))
    (sort (copy-sequence (agentel-files--cache-files cache)) #'string<)))

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
    (should (equal (agentel-files-test-listed dir)
                   '(".gitignore" "a.el" "sub/b.el")))))

(ert-deftest agentel-files-lists-files-outside-a-repository-but-not-hidden-ones ()
  (agentel-files-test-with-dir nil
    (agentel-files-test-touch dir "a.el" "sub/b.el" ".hidden/c.el")
    (should (equal (agentel-files-test-listed dir) '("a.el" "sub/b.el")))))

(ert-deftest agentel-files-lists-files-when-git-is-missing ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el")
    (let ((agentel-files--git-command '("agentel-no-such-command")))
      (should (equal (agentel-files-test-listed dir) '("a.el"))))))

(ert-deftest agentel-files-lists-nothing-for-a-repository-that-ignores-all-files ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.log")
    (write-region "*.log\n.gitignore\n" nil (expand-file-name ".gitignore" dir) nil 'silent)
    (should-not (agentel-files-test-listed dir))))

(ert-deftest agentel-files-does-not-take-the-optional-locks-of-git ()
  (should (member "--no-optional-locks" agentel-files--git-command)))

(ert-deftest agentel-files-completes-from-the-last-listing-without-waiting ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el" "sub/b.el")
    (agentel-files-test-with-chat dir
      (insert "look at @su")
      (should-not (agentel-files-test-candidates))
      (agentel-files-test-listed dir)
      (should (equal (agentel-files-test-candidates) '("sub/b.el")))
      (let ((capf (agentel-files-completion-at-point)))
        (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                       "su"))))))

(ert-deftest agentel-files-lists-again-only-when-the-listing-is-stale ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el")
    (let ((starts 0)
          (refresh (symbol-function 'agentel-files--refresh))
          (agentel-files-refresh-interval 5))
      (cl-letf (((symbol-function 'agentel-files--refresh)
                 (lambda (dir) (cl-incf starts) (funcall refresh dir))))
        (agentel-files-test-listed dir)
        (setq starts 0)
        (agentel-files--candidates dir)
        (should (= starts 0))
        (setf (agentel-files--cache-time (agentel-files--cache dir))
              (- (float-time) 6))
        (agentel-files--candidates dir)
        (should (= starts 1))))))

(ert-deftest agentel-files-lists-the-files-when-a-session-starts ()
  (agentel-files-test-with-dir t
    (agentel-files-test-touch dir "a.el")
    (let ((agentel-session--registry nil)
          (agentel-session-changed-functions nil))
      (agentel-files--on-started (agentel-session-create :cwd dir) nil nil)
      (should (agentel-files--cache-process (agentel-files--cache dir))))))

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
