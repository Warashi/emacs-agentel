;;; agentel-commonmark-test.el --- Tests for agentel-commonmark  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-commonmark)

(defconst agentel-commonmark-test--cases
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "spec/expected.eld"
                       (file-name-directory (or load-file-name buffer-file-name))))
    (read (current-buffer)))
  "The examples of the specs with the trees expected for them.")

(defconst agentel-commonmark-test--positions '(:begin :end :inner-begin :inner-end)
  "The attributes holding positions, which the specs do not tell.")

(defun agentel-commonmark-test--text (trees)
  "Return the text of TREES as the alternative text of an image shows it."
  (mapconcat (lambda (tree)
               (cond ((stringp tree) tree)
                     ((eq (car tree) 'hard-break) "\n")
                     (t (agentel-commonmark-test--text (cddr tree)))))
             trees ""))

(defun agentel-commonmark-test--comparable (tree)
  "Return TREE as the specs tell it: without positions, an image with its text."
  (if (stringp tree)
      tree
    (let ((attributes (cl-loop for (key value) on (cadr tree) by #'cddr
                               unless (memq key agentel-commonmark-test--positions)
                               append (list key value))))
      `(,(car tree) ,attributes
        ,@(if (eq (car tree) 'image)
              (let ((text (agentel-commonmark-test--text (cddr tree))))
                (and (> (length text) 0) (list text)))
            (mapcar #'agentel-commonmark-test--comparable (cddr tree)))))))

(defun agentel-commonmark-test--failing ()
  "Return the examples whose tree is not the expected one, as (SOURCE . NUMBER)."
  (cl-loop for case in agentel-commonmark-test--cases
           unless (equal (agentel-commonmark-test--comparable
                          (agentel-commonmark-parse (plist-get case :markdown)))
                         (plist-get case :expected))
           collect (cons (plist-get case :source) (plist-get case :example))))

(ert-deftest agentel-commonmark-gives-the-trees-of-the-spec-examples ()
  (should (equal (agentel-commonmark-test--failing) nil)))

(defun agentel-commonmark-test--misplaced (tree begin end)
  "Return the nodes of TREE not in order within BEGIN and END."
  (unless (stringp tree)
    (let* ((attributes (cadr tree))
           (node-begin (plist-get attributes :begin))
           (node-end (plist-get attributes :end))
           (inner-begin (or (plist-get attributes :inner-begin) node-begin))
           (inner-end (or (plist-get attributes :inner-end) node-end))
           (position inner-begin)
           misplaced)
      (unless (<= begin node-begin inner-begin inner-end node-end end)
        (push tree misplaced))
      (dolist (child (cddr tree))
        (unless (stringp child)
          (setq misplaced (append (agentel-commonmark-test--misplaced child position inner-end)
                                  misplaced))
          (setq position (plist-get (cadr child) :end))))
      misplaced)))

(ert-deftest agentel-commonmark-places-nodes-within-their-parents-in-order ()
  (dolist (case agentel-commonmark-test--cases)
    (let ((markdown (plist-get case :markdown)))
      (should (equal (list (plist-get case :source) (plist-get case :example)
                           (agentel-commonmark-test--misplaced
                            (agentel-commonmark-parse markdown) 0 (length markdown)))
                     (list (plist-get case :source) (plist-get case :example) nil))))))

(ert-deftest agentel-commonmark-ends-a-paragraph-before-the-table-after-it ()
  (let ((markdown "Text\n| a |\n|---|\n| b |\n"))
    (should-not (agentel-commonmark-test--misplaced
                 (agentel-commonmark-parse markdown) 0 (length markdown)))
    (should (equal (plist-get (cadr (nth 2 (agentel-commonmark-parse markdown))) :end) 4))))

(provide 'agentel-commonmark-test)
;;; agentel-commonmark-test.el ends here
