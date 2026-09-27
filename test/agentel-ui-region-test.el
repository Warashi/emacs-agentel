;;; agentel-ui-region-test.el --- Tests for agentel-ui-region  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-ui-region)

(agentel-store-define 'agentel-ui-region-test-note
  (lambda (message data)
    (pcase message
      (`(show ,text) `((text . ,text)))
      ;; Changes the data in place, as the update of a tool call does.
      (`(append ,text) (setf (alist-get 'text data)
                             (concat (alist-get 'text data) text))
                       data))))

(defvar agentel-ui-region-test-drawn nil
  "Texts of the notes drawn, newest first.")

(agentel-ui-define-view 'agentel-ui-region-test-note
  (lambda (model options)
    (let ((text (agentel-store-get model 'text)))
      (push text agentel-ui-region-test-drawn)
      (cond ((plist-get options :collapsed) (concat text "…"))
            ((string-prefix-p "wide" text)
             (agentel-ui-one-line "" text (plist-get options :width)))
            (t text))))
  :collapsed nil)

(agentel-store-define 'agentel-ui-region-test-folded
  (lambda (message _data)
    (pcase message (`(show ,text) `((text . ,text))))))

(agentel-ui-define-view 'agentel-ui-region-test-folded
  (lambda (model options)
    (if (plist-get options :collapsed)
        "folded"
      (agentel-store-get model 'text)))
  :collapsed t)

(defmacro agentel-ui-region-test-with-region (&rest body)
  "Run BODY in a buffer with a region bound to `region' and a store to `store'.
The buffer ends with the text \"END\" after the region."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((region (agentel-ui-region-create (point-min) 'agentel-ui-region-test))
           (store (agentel-store-create))
           (agentel-ui-region-test-drawn nil))
       (save-excursion (goto-char (point-max)) (insert "END"))
       (set-marker (agentel-ui-region-end region) (point-min))
       ,@body)))

(defun agentel-ui-region-test-note (store key text)
  "Show TEXT as the note KEY of STORE and return it."
  (agentel-store-dispatch store key 'agentel-ui-region-test-note `(show ,text)))

(defun agentel-ui-region-test-text ()
  "Return the text of the current buffer without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest agentel-ui-region-draws-models-in-order-apart-by-a-blank-line ()
  (agentel-ui-region-test-with-region
    (agentel-ui-region-render region (list (agentel-ui-region-test-note store 'a "one")
                                           (agentel-ui-region-test-note store 'b "two")))
    (should (equal (agentel-ui-region-test-text) "one\n\ntwoEND"))))

(ert-deftest agentel-ui-region-draws-nothing-for-no-models ()
  (agentel-ui-region-test-with-region
    (agentel-ui-region-render region nil)
    (should (equal (agentel-ui-region-test-text) "END"))))

(ert-deftest agentel-ui-region-draws-again-only-the-changed-models ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two")))
      (agentel-ui-region-render region (list a b))
      (setq agentel-ui-region-test-drawn nil)
      (agentel-store-update store a '(append " more"))
      (agentel-ui-region-render region (list a b))
      (should (equal agentel-ui-region-test-drawn '("one more")))
      (should (equal (agentel-ui-region-test-text) "one more\n\ntwoEND")))))

(ert-deftest agentel-ui-region-removes-models-left-out ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two"))
          (c (agentel-ui-region-test-note store 'c "three")))
      (agentel-ui-region-render region (list a b c))
      (agentel-ui-region-render region (list b))
      (should (equal (agentel-ui-region-test-text) "twoEND"))
      (agentel-ui-region-render region nil)
      (should (equal (agentel-ui-region-test-text) "END")))))

(ert-deftest agentel-ui-region-puts-a-model-back-where-it-belongs ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two"))
          (c (agentel-ui-region-test-note store 'c "three")))
      (agentel-ui-region-render region (list c))
      (agentel-ui-region-render region (list a c))
      (should (equal (agentel-ui-region-test-text) "one\n\nthreeEND"))
      (setq agentel-ui-region-test-drawn nil)
      (agentel-ui-region-render region (list a b c))
      (should (equal agentel-ui-region-test-drawn '("two")))
      (should (equal (agentel-ui-region-test-text) "one\n\ntwo\n\nthreeEND"))
      (agentel-store-update store c '(show "four"))
      (agentel-ui-region-render region (list a b c))
      (should (equal (agentel-ui-region-test-text) "one\n\ntwo\n\nfourEND")))))

(defun agentel-ui-region-test-edits (function)
  "Return how many times FUNCTION changes the text of the current buffer."
  (let ((edits 0))
    (let ((after-change-functions (list (lambda (&rest _) (setq edits (1+ edits))))))
      (funcall function))
    edits))

(ert-deftest agentel-ui-region-leaves-out-and-puts-back-models-in-runs ()
  (agentel-ui-region-test-with-region
    (let* ((models (mapcar (lambda (i) (agentel-ui-region-test-note store i (format "n%d" i)))
                           (number-sequence 1 100)))
           (few (list (nth 0 models) (nth 50 models) (nth 99 models))))
      (agentel-ui-region-render region models)
      (let ((all (agentel-ui-region-test-text)))
        (should (<= (agentel-ui-region-test-edits
                     (lambda () (agentel-ui-region-render region few)))
                    2))
        (should (equal (agentel-ui-region-test-text) "n1\n\nn51\n\nn100END"))
        (should (<= (agentel-ui-region-test-edits
                     (lambda () (agentel-ui-region-render region models)))
                    2))
        (should (equal (agentel-ui-region-test-text) all))
        (agentel-store-update store (nth 49 models) '(show "changed"))
        (agentel-ui-region-render region models)
        (should (string-match-p "\n\nn49\n\nchanged\n\nn51\n\n"
                                (agentel-ui-region-test-text)))))))

(ert-deftest agentel-ui-region-moves-a-model-drawn-out-of-order ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two")))
      (agentel-ui-region-render region (list a b))
      (agentel-ui-region-render region (list b a))
      (should (equal (agentel-ui-region-test-text) "two\n\noneEND"))
      (agentel-store-update store a '(show "three"))
      (agentel-ui-region-render region (list b a))
      (should (equal (agentel-ui-region-test-text) "two\n\nthreeEND")))))

(ert-deftest agentel-ui-region-marks-the-text-of-each-model ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two")))
      (agentel-ui-region-render region (list a b))
      (should (eq (agentel-ui-region-model-at region 1) a))
      (should (eq (agentel-ui-region-model-at region 4) b))
      (should (eq (agentel-ui-region-model-at region 6) b))
      (should-not (agentel-ui-region-model-at region 9)))))

(ert-deftest agentel-ui-region-text-is-read-only ()
  (agentel-ui-region-test-with-region
    (agentel-ui-region-render region (list (agentel-ui-region-test-note store 'a "one")))
    (goto-char (point-min))
    (should-error (insert "x") :type 'text-read-only)
    (goto-char (point-max))
    (insert "x")))

(ert-deftest agentel-ui-region-keeps-changes-out-of-the-undo-history ()
  (agentel-ui-region-test-with-region
    (buffer-enable-undo)
    (goto-char (point-max))
    (insert "typed")
    (undo-boundary)
    (agentel-ui-region-render region (list (agentel-ui-region-test-note store 'a "one")))
    (should-not buffer-undo-list)))

(ert-deftest agentel-ui-region-keeps-point-in-a-model-drawn-again ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one"))
          (b (agentel-ui-region-test-note store 'b "two")))
      (agentel-ui-region-render region (list a b))
      (goto-char 7)
      (agentel-store-update store a '(show "first"))
      (agentel-ui-region-render region (list a b))
      (should (equal (buffer-substring-no-properties (point) (point-max)) "woEND"))
      (goto-char 3)
      (agentel-store-update store a '(show "one"))
      (agentel-ui-region-render region (list a b))
      (should (equal (buffer-substring-no-properties (point) (point-max))
                     "e\n\ntwoEND")))))

(ert-deftest agentel-ui-region-keeps-point-after-the-region ()
  (agentel-ui-region-test-with-region
    (goto-char (point-max))
    (agentel-ui-region-render region (list (agentel-ui-region-test-note store 'a "one")))
    (should (eobp))))

(ert-deftest agentel-ui-region-folds-a-model-as-its-view-says ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-store-dispatch store 'a 'agentel-ui-region-test-folded
                                     '(show "open"))))
      (agentel-ui-region-render region (list a))
      (should (equal (agentel-ui-region-test-text) "foldedEND"))
      (agentel-ui-region-toggle region a)
      (should (equal (agentel-ui-region-test-text) "openEND"))
      (agentel-ui-region-toggle region a)
      (should (equal (agentel-ui-region-test-text) "foldedEND")))))

(ert-deftest agentel-ui-region-keeps-a-fold-while-a-model-is-left-out ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a "one")))
      (agentel-ui-region-render region (list a))
      (agentel-ui-region-toggle region a)
      (agentel-ui-region-render region nil)
      (agentel-ui-region-render region (list a))
      (should (equal (agentel-ui-region-test-text) "one…END")))))

(ert-deftest agentel-ui-region-fits-models-again-when-the-width-changes ()
  (agentel-ui-region-test-with-region
    (let ((a (agentel-ui-region-test-note store 'a (concat "wide" (make-string 50 ?w))))
          (b (agentel-ui-region-test-note store 'b "two")))
      (let ((agentel-ui-max-line-width 10))
        (agentel-ui-region-render region (list a b)))
      (should (equal (agentel-ui-region-test-text) "widewwwww…\n\ntwoEND"))
      (setq agentel-ui-region-test-drawn nil)
      (let ((agentel-ui-max-line-width 12))
        (agentel-ui-region-render region (list a b)))
      (should (equal (agentel-ui-region-test-text) "widewwwwwww…\n\ntwoEND"))
      (should (equal (length agentel-ui-region-test-drawn) 1)))))

(provide 'agentel-ui-region-test)
;;; agentel-ui-region-test.el ends here
