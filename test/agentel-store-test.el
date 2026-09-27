;;; agentel-store-test.el --- Tests for agentel-store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-store)

(agentel-store-define 'agentel-store-test-counter
  (lambda (message data)
    (pcase message
      (`(add ,n) `((count . ,(+ n (or (alist-get 'count data) 0))))))))

(defun agentel-store-test-counts (store)
  "Return the counts of the models of STORE, oldest first."
  (mapcar (lambda (model) (agentel-store-get model 'count)) (agentel-store-models store)))

(ert-deftest agentel-store-dispatch-keeps-one-model-per-key ()
  (let ((store (agentel-store-create)))
    (agentel-store-dispatch store 'c 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store 'c 'agentel-store-test-counter '(add 2))
    (should (equal (agentel-store-test-counts store) '(3)))
    (should (eq (agentel-store-find store 'c) (agentel-store-last store)))))

(ert-deftest agentel-store-find-if-returns-the-newest-model-that-matches ()
  (let ((store (agentel-store-create)))
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 2))
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 3))
    (should (equal (agentel-store-get
                    (agentel-store-find-if store (lambda (m) (< (agentel-store-get m 'count) 3)))
                    'count)
                   2))
    (should-not (agentel-store-find-if store (lambda (m) (> (agentel-store-get m 'count) 3))))))

(ert-deftest agentel-store-dispatch-without-a-key-adds-a-model-each-time ()
  (let ((store (agentel-store-create)))
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 2))
    (should (equal (agentel-store-test-counts store) '(1 2)))
    (should (equal (agentel-store-get (agentel-store-last store) 'count) 2))
    (should (equal (agentel-store-get (agentel-store-last store 1) 'count) 1))))

(ert-deftest agentel-store-update-changes-a-model-without-a-key ()
  (let* ((store (agentel-store-create))
         (model (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1))))
    (agentel-store-update store model '(add 2))
    (should (equal (agentel-store-test-counts store) '(3)))))

(ert-deftest agentel-store-counts-the-changes-of-a-model ()
  (let* ((store (agentel-store-create))
         (a (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1)))
         (b (agentel-store-dispatch store 'b 'agentel-store-test-counter '(add 1)))
         (first (agentel-store-model-revision a)))
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (should-not (equal (agentel-store-model-revision a) first))
    (let ((second (agentel-store-model-revision a)))
      (agentel-store-update store b '(add 1))
      (should (equal (agentel-store-model-revision a) second)))))

(ert-deftest agentel-store-counts-a-change-that-keeps-the-same-data ()
  (let* ((store (agentel-store-create))
         (model (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1)))
         (first (agentel-store-model-revision model)))
    (agentel-store-define 'agentel-store-test-in-place
      (lambda (_message data) data))
    (setf (agentel-store-model-type model) 'agentel-store-test-in-place)
    (agentel-store-update store model '(anything))
    (should-not (equal (agentel-store-model-revision model) first))))

(ert-deftest agentel-store-tells-subscribers-which-model-was-added-or-changed ()
  (let ((store (agentel-store-create))
        told)
    (agentel-store-subscribe store (lambda (model added)
                                     (push (list (agentel-store-model-key model) added) told)))
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store 'b 'agentel-store-test-counter '(add 1))
    (should (equal (reverse told) '((a t) (a nil) (b t))))))

(ert-deftest agentel-store-tells-a-subscriber-of-one-key-only-of-its-model ()
  (let ((store (agentel-store-create))
        told)
    (agentel-store-subscribe store (lambda (model added)
                                     (push (list (agentel-store-model-key model) added) told))
                             'a)
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store 'b 'agentel-store-test-counter '(add 1))
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (should (equal (reverse told) '((a t) (a nil))))))

(ert-deftest agentel-store-stops-telling-an-unsubscribed-function-of-one-key ()
  (let* ((store (agentel-store-create))
         (told 0)
         (subscriber (lambda (_ _) (setq told (1+ told)))))
    (agentel-store-subscribe store subscriber 'a)
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (agentel-store-unsubscribe store subscriber)
    (agentel-store-dispatch store 'a 'agentel-store-test-counter '(add 1))
    (should (= told 1))
    (should-not (agentel-store-subscribers store))))

(ert-deftest agentel-store-stops-telling-an-unsubscribed-function ()
  (let* ((store (agentel-store-create))
         (told 0)
         (subscriber (lambda (_ _) (setq told (1+ told)))))
    (agentel-store-subscribe store subscriber)
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1))
    (agentel-store-unsubscribe store subscriber)
    (agentel-store-dispatch store nil 'agentel-store-test-counter '(add 1))
    (should (= told 1))
    (should-not (agentel-store-subscribers store))))

(provide 'agentel-store-test)
;;; agentel-store-test.el ends here
