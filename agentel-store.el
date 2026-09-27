;;; agentel-store.el --- Models changed by messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; State is kept in the way of Elm: a type of model defines how a
;; message changes its data.  A store keeps the models of one thing,
;; such as the conversation of a session, in the order they were added,
;; and messages are sent to them through the store.  Whoever shows or
;; follows the models subscribes to the store, so one store can be
;; shown in more than one way.  Nothing here knows what the models are
;; about.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Models

(defvar agentel-store--updates nil
  "Alist of model types and the functions that update them.")

(defun agentel-store-define (type update)
  "Define the model TYPE whose data is changed by UPDATE.
UPDATE is a function taking a message and the data alist of a model,
nil for a new one, and returning its new data."
  (declare (indent 1))
  (setf (alist-get type agentel-store--updates) update))

(cl-defstruct (agentel-store-model (:constructor agentel-store-model--make)
                                   (:copier nil))
  "A piece of what a store keeps."
  key type data)

(defun agentel-store-get (model key)
  "Return the value the data of MODEL has under KEY."
  (alist-get key (agentel-store-model-data model)))

;;;; Stores

(cl-defstruct (agentel-store (:constructor agentel-store-create)
                             (:conc-name agentel-store--)
                             (:copier nil))
  "Models in the order they were added, and the functions watching them."
  (newest nil :documentation "Models, newest first.")
  (index (make-hash-table :test 'equal) :documentation "Models by key.")
  (subscribers nil :documentation "Functions told of changes, oldest first."))

(defun agentel-store-models (store)
  "Return the models of STORE, oldest first."
  (reverse (agentel-store--newest store)))

(defun agentel-store-last (store &optional n)
  "Return the model added last to STORE, or the Nth one before it."
  (nth (or n 0) (agentel-store--newest store)))

(defun agentel-store-find (store key)
  "Return the model of STORE under KEY."
  (gethash key (agentel-store--index store)))

(defun agentel-store-subscribers (store)
  "Return the functions told of the changes of STORE."
  (agentel-store--subscribers store))

(defun agentel-store-subscribe (store function)
  "Call FUNCTION whenever a model of STORE is added or changed.
It is called with the model and non-nil when the model was added."
  (setf (agentel-store--subscribers store)
        (append (agentel-store--subscribers store) (list function))))

(defun agentel-store-unsubscribe (store function)
  "Stop calling FUNCTION for the changes of STORE."
  (setf (agentel-store--subscribers store)
        (remq function (agentel-store--subscribers store))))

(defun agentel-store--tell (store model added)
  "Tell the subscribers of STORE that MODEL changed, or was ADDED."
  (dolist (function (agentel-store--subscribers store))
    (funcall function model added)))

(defun agentel-store-update (store model message)
  "Change the data of MODEL of STORE by MESSAGE."
  (setf (agentel-store-model-data model)
        (funcall (alist-get (agentel-store-model-type model) agentel-store--updates)
                 message (agentel-store-model-data model)))
  (agentel-store--tell store model nil))

(defun agentel-store-dispatch (store key type message)
  "Send MESSAGE to the model of TYPE under KEY in STORE and return it.
Without such a model, one is added after the others, so a nil KEY adds
one every time."
  (if-let* ((model (and key (agentel-store-find store key))))
      (progn (agentel-store-update store model message) model)
    (let ((model (agentel-store-model--make
                  :key key :type type
                  :data (funcall (alist-get type agentel-store--updates)
                                 message nil))))
      (push model (agentel-store--newest store))
      (when key (puthash key model (agentel-store--index store)))
      (agentel-store--tell store model t)
      model)))

(provide 'agentel-store)
;;; agentel-store.el ends here
