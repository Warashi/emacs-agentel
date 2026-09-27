;;; agentel-ui.el --- Display components shared by agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; What is shown is written in the way of Elm: a type of model defines
;; how a message changes its data (update) and how the data looks
;; (view).  A store keeps the models of one thing, such as the
;; transcript of a session, and messages are sent to them through the
;; store.  Presentations subscribe to a store and put the views where
;; they belong, so one store can be shown in more than one way.
;;
;; The file also has components that decide how things look regardless
;; of what they show, such as how wide a summary line may be or how a
;; state is shown.  Features build their lines with them, so the look
;; changes in one place.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Models

(defvar agentel-ui--kinds nil
  "Alist of model types and the plists defining them.")

(defun agentel-ui-define (type &rest definition)
  "Define the model TYPE by the plist DEFINITION.
:update is a function taking a message and the data alist of a model,
nil for a new one, and returning its new data.  :view is a function
taking the model and a plist of how a presentation shows it, and
returning its text.  Presentations read more properties of their own
with `agentel-ui-kind'."
  (declare (indent 1))
  (setf (alist-get type agentel-ui--kinds) definition))

(defun agentel-ui-kind (type property)
  "Return PROPERTY of the definition of the model TYPE."
  (plist-get (alist-get type agentel-ui--kinds) property))

(cl-defstruct (agentel-ui-model (:constructor agentel-ui-model--make)
                                (:copier nil))
  "A piece of what a store keeps."
  key type data)

(defun agentel-ui-get (model key)
  "Return the value the data of MODEL has under KEY."
  (alist-get key (agentel-ui-model-data model)))

(defun agentel-ui-view (model &rest options)
  "Return the text of MODEL shown with the plist OPTIONS."
  (funcall (agentel-ui-kind (agentel-ui-model-type model) :view) model options))

;;;; Stores

(cl-defstruct (agentel-ui-store (:constructor agentel-ui-store-create)
                                (:copier nil))
  "Models in the order they were added, and the functions watching them."
  (models nil :documentation "Models, newest first.")
  (index (make-hash-table :test 'equal) :documentation "Models by key.")
  (subscribers nil :documentation "Functions told of changes, oldest first."))

(defun agentel-ui-models (store)
  "Return the models of STORE, oldest first."
  (reverse (agentel-ui-store-models store)))

(defun agentel-ui-last (store)
  "Return the model added last to STORE."
  (car (agentel-ui-store-models store)))

(defun agentel-ui-find (store key)
  "Return the model of STORE under KEY."
  (gethash key (agentel-ui-store-index store)))

(defun agentel-ui-subscribe (store function)
  "Call FUNCTION whenever a model of STORE is added or changed.
It is called with the model and non-nil when the model was added."
  (setf (agentel-ui-store-subscribers store)
        (append (agentel-ui-store-subscribers store) (list function))))

(defun agentel-ui-unsubscribe (store function)
  "Stop calling FUNCTION for the changes of STORE."
  (setf (agentel-ui-store-subscribers store)
        (remq function (agentel-ui-store-subscribers store))))

(defun agentel-ui--tell (store model added)
  "Tell the subscribers of STORE that MODEL changed, or was ADDED."
  (dolist (function (agentel-ui-store-subscribers store))
    (funcall function model added)))

(defun agentel-ui-update (store model message)
  "Change the data of MODEL of STORE by MESSAGE."
  (setf (agentel-ui-model-data model)
        (funcall (agentel-ui-kind (agentel-ui-model-type model) :update)
                 message (agentel-ui-model-data model)))
  (agentel-ui--tell store model nil))

(defun agentel-ui-dispatch (store key type message)
  "Send MESSAGE to the model of TYPE under KEY in STORE and return it.
Without such a model, one is added after the others, so a nil KEY adds
one every time."
  (if-let* ((model (and key (agentel-ui-find store key))))
      (progn (agentel-ui-update store model message) model)
    (let ((model (agentel-ui-model--make
                  :key key :type type
                  :data (funcall (agentel-ui-kind type :update) message nil))))
      (push model (agentel-ui-store-models store))
      (when key (puthash key model (agentel-ui-store-index store)))
      (agentel-ui--tell store model t)
      model)))

;;;; Components

(defcustom agentel-ui-max-line-width 80
  "Largest width of a summary line, including its prefix."
  :type 'natnum
  :group 'agentel)

(defun agentel-ui-line-width ()
  "Return the width of a summary line in this buffer.
It is the narrowest window showing the buffer, up to
`agentel-ui-max-line-width'."
  ;; `window-max-chars-per-line' selects the window, which moves point
  ;; to the point of that window.
  (save-excursion
    (apply #'min agentel-ui-max-line-width
           (mapcar #'window-max-chars-per-line
                   (get-buffer-window-list nil nil t)))))

(defun agentel-ui-one-line (prefix text &optional width)
  "Return PREFIX followed by the first line of TEXT as one summary line.
The line fits in WIDTH, which defaults to `agentel-ui-line-width', and
ends in … when part of TEXT is left out.  It carries the
`agentel-ui-fits-width' property, so text that has to be made again
when the width changes can be found."
  (let* ((width (or width (agentel-ui-line-width)))
         (lines (split-string (string-trim (or text "")) "\n"))
         (line (concat prefix (car lines))))
    (propertize
     (if (cdr lines)
         (concat (truncate-string-to-width line (- width (string-width "…")))
                 "…")
       (truncate-string-to-width line width nil nil "…"))
     'agentel-ui-fits-width t)))

(defvar agentel-ui--followers nil
  "Buffers told when their line width changes.")

(defvar-local agentel-ui--width nil
  "Line width of this buffer when it was last checked.")

(defvar-local agentel-ui--on-width-change nil
  "Function called with no arguments when the line width changes.")

(defun agentel-ui-follow-width (function)
  "Call FUNCTION in this buffer whenever its line width changes.
FUNCTION makes the text depending on `agentel-ui-line-width' again."
  (setq agentel-ui--width (agentel-ui-line-width))
  (setq agentel-ui--on-width-change function)
  (cl-pushnew (current-buffer) agentel-ui--followers)
  ;; Window hooks local to a buffer miss a window that stops showing it.
  (add-hook 'window-size-change-functions #'agentel-ui--check-widths)
  (add-hook 'window-buffer-change-functions #'agentel-ui--check-widths))

(defun agentel-ui--check-widths (&rest _)
  "Tell the followers whose line width changed."
  (setq agentel-ui--followers
        (seq-filter (lambda (buffer)
                      ;; Changing the major mode kills the local function.
                      (and (buffer-live-p buffer)
                           (buffer-local-value 'agentel-ui--on-width-change buffer)))
                    agentel-ui--followers))
  (dolist (buffer agentel-ui--followers)
    (with-current-buffer buffer
      (let ((width (agentel-ui-line-width)))
        (unless (eql width agentel-ui--width)
          (setq agentel-ui--width width)
          (funcall agentel-ui--on-width-change))))))

(defcustom agentel-ui-state-icons
  '((starting . "⏳") (running . "🏃") (waiting . "🙋") (idle . "💤")
    (paused . "💤") (completed . "✅") (failed . "❌") (cancelled . "🚫")
    (exited . "🔌"))
  "Icons shown for the states of sessions and background tasks.
Each icon should take the same width in every terminal, so an emoji
that needs a variation selector to be wide does not fit."
  :type '(alist :key-type symbol :value-type string)
  :group 'agentel)

(defun agentel-ui-state (state)
  "Return how the symbol STATE is shown.
It is the icon of STATE in `agentel-ui-state-icons', naming STATE on
hover, or STATE in brackets when it has no icon."
  (if-let* ((icon (alist-get state agentel-ui-state-icons)))
      (propertize icon 'help-echo (symbol-name state))
    (format "[%s]" state)))

(provide 'agentel-ui)
;;; agentel-ui.el ends here
