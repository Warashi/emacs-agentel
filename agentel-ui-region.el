;;; agentel-ui-region.el --- Models drawn in a region of a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A region shows a list of models of `agentel-store' in a part of a
;; buffer, each by its view of `agentel-ui', apart by a blank line.
;; Whoever owns the region decides which models it shows and hands
;; the whole list to `agentel-ui-region-render' whenever it may have
;; changed; the region finds what differs from what it shows and
;; changes only that text.  A model is drawn again when its revision or
;; the line width it fits in changed, or when it became or stopped being
;; the first one; folding it draws it again at once.  The models kept in
;; the order they were in are passed over first without looking them up,
;; so a change to one model or a new one at the end costs little more
;; than comparing a few numbers for each model.
;;
;; The text of a model starts at a marker that advances over text
;; inserted at it, so a model inserted in front of another one leaves
;; the other one starting after it.  The text is read-only and carries
;; the model in a text property named by the owner, so a buffer may
;; hold more than one region.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-store)
(require 'agentel-ui)

(cl-defstruct (agentel-ui-region (:constructor agentel-ui-region--make)
                                 (:conc-name agentel-ui-region--)
                                 (:copier nil))
  "Models drawn in a part of a buffer."
  (end nil :documentation "Marker at the end of the region.")
  (property nil :documentation "Text property holding the model of the text.")
  (drawn nil :documentation "Drawings of the models shown, in order.")
  (index (make-hash-table :test 'eq) :documentation "Drawings by model.")
  (collapsed (make-hash-table :test 'eq :weakness 'key)
             :documentation "Folds of the models, also of those left out."))

(cl-defstruct (agentel-ui-region--drawing (:copier nil))
  "How a model was drawn."
  model start revision width fits first)

(defun agentel-ui-region-create (pos property)
  "Return an empty region at POS of the current buffer.
The text of each model carries it in the text property PROPERTY."
  (agentel-ui-region--make :end (copy-marker pos t) :property property))

(defun agentel-ui-region-end (region)
  "Return the marker at the end of REGION.
Text inserted at it goes in front of it, so it stays the end."
  (agentel-ui-region--end region))

(defun agentel-ui-region-model-at (region pos)
  "Return the model whose text of REGION is at POS."
  (get-text-property pos (agentel-ui-region--property region)))

(defmacro agentel-ui-region--changing (&rest body)
  "Run BODY, which changes the read-only text of a region.
The change is not recorded for undo.  Undo records positions, and
recorded edits elsewhere would point at the wrong text after the region
grows above them, so the undo history is dropped."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     (prog1 (let ((buffer-undo-list t)) ,@body)
       (unless (eq buffer-undo-list t)
         (setq buffer-undo-list nil)))))

(defun agentel-ui-region--collapsed-p (region model)
  "Return non-nil if MODEL is folded in REGION."
  (let ((collapsed (gethash model (agentel-ui-region--collapsed region) 'unset)))
    (if (eq collapsed 'unset)
        (agentel-ui-view-property (agentel-store-model-type model) :collapsed)
      collapsed)))

(defun agentel-ui-region--stale-p (drawing first width)
  "Return non-nil if DRAWING has to be drawn again.
FIRST says whether it is the first model now, WIDTH the line width."
  (not (and (= (agentel-ui-region--drawing-revision drawing)
               (agentel-store-model-revision (agentel-ui-region--drawing-model drawing)))
            (eq (agentel-ui-region--drawing-first drawing) first)
            (or (not (agentel-ui-region--drawing-fits drawing))
                (= (agentel-ui-region--drawing-width drawing) width)))))

(defun agentel-ui-region--text-end (region drawing)
  "Return the position after the text of DRAWING in REGION."
  (next-single-property-change (agentel-ui-region--drawing-start drawing)
                               (agentel-ui-region--property region) nil
                               (marker-position (agentel-ui-region--end region))))

(defun agentel-ui-region--text (region drawing first width)
  "Return the text of DRAWING of REGION, as the first model if FIRST, in WIDTH.
DRAWING records how it is drawn."
  (let* ((model (agentel-ui-region--drawing-model drawing))
         (text (concat (if first "" "\n\n")
                       (agentel-ui-view model
                                        :collapsed (agentel-ui-region--collapsed-p region model)
                                        :width width))))
    (add-text-properties 0 (length text)
                         (list (agentel-ui-region--property region) model
                               'read-only t
                               'rear-nonsticky t
                               'front-sticky '(read-only))
                         text)
    (setf (agentel-ui-region--drawing-revision drawing) (agentel-store-model-revision model)
          (agentel-ui-region--drawing-width drawing) width
          (agentel-ui-region--drawing-fits drawing)
          (text-property-any 0 (length text) 'agentel-ui-fits-width t text)
          (agentel-ui-region--drawing-first drawing) first)
    text))

(defun agentel-ui-region--insert (drawings texts pos)
  "Insert TEXTS, the texts of DRAWINGS in order, at POS."
  (save-excursion
    (goto-char pos)
    (insert (apply #'concat texts)))
  ;; The starts advance over the text inserted at them.
  (let ((at pos))
    (cl-mapc (lambda (drawing text)
               (if-let* ((start (agentel-ui-region--drawing-start drawing)))
                   (set-marker start at)
                 (setf (agentel-ui-region--drawing-start drawing) (copy-marker at t)))
               (setq at (+ at (length text))))
             drawings texts)))

(defun agentel-ui-region--delete (region drawing)
  "Remove the text of DRAWING from REGION."
  (delete-region (agentel-ui-region--drawing-start drawing)
                 (agentel-ui-region--text-end region drawing)))

(defun agentel-ui-region--redraw (region drawing first width)
  "Draw DRAWING of REGION again in place, as the first model if FIRST, in WIDTH.
Point in its text stays where it was in the text."
  (let* ((start (marker-position (agentel-ui-region--drawing-start drawing)))
         (end (agentel-ui-region--text-end region drawing))
         (offset (and (<= start (point)) (< (point) end) (- (point) start)))
         (text (agentel-ui-region--text region drawing first width)))
    (agentel-ui-region--delete region drawing)
    (agentel-ui-region--insert (list drawing) (list text) start)
    (when offset
      (goto-char (min (+ start offset) (agentel-ui-region--text-end region drawing))))))

(defun agentel-ui-region--rearrange (region drawings models first width)
  "Show MODELS in REGION in place of DRAWINGS, the rest of what it shows.
FIRST says whether the first of them is the first model, WIDTH is the
line width.  Return the drawings of MODELS.

Every change of the buffer moves all its markers, so the models left
out next to each other are removed at once, and so are the new models
next to each other inserted."
  (let ((index (agentel-ui-region--index region))
        (end (agentel-ui-region--end region))
        (wanted (make-hash-table :test 'eq :size (length models)))
        kept gone pending texts)
    (dolist (model models)
      (puthash model t wanted))
    ;; The markers of the drawings left out are not detached, which
    ;; would walk all the markers of the buffer for each of them.
    (dolist (drawing drawings)
      (let ((model (agentel-ui-region--drawing-model drawing)))
        (cond ((gethash model wanted)
               (when gone
                 (delete-region gone (agentel-ui-region--drawing-start drawing))
                 (setq gone nil))
               (push drawing kept))
              (t
               (unless gone
                 (setq gone (marker-position (agentel-ui-region--drawing-start drawing))))
               (remhash model index)))))
    (when gone
      (delete-region gone end))
    (setq kept (nreverse kept))
    (let ((flush (lambda ()
                   (when pending
                     (agentel-ui-region--insert
                      (nreverse pending) (nreverse texts)
                      (if kept
                          (marker-position (agentel-ui-region--drawing-start (car kept)))
                        (marker-position end)))
                     (setq pending nil texts nil)))))
      (prog1
          (mapcar
           (lambda (model)
             (let ((drawing (gethash model index)))
               (cond
                ((and drawing (eq drawing (car kept)))
                 (funcall flush)
                 (pop kept)
                 (when (agentel-ui-region--stale-p drawing first width)
                   (agentel-ui-region--redraw region drawing first width)))
                (t
                 (when drawing
                   ;; Drawn out of order, so it moves here.
                   (agentel-ui-region--delete region drawing)
                   (setq kept (delq drawing kept)))
                 (setq drawing (or drawing (make-agentel-ui-region--drawing :model model)))
                 (puthash model drawing index)
                 (push drawing pending)
                 (push (agentel-ui-region--text region drawing first width) texts)))
               (setq first nil)
               drawing))
           models)
        (funcall flush)))))

(defun agentel-ui-region-render (region models)
  "Show MODELS, a list in order, in REGION.
Only the models that are new to REGION or changed are drawn, and the
text of the models left out is removed."
  (let ((drawings (agentel-ui-region--drawn region))
        (width (agentel-ui-line-width))
        (first t)
        same)
    (agentel-ui-region--changing
      (while (and drawings models
                  (eq (agentel-ui-region--drawing-model (car drawings)) (car models)))
        (when (agentel-ui-region--stale-p (car drawings) first width)
          (agentel-ui-region--redraw region (car drawings) first width))
        (setq first nil
              same drawings
              drawings (cdr drawings)
              models (cdr models)))
      (when (or drawings models)
        (let ((rest (agentel-ui-region--rearrange region drawings models first width)))
          (if same
              (setcdr same rest)
            (setf (agentel-ui-region--drawn region) rest)))))))

(defun agentel-ui-region-toggle (region model)
  "Fold or unfold MODEL in REGION."
  (puthash model (not (agentel-ui-region--collapsed-p region model))
           (agentel-ui-region--collapsed region))
  (when-let* ((drawing (gethash model (agentel-ui-region--index region))))
    (agentel-ui-region--changing
      (agentel-ui-region--redraw region drawing
                                 (agentel-ui-region--drawing-first drawing)
                                 (agentel-ui-line-width)))))

(provide 'agentel-ui-region)
;;; agentel-ui-region.el ends here
