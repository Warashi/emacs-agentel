;;; agentel-ui-region.el --- Models drawn in a region of a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A region shows a list of models of `agentel-store' in a part of a
;; buffer, each by its view of `agentel-ui', apart by a blank line.
;; Whoever owns the region decides which models it shows and hands
;; the whole list to `agentel-ui-region-render' whenever it may have
;; changed; the region finds what differs from what it shows and
;; changes only that text.  A model is drawn again when its revision,
;; its fold or the line width it fits in changed, or when it became or
;; stopped being the first one.
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
  model start revision collapsed width fits first)

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

(defun agentel-ui-region--stale-p (region drawing first width)
  "Return non-nil if DRAWING of REGION has to be drawn again.
FIRST says whether it is the first model now, WIDTH the line width."
  (let ((model (agentel-ui-region--drawing-model drawing)))
    (not (and (= (agentel-ui-region--drawing-revision drawing)
                 (agentel-store-model-revision model))
              (eq (agentel-ui-region--drawing-collapsed drawing)
                  (agentel-ui-region--collapsed-p region model))
              (eq (agentel-ui-region--drawing-first drawing) first)
              (or (not (agentel-ui-region--drawing-fits drawing))
                  (= (agentel-ui-region--drawing-width drawing) width))))))

(defun agentel-ui-region--text-end (region drawing)
  "Return the position after the text of DRAWING in REGION."
  (next-single-property-change (agentel-ui-region--drawing-start drawing)
                               (agentel-ui-region--property region) nil
                               (marker-position (agentel-ui-region--end region))))

(defun agentel-ui-region--insert (region drawing pos first width)
  "Draw DRAWING of REGION at POS, as the first model if FIRST, in WIDTH."
  (let* ((model (agentel-ui-region--drawing-model drawing))
         (collapsed (agentel-ui-region--collapsed-p region model))
         (text (concat (if first "" "\n\n")
                       (agentel-ui-view model :collapsed collapsed :width width))))
    (add-text-properties 0 (length text)
                         (list (agentel-ui-region--property region) model
                               'read-only t
                               'rear-nonsticky t
                               'front-sticky '(read-only))
                         text)
    (setf (agentel-ui-region--drawing-revision drawing) (agentel-store-model-revision model)
          (agentel-ui-region--drawing-collapsed drawing) collapsed
          (agentel-ui-region--drawing-width drawing) width
          (agentel-ui-region--drawing-fits drawing)
          (text-property-any 0 (length text) 'agentel-ui-fits-width t text)
          (agentel-ui-region--drawing-first drawing) first)
    (save-excursion
      (goto-char pos)
      (insert text))
    ;; The start advances over the text inserted at it.
    (if-let* ((start (agentel-ui-region--drawing-start drawing)))
        (set-marker start pos)
      (setf (agentel-ui-region--drawing-start drawing) (copy-marker pos t)))))

(defun agentel-ui-region--delete (region drawing)
  "Remove the text of DRAWING from REGION."
  (delete-region (agentel-ui-region--drawing-start drawing)
                 (agentel-ui-region--text-end region drawing)))

(defun agentel-ui-region--redraw (region drawing first width)
  "Draw DRAWING of REGION again in place, as the first model if FIRST, in WIDTH.
Point in its text stays where it was in the text."
  (let* ((start (marker-position (agentel-ui-region--drawing-start drawing)))
         (end (agentel-ui-region--text-end region drawing))
         (offset (and (<= start (point)) (< (point) end) (- (point) start))))
    (agentel-ui-region--delete region drawing)
    (agentel-ui-region--insert region drawing start first width)
    (when offset
      (goto-char (min (+ start offset) (agentel-ui-region--text-end region drawing))))))

(defun agentel-ui-region-render (region models)
  "Show MODELS, a list in order, in REGION, and return those drawn.
Only the models that are new to REGION or changed are drawn, and the
text of the models left out is removed."
  (let ((index (agentel-ui-region--index region))
        (wanted (make-hash-table :test 'eq :size (length models)))
        (width (agentel-ui-line-width))
        (first t)
        kept drawn)
    (dolist (model models)
      (puthash model t wanted))
    (agentel-ui-region--changing
      (dolist (drawing (agentel-ui-region--drawn region))
        (if (gethash (agentel-ui-region--drawing-model drawing) wanted)
            (push drawing kept)
          (agentel-ui-region--delete region drawing)
          (set-marker (agentel-ui-region--drawing-start drawing) nil)
          (remhash (agentel-ui-region--drawing-model drawing) index)))
      (setq kept (nreverse kept))
      (setf (agentel-ui-region--drawn region)
            (mapcar
             (lambda (model)
               (let ((drawing (gethash model index)))
                 (cond
                  ((and drawing (eq drawing (car kept)))
                   (pop kept)
                   (when (agentel-ui-region--stale-p region drawing first width)
                     (agentel-ui-region--redraw region drawing first width)
                     (push model drawn)))
                  (t
                   (when drawing
                     ;; Drawn out of order, so it moves here.
                     (agentel-ui-region--delete region drawing)
                     (setq kept (delq drawing kept)))
                   (setq drawing (or drawing (make-agentel-ui-region--drawing :model model)))
                   (puthash model drawing index)
                   (agentel-ui-region--insert
                    region drawing
                    (if kept
                        (marker-position (agentel-ui-region--drawing-start (car kept)))
                      (marker-position (agentel-ui-region--end region)))
                    first width)
                   (push model drawn)))
                 (setq first nil)
                 drawing))
             models)))
    (nreverse drawn)))

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
