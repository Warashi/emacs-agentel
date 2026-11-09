;;; agentel-markdown.el --- Markdown in agent messages for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Agents answer in Markdown.  Once a message is complete, it is parsed
;; as CommonMark with the extensions of GFM, and its headings, emphasis,
;; strikethrough, inline code, links and code blocks are highlighted;
;; a link, bare URLs included, browses its URL when pushed, and code
;; blocks are fontified with the major mode of their language.
;; The columns of tables are aligned by padding displayed on their pipes;
;; a table wider than the window has its rows displayed with the cells
;; wrapped in narrower columns.
;; The markup itself stays in the text, so copying a message gives back
;; what the agent wrote.  `agentel-markdown-copy-code' copies the code
;; of a block without its fences.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'agentel-session)
(require 'agentel-chat)
(require 'agentel-commonmark)

(defface agentel-markdown-heading-face
  '((t :inherit bold :height 1.1))
  "Face of Markdown headings."
  :group 'agentel)

(defface agentel-markdown-code-face
  '((t :inherit font-lock-constant-face))
  "Face of inline code."
  :group 'agentel)

(defface agentel-markdown-code-block-face
  '((((background light)) :background "grey95" :extend t)
    (((background dark)) :background "grey15" :extend t))
  "Face of fenced code blocks."
  :group 'agentel)

(defface agentel-markdown-strikethrough-face
  '((t :strike-through t))
  "Face of struck through text."
  :group 'agentel)

(defface agentel-markdown-markup-face
  '((t :inherit shadow))
  "Face of Markdown markup characters."
  :group 'agentel)

(defcustom agentel-markdown-languages
  '(("sh" . sh-mode) ("bash" . sh-mode) ("shell" . sh-mode) ("zsh" . sh-mode)
    ("console" . sh-mode) ("elisp" . emacs-lisp-mode) ("lisp" . lisp-mode)
    ("js" . js-mode) ("javascript" . js-mode) ("json" . js-json-mode)
    ("ts" . typescript-ts-mode) ("typescript" . typescript-ts-mode)
    ("py" . python-mode) ("python" . python-mode) ("rb" . ruby-mode)
    ("go" . go-ts-mode) ("rs" . rust-ts-mode) ("rust" . rust-ts-mode)
    ("yml" . yaml-ts-mode) ("yaml" . yaml-ts-mode) ("nix" . nix-mode)
    ("diff" . diff-mode) ("patch" . diff-mode) ("org" . org-mode))
  "Major modes of code block languages whose mode is not LANGUAGE-mode."
  :type '(alist :key-type string :value-type function)
  :group 'agentel)

(defun agentel-markdown--mode (language)
  "Return the major mode that fontifies LANGUAGE, or nil."
  (let ((mode (or (cdr (assoc (downcase language) agentel-markdown-languages))
                  (intern-soft (concat (downcase language) "-mode")))))
    (and mode (fboundp mode) mode)))

(defun agentel-markdown--copy-faces (string start)
  "Add the faces of STRING to the current buffer from position START."
  (let ((pos 0)
        (length (length string)))
    (while (< pos length)
      (let ((next (min (next-single-property-change pos 'face string length)
                       (next-single-property-change pos 'font-lock-face string length)))
            (face (or (get-text-property pos 'face string)
                      (get-text-property pos 'font-lock-face string))))
        (when face
          (add-face-text-property (+ start pos) (+ start next) face t))
        (setq pos next)))))

(defun agentel-markdown--fontify-code (start end language)
  "Highlight the code between START and END written in LANGUAGE, or nil."
  (when-let* ((mode (and language (agentel-markdown--mode language))))
    (let ((code (buffer-substring-no-properties start end)))
      (agentel-markdown--copy-faces
       (with-temp-buffer
         (insert code)
         (delay-mode-hooks (funcall mode))
         (ignore-errors (font-lock-ensure))
         (buffer-string))
       start)))
  (add-face-text-property start end 'agentel-markdown-code-block-face t))

;;;; Highlighting

(defun agentel-markdown--walk (tree function)
  "Call FUNCTION on each node of TREE, outermost first."
  (unless (stringp tree)
    (funcall function tree)
    (dolist (child (cddr tree))
      (agentel-markdown--walk child function))))

(defun agentel-markdown--at (node key)
  "Return the position in the buffer of the attribute KEY of NODE."
  (1+ (plist-get (cadr node) key)))

(defun agentel-markdown--mark (node face)
  "Give the content of NODE FACE and dim the markup around it."
  (add-face-text-property (agentel-markdown--at node :inner-begin)
                          (agentel-markdown--at node :inner-end) face)
  (add-face-text-property (agentel-markdown--at node :begin)
                          (agentel-markdown--at node :inner-begin)
                          'agentel-markdown-markup-face)
  (add-face-text-property (agentel-markdown--at node :inner-end)
                          (agentel-markdown--at node :end)
                          'agentel-markdown-markup-face))

(defun agentel-markdown--code (node)
  "Return the code of the code block NODE without its last newline."
  (string-remove-suffix "\n" (or (nth 2 node) "")))

(defun agentel-markdown--highlight-node (node)
  "Highlight the markup of NODE, a node of the tree of the buffer."
  (pcase (car node)
    ('heading
     (add-face-text-property (agentel-markdown--at node :begin) (agentel-markdown--at node :end)
                             'agentel-markdown-heading-face))
    ('emphasis (agentel-markdown--mark node 'italic))
    ('strong (agentel-markdown--mark node 'bold))
    ('strikethrough (agentel-markdown--mark node 'agentel-markdown-strikethrough-face))
    ('code (agentel-markdown--mark node 'agentel-markdown-code-face))
    ((or 'link 'image)
     (let ((url (plist-get (cadr node) :destination)))
       (make-text-button (agentel-markdown--at node :inner-begin)
                         (agentel-markdown--at node :inner-end)
                         'action (lambda (_) (browse-url url))
                         'help-echo url)))
    ('code-block
     (agentel-markdown--fontify-code (agentel-markdown--at node :inner-begin)
                                     (agentel-markdown--at node :inner-end)
                                     (plist-get (cadr node) :language))
     (add-face-text-property (agentel-markdown--at node :begin)
                             (agentel-markdown--at node :inner-begin)
                             'agentel-markdown-markup-face)
     (add-face-text-property (agentel-markdown--at node :inner-end)
                             (agentel-markdown--at node :end)
                             'agentel-markdown-markup-face)
     (put-text-property (agentel-markdown--at node :begin) (agentel-markdown--at node :end)
                        'agentel-markdown-code-block (agentel-markdown--code node)))))

(defun agentel-markdown-code-blocks (text)
  "Return the code blocks of TEXT as (LANGUAGE . CODE), in order.
LANGUAGE is empty for a block without one."
  (let (blocks)
    (agentel-markdown--walk
     (agentel-commonmark-parse text)
     (lambda (node)
       (when (eq (car node) 'code-block)
         (push (cons (or (plist-get (cadr node) :language) "") (agentel-markdown--code node))
               blocks))))
    (nreverse blocks)))

;;;; Tables

(defun agentel-markdown--table-cells ()
  "Return the cells of the table row at point as (START . END).
A cell ends at the pipe after it, or at the end of the line for a last
cell without one."
  (let ((end (pos-eol))
        (cell (pos-bol))
        cells)
    (save-excursion
      (skip-chars-forward " \t")
      (when (eq (char-after) ?|)
        (setq cell (1+ (point))))
      (goto-char cell)
      (while (search-forward "|" end t)
        (unless (eq (char-before (1- (point))) ?\\)
          (push (cons cell (1- (point))) cells)
          (setq cell (point))))
      (unless (string-blank-p (buffer-substring-no-properties cell end))
        (push (cons cell end) cells)))
    (nreverse cells)))

(defun agentel-markdown--lines (start count)
  "Return COUNT lines from START as (BOL . EOL)."
  (save-excursion
    (goto-char start)
    (let (lines)
      (dotimes (_ count)
        (push (cons (pos-bol) (pos-eol)) lines)
        (forward-line))
      (nreverse lines))))

(defun agentel-markdown--cell-width (cell)
  "Return the width of CELL, which is (START . END)."
  (string-width (buffer-substring-no-properties (car cell) (cdr cell))))

(defun agentel-markdown--delimiter-row-p (index)
  "Return non-nil if the row at INDEX of a table is its delimiter row."
  (= index 1))

(defun agentel-markdown--paddings (rows)
  "Return the paddings of the cells of ROWS to the widest of their column.
Each row has a list of (PIPE . PAD), PAD columns to show on the pipe at
PIPE that ends a cell."
  (let ((widths (make-vector (apply #'max (mapcar #'length rows)) 0)))
    (dolist (row rows)
      (let ((column 0))
        (dolist (cell row)
          (aset widths column (max (aref widths column)
                                   (agentel-markdown--cell-width cell)))
          (setq column (1+ column)))))
    (mapcar (lambda (row)
              (let ((column 0)
                    paddings)
                (dolist (cell row)
                  (let ((pad (- (aref widths column) (agentel-markdown--cell-width cell)))
                        (pipe (cdr cell)))
                    (when (and (> pad 0) (eq (char-after pipe) ?|))
                      (push (cons pipe pad) paddings)))
                  (setq column (1+ column)))
                paddings))
            rows)))

(defun agentel-markdown--padded-width (line paddings)
  "Return the width of LINE, which is (BOL . EOL), shown with PADDINGS."
  (apply #'+ (string-width (buffer-substring-no-properties (car line) (cdr line)))
         (mapcar #'cdr paddings)))

(defun agentel-markdown--pad-table (paddings)
  "Show the rows of a table with PADDINGS on the pipes ending their cells.
The text stays as it was written."
  (let ((index 0))
    (dolist (row paddings)
      (let ((fill (if (agentel-markdown--delimiter-row-p index) ?- ?\s)))
        (dolist (padding row)
          (put-text-property (car padding) (1+ (car padding)) 'display
                             (concat (make-string (cdr padding) fill) "|"))))
      (setq index (1+ index)))))

(defun agentel-markdown--cell-text (cell)
  "Return the text of CELL, which is (START . END), without spaces around it."
  (string-trim (buffer-substring (car cell) (cdr cell))))

(defun agentel-markdown--fit-columns (widths room)
  "Return WIDTHS, a list, narrowed from the widest until they add up to ROOM.
No column gets narrower than 2, so that a wide character fits in it."
  (let ((widths (copy-sequence widths)))
    (while (and (> (apply #'+ widths) room)
                (> (apply #'max widths) 2))
      (let ((widest (cl-position (apply #'max widths) widths)))
        (setf (nth widest widths) (1- (nth widest widths)))))
    widths))

(defun agentel-markdown--words (text)
  "Return TEXT split where a line may break.
A line may break at a space and around a character, such as a CJK
one, of the category of characters breaking a line anywhere."
  (let ((start 0)
        (pos 0)
        words)
    (while (< pos (length text))
      (let ((char (aref text pos)))
        (when (or (memq char '(?\s ?\t))
                  (aref (char-category-set char) ?|))
          (when (< start pos)
            (push (substring text start pos) words))
          (push (substring text pos (1+ pos)) words)
          (setq start (1+ pos))))
      (setq pos (1+ pos)))
    (when (< start pos)
      (push (substring text start pos) words))
    (nreverse words)))

(defun agentel-markdown--cut (word width)
  "Return how many characters from the start of WORD fit in WIDTH, at least 1."
  (let ((cut 1))
    (while (and (< cut (length word))
                (<= (string-width word 0 (1+ cut)) width))
      (setq cut (1+ cut)))
    cut))

(defun agentel-markdown--wrap (text width)
  "Return TEXT broken into lines no wider than WIDTH.
A word wider than WIDTH is broken where it reaches it."
  (let ((line "")
        lines)
    (dolist (word (agentel-markdown--words text))
      (cond ((<= (string-width (concat line word)) width)
             (unless (and (string-empty-p line) (string-blank-p word))
               (setq line (concat line word))))
            ((string-blank-p word)
             (push line lines)
             (setq line ""))
            (t
             (unless (string-empty-p line)
               (push (string-trim-right line) lines))
             (while (> (string-width word) width)
               (let ((cut (agentel-markdown--cut word width)))
                 (push (substring word 0 cut) lines)
                 (setq word (substring word cut))))
             (setq line word))))
    (unless (string-empty-p line)
      (push (string-trim-right line) lines))
    (nreverse lines)))

(defun agentel-markdown--wrapped-row (row widths)
  "Return the lines showing ROW with its cells wrapped in WIDTHS."
  (let* ((cells (cl-mapcar (lambda (width index)
                             (when-let* ((cell (nth index row)))
                               (agentel-markdown--wrap (agentel-markdown--cell-text cell)
                                                       width)))
                           widths (number-sequence 0 (1- (length widths)))))
         (height (apply #'max 1 (mapcar #'length cells)))
         lines)
    (dotimes (index height)
      (push (concat "|"
                    (mapconcat
                     (lambda (pair)
                       (let ((text (or (nth index (car pair)) "")))
                         (concat " " text
                                 (make-string (max 0 (- (cdr pair) (string-width text))) ?\s)
                                 " |")))
                     (cl-mapcar #'cons cells widths)))
            lines))
    (string-join (nreverse lines) "\n")))

(defun agentel-markdown--wrap-table (rows lines width)
  "Show ROWS, which are at LINES, with their cells wrapped to fit in WIDTH.
Each row is shown in place of its line, so the text stays as it was
written."
  (let* ((count (apply #'max (mapcar #'length rows)))
         (widths (make-list count 0))
         (index 0))
    (dolist (row rows)
      (unless (agentel-markdown--delimiter-row-p index)
        (let ((column 0))
          (dolist (cell row)
            (setf (nth column widths)
                  (max (nth column widths)
                       (string-width (agentel-markdown--cell-text cell))))
            (setq column (1+ column)))))
      (setq index (1+ index)))
    ;; A row takes a pipe and two spaces for each column, and a pipe.
    (setq widths (agentel-markdown--fit-columns widths (- width 1 (* 3 count))))
    (setq index 0)
    (cl-mapc (lambda (row line)
               (put-text-property
                (car line) (cdr line) 'display
                (if (agentel-markdown--delimiter-row-p index)
                    (concat "|" (mapconcat (lambda (width) (make-string (+ width 2) ?-))
                                           widths "|")
                            "|")
                  (agentel-markdown--wrapped-row row widths)))
               (setq index (1+ index)))
             rows lines)))

(defun agentel-markdown--align-table (rows start width)
  "Show the cells of ROWS, a table at START, aligned in columns.
They are padded to the widest of their column, or wrapped in their
column when the padded table is wider than WIDTH.  The table is
marked to be formatted again when the width changes."
  (let ((lines (agentel-markdown--lines start (length rows)))
        (paddings (agentel-markdown--paddings rows)))
    (if (or (not width)
            (<= (apply #'max (cl-mapcar #'agentel-markdown--padded-width lines paddings))
                width))
        (agentel-markdown--pad-table paddings)
      (agentel-markdown--wrap-table rows lines width))
    (put-text-property start (cdr (car (last lines))) 'agentel-ui-fits-width t)))

(defun agentel-markdown--tables (tree width)
  "Align the columns of the tables of TREE to fit in WIDTH.
TREE is the tree of the buffer."
  (agentel-markdown--walk
   tree
   (lambda (node)
     (when (eq (car node) 'table)
       (save-excursion
         (goto-char (agentel-markdown--at node :begin))
         (let ((start (pos-bol))
               rows)
           ;; The delimiter row is not a row of the tree.
           (dotimes (_ (1+ (length (cddr node))))
             (push (agentel-markdown--table-cells) rows)
             (forward-line))
           (agentel-markdown--align-table (nreverse rows) start width)))))))

(defun agentel-markdown-format (text &optional width)
  "Return TEXT with its Markdown highlighted.
Tables wider than WIDTH have their cells wrapped to fit in it; without
WIDTH, no table is wrapped."
  (let ((tree (agentel-commonmark-parse text)))
    (with-temp-buffer
      (insert text)
      (agentel-markdown--walk tree #'agentel-markdown--highlight-node)
      (agentel-markdown--tables tree width)
      (buffer-string))))

(setq agentel-chat-format-message-function #'agentel-markdown-format)

;;;; Copying code

(defun agentel-markdown--turn-blocks (items)
  "Return the code blocks of the finished agent messages of the last turn.
The turn is the ITEMS of a conversation from the last prompt on, or
all of them without a prompt."
  (let ((turn items))
    (let ((tail items))
      (while tail
        (when (eq (agentel-store-model-type (car tail)) 'user)
          (setq turn tail))
        (setq tail (cdr tail))))
    (mapcan (lambda (item)
              (and (eq (agentel-store-model-type item) 'agent)
                   (agentel-store-get item 'finished)
                   (agentel-markdown-code-blocks (agentel-store-get item 'text))))
            turn)))

(defun agentel-markdown--choices (blocks)
  "Return BLOCKS, which are (LANGUAGE . CODE), as (LABEL . CODE).
A label is the language and the first line of the code, numbered by
position when an earlier block has the same."
  (let ((index 0) choices)
    (dolist (block blocks)
      (let* ((line (car (split-string (cdr block) "\n")))
             (label (if (string-empty-p (car block))
                        line
                      (format "%s: %s" (car block) line))))
        (setq index (1+ index))
        (when (assoc label choices)
          (setq label (format "%s (%d)" label index)))
        (push (cons label (cdr block)) choices)))
    (nreverse choices)))

(defun agentel-markdown--choose-block (blocks)
  "Return the code of one of BLOCKS, asking which when there are several.
BLOCKS are (LANGUAGE . CODE); the last one is the default."
  (if (cdr blocks)
      (let* ((choices (agentel-markdown--choices blocks))
             (choice (completing-read "Copy code block: "
                                      (agentel-chat-ordered-completion
                                       (mapcar #'car choices))
                                      nil t nil nil (car (car (last choices))))))
        (cdr (assoc choice choices)))
    (cdr (car blocks))))

(defun agentel-markdown-copy-code ()
  "Copy the code of a code block, without its fences.
The block is the one at point, or else one the agent wrote in the
last turn, asked for when there are several."
  (interactive)
  (let ((code (or (get-text-property (point) 'agentel-markdown-code-block)
                  (when-let* ((session (agentel-session-current))
                              (blocks (agentel-markdown--turn-blocks
                                       (agentel-conversation-items session))))
                    (agentel-markdown--choose-block blocks)))))
    (unless code
      (user-error "No code block in the last turn"))
    (kill-new code)
    (message "Copied the code block")))

(keymap-set agentel-chat-mode-map "C-c C-w" #'agentel-markdown-copy-code)

(provide 'agentel-markdown)
;;; agentel-markdown.el ends here
