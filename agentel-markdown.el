;;; agentel-markdown.el --- Markdown in agent messages for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Agents answer in Markdown.  Once a message is complete, its headings,
;; emphasis, inline code, links and fenced code blocks are highlighted;
;; code blocks are fontified with the major mode of their language.
;; The markup itself stays in the text, so copying a message gives back
;; what the agent wrote.  `agentel-markdown-copy-code' copies the code
;; of a block without its fences.

;;; Code:

(require 'subr-x)
(require 'button)
(require 'agentel-session)
(require 'agentel-chat)

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
  "Highlight the code between START and END written in LANGUAGE."
  (when-let* ((mode (agentel-markdown--mode language)))
    (let ((code (buffer-substring-no-properties start end)))
      (agentel-markdown--copy-faces
       (with-temp-buffer
         (insert code)
         (delay-mode-hooks (funcall mode))
         (ignore-errors (font-lock-ensure))
         (buffer-string))
       start)))
  (add-face-text-property start end 'agentel-markdown-code-block-face t)
  (put-text-property start end 'agentel-markdown-code t))

(defun agentel-markdown--each-block (function)
  "Call FUNCTION on each fenced code block of the current buffer.
It is called with the language, the start of the opening fence, the
start and end of the code, and the end of the closing fence."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*```\\([^`\n]*\\)\n" nil t)
    (let ((language (string-trim (match-string 1)))
          (fence (match-beginning 0))
          (start (point)))
      (if (re-search-forward "^[ \t]*```[ \t]*$" nil t)
          ;; FUNCTION may run a major mode, which clobbers the match data.
          (let ((end (match-beginning 0))
                (close (match-end 0)))
            (funcall function language fence start end close)
            (goto-char close))
        (goto-char (point-max))))))

(defun agentel-markdown--code (start end)
  "Return the code between START and END without its last newline."
  (string-remove-suffix "\n" (buffer-substring-no-properties start end)))

(defun agentel-markdown--code-blocks ()
  "Highlight the fenced code blocks of the current buffer."
  (agentel-markdown--each-block
   (lambda (language fence start end close)
     (agentel-markdown--fontify-code start end language)
     (add-face-text-property fence start 'agentel-markdown-markup-face)
     (add-face-text-property end close 'agentel-markdown-markup-face)
     (put-text-property fence close 'agentel-markdown-code t)
     (put-text-property fence close 'agentel-markdown-code-block
                        (agentel-markdown--code start end)))))

(defun agentel-markdown-code-blocks (text)
  "Return the fenced code blocks of TEXT as (LANGUAGE . CODE), in order."
  (with-temp-buffer
    (insert text)
    (let (blocks)
      (agentel-markdown--each-block
       (lambda (language _fence start end _close)
         (push (cons language (agentel-markdown--code start end)) blocks)))
      (nreverse blocks))))

(defun agentel-markdown--each (regexp function)
  "Call FUNCTION after each match of REGEXP outside code."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (get-text-property (match-beginning 0) 'agentel-markdown-code)
      (save-match-data (funcall function)))))

(defun agentel-markdown--emphasis (regexp face)
  "Give the text matched by REGEXP group 2 FACE and dim group 1 and 3."
  (agentel-markdown--each
   regexp
   (lambda ()
     (add-face-text-property (match-beginning 2) (match-end 2) face)
     (add-face-text-property (match-beginning 1) (match-end 1)
                             'agentel-markdown-markup-face)
     (add-face-text-property (match-beginning 3) (match-end 3)
                             'agentel-markdown-markup-face))))

(defun agentel-markdown--inline ()
  "Highlight the inline Markdown of the current buffer."
  (agentel-markdown--each
   "\\(`\\)\\([^`\n]+\\)\\(`\\)"
   (lambda ()
     (add-face-text-property (match-beginning 2) (match-end 2)
                             'agentel-markdown-code-face)
     (add-face-text-property (match-beginning 1) (match-end 1)
                             'agentel-markdown-markup-face)
     (add-face-text-property (match-beginning 3) (match-end 3)
                             'agentel-markdown-markup-face)
     (put-text-property (match-beginning 0) (match-end 0) 'agentel-markdown-code t)))
  (agentel-markdown--each
   "^#\\{1,6\\}[ \t]+.*$"
   (lambda () (add-face-text-property (match-beginning 0) (match-end 0)
                                      'agentel-markdown-heading-face)))
  (agentel-markdown--emphasis "\\(\\*\\*\\)\\([^*\n]+\\)\\(\\*\\*\\)" 'bold)
  ;; A star followed by a space starts a list item, not emphasis.
  (agentel-markdown--emphasis
   "\\(?:^\\|[^*[:alnum:]]\\)\\(\\*\\)\\([^* \n][^*\n]*\\)\\(\\*\\)" 'italic)
  (agentel-markdown--each
   "\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))"
   (lambda ()
     (let ((url (match-string 2)))
       (make-text-button (match-beginning 1) (match-end 1)
                         'action (lambda (_) (browse-url url))
                         'help-echo url)))))

;;;; Tables

(defconst agentel-markdown--delimiter-regexp
  "[ \t]*|?\\(?:[ \t]*:?-+:?[ \t]*|\\)*[ \t]*:?-+:?[ \t]*|?[ \t]*$"
  "Regexp matching the delimiter row under the header of a table.")

(defun agentel-markdown--table-line-p ()
  "Return non-nil if the current line can be a row of a table."
  (and (not (eobp))
       (not (get-text-property (point) 'agentel-markdown-code))
       (string-search "|" (buffer-substring-no-properties (pos-bol) (pos-eol)))))

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

(defun agentel-markdown--each-table (function)
  "Call FUNCTION on the rows of each table of the current buffer.
A row is the list of its cells, as (START . END)."
  (goto-char (point-min))
  (while (not (eobp))
    (if (and (agentel-markdown--table-line-p)
             (save-excursion
               (forward-line)
               (and (agentel-markdown--table-line-p)
                    (looking-at-p agentel-markdown--delimiter-regexp))))
        (let (rows)
          (while (agentel-markdown--table-line-p)
            (push (agentel-markdown--table-cells) rows)
            (forward-line))
          (funcall function (nreverse rows)))
      (forward-line))))

(defun agentel-markdown--cell-width (cell)
  "Return the width of CELL, which is (START . END)."
  (string-width (buffer-substring-no-properties (car cell) (cdr cell))))

(defun agentel-markdown--align-table (rows)
  "Show the cells of ROWS padded to the widest of their column.
The padding is displayed on the pipe that ends a cell, so the text
stays as it was written."
  (let ((widths (make-vector (apply #'max (mapcar #'length rows)) 0)))
    (dolist (row rows)
      (let ((column 0))
        (dolist (cell row)
          (aset widths column (max (aref widths column)
                                   (agentel-markdown--cell-width cell)))
          (setq column (1+ column)))))
    (dolist (row rows)
      (let ((column 0))
        (dolist (cell row)
          (let ((pad (- (aref widths column) (agentel-markdown--cell-width cell)))
                (pipe (cdr cell)))
            (when (and (> pad 0) (eq (char-after pipe) ?|))
              (put-text-property pipe (1+ pipe) 'display
                                 (concat (make-string pad ?\s) "|"))))
          (setq column (1+ column)))))))

(defun agentel-markdown--tables ()
  "Align the columns of the tables of the current buffer."
  (agentel-markdown--each-table #'agentel-markdown--align-table))

(defun agentel-markdown-format (text)
  "Return TEXT with its Markdown highlighted."
  (with-temp-buffer
    (insert text)
    (agentel-markdown--code-blocks)
    (agentel-markdown--tables)
    (agentel-markdown--inline)
    (remove-text-properties (point-min) (point-max) '(agentel-markdown-code nil))
    (buffer-string)))

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
