;;; agentel-commonmark.el --- CommonMark parser for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-commonmark-parse' reads Markdown as CommonMark with the GFM
;; extensions of tables, strikethrough, task list items and autolinks.
;; It follows the parsing strategy of the appendix of the CommonMark
;; spec, as its reference implementation commonmark.js does.  Raw HTML
;; is left as text.
;;
;; The tree it gives has nodes of the form (TYPE ATTRIBUTES . CHILDREN),
;; with text as strings.  ATTRIBUTES is a plist whose :begin and :end
;; are the positions in the Markdown the node spans, counted from 0;
;; nodes with markup around their content also have :inner-begin and
;; :inner-end around it.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'subr-x)
(require 'agentel-commonmark-entities)

;;;; Blocks

(cl-defstruct (agentel-commonmark--block
               (:constructor agentel-commonmark--make-block)
               (:conc-name agentel-commonmark--b-)
               (:copier nil))
  "A block being parsed."
  type parent children (open t) (content "") segments begin end data)

(defvar agentel-commonmark--text nil "The Markdown being parsed.")
(defvar agentel-commonmark--line nil "The line being parsed, without its line ending.")
(defvar agentel-commonmark--line-begin 0 "The position of the line being parsed.")
(defvar agentel-commonmark--line-end 0 "The position of the end of the line being parsed.")
(defvar agentel-commonmark--previous-line-end 0 "The position of the end of the previous line.")
(defvar agentel-commonmark--offset 0 "The index in the line up to which it is parsed.")
(defvar agentel-commonmark--column 0 "The column the offset is at.")
(defvar agentel-commonmark--next-nonspace 0 "The index of the next character not a space.")
(defvar agentel-commonmark--next-nonspace-column 0 "The column of the next character not a space.")
(defvar agentel-commonmark--indent 0 "The columns from the offset to the next character not a space.")
(defvar agentel-commonmark--indented nil "Whether the indent makes an indented code block.")
(defvar agentel-commonmark--blank nil "Whether the rest of the line is blank.")
(defvar agentel-commonmark--partially-consumed-tab nil
  "Whether the offset is inside a tab of which some columns are consumed.")
(defvar agentel-commonmark--all-closed t "Whether the unmatched blocks are closed.")
(defvar agentel-commonmark--last-matched-container nil "The last block the line continues.")
(defvar agentel-commonmark--tip nil "The innermost open block.")
(defvar agentel-commonmark--oldtip nil "The innermost open block before the line.")
(defvar agentel-commonmark--refmap nil "Map normalized link labels to (DESTINATION . TITLE).")

(defconst agentel-commonmark--escapable "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-"
  "The characters a backslash escapes.")

(defconst agentel-commonmark--entity-regexp
  (rx "&" (or (seq "#" (any "xX") (** 1 6 (any "0-9a-fA-F")))
              (seq "#" (** 1 7 (any "0-9")))
              (seq (any "a-zA-Z") (** 1 31 (any "a-zA-Z0-9"))))
      ";")
  "Regexp matching an entity or numeric character reference.")

(defun agentel-commonmark--peek (string index)
  "Return the character of STRING at INDEX, or nil past its end."
  (and (< index (length string)) (aref string index)))

(defun agentel-commonmark--space-or-tab-p (char)
  "Return non-nil if CHAR is a space or a tab."
  (memq char '(?\s ?\t)))

(defun agentel-commonmark--advance-offset (count columns)
  "Advance the offset over COUNT characters, or COUNT columns if COLUMNS."
  (let ((line agentel-commonmark--line))
    (while (and (> count 0) (< agentel-commonmark--offset (length line)))
      (if (eq (aref line agentel-commonmark--offset) ?\t)
          (let ((to-tab (- 4 (% agentel-commonmark--column 4))))
            (if columns
                (let ((advance (min to-tab count)))
                  (setq agentel-commonmark--partially-consumed-tab (> to-tab count))
                  (cl-incf agentel-commonmark--column advance)
                  (unless agentel-commonmark--partially-consumed-tab
                    (cl-incf agentel-commonmark--offset))
                  (cl-decf count advance))
              (setq agentel-commonmark--partially-consumed-tab nil)
              (cl-incf agentel-commonmark--column to-tab)
              (cl-incf agentel-commonmark--offset)
              (cl-decf count)))
        (setq agentel-commonmark--partially-consumed-tab nil)
        (cl-incf agentel-commonmark--offset)
        (cl-incf agentel-commonmark--column)
        (cl-decf count)))))

(defun agentel-commonmark--advance-next-nonspace ()
  "Advance the offset to the next character not a space."
  (setq agentel-commonmark--offset agentel-commonmark--next-nonspace
        agentel-commonmark--column agentel-commonmark--next-nonspace-column
        agentel-commonmark--partially-consumed-tab nil))

(defun agentel-commonmark--find-next-nonspace ()
  "Find the next character not a space from the offset."
  (let ((line agentel-commonmark--line)
        (index agentel-commonmark--offset)
        (column agentel-commonmark--column))
    (while (and (< index (length line))
                (agentel-commonmark--space-or-tab-p (aref line index)))
      (setq column (if (eq (aref line index) ?\s) (1+ column) (+ column (- 4 (% column 4))))
            index (1+ index)))
    (setq agentel-commonmark--blank (>= index (length line))
          agentel-commonmark--next-nonspace index
          agentel-commonmark--next-nonspace-column column
          agentel-commonmark--indent (- column agentel-commonmark--column)
          agentel-commonmark--indented (>= agentel-commonmark--indent 4))))

(defun agentel-commonmark--rest ()
  "Return the line from the next character not a space."
  (substring agentel-commonmark--line agentel-commonmark--next-nonspace))

(defun agentel-commonmark--add-content (block text position)
  "Add TEXT, which is at POSITION in the Markdown, to the content of BLOCK."
  (push (cons (length (agentel-commonmark--b-content block)) position)
        (agentel-commonmark--b-segments block))
  (setf (agentel-commonmark--b-content block)
        (concat (agentel-commonmark--b-content block) text)))

(defun agentel-commonmark--add-line ()
  "Add the line from the offset to the content of the innermost block."
  (let ((tip agentel-commonmark--tip)
        (position (+ agentel-commonmark--line-begin agentel-commonmark--offset)))
    (when agentel-commonmark--partially-consumed-tab
      (agentel-commonmark--add-content
       tip (make-string (- 4 (% agentel-commonmark--column 4)) ?\s) position)
      (cl-incf agentel-commonmark--offset)
      (cl-incf position))
    (agentel-commonmark--add-content
     tip (concat (substring agentel-commonmark--line agentel-commonmark--offset) "\n")
     position)))

(defun agentel-commonmark--position (block index)
  "Return the position in the Markdown of INDEX in the content of BLOCK."
  (let ((segment (cl-find-if (lambda (segment) (<= (car segment) index))
                             (agentel-commonmark--b-segments block))))
    (if segment
        (+ (cdr segment) (- index (car segment)))
      (agentel-commonmark--b-begin block))))

(defun agentel-commonmark--drop-content (block count)
  "Drop COUNT characters from the start of the content of BLOCK."
  (let (kept)
    (dolist (segment (reverse (agentel-commonmark--b-segments block)))
      (if (>= (car segment) count)
          (push (cons (- (car segment) count) (cdr segment)) kept)
        (setq kept (list (cons 0 (+ (cdr segment) (- count (car segment))))))))
    (setf (agentel-commonmark--b-segments block) kept
          (agentel-commonmark--b-content block)
          (substring (agentel-commonmark--b-content block) count))))

(defun agentel-commonmark--can-contain-p (parent type)
  "Return non-nil if a block of type PARENT can contain one of TYPE."
  (pcase parent
    ((or 'document 'block-quote 'item) (not (eq type 'item)))
    ('list (eq type 'item))
    (_ nil)))

(defun agentel-commonmark--accepts-lines-p (type)
  "Return non-nil if a block of TYPE takes lines as its content."
  (memq type '(paragraph code-block)))

(defun agentel-commonmark--add-child (type offset)
  "Add a block of TYPE starting at OFFSET of the line to the innermost block."
  (while (not (agentel-commonmark--can-contain-p
               (agentel-commonmark--b-type agentel-commonmark--tip) type))
    (agentel-commonmark--finalize agentel-commonmark--tip
                                  agentel-commonmark--previous-line-end))
  (let ((block (agentel-commonmark--make-block
                :type type :parent agentel-commonmark--tip
                :begin (+ agentel-commonmark--line-begin offset))))
    (setf (agentel-commonmark--b-children agentel-commonmark--tip)
          (nconc (agentel-commonmark--b-children agentel-commonmark--tip) (list block)))
    (setq agentel-commonmark--tip block)))

(defun agentel-commonmark--unlink (block)
  "Remove BLOCK from its parent."
  (let ((parent (agentel-commonmark--b-parent block)))
    (setf (agentel-commonmark--b-children parent)
          (delq block (agentel-commonmark--b-children parent)))))

(defun agentel-commonmark--replace (block new)
  "Put NEW in place of BLOCK in its parent."
  (let ((parent (agentel-commonmark--b-parent block)))
    (setf (agentel-commonmark--b-parent new) parent
          (agentel-commonmark--b-children parent)
          (cl-substitute new block (agentel-commonmark--b-children parent)))))

(defun agentel-commonmark--take-references (block)
  "Take the link reference definitions at the start of the content of BLOCK.
Return non-nil if there were any."
  (let (found count)
    (while (and (string-prefix-p "[" (agentel-commonmark--b-content block))
                (> (setq count (agentel-commonmark--parse-reference
                                (agentel-commonmark--b-content block)))
                   0))
      (agentel-commonmark--drop-content block count)
      (setq found t))
    found))

(defun agentel-commonmark--finalize (block end)
  "Close BLOCK, which ends at END."
  (setf (agentel-commonmark--b-open block) nil
        (agentel-commonmark--b-end block) end)
  (pcase (agentel-commonmark--b-type block)
    ('paragraph
     (when (and (agentel-commonmark--take-references block)
                (string-match-p "\\`[ \t\n\r]*\\'" (agentel-commonmark--b-content block)))
       (agentel-commonmark--unlink block)))
    ('code-block (agentel-commonmark--finalize-code-block block)))
  (setq agentel-commonmark--tip (agentel-commonmark--b-parent block)))

(defun agentel-commonmark--finalize-code-block (block)
  "Take the info string and the code of the code block BLOCK."
  (let* ((content (agentel-commonmark--b-content block))
         (data (agentel-commonmark--b-data block))
         (start 0)
         code)
    (if (plist-get data :fenced)
        (let* ((newline (string-search "\n" content))
               (info (agentel-commonmark--unescape (string-trim (substring content 0 newline))))
               (word (car (split-string info "[ \t\n\r\f\v]+" t))))
          (setq start (1+ newline)
                code (substring content start))
          (setq data (plist-put data :language word)))
      (setq code (replace-regexp-in-string "\\(?:\n *\\)+\\'" "\n" content)))
    (setf (agentel-commonmark--b-data block)
          (thread-first data
                        (plist-put :code code)
                        (plist-put :inner-begin (min (agentel-commonmark--position block start)
                                                     (agentel-commonmark--b-end block)))
                        (plist-put :inner-end
                                   (min (agentel-commonmark--position block (+ start (length code)))
                                        (agentel-commonmark--b-end block)))))))

(defun agentel-commonmark--close-unmatched-blocks ()
  "Close the blocks the line does not continue."
  (unless agentel-commonmark--all-closed
    (while (not (eq agentel-commonmark--oldtip agentel-commonmark--last-matched-container))
      (let ((parent (agentel-commonmark--b-parent agentel-commonmark--oldtip)))
        (agentel-commonmark--finalize agentel-commonmark--oldtip
                                      agentel-commonmark--previous-line-end)
        (setq agentel-commonmark--oldtip parent)))
    (setq agentel-commonmark--all-closed t)))

;;;;; Continuing blocks

(defun agentel-commonmark--continue (block)
  "Continue BLOCK with the line.
Return 0 if it continues, 1 if it does not and 2 if the line is done."
  (let ((data (agentel-commonmark--b-data block)))
    (pcase (agentel-commonmark--b-type block)
      ('block-quote
       (if (and (not agentel-commonmark--indented)
                (eq (agentel-commonmark--peek agentel-commonmark--line
                                              agentel-commonmark--next-nonspace)
                    ?>))
           (progn
             (agentel-commonmark--advance-next-nonspace)
             (agentel-commonmark--advance-offset 1 nil)
             (when (agentel-commonmark--space-or-tab-p
                    (agentel-commonmark--peek agentel-commonmark--line agentel-commonmark--offset))
               (agentel-commonmark--advance-offset 1 t))
             0)
         1))
      ('item
       (let ((width (+ (plist-get data :marker-offset) (plist-get data :padding))))
         (cond (agentel-commonmark--blank
                (if (null (agentel-commonmark--b-children block))
                    1
                  (agentel-commonmark--advance-next-nonspace)
                  0))
               ((>= agentel-commonmark--indent width)
                (agentel-commonmark--advance-offset width t)
                0)
               (t 1))))
      ('code-block
       (if (plist-get data :fenced)
           (agentel-commonmark--continue-fence block)
         (cond ((>= agentel-commonmark--indent 4)
                (agentel-commonmark--advance-offset 4 t)
                0)
               (agentel-commonmark--blank
                (agentel-commonmark--advance-next-nonspace)
                0)
               (t 1))))
      ((or 'paragraph 'table) (if agentel-commonmark--blank 1 0))
      ('list 0)
      (_ 1))))

(defun agentel-commonmark--continue-fence (block)
  "Continue the fenced code block BLOCK with the line, or close it."
  (let ((data (agentel-commonmark--b-data block))
        (rest (agentel-commonmark--rest)))
    (if (and (<= agentel-commonmark--indent 3)
             (eq (agentel-commonmark--peek rest 0) (plist-get data :fence-char))
             (string-match "\\`\\(?:`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\'" rest)
             (>= (- (length (string-trim-right rest)) 0) (plist-get data :fence-length)))
        (progn
          (agentel-commonmark--finalize block agentel-commonmark--line-end)
          2)
      (let ((count (plist-get data :fence-offset)))
        (while (and (> count 0)
                    (agentel-commonmark--space-or-tab-p
                     (agentel-commonmark--peek agentel-commonmark--line agentel-commonmark--offset)))
          (agentel-commonmark--advance-offset 1 t)
          (cl-decf count)))
      0)))

;;;;; Starting blocks

(defun agentel-commonmark--start-block-quote (_container)
  "Start a block quote at the line, returning 1 if it does."
  (when (and (not agentel-commonmark--indented)
             (eq (agentel-commonmark--peek agentel-commonmark--line agentel-commonmark--next-nonspace)
                 ?>))
    (agentel-commonmark--advance-next-nonspace)
    (agentel-commonmark--advance-offset 1 nil)
    (when (agentel-commonmark--space-or-tab-p
           (agentel-commonmark--peek agentel-commonmark--line agentel-commonmark--offset))
      (agentel-commonmark--advance-offset 1 t))
    (agentel-commonmark--close-unmatched-blocks)
    (agentel-commonmark--add-child 'block-quote agentel-commonmark--next-nonspace)
    1))

(defun agentel-commonmark--start-atx-heading (_container)
  "Start an ATX heading at the line, returning 2 if it does."
  (let ((rest (agentel-commonmark--rest)))
    (when (and (not agentel-commonmark--indented)
               (string-match "\\`#\\{1,6\\}\\(?:[ \t]+\\|\\'\\)" rest))
      (let ((level (length (string-trim (match-string 0 rest)))))
        (agentel-commonmark--advance-next-nonspace)
        (agentel-commonmark--advance-offset (match-end 0) nil)
        (agentel-commonmark--close-unmatched-blocks)
        (let ((heading (agentel-commonmark--add-child 'heading agentel-commonmark--next-nonspace))
              (text (substring agentel-commonmark--line agentel-commonmark--offset)))
          (setq text (replace-regexp-in-string "\\`[ \t]*#+[ \t]*\\'" "" text))
          (setq text (replace-regexp-in-string "[ \t]+#+[ \t]*\\'" "" text))
          (agentel-commonmark--add-content
           heading text (+ agentel-commonmark--line-begin agentel-commonmark--offset))
          (setf (agentel-commonmark--b-data heading) (list :level level))
          (agentel-commonmark--advance-offset
           (- (length agentel-commonmark--line) agentel-commonmark--offset) nil))
        2))))

(defun agentel-commonmark--start-fence (_container)
  "Start a fenced code block at the line, returning 2 if it does."
  (let ((rest (agentel-commonmark--rest)))
    (when (and (not agentel-commonmark--indented)
               (or (and (string-match "\\``\\{3,\\}" rest)
                        (not (string-search "`" (substring rest (match-end 0)))))
                   (string-match "\\`~\\{3,\\}" rest)))
      (let ((length (match-end 0)))
        (agentel-commonmark--close-unmatched-blocks)
        (let ((block (agentel-commonmark--add-child 'code-block agentel-commonmark--next-nonspace)))
          (setf (agentel-commonmark--b-data block)
                (list :fenced t :fence-length length :fence-char (aref rest 0)
                      :fence-offset agentel-commonmark--indent)))
        (agentel-commonmark--advance-next-nonspace)
        (agentel-commonmark--advance-offset length nil)
        2))))

(defun agentel-commonmark--start-setext-heading (container)
  "Turn the paragraph CONTAINER into a setext heading, returning 2 if it does."
  (let ((rest (agentel-commonmark--rest)))
    (when (and (not agentel-commonmark--indented)
               (eq (agentel-commonmark--b-type container) 'paragraph)
               (string-match "\\`\\(?:=+\\|-+\\)[ \t]*\\'" rest))
      (agentel-commonmark--close-unmatched-blocks)
      (agentel-commonmark--take-references container)
      (when (> (length (agentel-commonmark--b-content container)) 0)
        (let ((heading (agentel-commonmark--make-block
                        :type 'heading
                        :begin (agentel-commonmark--b-begin container)
                        :content (agentel-commonmark--b-content container)
                        :segments (agentel-commonmark--b-segments container)
                        :data (list :level (if (eq (aref rest 0) ?=) 1 2)))))
          (agentel-commonmark--replace container heading)
          (setq agentel-commonmark--tip heading)
          (agentel-commonmark--advance-offset
           (- (length agentel-commonmark--line) agentel-commonmark--offset) nil)
          2)))))

(defun agentel-commonmark--start-thematic-break (_container)
  "Start a thematic break at the line, returning 2 if it does."
  (when (and (not agentel-commonmark--indented)
             (string-match-p "\\`\\(?:\\(?:\\*[ \t]*\\)\\{3,\\}\\|\\(?:_[ \t]*\\)\\{3,\\}\\|\\(?:-[ \t]*\\)\\{3,\\}\\)\\'"
                             (agentel-commonmark--rest)))
    (agentel-commonmark--close-unmatched-blocks)
    (agentel-commonmark--add-child 'thematic-break agentel-commonmark--next-nonspace)
    (agentel-commonmark--advance-offset
     (- (length agentel-commonmark--line) agentel-commonmark--offset) nil)
    2))

(defun agentel-commonmark--parse-list-marker (container)
  "Parse a list marker at the line in CONTAINER and return its data, or nil."
  (let ((rest (agentel-commonmark--rest))
        data marker)
    (when (< agentel-commonmark--indent 4)
      (cond ((string-match "\\`[*+-]" rest)
             (setq data (list :type 'bullet :bullet-char (aref rest 0))))
            ((and (string-match "\\`\\([0-9]\\{1,9\\}\\)\\([.)]\\)" rest)
                  (or (not (eq (agentel-commonmark--b-type container) 'paragraph))
                      (= (string-to-number (match-string 1 rest)) 1)))
             (setq data (list :type 'ordered :start (string-to-number (match-string 1 rest))
                              :delimiter (aref (match-string 2 rest) 0)))))
      (when data
        (setq marker (match-end 0))
        (let ((next (agentel-commonmark--peek rest marker)))
          (when (and (or (null next) (agentel-commonmark--space-or-tab-p next))
                     (not (and (eq (agentel-commonmark--b-type container) 'paragraph)
                               (string-match-p "\\`[ \t]*\\'" (substring rest marker)))))
            (agentel-commonmark--advance-next-nonspace)
            (agentel-commonmark--advance-offset marker t)
            (let ((spaces-column agentel-commonmark--column)
                  (spaces-offset agentel-commonmark--offset)
                  spaces)
              (while (progn
                       (agentel-commonmark--advance-offset 1 t)
                       (and (< (- agentel-commonmark--column spaces-column) 5)
                            (agentel-commonmark--space-or-tab-p
                             (agentel-commonmark--peek agentel-commonmark--line
                                                       agentel-commonmark--offset)))))
              (setq spaces (- agentel-commonmark--column spaces-column))
              (if (or (>= spaces 5) (< spaces 1)
                      (null (agentel-commonmark--peek agentel-commonmark--line
                                                      agentel-commonmark--offset)))
                  (progn
                    (setq data (plist-put data :padding (1+ marker)))
                    (setq agentel-commonmark--column spaces-column
                          agentel-commonmark--offset spaces-offset)
                    (when (agentel-commonmark--space-or-tab-p
                           (agentel-commonmark--peek agentel-commonmark--line
                                                     agentel-commonmark--offset))
                      (agentel-commonmark--advance-offset 1 t)))
                (setq data (plist-put data :padding (+ marker spaces)))))
            (plist-put data :marker-offset agentel-commonmark--indent)))))))

(defun agentel-commonmark--lists-match-p (a b)
  "Return non-nil if items with list data A and B belong to the same list."
  (and (eq (plist-get a :type) (plist-get b :type))
       (eq (plist-get a :delimiter) (plist-get b :delimiter))
       (eq (plist-get a :bullet-char) (plist-get b :bullet-char))))

(defun agentel-commonmark--start-list-item (container)
  "Start a list item at the line in CONTAINER, returning 1 if it does."
  (let (data)
    (when (and (or (not agentel-commonmark--indented)
                   (eq (agentel-commonmark--b-type container) 'list))
               (setq data (agentel-commonmark--parse-list-marker container)))
      (agentel-commonmark--close-unmatched-blocks)
      (unless (and (eq (agentel-commonmark--b-type agentel-commonmark--tip) 'list)
                   (agentel-commonmark--lists-match-p
                    (agentel-commonmark--b-data container) data))
        (setf (agentel-commonmark--b-data
               (agentel-commonmark--add-child 'list agentel-commonmark--next-nonspace))
              data))
      (setf (agentel-commonmark--b-data
             (agentel-commonmark--add-child 'item agentel-commonmark--next-nonspace))
            data)
      1)))

(defun agentel-commonmark--start-indented-code (_container)
  "Start an indented code block at the line, returning 2 if it does."
  (when (and agentel-commonmark--indented
             (not (eq (agentel-commonmark--b-type agentel-commonmark--tip) 'paragraph))
             (not agentel-commonmark--blank))
    (agentel-commonmark--advance-offset 4 t)
    (agentel-commonmark--close-unmatched-blocks)
    (agentel-commonmark--add-child 'code-block agentel-commonmark--offset)
    2))

(defconst agentel-commonmark--block-starts
  '(agentel-commonmark--start-block-quote
    agentel-commonmark--start-atx-heading
    agentel-commonmark--start-fence
    agentel-commonmark--start-setext-heading
    agentel-commonmark--start-thematic-break
    agentel-commonmark--start-list-item
    agentel-commonmark--start-indented-code
    agentel-commonmark--start-table)
  "The functions starting blocks, in the order they are tried.")

;;;;; Tables

(defun agentel-commonmark--row-cells (text)
  "Return the cells of the table row TEXT as (START . END) in it."
  (let ((index 0)
        (length (length text))
        start cells)
    (while (and (< index length) (agentel-commonmark--space-or-tab-p (aref text index)))
      (cl-incf index))
    (when (and (< index length) (eq (aref text index) ?|))
      (cl-incf index))
    (setq start index)
    (while (< index length)
      (pcase (aref text index)
        (?\\ (cl-incf index (if (eq (agentel-commonmark--peek text (1+ index)) ?|) 2 1)))
        (?| (push (cons start index) cells)
            (setq index (1+ index)
                  start index))
        (_ (cl-incf index))))
    (unless (string-match-p "\\`[ \t]*\\'" (substring text start))
      (push (cons start length) cells))
    (nreverse cells)))

(defun agentel-commonmark--delimiter-row (text)
  "Return the alignments of the columns of the delimiter row TEXT, or nil."
  (let ((cells (agentel-commonmark--row-cells text)))
    (when (and cells
               (cl-every (lambda (cell)
                           (string-match-p "\\`[ \t]*:?-+:?[ \t]*\\'"
                                           (substring text (car cell) (cdr cell))))
                         cells))
      (mapcar (lambda (cell)
                (let ((spec (string-trim (substring text (car cell) (cdr cell)))))
                  (cond ((and (string-prefix-p ":" spec) (string-suffix-p ":" spec)) "center")
                        ((string-prefix-p ":" spec) "left")
                        ((string-suffix-p ":" spec) "right"))))
              cells))))

(defun agentel-commonmark--make-row (text position aligns header)
  "Return a row block of the table row TEXT at POSITION with ALIGNS.
HEADER says whether it is the header row."
  (let* ((cells (agentel-commonmark--row-cells text))
         (row (agentel-commonmark--make-block
               :type 'table-row :open nil :begin position
               :end (+ position (length text)) :data (list :header header)))
         children)
    (dolist (align aligns)
      (let* ((cell (pop cells))
             (block (agentel-commonmark--make-block :type 'table-cell :open nil :parent row
                                                    :data (list :align align))))
        (if (null cell)
            (setf (agentel-commonmark--b-begin block) (+ position (length text))
                  (agentel-commonmark--b-end block) (+ position (length text)))
          (let ((start (car cell))
                (end (cdr cell)))
            (while (and (< start end) (agentel-commonmark--space-or-tab-p (aref text start)))
              (cl-incf start))
            (while (and (> end start) (agentel-commonmark--space-or-tab-p (aref text (1- end))))
              (cl-decf end))
            (setf (agentel-commonmark--b-begin block) (+ position (car cell))
                  (agentel-commonmark--b-end block) (+ position (cdr cell)))
            ;; An escaped pipe stands for a pipe, even in code.
            (let ((index start))
              (while (< index end)
                (let ((next (or (string-search "\\|" text index) end)))
                  (setq next (min next end))
                  (agentel-commonmark--add-content
                   block (substring text index next) (+ position index))
                  (setq index next)
                  (when (< index end)
                    (cl-incf index))))
              (when (= start end)
                (agentel-commonmark--add-content block "" (+ position start))))))
        (push block children)))
    (setf (agentel-commonmark--b-children row) (nreverse children))
    row))

(defun agentel-commonmark--start-table (container)
  "Turn the last line of the paragraph CONTAINER into the header of a table.
Return 2 if the line is the delimiter row of a table."
  (when (and (not agentel-commonmark--indented)
             (eq (agentel-commonmark--b-type container) 'paragraph))
    (let ((aligns (agentel-commonmark--delimiter-row (agentel-commonmark--rest))))
      (when aligns
        (let* ((content (agentel-commonmark--b-content container))
               (body (substring content 0 -1))
               (start (let ((newline (string-match-p "\n[^\n]*\\'" body)))
                        (if newline (1+ newline) 0)))
               (header (substring body start)))
          (when (= (length (agentel-commonmark--row-cells header)) (length aligns))
            (agentel-commonmark--close-unmatched-blocks)
            (let* ((position (agentel-commonmark--position container start))
                   (table (agentel-commonmark--make-block
                           :type 'table :begin position :data (list :aligns aligns)))
                   (row (agentel-commonmark--make-row header position aligns t)))
              (setf (agentel-commonmark--b-parent row) table
                    (agentel-commonmark--b-children table) (list row))
              (if (= start 0)
                  (agentel-commonmark--replace container table)
                (setf (agentel-commonmark--b-content container) (substring content 0 start))
                (agentel-commonmark--finalize
                 container (agentel-commonmark--position container (1- start)))
                (let ((parent (agentel-commonmark--b-parent container)))
                  (setf (agentel-commonmark--b-parent table) parent
                        (agentel-commonmark--b-children parent)
                        (nconc (agentel-commonmark--b-children parent) (list table)))))
              (setq agentel-commonmark--tip table)
              (agentel-commonmark--advance-offset
               (- (length agentel-commonmark--line) agentel-commonmark--offset) nil)
              2)))))))

(defun agentel-commonmark--add-row (table)
  "Add the line from the offset as a row of TABLE."
  (let* ((row (agentel-commonmark--make-row
               (substring agentel-commonmark--line agentel-commonmark--offset)
               (+ agentel-commonmark--line-begin agentel-commonmark--offset)
               (plist-get (agentel-commonmark--b-data table) :aligns) nil)))
    (setf (agentel-commonmark--b-parent row) table
          (agentel-commonmark--b-children table)
          (nconc (agentel-commonmark--b-children table) (list row)))))

;;;;; Lines

(defun agentel-commonmark--start-blocks (container)
  "Start the blocks the line opens in CONTAINER and return the innermost."
  (let ((matched-leaf (and (not (eq (agentel-commonmark--b-type container) 'paragraph))
                           (agentel-commonmark--accepts-lines-p
                            (agentel-commonmark--b-type container))))
        (starting t))
    (while (and starting (not matched-leaf))
      (agentel-commonmark--find-next-nonspace)
      (if (and (not agentel-commonmark--indented)
               (not (string-match-p "\\`[#`~*+_=<>0-9|:-]" (agentel-commonmark--rest))))
          (progn
            (agentel-commonmark--advance-next-nonspace)
            (setq starting nil))
        (let ((result (cl-some (lambda (start) (funcall start container))
                               agentel-commonmark--block-starts)))
          (pcase result
            (1 (setq container agentel-commonmark--tip))
            (2 (setq container agentel-commonmark--tip
                     matched-leaf t))
            (_ (agentel-commonmark--advance-next-nonspace)
               (setq starting nil))))))
    container))

(defun agentel-commonmark--incorporate-line (document begin end)
  "Add the line from BEGIN to END to DOCUMENT."
  (setq agentel-commonmark--line (substring agentel-commonmark--text begin end)
        agentel-commonmark--line-begin begin
        agentel-commonmark--line-end end
        agentel-commonmark--offset 0
        agentel-commonmark--column 0
        agentel-commonmark--blank nil
        agentel-commonmark--partially-consumed-tab nil
        agentel-commonmark--oldtip agentel-commonmark--tip)
  (let ((container document)
        (continuing t)
        done last)
    (while (and continuing
                (setq last (car (last (agentel-commonmark--b-children container))))
                (agentel-commonmark--b-open last))
      (setq container last)
      (agentel-commonmark--find-next-nonspace)
      (pcase (agentel-commonmark--continue container)
        (1 (setq container (agentel-commonmark--b-parent container)
                 continuing nil))
        (2 (setq done t
                 continuing nil))))
    (unless done
      (setq agentel-commonmark--all-closed (eq container agentel-commonmark--oldtip)
            agentel-commonmark--last-matched-container container)
      (setq container (agentel-commonmark--start-blocks container))
      (if (and (not agentel-commonmark--all-closed)
               (not agentel-commonmark--blank)
               (eq (agentel-commonmark--b-type agentel-commonmark--tip) 'paragraph))
          (agentel-commonmark--add-line)
        (agentel-commonmark--close-unmatched-blocks)
        (cond ((agentel-commonmark--accepts-lines-p (agentel-commonmark--b-type container))
               (agentel-commonmark--add-line))
              ((eq (agentel-commonmark--b-type container) 'table)
               (when (and (not agentel-commonmark--blank)
                          (< agentel-commonmark--offset (length agentel-commonmark--line)))
                 (agentel-commonmark--add-row container)))
              ((and (< agentel-commonmark--offset (length agentel-commonmark--line))
                    (not agentel-commonmark--blank))
               (agentel-commonmark--add-child 'paragraph agentel-commonmark--next-nonspace)
               (agentel-commonmark--advance-next-nonspace)
               (agentel-commonmark--add-line))))))
  (setq agentel-commonmark--previous-line-end end))

;;;; Inlines

(cl-defstruct (agentel-commonmark--node
               (:constructor agentel-commonmark--make-node)
               (:conc-name agentel-commonmark--n-)
               (:copier nil))
  "An inline being parsed."
  type literal begin end inner-begin inner-end destination title
  parent first last prev next)

(cl-defstruct (agentel-commonmark--delimiter
               (:constructor agentel-commonmark--make-delimiter)
               (:conc-name agentel-commonmark--d-)
               (:copier nil))
  "A run of delimiters that may open or close emphasis."
  char count original node previous next can-open can-close)

(cl-defstruct (agentel-commonmark--bracket
               (:constructor agentel-commonmark--make-bracket)
               (:conc-name agentel-commonmark--k-)
               (:copier nil))
  "An opening bracket of a link or an image."
  node previous previous-delimiter index image (active t) bracket-after)

(defvar agentel-commonmark--block nil "The block whose inlines are parsed.")
(defvar agentel-commonmark--shift 0 "The index in the block content of the subject.")
(defvar agentel-commonmark--delimiters nil "The innermost delimiter run.")
(defvar agentel-commonmark--brackets nil "The innermost opening bracket.")

(defun agentel-commonmark--source (point)
  "Return the position in the Markdown of POINT in the subject."
  (agentel-commonmark--position agentel-commonmark--block
                                (+ agentel-commonmark--shift (1- point))))

(defun agentel-commonmark--append (parent child)
  "Append CHILD to the children of PARENT."
  (agentel-commonmark--unlink-node child)
  (setf (agentel-commonmark--n-parent child) parent)
  (let ((last (agentel-commonmark--n-last parent)))
    (if last
        (setf (agentel-commonmark--n-next last) child
              (agentel-commonmark--n-prev child) last)
      (setf (agentel-commonmark--n-first parent) child))
    (setf (agentel-commonmark--n-last parent) child))
  child)

(defun agentel-commonmark--unlink-node (node)
  "Remove NODE from its parent."
  (let ((parent (agentel-commonmark--n-parent node))
        (prev (agentel-commonmark--n-prev node))
        (next (agentel-commonmark--n-next node)))
    (if prev
        (setf (agentel-commonmark--n-next prev) next)
      (when parent (setf (agentel-commonmark--n-first parent) next)))
    (if next
        (setf (agentel-commonmark--n-prev next) prev)
      (when parent (setf (agentel-commonmark--n-last parent) prev)))
    (setf (agentel-commonmark--n-parent node) nil
          (agentel-commonmark--n-prev node) nil
          (agentel-commonmark--n-next node) nil)))

(defun agentel-commonmark--insert-after (node new)
  "Insert NEW after NODE."
  (agentel-commonmark--unlink-node new)
  (let ((next (agentel-commonmark--n-next node))
        (parent (agentel-commonmark--n-parent node)))
    (setf (agentel-commonmark--n-next new) next
          (agentel-commonmark--n-prev new) node
          (agentel-commonmark--n-next node) new
          (agentel-commonmark--n-parent new) parent)
    (if next
        (setf (agentel-commonmark--n-prev next) new)
      (setf (agentel-commonmark--n-last parent) new))))

(defun agentel-commonmark--add-text (parent literal start end)
  "Append LITERAL, the subject from START to END, as text to PARENT."
  (agentel-commonmark--append
   parent (agentel-commonmark--make-node
           :type 'text :literal literal
           :begin (agentel-commonmark--source start)
           :end (agentel-commonmark--source end))))

(defun agentel-commonmark--decode-entity (reference)
  "Return the text the character REFERENCE stands for, or REFERENCE."
  (let ((code (cond ((string-match "\\`&#[xX]\\([0-9a-fA-F]+\\);\\'" reference)
                     (string-to-number (match-string 1 reference) 16))
                    ((string-match "\\`&#\\([0-9]+\\);\\'" reference)
                     (string-to-number (match-string 1 reference))))))
    (cond ((null code)
           (gethash (substring reference 1 -1) agentel-commonmark-entities reference))
          ((or (= code 0) (> code #x10ffff) (<= #xd800 code #xdfff)) "\ufffd")
          (t (string code)))))

(defun agentel-commonmark--unescape (string)
  "Return STRING with its backslash escapes and character references resolved."
  (replace-regexp-in-string
   (concat (rx "\\" (any "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-")) "\\|"
           agentel-commonmark--entity-regexp)
   (lambda (match)
     (if (eq (aref match 0) ?\\)
         (substring match 1)
       (agentel-commonmark--decode-entity match)))
   string t t))

(defun agentel-commonmark--normalize-uri (uri)
  "Return URI with the characters a URL cannot hold percent-encoded."
  (let ((index 0)
        (length (length uri))
        parts)
    (while (< index length)
      (let ((char (aref uri index)))
        (cond ((and (eq char ?%) (< (+ index 2) length)
                    (string-match-p "\\`[0-9a-fA-F]\\{2\\}" (substring uri (1+ index) (+ index 3))))
               (push (substring uri index (+ index 3)) parts)
               (cl-incf index 2))
              ((or (and (< char 128) (string-match-p "[A-Za-z0-9]" (string char)))
                   (memq char (string-to-list ";/?:@&=+$,-_.!~*'()#")))
               (push (string char) parts))
              (t (push (mapconcat (lambda (byte) (format "%%%02X" byte))
                                  (encode-coding-string (string char) 'utf-8))
                       parts))))
      (cl-incf index))
    (apply #'concat (nreverse parts))))

(defun agentel-commonmark--normalize-label (label)
  "Return LABEL, with its brackets, as compared with other labels."
  (upcase (downcase (replace-regexp-in-string
                     "[ \t\r\n]+" " " (string-trim (substring label 1 -1))))))

(defun agentel-commonmark--whitespace-p (char)
  "Return non-nil if CHAR is Unicode whitespace."
  (or (memq char '(?\t ?\n ?\f ?\r))
      (eq (get-char-code-property char 'general-category) 'Zs)))

(defun agentel-commonmark--punctuation-p (char)
  "Return non-nil if CHAR is Unicode punctuation or a symbol."
  (memq (get-char-code-property char 'general-category)
        '(Pc Pd Ps Pe Pi Pf Po Sm Sc Sk So)))

;;;;; Links

(defconst agentel-commonmark--label-regexp
  (rx "[" (** 0 1000 (or (not (any "[]\\")) (seq "\\" anychar))) "]")
  "Regexp matching a link label.")

(defun agentel-commonmark--parse-link-label ()
  "Skip a link label at point and return its length, or 0."
  (if (and (looking-at agentel-commonmark--label-regexp)
           (<= (- (match-end 0) (match-beginning 0)) 1001))
      (prog1 (- (match-end 0) (match-beginning 0))
        (goto-char (match-end 0)))
    0))

(defun agentel-commonmark--spnl ()
  "Skip spaces and tabs with up to one line ending among them."
  (looking-at "[ \t]*\\(?:\n[ \t]*\\)?")
  (goto-char (match-end 0))
  t)

(defun agentel-commonmark--parse-link-destination ()
  "Skip a link destination at point and return it, or nil."
  (if (looking-at "<\\(?:[^<>\n\\]\\|\\\\[^z-a]\\)*>")
      (prog1 (agentel-commonmark--normalize-uri
              (agentel-commonmark--unescape
               (buffer-substring-no-properties (1+ (match-beginning 0)) (1- (match-end 0)))))
        (goto-char (match-end 0)))
    (unless (eq (char-after) ?<)
      (let ((start (point))
            (parens 0)
            char done)
        (while (and (not done) (setq char (char-after)))
          (cond ((and (eq char ?\\) (char-after (1+ (point)))
                      (string-search (string (char-after (1+ (point))))
                                     agentel-commonmark--escapable))
                 (forward-char 2))
                ((eq char ?\()
                 (forward-char)
                 (cl-incf parens))
                ((eq char ?\))
                 (if (< parens 1)
                     (setq done t)
                   (forward-char)
                   (cl-decf parens)))
                ((or (<= char 32) (= char 127))
                 (setq done t))
                (t (forward-char))))
        (cond ((and (= (point) start) (not (eq char ?\))))
               (goto-char start)
               nil)
              ((/= parens 0) nil)
              (t (agentel-commonmark--normalize-uri
                  (agentel-commonmark--unescape
                   (buffer-substring-no-properties start (point))))))))))

(defun agentel-commonmark--parse-link-title ()
  "Skip a link title at point and return it, or nil."
  (when (looking-at (rx (or (seq "\"" (* (or (seq "\\" anychar) (not (any "\\\"")))) "\"")
                            (seq "'" (* (or (seq "\\" anychar) (not (any "\\'")))) "'")
                            (seq "(" (* (or (seq "\\" anychar) (not (any "\\()")))) ")"))))
    (goto-char (match-end 0))
    (agentel-commonmark--unescape
     (buffer-substring-no-properties (1+ (match-beginning 0)) (1- (match-end 0))))))

(defun agentel-commonmark--parse-reference (content)
  "Take a link reference definition at the start of CONTENT.
Return how many characters it takes, or 0 if there is none."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((label-length (agentel-commonmark--parse-link-label))
          destination title before-title at-end)
      (if (or (= label-length 0) (not (eq (char-after) ?:)))
          0
        (let ((label (buffer-substring-no-properties 1 (1+ label-length))))
          (forward-char)
          (agentel-commonmark--spnl)
          (setq destination (agentel-commonmark--parse-link-destination))
          (if (null destination)
              0
            (setq before-title (point))
            (agentel-commonmark--spnl)
            (when (/= (point) before-title)
              (setq title (agentel-commonmark--parse-link-title)))
            (unless title
              (goto-char before-title))
            (setq at-end (looking-at "[ \t]*\\(?:\n\\|\\'\\)"))
            (when (and (not at-end) title)
              (setq title nil)
              (goto-char before-title)
              (setq at-end (looking-at "[ \t]*\\(?:\n\\|\\'\\)")))
            (if (not at-end)
                0
              (goto-char (match-end 0))
              (let ((key (agentel-commonmark--normalize-label label)))
                (if (string-empty-p key)
                    0
                  (unless (gethash key agentel-commonmark--refmap)
                    (puthash key (cons destination title) agentel-commonmark--refmap))
                  (1- (point)))))))))))

(defun agentel-commonmark--add-bracket (node index image)
  "Push an opening bracket for NODE at INDEX, of an image if IMAGE."
  (when agentel-commonmark--brackets
    (setf (agentel-commonmark--k-bracket-after agentel-commonmark--brackets) t))
  (setq agentel-commonmark--brackets
        (agentel-commonmark--make-bracket
         :node node :previous agentel-commonmark--brackets
         :previous-delimiter agentel-commonmark--delimiters
         :index index :image image)))

(defun agentel-commonmark--remove-bracket ()
  "Pop the innermost opening bracket."
  (setq agentel-commonmark--brackets
        (agentel-commonmark--k-previous agentel-commonmark--brackets)))

(defun agentel-commonmark--parse-close-bracket (parent)
  "Parse a closing bracket at point into PARENT."
  (let* ((bracket (point))
         (start (progn (forward-char) (point)))
         (opener agentel-commonmark--brackets))
    (cond
     ((null opener)
      (agentel-commonmark--add-text parent "]" bracket start))
     ((not (agentel-commonmark--k-active opener))
      (agentel-commonmark--add-text parent "]" bracket start)
      (agentel-commonmark--remove-bracket))
     (t
      (let ((image (agentel-commonmark--k-image opener))
            (save (point))
            matched destination title)
        (when (eq (char-after) ?\()
          (forward-char)
          (agentel-commonmark--spnl)
          (if (and (setq destination (agentel-commonmark--parse-link-destination))
                   (agentel-commonmark--spnl)
                   (or (not (memq (char-before) '(?\s ?\t ?\n)))
                       (setq title (agentel-commonmark--parse-link-title))
                       t)
                   (agentel-commonmark--spnl)
                   (eq (char-after) ?\)))
              (progn (forward-char) (setq matched t))
            (goto-char save)
            (setq title nil)))
        (unless matched
          (let* ((before-label (point))
                 (length (agentel-commonmark--parse-link-label))
                 (label (cond ((> length 2)
                               (buffer-substring-no-properties before-label (+ before-label length)))
                              ((not (agentel-commonmark--k-bracket-after opener))
                               (buffer-substring-no-properties
                                (agentel-commonmark--k-index opener) start))))
                 (reference (and label
                                 (gethash (agentel-commonmark--normalize-label label)
                                          agentel-commonmark--refmap))))
            (when (= length 0)
              (goto-char save))
            (when reference
              (setq destination (car reference)
                    title (cdr reference)
                    matched t))))
        (if (not matched)
            (progn
              (agentel-commonmark--remove-bracket)
              (goto-char start)
              (agentel-commonmark--add-text parent "]" bracket start))
          (let* ((opener-node (agentel-commonmark--k-node opener))
                 (node (agentel-commonmark--make-node
                        :type (if image 'image 'link)
                        :destination destination
                        :title (and title (not (string-empty-p title)) title)
                        :begin (agentel-commonmark--n-begin opener-node)
                        :end (agentel-commonmark--source (point))
                        :inner-begin (agentel-commonmark--n-end opener-node)
                        :inner-end (agentel-commonmark--source bracket)))
                 (child (agentel-commonmark--n-next opener-node)))
            (while child
              (let ((next (agentel-commonmark--n-next child)))
                (agentel-commonmark--append node child)
                (setq child next)))
            (agentel-commonmark--append parent node)
            (agentel-commonmark--process-emphasis
             (agentel-commonmark--k-previous-delimiter opener))
            (agentel-commonmark--remove-bracket)
            (agentel-commonmark--unlink-node opener-node)
            (unless image
              (let ((outer agentel-commonmark--brackets))
                (while outer
                  (unless (agentel-commonmark--k-image outer)
                    (setf (agentel-commonmark--k-active outer) nil))
                  (setq outer (agentel-commonmark--k-previous outer))))))))))
    t))

;;;;; Emphasis

(defun agentel-commonmark--scan-delimiters (char)
  "Return (COUNT CAN-OPEN CAN-CLOSE) of the run of CHAR at point."
  (let* ((start (point))
         (count (progn (skip-chars-forward (string char)) (- (point) start)))
         (before (if (= start (point-min)) ?\n (char-before start)))
         (after (or (char-after) ?\n))
         (after-space (agentel-commonmark--whitespace-p after))
         (after-punct (agentel-commonmark--punctuation-p after))
         (before-space (agentel-commonmark--whitespace-p before))
         (before-punct (agentel-commonmark--punctuation-p before))
         (left (and (not after-space)
                    (or (not after-punct) before-space before-punct)))
         (right (and (not before-space)
                     (or (not before-punct) after-space after-punct))))
    (goto-char start)
    (if (eq char ?_)
        (list count (and left (or (not right) before-punct)) (and right (or (not left) after-punct)))
      (list count left right))))

(defun agentel-commonmark--handle-delimiter (parent char)
  "Parse a run of CHAR at point into PARENT as text that may become emphasis."
  (pcase-let ((`(,count ,can-open ,can-close) (agentel-commonmark--scan-delimiters char))
              (start (point)))
    (forward-char count)
    (let ((node (agentel-commonmark--add-text
                 parent (buffer-substring-no-properties start (point)) start (point))))
      (unless (and (eq char ?~) (> count 2))
        (setq agentel-commonmark--delimiters
              (agentel-commonmark--make-delimiter
               :char char :count count :original count :node node
               :previous agentel-commonmark--delimiters
               :can-open can-open :can-close can-close))
        (when (agentel-commonmark--d-previous agentel-commonmark--delimiters)
          (setf (agentel-commonmark--d-next
                 (agentel-commonmark--d-previous agentel-commonmark--delimiters))
                agentel-commonmark--delimiters))))
    t))

(defun agentel-commonmark--remove-delimiter (delimiter)
  "Remove DELIMITER from the delimiter stack."
  (let ((previous (agentel-commonmark--d-previous delimiter))
        (next (agentel-commonmark--d-next delimiter)))
    (when previous
      (setf (agentel-commonmark--d-next previous) next))
    (if next
        (setf (agentel-commonmark--d-previous next) previous)
      (setq agentel-commonmark--delimiters previous))))

(defun agentel-commonmark--matching-opener (closer bottom)
  "Return the delimiter run opening what CLOSER closes, above BOTTOM."
  (let ((opener (agentel-commonmark--d-previous closer))
        found)
    (while (and opener (not (eq opener bottom)) (not found))
      (let ((odd (and (or (agentel-commonmark--d-can-open closer)
                          (agentel-commonmark--d-can-close opener))
                      (/= (% (agentel-commonmark--d-original closer) 3) 0)
                      (= (% (+ (agentel-commonmark--d-original opener)
                               (agentel-commonmark--d-original closer))
                            3)
                         0))))
        (if (and (eq (agentel-commonmark--d-char opener) (agentel-commonmark--d-char closer))
                 (agentel-commonmark--d-can-open opener)
                 (if (eq (agentel-commonmark--d-char closer) ?~)
                     (= (agentel-commonmark--d-count opener) (agentel-commonmark--d-count closer))
                   (not odd)))
            (setq found opener)
          (setq opener (agentel-commonmark--d-previous opener)))))
    found))

(defun agentel-commonmark--emphasize (opener closer)
  "Make emphasis of what is between OPENER and CLOSER."
  (let* ((char (agentel-commonmark--d-char closer))
         (use (cond ((eq char ?~) (agentel-commonmark--d-count closer))
                    ((and (>= (agentel-commonmark--d-count closer) 2)
                          (>= (agentel-commonmark--d-count opener) 2))
                     2)
                    (t 1)))
         (opener-node (agentel-commonmark--d-node opener))
         (closer-node (agentel-commonmark--d-node closer)))
    (cl-decf (agentel-commonmark--d-count opener) use)
    (cl-decf (agentel-commonmark--d-count closer) use)
    (setf (agentel-commonmark--n-literal opener-node)
          (substring (agentel-commonmark--n-literal opener-node) use))
    (setf (agentel-commonmark--n-literal closer-node)
          (substring (agentel-commonmark--n-literal closer-node) use))
    (cl-decf (agentel-commonmark--n-end opener-node) use)
    (cl-incf (agentel-commonmark--n-begin closer-node) use)
    (let* ((begin (agentel-commonmark--n-end opener-node))
           (end (agentel-commonmark--n-begin closer-node))
           (node (agentel-commonmark--make-node
                  :type (cond ((eq char ?~) 'strikethrough) ((= use 1) 'emphasis) (t 'strong))
                  :begin begin :end end
                  :inner-begin (+ begin use) :inner-end (- end use)))
           (child (agentel-commonmark--n-next opener-node)))
      (while (and child (not (eq child closer-node)))
        (let ((next (agentel-commonmark--n-next child)))
          (agentel-commonmark--append node child)
          (setq child next)))
      (agentel-commonmark--insert-after opener-node node))
    (unless (eq (agentel-commonmark--d-next opener) closer)
      (setf (agentel-commonmark--d-next opener) closer
            (agentel-commonmark--d-previous closer) opener))
    (when (= (agentel-commonmark--d-count opener) 0)
      (agentel-commonmark--unlink-node opener-node)
      (agentel-commonmark--remove-delimiter opener))
    (if (= (agentel-commonmark--d-count closer) 0)
        (let ((next (agentel-commonmark--d-next closer)))
          (agentel-commonmark--unlink-node closer-node)
          (agentel-commonmark--remove-delimiter closer)
          next)
      closer)))

(defun agentel-commonmark--process-emphasis (bottom)
  "Turn the delimiter runs above BOTTOM into emphasis where they match."
  (let ((openers-bottom (make-hash-table :test #'equal))
        (closer agentel-commonmark--delimiters))
    (while (and closer (not (eq (agentel-commonmark--d-previous closer) bottom)))
      (setq closer (agentel-commonmark--d-previous closer)))
    (while closer
      (if (not (agentel-commonmark--d-can-close closer))
          (setq closer (agentel-commonmark--d-next closer))
        (let* ((key (list (agentel-commonmark--d-char closer)
                          (agentel-commonmark--d-can-open closer)
                          (% (agentel-commonmark--d-original closer) 3)))
               (opener (agentel-commonmark--matching-opener
                        closer (gethash key openers-bottom bottom))))
          (if opener
              (setq closer (agentel-commonmark--emphasize opener closer))
            (puthash key (agentel-commonmark--d-previous closer) openers-bottom)
            (let ((next (agentel-commonmark--d-next closer)))
              (unless (agentel-commonmark--d-can-open closer)
                (agentel-commonmark--remove-delimiter closer))
              (setq closer next))))))
    (while (and agentel-commonmark--delimiters
                (not (eq agentel-commonmark--delimiters bottom)))
      (agentel-commonmark--remove-delimiter agentel-commonmark--delimiters))))

;;;;; Other inlines

(defun agentel-commonmark--parse-newline (parent)
  "Parse a line ending at point into PARENT as a soft or hard break."
  (let ((start (point))
        (last (agentel-commonmark--n-last parent))
        (type 'soft-break)
        (begin (agentel-commonmark--source (point))))
    (forward-char)
    (when (and last (eq (agentel-commonmark--n-type last) 'text)
               (string-suffix-p " " (agentel-commonmark--n-literal last)))
      (let* ((literal (agentel-commonmark--n-literal last))
             (trimmed (string-trim-right literal " +")))
        (when (string-suffix-p "  " literal)
          (setq type 'hard-break))
        (cl-decf (agentel-commonmark--n-end last) (- (length literal) (length trimmed)))
        (setf (agentel-commonmark--n-literal last) trimmed)
        (setq begin (agentel-commonmark--n-end last))))
    (agentel-commonmark--append
     parent (agentel-commonmark--make-node
             :type type :begin begin :end (agentel-commonmark--source (1+ start))))
    (skip-chars-forward " ")
    t))

(defun agentel-commonmark--parse-backslash (parent)
  "Parse a backslash at point into PARENT."
  (let ((start (point)))
    (forward-char)
    (cond ((eq (char-after) ?\n)
           (forward-char)
           (agentel-commonmark--append
            parent (agentel-commonmark--make-node
                    :type 'hard-break :begin (agentel-commonmark--source start)
                    :end (agentel-commonmark--source (point)))))
          ((and (char-after) (string-search (string (char-after)) agentel-commonmark--escapable))
           (forward-char)
           (agentel-commonmark--add-text parent (string (char-before)) start (point)))
          (t (agentel-commonmark--add-text parent "\\" start (point))))
    t))

(defun agentel-commonmark--parse-backticks (parent)
  "Parse a code span, or the backticks at point, into PARENT."
  (let* ((start (point))
         (ticks (progn (skip-chars-forward "`") (- (point) start)))
         (after (point))
         found)
    (while (and (not found) (re-search-forward "`+" nil t))
      (when (= (- (match-end 0) (match-beginning 0)) ticks)
        (setq found t)))
    (if (not found)
        (progn
          (goto-char after)
          (agentel-commonmark--add-text parent (make-string ticks ?`) start after))
      (let ((contents (string-replace "\n" " " (buffer-substring-no-properties
                                                after (match-beginning 0)))))
        (when (and (string-match-p "[^ ]" contents)
                   (string-prefix-p " " contents)
                   (string-suffix-p " " contents))
          (setq contents (substring contents 1 -1)))
        (agentel-commonmark--append
         parent (agentel-commonmark--make-node
                 :type 'code :literal contents
                 :begin (agentel-commonmark--source start)
                 :end (agentel-commonmark--source (point))
                 :inner-begin (agentel-commonmark--source after)
                 :inner-end (agentel-commonmark--source (match-beginning 0))))))
    t))

(defun agentel-commonmark--parse-autolink (parent)
  "Parse an autolink at point into PARENT, returning nil if there is none."
  (let ((start (point))
        destination)
    (cond ((looking-at (rx "<" (group (+ (any "a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-")) "@"
                                      (any "a-zA-Z0-9")
                                      (? (** 0 61 (any "a-zA-Z0-9-")) (any "a-zA-Z0-9"))
                                      (* "." (any "a-zA-Z0-9")
                                         (? (** 0 61 (any "a-zA-Z0-9-")) (any "a-zA-Z0-9"))))
                           ">"))
           (setq destination (concat "mailto:" (match-string 1))))
          ((looking-at (rx "<" (group (any "a-zA-Z") (** 1 31 (any "a-zA-Z0-9.+-")) ":"
                                      (* (not (any "<>" (0 . 32)))))
                           ">"))
           (setq destination (match-string 1))))
    (when destination
      (let* ((text (match-string-no-properties 1))
             (node (agentel-commonmark--make-node
                    :type 'link :destination (agentel-commonmark--normalize-uri destination)
                    :begin (agentel-commonmark--source start)
                    :end (agentel-commonmark--source (match-end 0))
                    :inner-begin (agentel-commonmark--source (1+ start))
                    :inner-end (agentel-commonmark--source (1- (match-end 0))))))
        (goto-char (match-end 0))
        (agentel-commonmark--add-text node text (1+ start) (1- (point)))
        (agentel-commonmark--append parent node)
        t))))

(defun agentel-commonmark--parse-inline (parent)
  "Parse the inline at point into PARENT, returning nil at the end."
  (let ((char (char-after))
        (start (point)))
    (when char
      (unless (pcase char
                (?\n (agentel-commonmark--parse-newline parent))
                (?\\ (agentel-commonmark--parse-backslash parent))
                (?` (agentel-commonmark--parse-backticks parent))
                ((or ?* ?_ ?~) (agentel-commonmark--handle-delimiter parent char))
                (?\[ (forward-char)
                     (agentel-commonmark--add-bracket
                      (agentel-commonmark--add-text parent "[" start (point)) start nil)
                     t)
                (?! (forward-char)
                    (if (eq (char-after) ?\[)
                        (progn
                          (forward-char)
                          (agentel-commonmark--add-bracket
                           (agentel-commonmark--add-text parent "![" start (point)) (1+ start) t))
                      (agentel-commonmark--add-text parent "!" start (point)))
                    t)
                (?\] (agentel-commonmark--parse-close-bracket parent))
                (?< (agentel-commonmark--parse-autolink parent))
                (?& (when (looking-at agentel-commonmark--entity-regexp)
                      (goto-char (match-end 0))
                      (agentel-commonmark--add-text
                       parent (agentel-commonmark--decode-entity (match-string 0)) start (point))
                      t))
                (_ (skip-chars-forward "^\n`[]\\\\!<&*_~")
                   (when (> (point) start)
                     (agentel-commonmark--add-text
                      parent (buffer-substring-no-properties start (point)) start (point)))))
        (forward-char)
        (agentel-commonmark--add-text parent (string char) start (point)))
      t)))

;;;;; Extended autolinks

(defun agentel-commonmark--alnum-p (char)
  "Return non-nil if CHAR is an ASCII letter or digit."
  (and char (or (<= ?a char ?z) (<= ?A char ?Z) (<= ?0 char ?9))))

(defun agentel-commonmark--domain-end (text start short)
  "Return the end of a valid domain at START of TEXT, or nil.
The domain may have no period if SHORT."
  (let ((index start)
        (periods 0)
        (last-underscores 0)
        (underscores 0)
        char)
    (while (and (setq char (agentel-commonmark--peek text index))
                (or (agentel-commonmark--alnum-p char) (memq char '(?- ?_ ?.))
                    (> char 127)))
      (pcase char
        (?_ (cl-incf underscores))
        (?. (setq last-underscores underscores
                  underscores 0)
            (cl-incf periods)))
      (cl-incf index))
    (and (> index start)
         (= last-underscores 0) (= underscores 0)
         (or short (> periods 0))
         index)))

(defun agentel-commonmark--link-end (text start end)
  "Return the end of the extended autolink from START of TEXT, ending by END."
  (let ((index start))
    (while (and (< index (length text))
                (not (agentel-commonmark--whitespace-p (aref text index)))
                (not (eq (aref text index) ?<)))
      (cl-incf index))
    (setq end (max end index))
    (let ((trimming t))
      (while (and trimming (> end start))
        (let ((last (aref text (1- end))))
          (cond ((memq last '(?? ?! ?. ?, ?: ?* ?_ ?~))
                 (cl-decf end))
                ((eq last ?\))
                 (let ((link (substring text start end)))
                   (if (> (cl-count ?\) link) (cl-count ?\( link))
                       (cl-decf end)
                     (setq trimming nil))))
                ((eq last ?\;)
                 (let ((amp (string-match "&[A-Za-z0-9]+;\\'" (substring text start end))))
                   (setq end (if amp (+ start amp) (1- end)))))
                (t (setq trimming nil))))))
    end))

(defun agentel-commonmark--autolink-at (text index)
  "Return (START END DESTINATION) of an extended autolink at INDEX of TEXT."
  (let ((boundary (or (= index 0)
                      (memq (aref text (1- index)) '(?\s ?\t ?\n ?* ?_ ?~ ?\()))))
    (cond
     ((and boundary (string-prefix-p "www." (substring text index)))
      (when-let* ((domain (agentel-commonmark--domain-end text index nil))
                  (end (agentel-commonmark--link-end text index domain)))
        (list index end (concat "http://" (substring text index end)))))
     ((and boundary (string-match "\\`\\(?:https?\\|ftp\\)://" (substring text index)))
      (let ((after (+ index (match-end 0))))
        (when-let* ((domain (agentel-commonmark--domain-end text after t)))
          (let ((end (agentel-commonmark--link-end text index domain)))
            (list index end (substring text index end))))))
     ((eq (aref text index) ?@)
      (let ((start index)
            (end (1+ index))
            (periods 0)
            (scanning t)
            char)
        (while (and (> start 0)
                    (or (agentel-commonmark--alnum-p (aref text (1- start)))
                        (memq (aref text (1- start)) '(?. ?+ ?- ?_))))
          (cl-decf start))
        (while (and scanning (setq char (agentel-commonmark--peek text end)))
          (cond ((agentel-commonmark--alnum-p char) (cl-incf end))
                ((and (eq char ?.)
                      (agentel-commonmark--alnum-p (agentel-commonmark--peek text (1+ end))))
                 (cl-incf periods)
                 (cl-incf end))
                ((memq char '(?- ?_)) (cl-incf end))
                (t (setq scanning nil))))
        (and (< start index) (> periods 0)
             (let ((last (aref text (1- end))))
               (or (<= ?a last ?z) (<= ?A last ?Z)))
             (list start end (concat "mailto:" (substring text start end)))))))))

(defun agentel-commonmark--autolinks (text)
  "Return the extended autolinks of TEXT as (START END DESTINATION)."
  (let ((index 0)
        (done 0)
        links)
    (while (< index (length text))
      (let ((link (agentel-commonmark--autolink-at text index)))
        (if (and link (>= (car link) done) (> (nth 1 link) (car link)))
            (progn
              (push link links)
              (setq done (nth 1 link)
                    index done))
          (cl-incf index))))
    (nreverse links)))

(defun agentel-commonmark--run-position (pieces index)
  "Return the position in the Markdown of INDEX of the text of PIECES.
PIECES are the (START . NODE) of the text nodes making up the text."
  (let* ((piece (cl-find-if (lambda (piece) (<= (car piece) index)) (reverse pieces)))
         (node (cdr piece))
         (offset (- index (car piece)))
         (literal (agentel-commonmark--n-literal node)))
    (cond ((= (length literal) (- (agentel-commonmark--n-end node)
                                  (agentel-commonmark--n-begin node)))
           (+ (agentel-commonmark--n-begin node) offset))
          ((< offset (length literal)) (agentel-commonmark--n-begin node))
          (t (agentel-commonmark--n-end node)))))

(defun agentel-commonmark--link-run (run)
  "Turn the extended autolinks in the text nodes of RUN into links."
  (let* ((pieces (let ((start 0))
                   (mapcar (lambda (node)
                             (prog1 (cons start node)
                               (cl-incf start (length (agentel-commonmark--n-literal node)))))
                           run)))
         (text (mapconcat #'agentel-commonmark--n-literal run ""))
         (links (agentel-commonmark--autolinks text)))
    (when links
      (let ((anchor (car run))
            (done 0)
            new)
        (cl-flet ((text-node (start end)
                    (agentel-commonmark--make-node
                     :type 'text :literal (substring text start end)
                     :begin (agentel-commonmark--run-position pieces start)
                     :end (agentel-commonmark--run-position pieces end))))
          (dolist (link links)
            (pcase-let ((`(,start ,end ,destination) link))
              (when (> start done)
                (push (text-node done start) new))
              (let ((node (agentel-commonmark--make-node
                           :type 'link :destination (agentel-commonmark--normalize-uri destination)
                           :begin (agentel-commonmark--run-position pieces start)
                           :end (agentel-commonmark--run-position pieces end))))
                (setf (agentel-commonmark--n-inner-begin node) (agentel-commonmark--n-begin node)
                      (agentel-commonmark--n-inner-end node) (agentel-commonmark--n-end node))
                (agentel-commonmark--append node (text-node start end))
                (push node new))
              (setq done end)))
          (when (< done (length text))
            (push (text-node done (length text)) new)))
        (dolist (node (nreverse new))
          (agentel-commonmark--insert-after anchor node)
          (setq anchor node))
        (mapc #'agentel-commonmark--unlink-node run)))))

(defun agentel-commonmark--extend-autolinks (parent)
  "Turn the extended autolinks in the text under PARENT into links."
  (let ((child (agentel-commonmark--n-first parent)))
    (while child
      (let ((next (agentel-commonmark--n-next child)))
        (pcase (agentel-commonmark--n-type child)
          ((or 'link 'image 'code) nil)
          ('text
           (let ((run (list child)))
             (while (and next (eq (agentel-commonmark--n-type next) 'text))
               (push next run)
               (setq next (agentel-commonmark--n-next next)))
             (agentel-commonmark--link-run (nreverse run))))
          (_ (agentel-commonmark--extend-autolinks child)))
        (setq child next)))))

;;;;; Trees of inlines

(defun agentel-commonmark--positions (node)
  "Return the attributes of the positions of NODE."
  (append (list :begin (agentel-commonmark--n-begin node)
                :end (agentel-commonmark--n-end node))
          (when (agentel-commonmark--n-inner-begin node)
            (list :inner-begin (agentel-commonmark--n-inner-begin node)
                  :inner-end (agentel-commonmark--n-inner-end node)))))

(defun agentel-commonmark--inline-children (parent)
  "Return the trees of the children of PARENT, with their text merged."
  (let ((child (agentel-commonmark--n-first parent))
        text trees)
    (while child
      (pcase (agentel-commonmark--n-type child)
        ('text (setq text (concat text (agentel-commonmark--n-literal child))))
        ('soft-break (setq text (concat text "\n")))
        (_ (when (> (length text) 0) (push text trees))
           (setq text nil)
           (push (agentel-commonmark--inline-tree child) trees)))
      (setq child (agentel-commonmark--n-next child)))
    (when (> (length text) 0) (push text trees))
    (nreverse trees)))

(defun agentel-commonmark--inline-tree (node)
  "Return the tree of the inline NODE."
  (let ((type (agentel-commonmark--n-type node)))
    (pcase type
      ('code (list 'code (agentel-commonmark--positions node) (agentel-commonmark--n-literal node)))
      ((or 'link 'image)
       `(,type (:destination ,(agentel-commonmark--n-destination node)
                :title ,(agentel-commonmark--n-title node)
                ,@(agentel-commonmark--positions node))
               ,@(agentel-commonmark--inline-children node)))
      (_ `(,type ,(agentel-commonmark--positions node)
                 ,@(agentel-commonmark--inline-children node))))))

(defun agentel-commonmark--parse-inlines (block)
  "Return the trees of the inlines of BLOCK."
  (let* ((content (agentel-commonmark--b-content block))
         (start (progn (string-match "\\`[ \t\n\r]*" content) (match-end 0)))
         (subject (string-trim content "[ \t\n\r]+" "[ \t\n\r]+"))
         (root (agentel-commonmark--make-node :type 'root))
         (agentel-commonmark--block block)
         (agentel-commonmark--shift start)
         (agentel-commonmark--delimiters nil)
         (agentel-commonmark--brackets nil))
    (with-temp-buffer
      (insert subject)
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (agentel-commonmark--parse-inline root)))
      (agentel-commonmark--process-emphasis nil))
    (agentel-commonmark--extend-autolinks root)
    (agentel-commonmark--inline-children root)))

;;;; Trees

(defun agentel-commonmark--take-task (item)
  "Take the task list item marker of ITEM, if it has one."
  (let ((paragraph (car (agentel-commonmark--b-children item))))
    (when (and paragraph (eq (agentel-commonmark--b-type paragraph) 'paragraph)
               (string-match "\\`\\[\\([ xX]\\)\\][ \t\n]" (agentel-commonmark--b-content paragraph)))
      (let ((checked (not (equal (match-string 1 (agentel-commonmark--b-content paragraph)) " "))))
        (agentel-commonmark--drop-content paragraph 3)
        (setf (agentel-commonmark--b-data item)
              (plist-put (agentel-commonmark--b-data item) :task (if checked 'checked 'unchecked)))))))

(defun agentel-commonmark--tree (block)
  "Return the tree of BLOCK."
  (let ((positions (list :begin (agentel-commonmark--b-begin block)
                         :end (agentel-commonmark--b-end block)))
        (data (agentel-commonmark--b-data block)))
    (when (eq (agentel-commonmark--b-type block) 'item)
      (agentel-commonmark--take-task block)
      (setq data (agentel-commonmark--b-data block)))
    (cl-flet ((children () (mapcar #'agentel-commonmark--tree
                                   (agentel-commonmark--b-children block)))
              (inlines () (agentel-commonmark--parse-inlines block)))
      (pcase (agentel-commonmark--b-type block)
        ('document `(document ,positions ,@(children)))
        ('paragraph `(paragraph ,positions ,@(inlines)))
        ('heading `(heading (:level ,(plist-get data :level) ,@positions) ,@(inlines)))
        ('thematic-break `(thematic-break ,positions))
        ('block-quote `(block-quote ,positions ,@(children)))
        ('list `(list ,(if (eq (plist-get data :type) 'ordered)
                           `(:ordered t :start ,(plist-get data :start) ,@positions)
                         `(:ordered nil ,@positions))
                      ,@(children)))
        ('item `(item ,(if (plist-get data :task)
                           `(:checked ,(eq (plist-get data :task) 'checked) ,@positions)
                         positions)
                      ,@(children)))
        ('code-block
         (let ((code (plist-get data :code)))
           `(code-block (:language ,(plist-get data :language) ,@positions
                         :inner-begin ,(plist-get data :inner-begin)
                         :inner-end ,(plist-get data :inner-end))
                        ,@(and (> (length code) 0) (list code)))))
        ('table `(table ,positions ,@(children)))
        ('table-row `(table-row (:header ,(plist-get data :header) ,@positions) ,@(children)))
        ('table-cell `(table-cell (:align ,(plist-get data :align) ,@positions) ,@(inlines)))))))

(defun agentel-commonmark-parse (text)
  "Return the tree of the Markdown TEXT."
  (let* ((agentel-commonmark--text (string-replace "\0" "\ufffd" text))
         (document (agentel-commonmark--make-block :type 'document :begin 0))
         (agentel-commonmark--tip document)
         (agentel-commonmark--oldtip document)
         (agentel-commonmark--all-closed t)
         (agentel-commonmark--last-matched-container document)
         (agentel-commonmark--previous-line-end 0)
         (agentel-commonmark--refmap (make-hash-table :test #'equal))
         (length (length agentel-commonmark--text))
         (start 0)
         (case-fold-search nil))
    (while (and (< start length)
                (string-match "\r\n\\|\n\\|\r" agentel-commonmark--text start))
      (let ((end (match-beginning 0))
            (next (match-end 0)))
        (agentel-commonmark--incorporate-line document start end)
        (setq start next)))
    (when (< start length)
      (agentel-commonmark--incorporate-line document start length))
    (while agentel-commonmark--tip
      (agentel-commonmark--finalize agentel-commonmark--tip length))
    (agentel-commonmark--tree document)))

(provide 'agentel-commonmark)
;;; agentel-commonmark.el ends here
