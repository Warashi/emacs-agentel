;;; agentel-markdown.el --- Markdown in agent messages for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; Agents answer in Markdown.  Once a message is complete, its headings,
;; emphasis, inline code, links and fenced code blocks are highlighted;
;; code blocks are fontified with the major mode of their language.
;; The markup itself stays in the text, so copying a message gives back
;; what the agent wrote.

;;; Code:

(require 'subr-x)
(require 'button)
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

(defun agentel-markdown--code-blocks ()
  "Highlight the fenced code blocks of the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*```\\([^`\n]*\\)\n" nil t)
    (let ((language (string-trim (match-string 1)))
          (fence (match-beginning 0))
          (start (point)))
      (if (re-search-forward "^[ \t]*```[ \t]*$" nil t)
          ;; Fontifying the code runs a major mode, which clobbers the match data.
          (let ((end (match-beginning 0))
                (close (match-end 0)))
            (agentel-markdown--fontify-code start end language)
            (add-face-text-property fence start 'agentel-markdown-markup-face)
            (add-face-text-property end close 'agentel-markdown-markup-face)
            (put-text-property fence close 'agentel-markdown-code t)
            (goto-char close))
        (goto-char (point-max))))))

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

(defun agentel-markdown-format (text)
  "Return TEXT with its Markdown highlighted."
  (with-temp-buffer
    (insert text)
    (agentel-markdown--code-blocks)
    (agentel-markdown--inline)
    (remove-text-properties (point-min) (point-max) '(agentel-markdown-code nil))
    (buffer-string)))

(setq agentel-chat-format-message-function #'agentel-markdown-format)

(provide 'agentel-markdown)
;;; agentel-markdown.el ends here
