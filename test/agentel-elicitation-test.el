;;; agentel-elicitation-test.el --- Tests for agentel-elicitation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-elicitation)
(require 'agentel-test-helper)

(defun agentel-elicitation-test-ask ()
  "Send a prompt that makes the mock agent ask questions."
  (agentel-test-send "ask me")
  (agentel-test-wait-until
   (lambda () (eq (agentel-session-state agentel-chat--session) 'waiting))))

(defmacro agentel-elicitation-test-answering (single multiple &rest body)
  "Run BODY answering choices with SINGLE and multiple choices with MULTIPLE."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) ,single))
             ((symbol-function 'completing-read-multiple)
              (lambda (&rest _) ,multiple)))
     ,@body))

(ert-deftest agentel-elicitation-is-advertised ()
  (should (equal (agentel-elicitation--capabilities)
                 '((elicitation . ((form . nil)))))))

(ert-deftest agentel-elicitation-shows-the-questions ()
  (agentel-test-with-started _session nil
    (agentel-elicitation-test-ask)
    (agentel-test-wait-for-text "Which color do you prefer\\?")
    (agentel-test-wait-for-text "Red — Warm")
    (agentel-test-wait-for-text "\\[Answer\\] \\[Decline\\]")
    (should-not (save-excursion (goto-char (point-min))
                                (search-forward "Other" nil t)))))

(ert-deftest agentel-elicitation-sends-chosen-options ()
  (agentel-test-with-started _session nil
    (agentel-elicitation-test-ask)
    (agentel-elicitation-test-answering "Red" '("Apple" "Cherry")
      (agentel-chat-answer))
    (agentel-test-wait-for-text
     (regexp-quote "{\"action\":\"accept\",\"content\":{\"question_0\":\"Red\",\"question_1\":[\"Apple\",\"Cherry\"]}}"))
    (agentel-test-wait-for-text "→ Color: Red; Fruits: Apple, Cherry")))

(ert-deftest agentel-elicitation-sends-free-text-as-the-custom-answer ()
  (agentel-test-with-started _session nil
    (agentel-elicitation-test-ask)
    (agentel-elicitation-test-answering "Green" nil
      (agentel-chat-answer))
    (agentel-test-wait-for-text
     (regexp-quote "{\"action\":\"accept\",\"content\":{\"question_0_custom\":\"Green\"}}"))))

(ert-deftest agentel-elicitation-decline-button ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (goto-char (point-min))
    (search-forward "[Decline]")
    (push-button (1- (point)))
    (agentel-test-wait-for-text (regexp-quote "{\"action\":\"decline\"}"))
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))))

(ert-deftest agentel-elicitation-item-holds-the-question-and-its-fields ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'elicitation))
                          (agentel-conversation-items session))))
      (should (equal (agentel-store-model-data item)
                     '((question . "Please answer the following questions.")
                       (fields
                        ((key . question_0) (label . "Color")
                         (description . "Which color do you prefer?") (kind . choice)
                         (options ((label . "Red") (value . "Red") (description . "Warm"))
                                  ((label . "Blue") (value . "Blue") (description . "Cool"))))
                        ((key . question_1) (label . "Fruits")
                         (description . "Which fruits do you like?") (kind . choice)
                         (multiple . t)
                         (options ((label . "Apple") (value . "Apple"))
                                  ((label . "Banana") (value . "Banana"))
                                  ((label . "Cherry") (value . "Cherry")))))
                       (waiting . t)))))))

(ert-deftest agentel-elicitation-item-waits-until-it-is-answered ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'elicitation))
                          (agentel-conversation-items session))))
      (should (agentel-store-get item 'waiting))
      (agentel-chat-cancel)
      (agentel-test-wait-until (lambda () (not (agentel-store-get item 'waiting)))))))

(ert-deftest agentel-elicitation-is-forgotten-with-its-session ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'elicitation))
                          (agentel-conversation-items session))))
      (agentel-session-remove session)
      (should-not (gethash item agentel-elicitation--requests)))))

(ert-deftest agentel-elicitation-is-withdrawn-when-the-agent-exits ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'elicitation))
                          (agentel-conversation-items session))))
      (delete-process (agentel-connection-process (agentel-session-connection session)))
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'exited)))
      (agentel-test-wait-for-text "→ withdrawn")
      (should-not (agentel-session-waiting-p session))
      (cl-letf (((symbol-function 'agentel-connection-respond)
                 (lambda (&rest _) (ert-fail "Answered over an ended connection"))))
        (agentel-elicitation-decline item)))))

(ert-deftest agentel-elicitation-is-cancelled-when-its-session-ends-while-the-agent-runs ()
  (agentel-test-with-started session nil
    (let* ((connection (agentel-session-connection session))
           (child (agentel-session-create :connection connection :parent session))
           responses)
      (agentel-session-register child "child")
      (cl-letf (((symbol-function 'agentel-connection-respond)
                 (lambda (_connection id result) (push (cons id result) responses))))
        (agentel-elicitation--handle
         connection 99 '((sessionId . "child") (mode . "form") (message . "Name?")
                         (requestedSchema
                          (properties (name (type . "string") (title . "Name"))))))
        (agentel-session-send child '(end completed)))
      (should (equal responses '((99 (action . "cancel")))))
      (let ((item (seq-find (lambda (item) (eq (agentel-store-model-type item) 'elicitation))
                            (agentel-conversation-items child))))
        (should (agentel-store-get item 'withdrawn))
        (should-not (agentel-store-get item 'waiting))))))

(ert-deftest agentel-elicitation-is-withdrawn-when-the-turn-is-cancelled ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (agentel-chat-cancel)
    (agentel-test-wait-until (lambda () (not (eq (agentel-session-state session) 'waiting))))
    (agentel-test-wait-for-text "→ withdrawn")))

(defun agentel-elicitation-test-view (&rest messages)
  "Return the text of a form after MESSAGES."
  (let ((data nil))
    (dolist (message messages)
      (setq data (agentel-elicitation--update message data)))
    (substring-no-properties
     (agentel-ui-view (agentel-store-model--make :type 'elicitation :data data)))))

(defconst agentel-elicitation-test-ask
  '(ask "Pick one"
        (((key . color) (label . "Color") (kind . choice)
          (options ((label . "Red") (value . "r"))))
         ((key . fruits) (label . "Fruits") (kind . choice) (multiple . t)
          (options ((label . "Apple") (value . "a")) ((label . "Cherry") (value . "c"))))))
  "The message asking for a color.")

(ert-deftest agentel-elicitation-view-shows-what-it-was-asked ()
  (should (equal (agentel-elicitation-test-view agentel-elicitation-test-ask)
                 (concat "? Pick one\n  Color\n    • Red"
                         "\n  Fruits (any number)\n    • Apple\n    • Cherry"
                         "\n  [Answer] [Decline]"))))

(ert-deftest agentel-elicitation-view-shows-the-answers ()
  (should (equal (agentel-elicitation-test-view agentel-elicitation-test-ask
                                                '(answer ((color . "r") (fruits "a" "c"))
                                                         ((fruits . "Durian"))))
                 "? Pick one → Color: r; Fruits: a, c; Fruits: Durian")))

(ert-deftest agentel-elicitation-view-shows-why-it-was-not-answered ()
  (should (equal (agentel-elicitation-test-view agentel-elicitation-test-ask
                                                '(decline))
                 "? Pick one → declined"))
  (should (equal (agentel-elicitation-test-view agentel-elicitation-test-ask
                                                '(withdraw))
                 "? Pick one → withdrawn")))

(provide 'agentel-elicitation-test)
;;; agentel-elicitation-test.el ends here
