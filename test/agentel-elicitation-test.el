;;; agentel-elicitation-test.el --- Tests for agentel-elicitation  -*- lexical-binding: t; -*-

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

(ert-deftest agentel-elicitation-is-withdrawn-when-the-turn-is-cancelled ()
  (agentel-test-with-started session nil
    (agentel-elicitation-test-ask)
    (agentel-chat-cancel)
    (agentel-test-wait-until (lambda () (not (eq (agentel-session-state session) 'waiting))))
    (agentel-test-wait-for-text "→ withdrawn")))

(provide 'agentel-elicitation-test)
;;; agentel-elicitation-test.el ends here
