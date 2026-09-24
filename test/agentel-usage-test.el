;;; agentel-usage-test.el --- Tests for agentel-usage  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-usage)

(defmacro agentel-usage-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-usage--on-update)))
     (let ((session (agentel-session-create)))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-usage-test-update (update)
  "Dispatch the usage UPDATE to session s1."
  (agentel-session-dispatch
   `((sessionId . "s1") (update . ((sessionUpdate . "usage_update") ,@update)))))

(ert-deftest agentel-usage-shows-context-and-cost ()
  (agentel-usage-test-with-session
    (agentel-usage-test-update '((used . 24500) (size . 200000)
                                 (cost . ((amount . 0.1234) (currency . "USD")))))
    (should (equal (agentel-usage-summary session) "ctx 12% (24.5k/200k) · $0.12"))))

(ert-deftest agentel-usage-keeps-the-cost-when-an-update-has-none ()
  (agentel-usage-test-with-session
    (agentel-usage-test-update '((used . 1000) (size . 200000)
                                 (cost . ((amount . 0.5) (currency . "USD")))))
    (agentel-usage-test-update '((used . 190000) (size . 200000)))
    (should (equal (agentel-usage-summary session) "ctx 95% (190k/200k) · $0.50"))))

(ert-deftest agentel-usage-shows-other-currencies-by-code ()
  (agentel-usage-test-with-session
    (agentel-usage-test-update '((used . 0) (size . 1000000)
                                 (cost . ((amount . 3) (currency . "EUR")))))
    (should (equal (agentel-usage-summary session) "ctx 0% (0/1M) · 3.00 EUR"))))

(ert-deftest agentel-usage-is-empty-before-any-report ()
  (agentel-usage-test-with-session
    (should-not (agentel-usage-summary session))))

(ert-deftest agentel-usage-warns-when-the-context-is-almost-full ()
  (agentel-usage-test-with-session
    (agentel-usage-test-update '((used . 180000) (size . 200000)))
    (should (eq (get-text-property 4 'face (agentel-usage-summary session))
                'agentel-usage-full-face))))

(provide 'agentel-usage-test)
;;; agentel-usage-test.el ends here
