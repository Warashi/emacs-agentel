;;; agentel-resume-test.el --- Tests for agentel-resume  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-resume)
(require 'agentel-test-helper)

(ert-deftest agentel-resume-describes-a-past-session ()
  ;; The time is shown in the local time zone.
  (should (string-match-p
           "\\`2026-09-2[45] [0-9][0-9]:[0-9][0-9]  Write the parser\\'"
           (agentel-resume--describe
            '((sessionId . "old-1") (title . "Write the parser")
              (updatedAt . "2026-09-24T10:00:00Z"))))))

(ert-deftest agentel-resume-opens-the-chosen-session ()
  (agentel-test-with-started session nil
    (let (offered resumed)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered (all-completions "" collection))
                   (seq-find (lambda (c) (string-match-p "Write the parser" c))
                             offered))))
        (agentel-commands--run session "/resume")
        (setq resumed (agentel-test-wait-until
                       (lambda () (agentel-session-get "old-1")))))
      (unwind-protect
          (progn
            (should (= (length offered) 2))
            (should (equal (agentel-session-name resumed) "Write the parser"))
            (with-current-buffer (agentel-session-buffer resumed)
              (agentel-test-wait-for-text "The plan was to write tests first.")))
        (kill-buffer (agentel-session-buffer resumed))))))

(provide 'agentel-resume-test)
;;; agentel-resume-test.el ends here
