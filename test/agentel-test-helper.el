;;; agentel-test-helper.el --- Shared helpers for agentel tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst agentel-test-mock-agent
  (expand-file-name "agentel-mock-agent.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path of the scripted ACP agent used by the tests.")

(defun agentel-test-mock-command ()
  "Return the command line that runs the mock agent."
  (list (expand-file-name invocation-name invocation-directory)
        "--batch" "-Q" "-l" agentel-test-mock-agent))

(defun agentel-test-wait-until (predicate &optional timeout)
  "Process output until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return the value of PREDICATE, failing the test on timeout."
  (let ((deadline (+ (float-time) (or timeout 10)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (unless value
      (ert-fail "Timed out waiting for condition"))
    value))

(provide 'agentel-test-helper)
;;; agentel-test-helper.el ends here
