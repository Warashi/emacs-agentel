;;; agentel-connection-test.el --- Tests for agentel-connection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-connection)
(require 'agentel-test-helper)

(defmacro agentel-connection-test-with-mock (var &rest body)
  "Start the mock agent, bind the ready connection to VAR and run BODY."
  (declare (indent 1))
  `(let ((agentel-session--registry nil)
         (agentel-session-update-functions nil)
         (agentel-session-changed-functions nil)
         (ready nil))
     (let* ((command (agentel-test-mock-command))
            (,var (agentel-connection-start
                   :command (car command) :args (cdr command)
                   :cwd temporary-file-directory
                   :on-ready (lambda (_c) (setq ready t)))))
       (unwind-protect
           (progn
             (agentel-test-wait-until (lambda () ready))
             ,@body)
         (agentel-connection-shutdown ,var)))))

(ert-deftest agentel-connection-advertises-feature-capabilities ()
  (let ((agentel-connection-capability-functions
         (list (lambda () '((subagents . nil))))))
    (agentel-connection-test-with-mock conn
      (let ((received (alist-get 'receivedCapabilities
                                 (alist-get '_meta (agentel-connection-info conn)))))
        (should (assq 'subagents received))
        (should-not (assq 'fs received))
        (should-not (assq 'terminal received))))))

(ert-deftest agentel-connection-request-calls-back-with-result ()
  (agentel-connection-test-with-mock conn
    (let (result)
      (agentel-connection-request
       conn "session/new" `((cwd . ,temporary-file-directory) (mcpServers . []))
       :on-success (lambda (r) (setq result r)))
      (should (string-prefix-p
               "mock-"
               (alist-get 'sessionId (agentel-test-wait-until (lambda () result))))))))

(ert-deftest agentel-connection-request-calls-back-with-error ()
  (agentel-connection-test-with-mock conn
    (let (failure)
      (agentel-connection-request
       conn "no/such_method" nil
       :on-failure (lambda (e) (setq failure e)))
      (should (equal (alist-get 'code (agentel-test-wait-until (lambda () failure)))
                     -32601)))))

(ert-deftest agentel-connection-routes-updates-to-sessions ()
  (agentel-connection-test-with-mock conn
    (let (session updates)
      (add-hook 'agentel-session-update-functions
                (lambda (_s u) (push (alist-get 'sessionUpdate u) updates)))
      (agentel-connection-request
       conn "session/new" `((cwd . ,temporary-file-directory) (mcpServers . []))
       :on-success
       (lambda (r)
         (setq session (agentel-session-create :connection conn))
         (agentel-session-register session (alist-get 'sessionId r))))
      (agentel-test-wait-until
       (lambda () (member "available_commands_update" updates))))))

(ert-deftest agentel-connection-dispatches-requests-to-handlers ()
  (let* (asked
         (agentel-connection-request-handlers
          (list (cons "session/request_permission"
                      (lambda (conn id params)
                        (setq asked (alist-get 'options params))
                        (agentel-connection-respond
                         conn id '((outcome . ((outcome . "selected")
                                               (optionId . "reject"))))))))))
    (agentel-connection-test-with-mock conn
      (let (session result)
        (agentel-connection-request
         conn "session/new" `((cwd . ,temporary-file-directory) (mcpServers . []))
         :on-success
         (lambda (r)
           (setq session (agentel-session-create :connection conn))
           (agentel-session-register session (alist-get 'sessionId r))
           (agentel-connection-request
            conn "session/prompt"
            `((sessionId . ,(alist-get 'sessionId r))
              (prompt . [((type . "text") (text . "permission"))]))
            :on-success (lambda (r) (setq result r)))))
        (agentel-test-wait-until (lambda () result))
        (should (= (length asked) 3))))))

(ert-deftest agentel-connection-answers-unknown-requests-with-error ()
  (let ((agentel-connection-request-handlers nil))
    (agentel-connection-test-with-mock conn
      (let (result)
        (agentel-connection-request
         conn "session/new" `((cwd . ,temporary-file-directory) (mcpServers . []))
         :on-success
         (lambda (r)
           (agentel-connection-request
            conn "session/prompt"
            `((sessionId . ,(alist-get 'sessionId r))
              (prompt . [((type . "text") (text . "permission"))]))
            :on-success (lambda (r) (setq result r)))))
        ;; The mock treats the error response as a missing outcome.
        (should (equal (alist-get 'stopReason
                                  (agentel-test-wait-until (lambda () result)))
                       "end_turn"))))))

(ert-deftest agentel-connection-passes-other-notifications-to-features ()
  (let* ((agentel-connection-request-handlers
          (list (cons "session/request_permission" #'ignore)))
         seen
         (agentel-connection-notification-functions
          (list (lambda (_conn method params) (push (cons method params) seen)))))
    (agentel-connection-test-with-mock conn
      (let (session-id)
        (agentel-connection-request
         conn "session/new" `((cwd . ,temporary-file-directory) (mcpServers . []))
         :on-success
         (lambda (r)
           (setq session-id (alist-get 'sessionId r))
           (agentel-connection-request
            conn "session/prompt"
            `((sessionId . ,session-id)
              (prompt . [((type . "text") (text . "permission"))])))))
        (agentel-test-wait-until (lambda () session-id))
        (sleep-for 0.5)
        (agentel-connection-notify conn "session/cancel" `((sessionId . ,session-id)))
        (should (alist-get 'requestId
                           (cdr (agentel-test-wait-until
                                 (lambda () (assoc "$/cancel_request" seen))))))))))

(ert-deftest agentel-connection-exit-ends-its-sessions ()
  (agentel-connection-test-with-mock conn
    (let ((session (agentel-session-create :connection conn)))
      (agentel-session-register session "s1")
      (delete-process (agentel-connection-process conn))
      (agentel-test-wait-until
       (lambda () (eq (agentel-session-state session) 'exited))))))

(provide 'agentel-connection-test)
;;; agentel-connection-test.el ends here
