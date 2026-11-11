;;; agentel-background-test.el --- Tests for agentel-background  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-background)
(require 'agentel-test-helper)

(defmacro agentel-background-test-with-session (&rest body)
  "Run BODY with a registered session of an ACP v2 agent bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil))
     (let ((session (agentel-session-create
                     :connection (agentel-connection--make
                                  :info '((protocolVersion . 2))))))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-background-test-update (session &rest update)
  "Tell SESSION the `session/update' UPDATE, an alist."
  (agentel-session-dispatch `((sessionId . "s1") (update . ,update)))
  session)

(defun agentel-background-test-state (session &rest ids)
  "Tell SESSION that the tool calls IDS run in the background."
  (agentel-background-test-update
   session '(sessionUpdate . "state_update") '(state . "idle")
   `(background . ,(vconcat (mapcar (lambda (id) `((type . "tool_call") (toolCallId . ,id)))
                                    ids)))))

(defun agentel-background-test-titles (session)
  "Return the titles of the background work of SESSION."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'background)))
    (mapcar (lambda (item) (alist-get 'title (cdr item)))
            (agentel-store-get model 'items))))

(ert-deftest agentel-background-starts-with-the-title-the-tool-call-had ()
  (agentel-background-test-with-session
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1")
     '(name . "Workflow") '(title . "Workflow") '(status . "pending"))
    (agentel-background-test-state session "w1")
    (should (equal (agentel-background-test-titles session) '("Workflow")))))

(ert-deftest agentel-background-follows-the-title-of-its-tool-call ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1")
     '(title . "Workflow: research: sleep-0"))
    (should (equal (agentel-background-test-titles session)
                   '("Workflow: research: sleep-0")))))

(ert-deftest agentel-background-ignores-tool-calls-outside-the-background ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "t2") '(title . "Read"))
    (should (equal (agentel-background-test-titles session) '(nil)))))

(ert-deftest agentel-background-forgets-work-that-left-the-list ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-state session)
    (should-not (agentel-background-test-titles session))))

(ert-deftest agentel-background-keeps-the-list-when-the-agent-does-not-report-it ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "state_update") '(state . "running"))
    (should (equal (length (agentel-background-test-titles session)) 1))))

(ert-deftest agentel-background-ignores-work-of-unknown-types ()
  (agentel-background-test-with-session
    (agentel-background-test-update
     session '(sessionUpdate . "state_update") '(state . "idle")
     '(background . [((type . "subagent") (sessionId . "c1"))]))
    (should-not (agentel-background-test-titles session))))

(defun agentel-background-test-report (session id text)
  "Tell SESSION that the tool call ID reports TEXT."
  (agentel-background-test-update
   session '(sessionUpdate . "tool_call_update") `(toolCallId . ,id)
   `(content . [((type . "content") (content . ((type . "text") (text . ,text))))])))

(defmacro agentel-background-test-with-open (&rest body)
  "Run BODY in the buffer of the background work w1 of `session'."
  (declare (indent 0))
  `(let ((buffer (agentel-background-open session "w1")))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (kill-buffer buffer))))

(ert-deftest agentel-background-shows-the-report-of-the-work ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-report session "w1" "sleep-0: running")
    (agentel-background-test-with-open
      (should (equal (buffer-string) "sleep-0: running")))))

(ert-deftest agentel-background-shows-the-report-the-tool-call-had ()
  (agentel-background-test-with-session
    (agentel-background-test-report session "w1" "launched")
    (agentel-background-test-state session "w1")
    (agentel-background-test-with-open
      (should (equal (buffer-string) "launched")))))

(ert-deftest agentel-background-shows-each-new-report-in-place-of-the-last ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-report session "w1" "sleep-0: running")
    (agentel-background-test-with-open
      (agentel-background-test-report session "w1" "sleep-0: done")
      (should (equal (buffer-string) "sleep-0: done")))))

(ert-deftest agentel-background-keeps-the-last-report-once-the-work-ends ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-report session "w1" "sleep-0: done")
    (agentel-background-test-with-open
      (agentel-background-test-state session)
      (should (equal (buffer-string) "sleep-0: done")))))

(ert-deftest agentel-background-opens-the-same-buffer-again ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-with-open
      (should (eq (agentel-background-open session "w1") buffer)))))

(ert-deftest agentel-background-pinned-line-opens-the-work ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (let* ((model (agentel-store-find (agentel-session-store session) 'background))
           (line (agentel-ui-view model :width 80))
           opened)
      (cl-letf (((symbol-function 'agentel-background-open)
                 (lambda (s id) (setq opened (list s id))))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (funcall (keymap-lookup (get-text-property 0 'keymap line) "<mouse-1>")))
      (should (equal opened (list session "w1"))))))

(ert-deftest agentel-background-open-asks-for-the-work-of-the-session ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1") '(title . "Workflow"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'agentel-session-current) (lambda () session))
                ((symbol-function 'completing-read) (lambda (&rest _) "Workflow"))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (let ((buffer (call-interactively #'agentel-background-open)))
          (unwind-protect
              (should (buffer-live-p buffer))
            (kill-buffer buffer)))))))

(ert-deftest agentel-background-open-refuses-without-work ()
  (agentel-background-test-with-session
    (cl-letf (((symbol-function 'agentel-session-current) (lambda () session)))
      (should-error (call-interactively #'agentel-background-open) :type 'user-error))))

(ert-deftest agentel-background-lists-its-work-below-the-session ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1") '(title . "Workflow"))
    (let ((work (agentel-background--list-work session)))
      (should (equal (mapcar (lambda (w) (plist-get w :title)) work) '("Workflow")))
      (let (opened)
        (cl-letf (((symbol-function 'agentel-background-open)
                   (lambda (s id) (setq opened (list s id))))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (funcall (plist-get (car work) :open)))
        (should (equal opened (list session "w1")))))))

(ert-deftest agentel-background-tells-the-session-changed-with-its-titles ()
  (agentel-background-test-with-session
    (let (changed)
      (add-hook 'agentel-session-changed-functions (lambda (_s) (setq changed t)))
      (agentel-background-test-state session "w1")
      (should changed)
      (setq changed nil)
      (agentel-background-test-update
       session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1") '(title . "Workflow"))
      (should changed))))

(ert-deftest agentel-background-v2-workflow-is-pinned-until-it-ends ()
  (let ((agentel-acp-v2-agents '(mock)))
    (agentel-test-with-started session nil
      (agentel-test-send "workflow please")
      (agentel-test-wait-until
       (lambda () (string-match-p "Workflow: research: sleep-0"
                                  (or (overlay-get agentel-chat--pin 'before-string) ""))))
      (should (eq (agentel-session-state session) 'idle))
      (agentel-test-send "hello")
      (agentel-test-wait-for-text "Echo: hello")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (overlay-get agentel-chat--pin 'before-string)))))

(provide 'agentel-background-test)
;;; agentel-background-test.el ends here
