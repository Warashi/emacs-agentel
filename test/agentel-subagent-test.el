;;; agentel-subagent-test.el --- Tests for agentel-subagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-subagent)
(require 'agentel-test-helper)

(defun agentel-subagent-test-run (session)
  "Let the mock agent of SESSION run a subagent and return the child."
  (agentel-test-send "subagent please")
  (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
  (car (agentel-session-children session)))

(defun agentel-subagent-test-text (buffer)
  "Return the text of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest agentel-subagent-is-advertised ()
  (should (equal (agentel-subagent--capabilities)
                 '((subagents . nil)))))

(ert-deftest agentel-subagent-and-async-task-capabilities-share-the-wire ()
  (agentel-test-with-started session nil
    (let* ((received (alist-get 'receivedCapabilities
                                (alist-get '_meta
                                           (agentel-connection-info
                                            (agentel-session-connection session)))))
           (meta (alist-get '_meta received)))
      (should (assq 'subagents received))
      (should (= (cl-count '_meta received :key #'car) 1))
      (should (eq (alist-get 'terminal_output meta) t))
      (should-not (alist-get 'jetbrains meta)))))

(ert-deftest agentel-subagent-gets-its-own-session-and-buffer ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (should child)
      (should (equal (agentel-session-name child) "Explore the repository (Explore)"))
      (should (eq (agentel-session-state child) 'idle))
      (let ((text (agentel-subagent-test-text (agentel-session-buffer child))))
        (should (string-match-p "List the files and summarize them" text))
        (should (string-match-p "I will look at the files\\." text))
        (should (string-match-p "Read README\\.org" text))))))

(ert-deftest agentel-subagent-output-stays-out-of-the-parent ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (let ((text (agentel-subagent-test-text (current-buffer))))
      (should-not (string-match-p "I will look at the files" text))
      (should (string-match-p "Delegating to a subagent\\." text))
      (should (string-match-p "The subagent reported a README\\." text)))))

(ert-deftest agentel-subagent-keeps-parent-agent-tool-call ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (should (eq (agentel-store-get
                 (agentel-conversation-find session '(tool . "toolu_agent")) 'status)
                'done))))

(ert-deftest agentel-subagent-without-negotiation-stays-in-parent ()
  (let ((agentel-connection-capability-functions
         (remq #'agentel-subagent--capabilities
               agentel-connection-capability-functions)))
    (agentel-test-with-started session nil
      (agentel-test-send "subagent please")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (agentel-session-children session))
      (should (string-match-p "I will look at the files"
                              (agentel-subagent-test-text (current-buffer)))))))

(ert-deftest agentel-subagent-is-one-item-in-the-parent ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (agentel-test-wait-for-text "⎇ Explore the repository (Explore) 💤")
    (agentel-test-wait-for-text "Read README\\.org")))

(ert-deftest agentel-subagent-has-one-item-in-the-conversation-of-the-parent ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (should (= (cl-count 'subagent (agentel-conversation-items session)
                         :key #'agentel-store-model-type)
               1))))

(ert-deftest agentel-subagent-resumes-in-the-same-session ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-run session))
           (buffer (agentel-session-buffer child))
           (id (agentel-session-id child)))
      (dotimes (generation 2)
        (agentel-test-send "resume subagent")
        (agentel-test-wait-until
         (lambda () (and (eq (agentel-session-state session) 'idle)
                         (= (cl-count "Task: Check the new files."
                                      (split-string (agentel-subagent-test-text buffer)
                                                    "\n")
                                      :test #'string=)
                            (1+ generation)))))
        (should (equal (agentel-session-children session) (list child)))
        (should (eq (agentel-session-buffer child) buffer))
        (should (equal (agentel-session-id child) id))
        (should (eq (agentel-session-state child) 'idle))
        (should (= (cl-count 'subagent (agentel-conversation-items session)
                             :key #'agentel-store-model-type)
                   1)))
      (let ((text (agentel-subagent-test-text buffer)))
        (should (= (cl-count "Task: Check the new files."
                              (split-string text "\n") :test #'string=)
                   2))
        (should (= (cl-count "I am checking the new files."
                              (split-string text "\n") :test #'string=)
                   2))
        (should (string-match-p "Task: List the files" text))
        (should (string-match-p "Read README\\.org" text))))))

(ert-deftest agentel-subagent-resume-keeps-metadata-and-clears-old-activity ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-run session))
           (id (agentel-session-id child))
           (connection (agentel-session-connection session)))
      (agentel-subagent--change
       session `((sessionId . ,id) (state . ((state . "running")))))
      (should (eq (agentel-session-get id connection) child))
      (should-not (agentel-session-get id 'another-connection))
      (should (eq (agentel-session-state child) 'running))
      (should (string-match-p "⎇ Explore the repository (Explore) 🏃"
                              (agentel-subagent-test-pin)))
      (should-not (agentel-store-get
                   (agentel-conversation-find session (cons 'subagent id))
                   'activity))
      (should (equal (agentel-subagent--get child 'task)
                     "Explore the repository"))
      (agentel-subagent--change
       session `((sessionId . ,id)
                 (state . ((state . "idle") (stopReason . "end_turn")))))
      (should (eq (agentel-session-state child) 'idle))
      (should-not (agentel-subagent-test-pin)))))

(ert-deftest agentel-subagent-item-waits-while-the-child-does ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create))
         (child (agentel-session-create :parent parent))
         (grandchild (agentel-session-create :parent child)))
    (agentel-subagent--follow child)
    (agentel-session-register parent "p1")
    (agentel-session-register child "c1")
    (agentel-session-register grandchild "g1")
    (let ((item (agentel-conversation-find parent '(subagent . "c1"))))
      (should-not (agentel-store-get item 'waiting))
      (agentel-test-ask grandchild)
      (should (agentel-store-get item 'waiting))
      (agentel-test-answer grandchild)
      (should-not (agentel-store-get item 'waiting)))))

(agentel-store-define 'agentel-subagent-test-other (lambda (message _data) message))

(ert-deftest agentel-subagent-item-follows-only-what-it-shows-of-the-child ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create))
         (child (agentel-session-create :parent parent)))
    (agentel-subagent--follow child)
    (agentel-session-register parent "p1")
    (agentel-session-register child "c1")
    (let* ((item (agentel-conversation-find parent '(subagent . "c1")))
           (revision (agentel-store-model-revision item)))
      (agentel-store-dispatch (agentel-session-store child) 'other
                              'agentel-subagent-test-other '((anything . t)))
      (should (= (agentel-store-model-revision item) revision))
      (agentel-subagent--send child '(act "make"))
      (should (equal (agentel-store-get item 'activity) "make")))))

(ert-deftest agentel-subagent-item-opens-the-child-buffer ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (goto-char (point-min))
      (search-forward "⎇ Explore the repository")
      (agentel-subagent-open)
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child))))))

(ert-deftest agentel-subagent-history-is-replayed-into-its-buffer ()
  (agentel-test-with-started session '(:session-id "old-1")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (let ((child (car (agentel-session-children session))))
      (should (equal (agentel-session-name child) "Review the parser (Explore)"))
      (should (eq (agentel-session-state child) 'idle))
      (should (string-match-p "Replayed subagent text\\."
                              (agentel-subagent-test-text (agentel-session-buffer child)))))))

(ert-deftest agentel-subagent-buffer-comes-back-after-being-killed ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (kill-buffer (agentel-session-buffer child))
      (agentel-subagent-open child)
      (should (buffer-live-p (agentel-session-buffer child)))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (should (memq child (agentel-session-list)))
      (let ((text (agentel-subagent-test-text (agentel-session-buffer child))))
        (should (string-match-p "I will look at the files\\." text))
        (should-not (string-match-p "Earlier output" text))))))

(ert-deftest agentel-subagent-buffer-has-no-input ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-run session)))
      (with-current-buffer (agentel-session-buffer child)
        (should-not agentel-chat--input-start)
        (should-error (agentel-chat-send) :type 'user-error)))))

(defun agentel-subagent-test-pin ()
  "Return the lines pinned above the prompt of the current buffer."
  (overlay-get agentel-chat--pin 'before-string))

(defun agentel-subagent-test-background (session)
  "Let the mock agent of SESSION start a lingering subagent and return it."
  (agentel-test-send "background please")
  (agentel-test-wait-until
   (lambda () (string-match-p "make watch" (or (agentel-subagent-test-pin) ""))))
  (car (agentel-session-children session)))

(ert-deftest agentel-subagent-running-is-pinned-above-the-prompt ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-background session)
    (should (string-match-p "\\`⎇ Watch the build (general-purpose) 🏃 ↳ make watch\n\\'"
                            (substring-no-properties (agentel-subagent-test-pin))))
    (agentel-chat-cancel)
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (agentel-test-wait-until (lambda () (not (agentel-subagent-test-pin))))
    (should-not (agentel-subagent-test-pin))))

(ert-deftest agentel-subagent-continues-after-parent-cancel ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-background session))
           (continued nil)
           (agentel-session-changed-functions
            (cons (lambda (changed)
                    (when (and (eq changed session)
                               (eq (agentel-session-state session) 'idle)
                               (eq (agentel-session-state child) 'running))
                      (setq continued t)))
                  agentel-session-changed-functions)))
      (agentel-chat-cancel)
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should continued)
      (agentel-test-wait-until (lambda () (eq (agentel-session-state child) 'idle)))
      (should (string-match-p "Child continued after parent cancel"
                              (agentel-subagent-test-text (agentel-session-buffer child)))))))

(ert-deftest agentel-subagent-cancel-does-not-cancel-parent ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-background session)))
      (with-current-buffer (agentel-session-buffer child)
        (agentel-chat-cancel))
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should (eq (agentel-session-state child) 'idle))
      (should-not (agentel-session-ended child))
      (should (string-match-p "Parent continued"
                              (agentel-subagent-test-text (current-buffer))))
      (should-not (string-match-p "Turn ended: cancelled"
                                  (agentel-subagent-test-text (current-buffer)))))))

(ert-deftest agentel-subagent-replayed-child-resumes-in-same-buffer ()
  (agentel-test-with-started session '(:session-id "old-1")
    (let* ((child (car (agentel-session-children session)))
           (buffer (agentel-session-buffer child)))
      (agentel-test-send "resume subagent")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should (equal (agentel-session-children session) (list child)))
      (should (eq (agentel-session-buffer child) buffer))
      (should (string-match-p "I am checking the new files"
                              (agentel-subagent-test-text buffer))))))

(defun agentel-subagent-test-item (seen &optional width)
  "Return the text of a subagent item sent SEEN, shown WIDTH wide."
  (substring-no-properties
   (agentel-ui-view (agentel-store-model--make
                     :type 'subagent
                     :data (agentel-subagent--update `(show child ,seen) nil))
                    :width width)))

(defconst agentel-subagent-test-busy
  `((name . "Build") (state . running)
    (activity . ,(concat "make all\n" (make-string 100 ?x))))
  "What is seen of a subagent whose latest tool call is a long command.")

(ert-deftest agentel-subagent-item-shows-what-it-was-sent ()
  (should (equal (agentel-subagent-test-item
                  '((name . "Build") (state . running) (activity . "make")))
                 "⎇ Build 🏃\n    ↳ make")))

(ert-deftest agentel-subagent-item-shows-the-activity-on-one-line ()
  (should (string-suffix-p "\n    ↳ make all…"
                           (agentel-subagent-test-item agentel-subagent-test-busy))))

(ert-deftest agentel-subagent-item-fits-the-width-it-is-shown-in ()
  (should (string-suffix-p "\n    ↳ make…"
                           (agentel-subagent-test-item agentel-subagent-test-busy 11))))

(defun agentel-subagent-test-pinned (width &rest messages)
  "Return the pinned lines of the running subagents after MESSAGES.
They are shown WIDTH wide."
  (let (data)
    (dolist (message messages)
      (setq data (agentel-subagent--update-running message data)))
    (substring-no-properties
     (agentel-ui-view (agentel-store-model--make :type 'agentel-subagent-running
                                                 :data data)
                      :width width))))

(ert-deftest agentel-subagent-pin-shows-the-activity-on-one-line ()
  (should (equal (agentel-subagent-test-pinned
                  80 `(show child ,agentel-subagent-test-busy))
                 "⎇ Build 🏃 ↳ make all…")))

(ert-deftest agentel-subagent-pin-fits-the-width-it-is-shown-in ()
  (should (equal (agentel-subagent-test-pinned
                  15 `(show child ,agentel-subagent-test-busy))
                 "⎇ Build 🏃 ↳ m…")))

(ert-deftest agentel-subagent-pin-shows-the-running-children-oldest-first ()
  (should (equal (agentel-subagent-test-pinned
                  80
                  '(show one ((name . "One") (state . running)))
                  '(show two ((name . "Two") (state . running)))
                  '(show three ((name . "Three") (state . running)))
                  '(show one ((name . "One") (state . waiting)))
                  '(forget two))
                 "⎇ One 🙋\n⎇ Three 🏃")))

(ert-deftest agentel-subagent-pin-follows-the-child-until-it-ends ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create))
         (child (agentel-session-create :parent parent)))
    (agentel-subagent--follow child)
    (agentel-session-send child '(retitle "Build"))
    (agentel-session-register child "c1")
    (agentel-subagent--send child '(state running))
    (agentel-subagent--send child '(act "make"))
    (let ((running (agentel-store-find (agentel-session-store parent) 'subagents)))
      (should (equal (substring-no-properties (agentel-ui-view running :width 80))
                     "⎇ Build 🏃 ↳ make"))
      (agentel-session-send child '(end completed))
      (should (equal (agentel-ui-view running :width 80) "")))))

(ert-deftest agentel-subagent-runs-only-while-reported-running ()
  (let* ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (child (agentel-session-create)))
    (agentel-session-register child "c1")
    (should (eq (agentel-session-state child) 'idle))
    (agentel-subagent--send child '(assign "Build it"))
    (should (eq (agentel-session-state child) 'idle))
    (agentel-subagent--send child '(state running))
    (should (eq (agentel-session-state child) 'running))
    (agentel-subagent--send child '(state idle))
    (should (eq (agentel-session-state child) 'idle))))

(ert-deftest agentel-subagent-finished-is-not-pinned ()
  (agentel-test-with-started session nil
    (agentel-subagent-test-run session)
    (should-not (agentel-subagent-test-pin))))

(ert-deftest agentel-subagent-pin-opens-the-child-buffer ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-background session))
           (map (get-text-property 0 'keymap (agentel-subagent-test-pin))))
      (funcall (keymap-lookup map "<mouse-1>"))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (agentel-chat-cancel))))

(ert-deftest agentel-subagent-open-offers-the-running-children ()
  (agentel-test-with-started session nil
    (let ((child (agentel-subagent-test-background session)))
      (goto-char (point-max))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (should (equal (all-completions "" collection)
                                  '("Watch the build (general-purpose)")))
                   "Watch the build (general-purpose)")))
        (agentel-subagent-open))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))
      (agentel-chat-cancel))))

(ert-deftest agentel-subagent-open-without-running-children-is-a-user-error ()
  (agentel-test-with-started _session nil
    (goto-char (point-max))
    (should-error (agentel-subagent-open) :type 'user-error)))

(ert-deftest agentel-subagent-open-is-bound-in-the-session-buffer ()
  (should (eq (keymap-lookup agentel-chat-mode-map "C-c C-j") #'agentel-subagent-open)))

(ert-deftest agentel-subagent-buffers-go-with-the-parent ()
  (agentel-test-with-started session nil
    (let* ((child (agentel-subagent-test-run session))
           (buffer (agentel-session-buffer child)))
      (kill-buffer (current-buffer))
      (should-not (buffer-live-p buffer))
      (should-not (memq child (agentel-session-list))))))

(ert-deftest agentel-subagent-models-reject-an-unknown-message ()
  (dolist (update (list #'agentel-subagent--update-task
                        #'agentel-subagent--update-running
                        #'agentel-subagent--update))
    (should-error (funcall update '(forget) nil)
                  :type 'agentel-store-unknown-message)))

(ert-deftest agentel-subagent-idle-can-resume-and-wait ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create :cwd temporary-file-directory))
         child)
    (agentel-session-register parent "parent")
    (unwind-protect
        (progn
          (agentel-session-dispatch
           '((sessionId . "parent")
             (update . ((sessionUpdate . "subagent_update") (sessionId . "child")
                        (title . "Explore") (description . "Read files")
                        (capabilities . ((cancel . nil)))
                        (state . ((state . "running")))))))
          (setq child (agentel-session-get "child"))
          (should child)
          (dolist (state '("idle" "running" "requires_action" "running" "idle"))
            (agentel-session-dispatch
             `((sessionId . "parent")
               (update . ((sessionUpdate . "subagent_update") (sessionId . "child")
                          (state . ((state . ,state) (stopReason . "end_turn")))))))
            (should (eq (agentel-session-state child)
                        (intern (if (equal state "requires_action") "waiting" state))))
            (should (eq (and (agentel-session-waiting-p parent) t)
                        (equal state "requires_action")))
            (should-not (agentel-session-ended child))
            (should (equal (agentel-session-name child) "Read files (Explore)"))
            (should (equal (agentel-subagent--get child 'task) "Read files"))
            (should (assq 'cancel (agentel-subagent--get child 'capabilities))))
          (should (equal (agentel-session-children parent) (list child))))
      (when (and child (buffer-live-p (agentel-session-buffer child)))
        (kill-buffer (agentel-session-buffer child))))))

(ert-deftest agentel-subagent-is-named-by-its-task-and-role-told-apart ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create :cwd temporary-file-directory))
         child)
    (agentel-session-register parent "parent")
    (unwind-protect
        (progn
          (agentel-subagent--change parent '((sessionId . "child") (title . "Explore")))
          (setq child (agentel-session-get "child"))
          (should (equal (agentel-session-name child) "Explore"))
          (agentel-subagent--change parent '((sessionId . "child")
                                             (description . "Read files")))
          (should (equal (agentel-session-name child) "Read files (Explore)")))
      (when (and child (buffer-live-p (agentel-session-buffer child)))
        (kill-buffer (agentel-session-buffer child))))))

(ert-deftest agentel-subagent-handback-report-stays-in-the-child ()
  (let* ((agentel-session--registry nil)
         (parent (agentel-session-create))
         (child (agentel-session-create :parent parent))
         (update '((sessionUpdate . "tool_call") (toolCallId . "handback")
                   (name . "SubagentHandback") (title . "SubagentHandback")
                   (rawInput . ((message . "CHILD_RESUMED")))
                   (_meta . ((claudeCode . ((toolName . "SubagentHandback"))))))))
    (agentel-session-register child "child")
    (dotimes (_ 2)
      (agentel-session-dispatch `((sessionId . "child") (update . ,update))))
    (agentel-session-dispatch
     '((sessionId . "child")
       (update . ((sessionUpdate . "tool_call_update") (toolCallId . "handback")
                  (status . "completed")
                  (content . [((type . "content")
                               (content . ((type . "text") (text . "Report delivered"))))])))))
    (should (= (cl-count-if (lambda (item)
                             (string-match-p "CHILD_RESUMED"
                                             (or (agentel-store-get item 'text) "")))
                           (agentel-conversation-items child))
               1))
    (should-not (agentel-conversation-items parent))))

(provide 'agentel-subagent-test)
;;; agentel-subagent-test.el ends here
