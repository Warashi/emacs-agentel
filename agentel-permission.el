;;; agentel-permission.el --- Permission requests for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Answers `session/request_permission'.  The request is shown in the
;; transcript of the session that asked, with one button per option,
;; and can also be answered from the minibuffer with
;; `agentel-chat-answer'.

;;; Code:

(require 'cl-lib)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-chat)

(defface agentel-permission-face
  '((t :inherit warning))
  "Face of permission requests."
  :group 'agentel)

(defun agentel-permission--title (item)
  "Return the question of the permission request ITEM."
  (format "Allow %s?"
          (or (alist-get 'title (alist-get 'toolCall (plist-get item :params)))
              "this tool call")))

(defun agentel-permission--render (entry _options)
  "Render the permission request ENTRY."
  (let-alist (agentel-store-model-data entry)
    (if .outcome
        (propertize (format "%s → %s" .question .outcome)
                    'face 'agentel-chat-notice-face)
      (concat
       (propertize (concat "⚠ " .question) 'face 'agentel-permission-face)
       "\n  "
       (mapconcat
        (lambda (option)
          (buttonize (format "[%s]" (alist-get 'name option))
                     (lambda (_)
                       (agentel-permission-choose .item (alist-get 'optionId option)))))
        .options
        " ")))))

(defun agentel-permission--update (message data)
  "Return the DATA of a permission request changed by MESSAGE.
MESSAGE is (ask ITEM QUESTION OPTIONS) when the agent asks, and
\(close OUTCOME) when the request is answered or withdrawn.  ITEM is
kept only to answer it."
  (pcase message
    (`(ask ,item ,question ,options)
     `((item . ,item) (question . ,question) (options . ,options) (waiting . t)))
    (`(close ,outcome)
     (let-alist data
       `((item . ,.item) (question . ,.question) (options . ,.options)
         (outcome . ,outcome))))))

(agentel-conversation-define 'permission #'agentel-permission--update)
(agentel-ui-define-view 'permission #'agentel-permission--render)

(defun agentel-permission--send (item message)
  "Send MESSAGE to the conversation item of the permission request ITEM."
  (agentel-conversation-send (plist-get item :session)
                             (cons 'permission (plist-get item :id))
                             'permission message))

(defun agentel-permission--close (item outcome)
  "Stop waiting for ITEM, recording OUTCOME as its answer."
  (plist-put item :outcome outcome)
  (agentel-session-remove-pending (plist-get item :session) item)
  (agentel-permission--send item `(close ,outcome)))

(defun agentel-permission-choose (item option-id)
  "Answer the permission request ITEM with the option OPTION-ID."
  (unless (plist-get item :outcome)
    (let ((option (seq-find (lambda (o) (equal (alist-get 'optionId o) option-id))
                            (alist-get 'options (plist-get item :params)))))
      (agentel-connection-respond
       (plist-get item :connection) (plist-get item :id)
       `((outcome . ((outcome . "selected") (optionId . ,option-id)))))
      (agentel-permission--close item (or (alist-get 'name option) option-id)))))

(defun agentel-permission--ask (item)
  "Ask in the minibuffer how to answer the permission request ITEM."
  (let* ((options (mapcar (lambda (o) (cons (alist-get 'name o) (alist-get 'optionId o)))
                          (alist-get 'options (plist-get item :params))))
         (choice (completing-read (concat (agentel-permission--title item) " ")
                                  (agentel-chat-ordered-completion options)
                                  nil t)))
    (agentel-permission-choose item (cdr (assoc choice options)))))

(defun agentel-permission--handle (connection id params)
  "Handle the permission request ID with PARAMS on CONNECTION."
  (if-let* ((session (agentel-session-get (alist-get 'sessionId params) connection)))
      (let ((item (list :kind 'permission :id id :params params
                        :session session :connection connection)))
        (plist-put item :answer (lambda () (agentel-permission--ask item)))
        (agentel-session-add-pending session item)
        (agentel-permission--send
         item `(ask ,item ,(agentel-permission--title item)
                    ,(alist-get 'options params))))
    (agentel-connection-respond connection id '((outcome . ((outcome . "cancelled")))))))

(defun agentel-permission--withdraw (connection method params)
  "Drop the request of CONNECTION that the agent withdrew.
METHOD and PARAMS are those of the notification."
  (when (equal method "$/cancel_request")
    (dolist (session (agentel-session-list))
      (dolist (item (agentel-session-pending session))
        (when (and (eq (plist-get item :kind) 'permission)
                   (eq (plist-get item :connection) connection)
                   (equal (plist-get item :id) (alist-get 'requestId params)))
          (agentel-permission--close item "withdrawn"))))))

(setf (alist-get "session/request_permission" agentel-connection-request-handlers
                 nil nil #'equal)
      #'agentel-permission--handle)
(add-hook 'agentel-connection-notification-functions #'agentel-permission--withdraw)

(provide 'agentel-permission)
;;; agentel-permission.el ends here
