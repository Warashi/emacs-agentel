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
(require 'seq)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-chat)

(defface agentel-permission-face
  '((t :inherit warning))
  "Face of permission requests."
  :group 'agentel)

(defvar agentel-permission--requests (make-hash-table :test 'eq)
  "Requests waiting for an answer by the items that show them.
Each is a plist of the :connection and :id to answer, the :options to
answer with in the order of the choices of the item, and the :session
waiting for it, where it is pending.")

(defun agentel-permission--render (entry _options)
  "Render the permission request ENTRY."
  (let-alist (agentel-store-model-data entry)
    (cond
     (.answer (propertize (format "%s → %s" .question .answer)
                          'face 'agentel-chat-notice-face))
     (.withdrawn (propertize (format "%s → withdrawn" .question)
                             'face 'agentel-chat-notice-face))
     (t
      (concat
       (propertize (concat "⚠ " .question) 'face 'agentel-permission-face)
       "\n  "
       (mapconcat
        #'identity
        (seq-map-indexed
         (lambda (choice n)
           (buttonize (format "[%s]" choice)
                      (lambda (_) (agentel-permission-choose entry n))))
         .choices)
        " "))))))

(defun agentel-permission--update (message data)
  "Return the DATA of a permission request changed by MESSAGE.
MESSAGE is (ask QUESTION CHOICES) when the agent asks, with CHOICES a
list of texts, (answer CHOICE) when the user answers and (withdraw)
when the agent no longer asks."
  (pcase message
    (`(ask ,question ,choices)
     `((question . ,question) (choices . ,choices) (waiting . t)))
    (`(answer ,choice)
     (let-alist data
       `((question . ,.question) (choices . ,.choices) (answer . ,choice))))
    ('(withdraw)
     (let-alist data
       `((question . ,.question) (choices . ,.choices) (withdrawn . t))))))

(agentel-conversation-define 'permission #'agentel-permission--update)
(agentel-ui-define-view 'permission #'agentel-permission--render)

(defun agentel-permission-choose (item n)
  "Answer the permission request ITEM with its Nth choice."
  (when-let* ((request (gethash item agentel-permission--requests)))
    (agentel-connection-respond
     (plist-get request :connection) (plist-get request :id)
     `((outcome . ((outcome . "selected")
                   (optionId . ,(nth n (plist-get request :options)))))))
    (agentel-permission--close item `(answer ,(nth n (agentel-store-get item 'choices))))))

(defun agentel-permission--ask (item)
  "Ask in the minibuffer how to answer the permission request ITEM."
  (let* ((choices (agentel-store-get item 'choices))
         (choice (completing-read (concat (agentel-store-get item 'question) " ")
                                  (agentel-chat-ordered-completion choices)
                                  nil t)))
    (when-let* ((n (seq-position choices choice)))
      (agentel-permission-choose item n))))

(agentel-chat-define-answer 'permission #'agentel-permission--ask)

(defun agentel-permission--close (item message)
  "Stop waiting for an answer to ITEM and send MESSAGE to it."
  (let ((request (gethash item agentel-permission--requests)))
    (remhash item agentel-permission--requests)
    (agentel-session-remove-pending (plist-get request :session) request)
    (agentel-conversation-send (plist-get request :session)
                               (agentel-store-model-key item)
                               'permission message)))

(defun agentel-permission--handle (connection id params)
  "Handle the permission request ID with PARAMS on CONNECTION."
  (if-let* ((session (agentel-session-get (alist-get 'sessionId params) connection)))
      (let* ((options (append (alist-get 'options params) nil))
             (item (agentel-conversation-send
                    session (cons 'permission id) 'permission
                    `(ask ,(format "Allow %s?"
                                   (or (alist-get 'title (alist-get 'toolCall params))
                                       "this tool call"))
                          ,(mapcar (lambda (o) (or (alist-get 'name o)
                                                   (alist-get 'optionId o)))
                                   options))))
             (request (list :connection connection :id id :session session
                            :options (mapcar (lambda (o) (alist-get 'optionId o))
                                             options))))
        (puthash item request agentel-permission--requests)
        (agentel-session-add-pending session request))
    (agentel-connection-respond connection id '((outcome . ((outcome . "cancelled")))))))

(defun agentel-permission--withdraw (connection method params)
  "Drop the request of CONNECTION that the agent withdrew.
METHOD and PARAMS are those of the notification."
  (when (equal method "$/cancel_request")
    (let (withdrawn)
      (maphash (lambda (item request)
                 (when (and (eq (plist-get request :connection) connection)
                            (equal (plist-get request :id)
                                   (alist-get 'requestId params)))
                   (push item withdrawn)))
               agentel-permission--requests)
      (dolist (item withdrawn)
        (agentel-permission--close item '(withdraw))))))

(setf (alist-get "session/request_permission" agentel-connection-request-handlers
                 nil nil #'equal)
      #'agentel-permission--handle)
(add-hook 'agentel-connection-notification-functions #'agentel-permission--withdraw)

(provide 'agentel-permission)
;;; agentel-permission.el ends here
