;;; agentel-connection.el --- Agent process connection for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; A connection is one agent process spoken to over ACP with acp.el.
;; It initializes the agent with the capabilities the loaded features
;; declare, routes `session/update' notifications to the session
;; registry, and hands requests from the agent to the handler the
;; feature registered for the method.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'lisp-mnt)
(require 'agentel-session)

(defconst agentel-connection-protocol-version 1
  "ACP protocol version agentel speaks.")

(defvar agentel-connection-capability-functions nil
  "Functions returning client capability entries to advertise.
Each is called with no arguments and returns an alist that is merged
into `clientCapabilities' of the initialize request.")

(defvar agentel-connection-request-handlers nil
  "Alist of ACP method name to the function handling that request.
The function is called with the connection, the request id and the
params, and must answer with `agentel-connection-respond' or
`agentel-connection-respond-error'.")

(defvar agentel-connection-notification-functions nil
  "Abnormal hook run for notifications other than `session/update'.
Each function is called with the connection, the method and the
params, for example to withdraw a question on `$/cancel_request'.")

(defvar agentel-connection-exit-functions nil
  "Abnormal hook run with a connection whose agent exited on its own.")

(defconst agentel-connection--stderr-lines 20
  "Number of stderr lines of the agent kept for error reports.")

(cl-defstruct (agentel-connection (:constructor agentel-connection--make)
                                  (:copier nil))
  "One agent process."
  client info stderr stopping)

(defun agentel-connection--record-stderr (connection text)
  "Keep the last lines of the stderr TEXT of CONNECTION."
  (let ((lines (append (agentel-connection-stderr connection)
                       (split-string text "\n" t "[ \t]+"))))
    (setf (agentel-connection-stderr connection)
          (last lines agentel-connection--stderr-lines))))

(defun agentel-connection--capabilities ()
  "Return the client capabilities declared by the loaded features."
  (or (apply #'append
             (mapcar #'funcall agentel-connection-capability-functions))
      ;; An empty alist would be serialized as null instead of {}.
      (make-hash-table)))

(defun agentel-connection--version ()
  "Return the version of agentel from its package header."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents (locate-library "agentel.el"))
          (lm-header "Version")))
      "0"))

(defun agentel-connection--on-notification (connection notification)
  "Handle NOTIFICATION from the agent on CONNECTION."
  (let-alist notification
    (if (equal .method "session/update")
        (agentel-session-dispatch .params connection)
      (run-hook-with-args 'agentel-connection-notification-functions
                          connection .method .params))))

(defun agentel-connection--on-request (connection request)
  "Handle REQUEST from the agent on CONNECTION."
  (let-alist request
    (if-let* ((handler (alist-get .method agentel-connection-request-handlers
                                  nil nil #'equal)))
        (funcall handler connection .id .params)
      (agentel-connection-respond-error
       connection .id -32601 (format "Method not found: %s" .method)))))

(defun agentel-connection--on-exit (connection)
  "Mark the sessions of CONNECTION as ended after its process exited."
  (dolist (session (agentel-session-list))
    (when (and (eq (agentel-session-connection session) connection)
               (not (agentel-session-ended session)))
      (agentel-session-set-ended session 'exited)))
  (unless (agentel-connection-stopping connection)
    (run-hook-with-args 'agentel-connection-exit-functions connection)))

(cl-defun agentel-connection-start (&key command args env cwd on-ready on-failure)
  "Start the agent COMMAND with ARGS in CWD and initialize it.
ENV is a list of \"VAR=value\" strings added to the environment.
ON-READY is called with the connection once the agent answered
initialize, and ON-FAILURE with the error when it did not.  Return
the connection."
  (let* ((client (acp-make-client :command command
                                  :command-params args
                                  :environment-variables env))
         (connection (agentel-connection--make :client client))
         (default-directory (file-name-as-directory (or cwd default-directory))))
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (agentel-connection--on-notification connection notification)))
    (acp-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (agentel-connection--record-stderr
                  connection (or (alist-get 'message error) ""))))
    (acp-subscribe-to-requests
     :client client
     :on-request (lambda (request)
                   (agentel-connection--on-request connection request)))
    (agentel-connection-request
     connection "initialize"
     `((protocolVersion . ,agentel-connection-protocol-version)
       (clientInfo . ((name . "agentel")
                      (version . ,(agentel-connection--version))))
       (clientCapabilities . ,(agentel-connection--capabilities)))
     :on-success (lambda (result)
                   (setf (agentel-connection-info connection) result)
                   (when on-ready (funcall on-ready connection)))
     :on-failure (lambda (error)
                   (when on-failure (funcall on-failure error))))
    (add-function :after (process-sentinel (agentel-connection-process connection))
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (agentel-connection--on-exit connection))))
    connection))

(defun agentel-connection-process (connection)
  "Return the process of CONNECTION."
  (map-elt (agentel-connection-client connection) :process))

(defun agentel-connection-live-p (connection)
  "Return non-nil if the agent process of CONNECTION runs."
  (when-let* ((process (agentel-connection-process connection)))
    (process-live-p process)))

(cl-defun agentel-connection-request (connection method params &key on-success on-failure)
  "Send the request METHOD with PARAMS on CONNECTION.
ON-SUCCESS is called with the result and ON-FAILURE with the error."
  (acp-send-request
   :client (agentel-connection-client connection)
   :request `((:method . ,method) (:params . ,params))
   :on-success on-success
   :on-failure on-failure))

(defun agentel-connection-notify (connection method params)
  "Send the notification METHOD with PARAMS on CONNECTION."
  (acp-send-notification
   :client (agentel-connection-client connection)
   :notification `((:method . ,method) (:params . ,params))))

(defun agentel-connection-respond (connection id result)
  "Answer the agent request ID on CONNECTION with RESULT."
  (acp-send-response
   :client (agentel-connection-client connection)
   :response `((:request-id . ,id) (:result . ,result))))

(defun agentel-connection-respond-error (connection id code message)
  "Fail the agent request ID on CONNECTION with CODE and MESSAGE."
  (acp-send-response
   :client (agentel-connection-client connection)
   :response `((:request-id . ,id)
               (:error . ((code . ,code) (message . ,message))))))

(defun agentel-connection-shutdown (connection)
  "Stop the agent process of CONNECTION."
  (setf (agentel-connection-stopping connection) t)
  (acp-shutdown :client (agentel-connection-client connection))
  (agentel-connection--on-exit connection))

(provide 'agentel-connection)
;;; agentel-connection.el ends here
