(in-package #:agentcomms)

;;;; -- JSON-RPC Connection --

(defparameter *acp-default-request-timeout* nil
  "Seconds to wait for a response before cancelling, or NIL to wait indefinitely.")

(defparameter *acp-close-join-seconds* 5
  "Seconds to wait for the reader thread while closing a connection.")

(defparameter *acp-cancel-request-method* "$/cancel_request"
  "The protocol-level notification cancelling an in-flight request.")

(defvar *acp-inbound-request* nil
  "The inbound request handled by the current thread, when there is one.")


;;;; -- Peer Protocol --

(defclass acp-peer ()
  ()
  (:documentation
   "A party on one side of a connection that answers requests and notifications.

The agent and client roles are peers. Subclasses specialize the generic
functions below, each of which runs with *ACP-INBOUND-REQUEST* bound while
a request is in progress."))

(defgeneric peer-handle-request (peer connection method params)
  (:documentation
   "Return the JSON result for METHOD called with PARAMS by the peer.

Signal ACP-METHOD-ERROR to answer with a specific JSON-RPC error. Any
other error becomes an Internal Error response. A NIL result is sent as
an empty object."))

(defgeneric peer-handle-notification (peer connection method params)
  (:documentation "React to notification METHOD with PARAMS. Errors are logged, not sent."))

(defgeneric peer-connection-closed (peer connection reason)
  (:documentation "Observe CONNECTION closing for REASON, a string or NIL for a clean end."))

(defmethod peer-handle-request ((peer acp-peer) connection method params)
  "Answer every method with Method Not Found."
  (declare (ignore connection params))
  (error 'acp-method-error
         :code -32601
         :message (format nil "Method not found: ~A" method)))

(defmethod peer-handle-notification ((peer acp-peer) connection method params)
  "Ignore unrecognized notifications, as the protocol requires."
  (declare (ignore connection method params))
  nil)

(defmethod peer-connection-closed ((peer acp-peer) connection reason)
  "Ignore the close by default."
  (declare (ignore connection reason))
  nil)


;;;; -- Request Records --

(defclass acp-pending-request ()
  ((identifier
    :initarg :identifier
    :reader acp-pending-request-identifier
    :type integer
    :documentation "The JSON-RPC id of the outgoing request.")
   (method
    :initarg :method
    :reader acp-pending-request-method
    :type string
    :documentation "The requested method, for diagnostics.")
   (result
    :initform nil
    :accessor acp-pending-request-result
    :type t
    :documentation "The result value once a successful response arrives.")
   (failure
    :initform nil
    :accessor acp-pending-request-failure
    :type (or null condition)
    :documentation "The condition to signal to the waiter, when the request failed.")
   (done-p
    :initform nil
    :accessor acp-pending-request-done-p
    :type boolean
    :documentation "Whether a response or failure has been recorded."))
  (:documentation "The waiting side of one outgoing request."))

(defclass acp-inbound-request ()
  ((identifier
    :initarg :identifier
    :reader acp-inbound-request-identifier
    :type t
    :documentation "The JSON-RPC id chosen by the peer.")
   (method
    :initarg :method
    :reader acp-inbound-request-method
    :type string
    :documentation "The method being handled.")
   (cancelled-p
    :initform nil
    :accessor acp-inbound-request-cancelled-p
    :type boolean
    :documentation "Whether the peer cancelled this request."))
  (:documentation "One request from the peer that a handler thread is serving."))


;;;; -- Connection --

(defclass acp-connection ()
  ((channel
    :initarg :channel
    :reader acp-connection-channel
    :type acp-channel
    :documentation "The channel carrying this connection's messages.")
   (peer
    :initarg :peer
    :reader acp-connection-peer
    :type acp-peer
    :documentation "The local party answering the remote side.")
   (name
    :initarg :name
    :initform "agentcomms"
    :reader acp-connection-name
    :type string
    :documentation "A label used in thread names and diagnostics.")
   (request-timeout
    :initarg :request-timeout
    :initform *acp-default-request-timeout*
    :accessor acp-connection-request-timeout
    :type (or null real)
    :documentation "The default response deadline for outgoing requests.")
   (log-function
    :initarg :log-function
    :initform nil
    :accessor acp-connection-log-function
    :type (or null function)
    :documentation "An optional function receiving one diagnostic string per event.")
   (lock
    :initform (make-lock "agentcomms connection")
    :reader acp-connection-lock
    :type t
    :documentation "The lock guarding identifiers, tables, and the closed flag.")
   (condition
    :initform (make-condition-variable :name "agentcomms responses")
    :reader acp-connection-condition
    :type t
    :documentation "Signaled whenever a pending request completes.")
   (next-identifier
    :initform 0
    :accessor acp-connection-next-identifier
    :type integer
    :documentation "The id of the next outgoing request.")
   (pending
    :initform (make-hash-table :test #'eql)
    :reader acp-connection-pending
    :type hash-table
    :documentation "Outgoing requests awaiting responses, by id.")
   (inbound
    :initform (make-hash-table :test #'equal)
    :reader acp-connection-inbound
    :type hash-table
    :documentation "Peer requests being handled, by their id.")
   (reader-thread
    :initform nil
    :accessor acp-connection-reader-thread
    :type t
    :documentation "The thread reading and dispatching incoming messages.")
   (closed-p
    :initform nil
    :accessor acp-connection-closed-p
    :type boolean
    :documentation "Whether the connection has closed.")
   (close-reason
    :initform nil
    :accessor acp-connection-close-reason
    :type (or null string)
    :documentation "Why the connection closed, or NIL for a clean end."))
  (:documentation
   "A symmetric JSON-RPC 2.0 connection over a message channel.

Both sides may send requests and notifications. Incoming requests run on
their own threads so a long request such as a prompt never blocks the
notification that cancels it; incoming notifications run on the reader
thread in arrival order."))

(-> make-acp-connection
    (&key (:channel acp-channel) (:peer acp-peer) (:name string)
          (:request-timeout (or null real)) (:log-function (or null function)))
    acp-connection)
(defun make-acp-connection
    (&key channel peer (name "agentcomms") (request-timeout *acp-default-request-timeout*)
       log-function)
  "Return a running connection serving PEER over CHANNEL.

The reader thread starts immediately. REQUEST-TIMEOUT is the default
deadline in seconds for outgoing requests; NIL waits indefinitely."
  (let ((connection (make-instance 'acp-connection
                                   :channel channel
                                   :peer peer
                                   :name name
                                   :request-timeout request-timeout
                                   :log-function log-function)))
    (setf (acp-connection-reader-thread connection)
          (make-thread (lambda ()
                         (connection--reader-loop connection))
                       :name (format nil "~A reader" name)))
    connection))

(-> connection--log (acp-connection string &rest t) null)
(defun connection--log (connection control &rest arguments)
  "Pass one formatted diagnostic to the connection's log function, if any."
  (let ((function (acp-connection-log-function connection)))
    (when function
      (ignore-errors (funcall function (apply #'format nil control arguments)))))
  nil)

(-> connection-open-p (acp-connection) boolean)
(defun connection-open-p (connection)
  "Return true while CONNECTION has not closed."
  (not (acp-connection-closed-p connection)))

(-> connection-run (acp-connection) (or null string))
(defun connection-run (connection)
  "Block until CONNECTION closes and return its close reason."
  (let ((thread (acp-connection-reader-thread connection)))
    (when (and thread (not (eq thread (current-thread))))
      (join-thread thread)))
  (acp-connection-close-reason connection))


;;;; -- Outgoing Messages --

(-> connection--write (acp-connection t) null)
(defun connection--write (connection message)
  "Encode and send MESSAGE, closing the connection when the channel fails."
  (let ((text (json-encode message)))
    (handler-case
        (channel-write-message (acp-connection-channel connection) text)
      (acp-connection-closed (condition)
        (error condition))
      (error (cause)
        (connection-close connection
                          :reason (format nil "Writing to the channel failed: ~A" cause))
        (error 'acp-connection-closed
               :message (format nil "The ACP connection closed while writing: ~A" cause)))))
  nil)

(-> connection-notify (acp-connection string &optional t) null)
(defun connection-notify (connection method &optional params)
  "Send notification METHOD with PARAMS, which defaults to an empty object."
  (when (acp-connection-closed-p connection)
    (error 'acp-connection-closed))
  (connection--write connection
                     (json-object "jsonrpc" "2.0"
                                  "method" method
                                  "params" (or params (json-object)))))

(-> connection-cancel-request (acp-connection t) null)
(defun connection-cancel-request (connection identifier)
  "Ask the peer to cancel the request it received under IDENTIFIER."
  (connection-notify connection
                     *acp-cancel-request-method*
                     (json-object "requestId" identifier)))

(-> connection--deadline-passed-p ((or null integer)) boolean)
(defun connection--deadline-passed-p (deadline)
  "Return true when DEADLINE, in internal time units, has passed."
  (and deadline (>= (get-internal-real-time) deadline) t))

(-> connection--await (acp-connection acp-pending-request (or null real)) t)
(defun connection--await (connection pending timeout)
  "Wait for PENDING to complete within TIMEOUT seconds and return its result."
  (let ((deadline (and timeout
                       (+ (get-internal-real-time)
                          (ceiling (* timeout internal-time-units-per-second)))))
        (lock (acp-connection-lock connection)))
    (with-lock-held (lock)
      (loop
        (cond
          ((acp-pending-request-done-p pending)
           (let ((failure (acp-pending-request-failure pending)))
             (when failure
               (error failure))
             (return (acp-pending-request-result pending))))
          ((connection--deadline-passed-p deadline)
           (remhash (acp-pending-request-identifier pending)
                    (acp-connection-pending connection))
           (return ':timeout))
          (t
           (let ((remaining (and deadline
                                 (/ (- deadline (get-internal-real-time))
                                    internal-time-units-per-second))))
             (condition-wait (acp-connection-condition connection) lock
                             :timeout (and remaining (max remaining 0.001))))))))))

(-> connection-request (acp-connection string t &key (:timeout (or null real))) t)
(defun connection-request (connection method params
                           &key (timeout (acp-connection-request-timeout connection)))
  "Send request METHOD with PARAMS, an object or NIL, and return the peer's result.

Signal ACP-REMOTE-ERROR for an error response, ACP-CONNECTION-CLOSED when
the connection closes first, and ACP-TIMEOUT after TIMEOUT seconds, in
which case a cancellation notification is sent for the request."
  (let ((pending nil))
    (with-lock-held ((acp-connection-lock connection))
      (when (acp-connection-closed-p connection)
        (error 'acp-connection-closed))
      (let ((identifier (acp-connection-next-identifier connection)))
        (incf (acp-connection-next-identifier connection))
        (setf pending (make-instance 'acp-pending-request
                                     :identifier identifier
                                     :method method))
        (setf (gethash identifier (acp-connection-pending connection)) pending)))
    (handler-case
        (connection--write connection
                           (json-object "jsonrpc" "2.0"
                                        "id" (acp-pending-request-identifier pending)
                                        "method" method
                                        "params" (or params (json-object))))
      (error (condition)
        (with-lock-held ((acp-connection-lock connection))
          (remhash (acp-pending-request-identifier pending)
                   (acp-connection-pending connection)))
        (error condition)))
    (let ((result (connection--await connection pending timeout)))
      (when (eq result ':timeout)
        (ignore-errors
          (connection-cancel-request connection (acp-pending-request-identifier pending)))
        (error 'acp-timeout
               :message (format nil "~A received no response within ~A seconds."
                                method timeout)
               :seconds timeout))
      result)))


;;;; -- Incoming Messages --

(-> connection--error-object (integer string &optional t) hash-table)
(defun connection--error-object (code message &optional data)
  "Return a JSON-RPC error object."
  (json-object "code" code "message" message "data" data))

(-> connection--respond (acp-connection t t &key (:error-object t)) null)
(defun connection--respond (connection identifier result &key error-object)
  "Send the response to the peer request IDENTIFIER, ignoring a closed channel."
  (handler-case
      (connection--write connection
                         (if error-object
                             (json-object "jsonrpc" "2.0" "id" identifier
                                          "error" error-object)
                             (let ((message (json-object "jsonrpc" "2.0" "id" identifier)))
                               (setf (gethash "result" message) (or result (json-object)))
                               message)))
    (acp-connection-closed ()
      nil))
  nil)

(-> connection--identifier-p (t) boolean)
(defun connection--identifier-p (value)
  "Return true when VALUE is a JSON-RPC id this implementation accepts."
  (and (or (integerp value) (stringp value)) t))

(-> connection--complete-pending (acp-connection hash-table) null)
(defun connection--complete-pending (connection message)
  "Resolve the pending request answered by response MESSAGE."
  (let ((identifier (json-get message "id"))
        (pending nil))
    (with-lock-held ((acp-connection-lock connection))
      (when (integerp identifier)
        (setf pending (gethash identifier (acp-connection-pending connection)))
        (when pending
          (remhash identifier (acp-connection-pending connection))
          (multiple-value-bind (error-object error-present-p)
              (gethash "error" message)
            (if (and error-present-p (not (json-null-p error-object)))
                (setf (acp-pending-request-failure pending)
                      (let ((code (json-get error-object "code")))
                        (make-condition 'acp-remote-error
                                        :method (acp-pending-request-method pending)
                                        :code (if (integerp code) code -32603)
                                        :message (let ((text (json-get error-object "message")))
                                                   (if (stringp text) text "unspecified error"))
                                        :data (json-get error-object "data"))))
                (setf (acp-pending-request-result pending) (json-get message "result"))))
          (setf (acp-pending-request-done-p pending) t)
          (condition-notify (acp-connection-condition connection)))))
    (unless pending
      (connection--log connection "Ignoring a response to unknown request ~A."
                       (bounded-diagnostic identifier))))
  nil)

(-> acp-request-cancelled-p () boolean)
(defun acp-request-cancelled-p ()
  "Return true when the peer cancelled the request the current thread is handling."
  (let ((request *acp-inbound-request*))
    (and request (acp-inbound-request-cancelled-p request) t)))

(-> acp-check-cancelled () null)
(defun acp-check-cancelled ()
  "Signal ACP-REQUEST-CANCELLED when the current request was cancelled."
  (when (acp-request-cancelled-p)
    (error 'acp-request-cancelled))
  nil)

(-> connection--cancel-inbound (acp-connection hash-table) null)
(defun connection--cancel-inbound (connection params)
  "Mark the inbound request named by cancellation PARAMS as cancelled."
  (let ((identifier (json-get params "requestId")))
    (with-lock-held ((acp-connection-lock connection))
      (let ((request (and (connection--identifier-p identifier)
                          (gethash identifier (acp-connection-inbound connection)))))
        (when request
          (setf (acp-inbound-request-cancelled-p request) t)))))
  nil)

(-> connection--serve-request (acp-connection acp-inbound-request t) null)
(defun connection--serve-request (connection request params)
  "Handle inbound REQUEST with PARAMS on the current thread and respond."
  (let ((*acp-inbound-request* request)
        (identifier (acp-inbound-request-identifier request)))
    (unwind-protect
         (handler-case
             (let ((result (peer-handle-request (acp-connection-peer connection)
                                                connection
                                                (acp-inbound-request-method request)
                                                params)))
               (connection--respond connection identifier result))
           (acp-method-error (condition)
             (connection--respond connection identifier nil
                                  :error-object (connection--error-object
                                                 (acp-method-error-code condition)
                                                 (acp-error-message condition)
                                                 (acp-method-error-data condition))))
           (error (condition)
             (connection--log connection "Request ~A failed: ~A"
                              (acp-inbound-request-method request) condition)
             (connection--respond connection identifier nil
                                  :error-object (connection--error-object
                                                 -32603
                                                 (bounded-diagnostic
                                                  (princ-to-string condition))))))
      (with-lock-held ((acp-connection-lock connection))
        (remhash identifier (acp-connection-inbound connection)))))
  nil)

(-> connection--dispatch-request (acp-connection hash-table) null)
(defun connection--dispatch-request (connection message)
  "Register the peer request in MESSAGE and serve it on a fresh thread."
  (let* ((identifier (json-get message "id"))
         (method (json-get message "method"))
         (params (json-get message "params"))
         (request (make-instance 'acp-inbound-request
                                 :identifier identifier
                                 :method method)))
    (with-lock-held ((acp-connection-lock connection))
      (setf (gethash identifier (acp-connection-inbound connection)) request))
    (make-thread (lambda ()
                   (connection--serve-request connection request params))
                 :name (format nil "~A ~A" (acp-connection-name connection) method)))
  nil)

(-> connection--dispatch-notification (acp-connection hash-table) null)
(defun connection--dispatch-notification (connection message)
  "Handle the notification in MESSAGE on the reader thread."
  (let ((method (json-get message "method"))
        (params (json-get message "params")))
    (if (string= method *acp-cancel-request-method*)
        (when (json-object-p params)
          (connection--cancel-inbound connection params))
        (handler-case
            (peer-handle-notification (acp-connection-peer connection)
                                      connection method params)
          (error (condition)
            (connection--log connection "Notification ~A failed: ~A" method condition)))))
  nil)

(-> connection--dispatch (acp-connection string) null)
(defun connection--dispatch (connection text)
  "Decode and route one message TEXT received from the peer."
  (let ((message (handler-case
                     (json-decode text)
                   (acp-error (condition)
                     (connection--respond connection ':null nil
                                          :error-object (connection--error-object
                                                         -32700
                                                         (acp-error-message condition)))
                     (return-from connection--dispatch nil)))))
    (cond
      ((not (json-object-p message))
       (connection--respond connection ':null nil
                            :error-object (connection--error-object
                                           -32600 "A JSON-RPC message must be an object.")))
      ((not (equal (json-get message "jsonrpc") "2.0"))
       (connection--respond connection
                            (if (connection--identifier-p (json-get message "id"))
                                (json-get message "id")
                                ':null)
                            nil
                            :error-object (connection--error-object
                                           -32600 "The jsonrpc member must be \"2.0\".")))
      ((stringp (json-get message "method"))
       (multiple-value-bind (identifier present-p)
           (gethash "id" message)
         (cond
           ((not present-p)
            (connection--dispatch-notification connection message))
           ((connection--identifier-p identifier)
            (connection--dispatch-request connection message))
           (t
            (connection--respond connection ':null nil
                                 :error-object (connection--error-object
                                                -32600 "The request id must be a string or integer."))))))
      ((and (nth-value 1 (gethash "id" message))
            (or (nth-value 1 (gethash "result" message))
                (nth-value 1 (gethash "error" message))))
       (connection--complete-pending connection message))
      (t
       (connection--respond connection ':null nil
                            :error-object (connection--error-object
                                           -32600 "The message is neither a request, a notification, nor a response.")))))
  nil)

(-> connection--reader-loop (acp-connection) null)
(defun connection--reader-loop (connection)
  "Read messages until the channel ends or fails, then close the connection."
  (let ((reason nil))
    (handler-case
        (loop
          (let ((text (handler-case
                          (channel-read-message (acp-connection-channel connection))
                        (acp-message-too-large (condition)
                          (connection--respond connection ':null nil
                                               :error-object (connection--error-object
                                                              -32700
                                                              (acp-error-message condition)))
                          ':skipped))))
            (cond
              ((eq text ':skipped)
               nil)
              ((null text)
               (return))
              (t
               (connection--dispatch connection text)))))
      (error (condition)
        (setf reason (format nil "The reader failed: ~A" condition))))
    (connection-close connection :reason reason))
  nil)


;;;; -- Closing --

(-> connection--join-reader (acp-connection) boolean)
(defun connection--join-reader (connection)
  "Wait up to *ACP-CLOSE-JOIN-SECONDS* for the reader thread, destroying a stuck one.

Return true when the reader has ended."
  (let ((thread (acp-connection-reader-thread connection)))
    (cond
      ((or (null thread) (eq thread (current-thread)))
       t)
      (t
       (let ((deadline (+ (get-internal-real-time)
                          (* *acp-close-join-seconds* internal-time-units-per-second))))
         (loop while (and (thread-alive-p thread)
                          (< (get-internal-real-time) deadline))
               do (sleep 0.01))
         (when (thread-alive-p thread)
           (ignore-errors (destroy-thread thread)))
         (unless (thread-alive-p thread)
           (ignore-errors (join-thread thread)))
         (not (thread-alive-p thread)))))))

(-> connection-close (acp-connection &key (:reason (or null string))) null)
(defun connection-close (connection &key reason)
  "Close CONNECTION, failing every pending request, and notify the peer object.

Closing an already closed connection does nothing."
  (let ((pending nil))
    (with-lock-held ((acp-connection-lock connection))
      (when (acp-connection-closed-p connection)
        (return-from connection-close nil))
      (setf (acp-connection-closed-p connection) t
            (acp-connection-close-reason connection) reason)
      (maphash (lambda (identifier request)
                 (declare (ignore identifier))
                 (push request pending))
               (acp-connection-pending connection))
      (clrhash (acp-connection-pending connection))
      (dolist (request pending)
        (setf (acp-pending-request-failure request)
              (make-condition 'acp-connection-closed
                              :message (format nil "The ACP connection closed before ~A was answered~@[: ~A~]."
                                               (acp-pending-request-method request)
                                               reason))
              (acp-pending-request-done-p request) t))
      (condition-notify (acp-connection-condition connection)))
    (ignore-errors (channel-close (acp-connection-channel connection)))
    (connection--join-reader connection)
    (handler-case
        (peer-connection-closed (acp-connection-peer connection) connection reason)
      (error (condition)
        (connection--log connection "The peer's close hook failed: ~A" condition))))
  nil)
