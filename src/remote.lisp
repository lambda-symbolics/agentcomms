(in-package #:agentcomms)

;;;; -- Remote Transport Policy --

;;; The remote transport of the ACP proposal "Streamable HTTP & WebSocket
;;; Transport": one endpoint takes POST, GET, and DELETE for Streamable HTTP and
;;; a GET with Upgrade: websocket for WebSocket. HTTP/2 arrives with prior
;;; knowledge or an h2c upgrade; HTTP/1.1 requests are served too, so clients
;;; without cleartext HTTP/2 can connect.

(defparameter *acp-remote-default-port* 8737
  "The port an ACP remote server listens on when none is given.")

(defparameter *acp-remote-path* "/acp"
  "The endpoint path of the ACP remote transport.")

(defparameter *acp-remote-initialize-seconds* 60
  "How long an initialize POST waits for the agent's response.")

(defparameter *acp-remote-keepalive-seconds* 15
  "How often an idle event stream carries a comment, which detects departed clients.")

(defparameter *acp-remote-idle-seconds* 300
  "How long an HTTP connection without event streams or requests lives.")

(defparameter *acp-remote-queue-limit* 10000
  "The most messages an HTTP connection holds for streams the client has not opened.")

(defparameter *acp-remote-session-response-methods*
  '("session/new" "session/load" "session/resume")
  "Methods whose responses travel on the connection stream although they may name a session.")


;;;; -- Exchanges --

(defclass acp-remote-exchange ()
  ((request
    :initarg       :request
    :reader        acp-remote-exchange-request
    :type          http-request
    :documentation "The request this exchange answers."))
  (:documentation "One HTTP request and its response, independent of the HTTP version."))

(defgeneric remote-exchange-respond (exchange status &key headers body)
  (:documentation "Send a complete response with STATUS, HEADERS, and the octet or string BODY."))

(defgeneric remote-exchange-start-stream (exchange status &key headers)
  (:documentation "Send STATUS and HEADERS of a response whose body follows in pieces."))

(defgeneric remote-exchange-send (exchange octets)
  (:documentation "Send OCTETS of a streamed body at once; signal ACP-CONNECTION-CLOSED when the client left."))

(defgeneric remote-exchange-finish (exchange)
  (:documentation "End a streamed body."))

(defclass acp-remote-http1-exchange (acp-remote-exchange)
  ((stream
    :initarg       :stream
    :reader        acp-remote-http1-exchange-stream
    :type          stream
    :documentation "The socket's octet stream.")
   (socket
    :initarg       :socket
    :reader        acp-remote-http1-exchange-socket
    :documentation "The USOCKET socket under the stream."))
  (:documentation "An exchange on an HTTP/1.1 connection; streamed bodies end with the connection."))

(defclass acp-remote-h2-exchange (acp-remote-exchange)
  ((h2-stream
    :initarg       :h2-stream
    :reader        acp-remote-h2-exchange-h2-stream
    :type          h2-stream
    :documentation "The HTTP/2 stream carrying the exchange."))
  (:documentation "An exchange on one HTTP/2 stream."))

(defmethod remote-exchange-respond ((exchange acp-remote-http1-exchange) status &key headers body)
  "Write a complete HTTP/1.1 response."
  (http1-write-response (acp-remote-http1-exchange-stream exchange) status :headers headers
                                                                           :body    body))

(defmethod remote-exchange-start-stream ((exchange acp-remote-http1-exchange) status &key headers)
  "Write the head of an HTTP/1.1 response whose body ends when the connection closes."
  (http1-write-response-head (acp-remote-http1-exchange-stream exchange) status
                             :headers (append headers '(("Connection" . "close")))))

(defmethod remote-exchange-send ((exchange acp-remote-http1-exchange) octets)
  "Write OCTETS to the HTTP/1.1 connection and flush them."
  (let ((stream (acp-remote-http1-exchange-stream exchange)))
    (handler-case
        (progn
          (write-sequence octets stream)
          (force-output stream))
      (error ()
        (error 'acp-connection-closed :message "The client closed the event stream."))))
  nil)

(defmethod remote-exchange-finish ((exchange acp-remote-http1-exchange))
  "End a streamed HTTP/1.1 body; the caller closes the connection."
  nil)

(defmethod remote-exchange-respond ((exchange acp-remote-h2-exchange) status &key headers body)
  "Send a complete HTTP/2 response."
  (h2-stream-respond (acp-remote-h2-exchange-h2-stream exchange) status
                     :headers (remote--h2-fields headers)
                     :body    (and body (http--body-octets body))))

(defmethod remote-exchange-start-stream ((exchange acp-remote-h2-exchange) status &key headers)
  "Send the HEADERS frame of a streamed HTTP/2 response."
  (h2-stream-send-headers (acp-remote-h2-exchange-h2-stream exchange)
                          (cons (cons ":status" (princ-to-string status))
                                (remote--h2-fields headers))))

(defmethod remote-exchange-send ((exchange acp-remote-h2-exchange) octets)
  "Send OCTETS as HTTP/2 data, waiting for flow-control credit."
  (handler-case (h2-stream-write-data (acp-remote-h2-exchange-h2-stream exchange) octets)
    (h2-error ()
      (error 'acp-connection-closed :message "The client closed the event stream.")))
  nil)

(defmethod remote-exchange-finish ((exchange acp-remote-h2-exchange))
  "End the HTTP/2 stream."
  (handler-case (h2-stream-finish (acp-remote-h2-exchange-h2-stream exchange))
    (h2-error ()
      nil))
  nil)

(-> remote--h2-fields (list) list)
(defun remote--h2-fields (headers)
  "Return HEADERS as HTTP/2 fields, with lower-case names and no connection-specific fields."
  (loop for (name . value) in headers
        for field = (string-downcase name)
        unless (member field '("connection" "keep-alive" "transfer-encoding" "upgrade") :test #'string=)
          collect (cons field value)))


;;;; -- Servers and HTTP Connections --

(defclass acp-remote-server ()
  ((serve-function
    :initarg       :serve-function
    :reader        acp-remote-server-serve-function
    :type          function
    :documentation "Called with an ACP channel for each client; serves it and returns when it ends.")
   (host
    :initarg       :host
    :reader        acp-remote-server-host
    :type          string
    :documentation "The address the server listens on.")
   (port
    :initarg       :port
    :accessor      acp-remote-server-port
    :type          (integer 0 65535)
    :documentation "The port the server listens on; the bound port once started.")
   (token
    :initarg       :token
    :initform      nil
    :reader        acp-remote-server-token
    :type          (or null string)
    :documentation "The bearer token every request must carry, or NIL for none.")
   (transports
    :initarg       :transports
    :reader        acp-remote-server-transports
    :type          list
    :documentation "The enabled transports, :HTTP and :WEBSOCKET.")
   (allowed-origins
    :initarg       :allowed-origins
    :initform      nil
    :reader        acp-remote-server-allowed-origins
    :type          list
    :documentation "Origin header values accepted; other requests with an Origin are refused.")
   (maximum-message-characters
    :initarg       :maximum-message-characters
    :initform      *acp-maximum-message-characters*
    :reader        acp-remote-server-maximum-message-characters
    :type          integer
    :documentation "The longest ACP message accepted from a client.")
   (log-function
    :initarg       :log-function
    :initform      nil
    :reader        acp-remote-server-log-function
    :type          (or null function)
    :documentation "An optional function called with a diagnostic line.")
   (listener
    :initform      nil
    :accessor      acp-remote-server--listener
    :documentation "The listening socket.")
   (threads
    :initform      nil
    :accessor      acp-remote-server--threads
    :type          list
    :documentation "The accept and idle-reaper threads.")
   (workers
    :initform      nil
    :accessor      acp-remote-server--workers
    :type          list
    :documentation "The socket, agent, and routing threads that may still run.")
   (sockets
    :initform      nil
    :accessor      acp-remote-server--sockets
    :type          list
    :documentation "The accepted sockets still open.")
   (connections
    :initform      (make-hash-table :test 'equal)
    :reader        acp-remote-server--connections
    :documentation "The Streamable HTTP connections by Acp-Connection-Id.")
   (stopped-p
    :initform      nil
    :accessor      acp-remote-server--stopped-p
    :type          boolean
    :documentation "Whether the server stopped.")
   (lock
    :initform      (bt2:make-lock :name "agentcomms remote server")
    :reader        acp-remote-server--lock
    :documentation "The lock guarding the server and its HTTP connections.")
   (condition
    :initform      (bt2:make-condition-variable :name "agentcomms remote server")
    :reader        acp-remote-server--condition
    :documentation "Broadcast after every change event streams and waiters watch."))
  (:documentation "An ACP agent endpoint for remote clients over HTTP and WebSocket."))

(defclass acp-remote-connection ()
  ((id
    :initarg       :id
    :reader        acp-remote-connection-id
    :type          string
    :documentation "The Acp-Connection-Id the client sends with each request.")
   (channel
    :initarg       :channel
    :reader        acp-remote-connection--channel
    :type          acp-channel
    :documentation "The transport's end of the channel pair the agent serves.")
   (slots
    :initform      (make-hash-table :test 'equal)
    :reader        acp-remote-connection--slots
    :documentation "The event streams by key: :CONNECTION or a session id.")
   (routes
    :initform      (make-hash-table :test 'equal)
    :reader        acp-remote-connection--routes
    :documentation "For each pending client request id, the key of the stream its response takes.")
   (waiters
    :initform      (make-hash-table :test 'equal)
    :reader        acp-remote-connection--waiters
    :documentation "Request ids whose response answers the POST itself, mapped to the response once it arrives.")
   (queued
    :initform      0
    :accessor      acp-remote-connection--queued
    :type          (integer 0 *)
    :documentation "Messages held for all streams.")
   (last-active
    :initform      (get-universal-time)
    :accessor      acp-remote-connection--last-active
    :type          integer
    :documentation "When the client last sent a request.")
   (closed-p
    :initform      nil
    :accessor      acp-remote-connection--closed-p
    :type          boolean
    :documentation "Whether the connection ended."))
  (:documentation "One Streamable HTTP client connection and the agent serving it."))

(defclass acp-remote-slot ()
  ((messages
    :initform      nil
    :accessor      acp-remote-slot--messages
    :type          list
    :documentation "Message texts waiting for the stream, oldest first.")
   (owner
    :initform      nil
    :accessor      acp-remote-slot--owner
    :documentation "The exchange currently streaming these messages, or NIL."))
  (:documentation "The messages for one event stream of an HTTP connection."))


;;;; -- Server Lifecycle --

(-> acp-remote-server-start
    (function &key (:host string) (:port (integer 0 65535)) (:token (or null string))
              (:transports list) (:allowed-origins list) (:maximum-message-characters integer)
              (:log-function (or null function)))
    acp-remote-server)
(defun acp-remote-server-start (serve-function &key (host "127.0.0.1") (port *acp-remote-default-port*)
                                                     token (transports '(:http :websocket))
                                                     allowed-origins
                                                     (maximum-message-characters *acp-maximum-message-characters*)
                                                     log-function)
  "Listen on HOST and PORT and serve ACP clients until ACP-REMOTE-SERVER-STOP; return the server.

SERVE-FUNCTION receives one channel per client connection, in its own thread,
and returns once that connection ends; ACP-AGENT-SERVE on a fresh agent is the
usual body. TRANSPORTS lists :HTTP, for Streamable HTTP over HTTP/1.1 and
HTTP/2 with WebSocket on the same endpoint, and :WEBSOCKET, for WebSocket
alone. With TOKEN, every request must carry \"Authorization: Bearer TOKEN\"
or a token=TOKEN query parameter. Requests with an Origin header outside
ALLOWED-ORIGINS are refused, so web pages cannot reach the agent."
  (let ((server (make-instance 'acp-remote-server
                               :serve-function             serve-function
                               :host                       host
                               :port                       port
                               :token                      token
                               :transports                 transports
                               :allowed-origins            allowed-origins
                               :maximum-message-characters maximum-message-characters
                               :log-function               log-function)))
    (let ((listener (handler-case (tcp-listen host port)
                      (error (cause)
                        (error 'acp-error
                               :message (format nil "Could not listen on ~A port ~D: ~A" host port cause))))))
      (setf (acp-remote-server--listener server) listener
            (acp-remote-server-port server)      (tcp-local-port listener)))
    (setf (acp-remote-server--threads server)
          (list (bt2:make-thread (lambda () (remote--accept-loop server))
                                 :name "agentcomms remote accept")
                (bt2:make-thread (lambda () (remote--reap-loop server))
                                 :name "agentcomms remote reaper")))
    server))

(-> acp-remote-server-stop (acp-remote-server) null)
(defun acp-remote-server-stop (server)
  "Stop listening, end every client connection, and wait until every client is served out."
  (let ((sockets     nil)
        (connections nil))
    (bt2:with-lock-held ((acp-remote-server--lock server))
      (unless (acp-remote-server--stopped-p server)
        (setf (acp-remote-server--stopped-p server) t
              sockets                              (acp-remote-server--sockets server)
              connections                          (loop for connection being the hash-values
                                                           of (acp-remote-server--connections server)
                                                         collect connection)))
      (bt2:condition-broadcast (acp-remote-server--condition server)))
    (dolist (connection connections)
      (remote--end-connection server connection))
    (dolist (socket sockets)
      (remote--shut-down-socket socket))
    (dolist (thread (acp-remote-server--threads server))
      (bt2:join-thread thread))
    ;; Agents release their sessions as their channels end; wait for them.
    (dolist (thread (bt2:with-lock-held ((acp-remote-server--lock server))
                      (acp-remote-server--workers server)))
      (bt2:join-thread thread)))
  nil)

(-> remote--spawn (acp-remote-server function string) null)
(defun remote--spawn (server function name)
  "Run FUNCTION in a new thread named NAME that ACP-REMOTE-SERVER-STOP waits for."
  (bt2:with-lock-held ((acp-remote-server--lock server))
    (setf (acp-remote-server--workers server)
          (cons (bt2:make-thread function :name name)
                (remove-if-not #'bt2:thread-alive-p (acp-remote-server--workers server)))))
  nil)

(-> acp-remote-server-url (acp-remote-server &key (:scheme string)) string)
(defun acp-remote-server-url (server &key (scheme "http"))
  "Return the endpoint URL of SERVER with SCHEME, http or ws."
  (let ((host (acp-remote-server-host server)))
    (format nil "~A://~:[~A~;[~A]~]:~D~A" scheme (find #\: host) host
            (acp-remote-server-port server) *acp-remote-path*)))

(-> remote--log (acp-remote-server string &rest t) null)
(defun remote--log (server control &rest arguments)
  "Pass the line CONTROL formatted with ARGUMENTS to SERVER's log function, when there is one."
  (let ((function (acp-remote-server-log-function server)))
    (when function
      (funcall function (apply #'format nil control arguments))))
  nil)

(-> remote--shut-down-socket (t) null)
(defun remote--shut-down-socket (socket)
  "Shut SOCKET down in both directions, waking threads blocked reading it."
  (handler-case (socket-shutdown socket ':io)
    (error ()
      nil))
  nil)

(-> remote--accept-loop (acp-remote-server) null)
(defun remote--accept-loop (server)
  "Accept sockets until the server stops, serving each in its own thread, then close the listener.

Waiting with a timeout lets the loop notice the stop: closing a listening
socket does not wake a thread blocked accepting on it."
  (let ((listener (acp-remote-server--listener server)))
    (unwind-protect
         (loop until (bt2:with-lock-held ((acp-remote-server--lock server))
                       (acp-remote-server--stopped-p server))
               do (when (wait-for-input listener :timeout 1 :ready-only t)
                    (let ((socket (handler-case (tcp-accept listener)
                                    (error ()
                                      nil))))
                      (when socket
                        (bt2:with-lock-held ((acp-remote-server--lock server))
                          (push socket (acp-remote-server--sockets server)))
                        (remote--spawn server (lambda () (remote--serve-socket server socket))
                                       "agentcomms remote socket")))))
      (handler-case (close-tcp listener)
        (error ()
          nil))))
  nil)

(-> remote--reap-loop (acp-remote-server) null)
(defun remote--reap-loop (server)
  "End HTTP connections that stayed idle too long, until the server stops."
  (loop
    (let ((idle nil))
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (bt2:condition-wait (acp-remote-server--condition server) (acp-remote-server--lock server)
                            :timeout 30)
        (when (acp-remote-server--stopped-p server)
          (return))
        (setf idle (loop with now = (get-universal-time)
                         for connection being the hash-values of (acp-remote-server--connections server)
                         when (and (> (- now (acp-remote-connection--last-active connection))
                                      *acp-remote-idle-seconds*)
                                   (loop for slot being the hash-values
                                           of (acp-remote-connection--slots connection)
                                         never (acp-remote-slot--owner slot)))
                           collect connection)))
      (dolist (connection idle)
        (remote--log server "Ending idle ACP connection ~A." (acp-remote-connection-id connection))
        (remote--end-connection server connection))))
  nil)


;;;; -- Sockets --

(-> remote--serve-socket (acp-remote-server t) null)
(defun remote--serve-socket (server socket)
  "Serve the requests on SOCKET, switching to HTTP/2 or WebSocket when the client asks."
  (let ((stream (tcp-stream socket)))
    (unwind-protect
         (handler-case
             (loop
               (let ((request (handler-case
                                  (http1-read-request
                                   stream
                                   :maximum-body-octets (* 4 (acp-remote-server-maximum-message-characters
                                                              server)))
                                (http-request-error (condition)
                                  (http1-write-response stream (http-request-error-status condition)
                                                        :headers (append (http-request-error-headers condition)
                                                                         '(("Connection" . "close")))
                                                        :body    (acp-error-message condition))
                                  (return)))))
                 (cond
                   ((null request)
                    (return))
                   ((remote--h2-preface-p request)
                    (when (member ':http (acp-remote-server-transports server))
                      (remote--serve-h2 server socket :preface ':after-request-line))
                    (return))
                   ((and (member ':http (acp-remote-server-transports server))
                         (http-request-header-token-p request "upgrade" "h2c")
                         (http-request-header-token-p request "connection" "http2-settings")
                         (http-request-header request "http2-settings"))
                    (http1-write-response-head stream 101 :headers '(("Connection" . "Upgrade")
                                                                     ("Upgrade" . "h2c")))
                    (remote--serve-h2 server socket :upgrade-request request)
                    (return))
                   (t
                    (unless (and (remote--handle server (make-instance 'acp-remote-http1-exchange
                                                                       :request request
                                                                       :stream  stream
                                                                       :socket  socket))
                                 (http-request-keep-alive-p request))
                      (return))))))
           (error (condition)
             (remote--log server "ACP remote socket failed: ~A" condition)))
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (setf (acp-remote-server--sockets server)
              (remove socket (acp-remote-server--sockets server))))
      (remote--shut-down-socket socket)
      (handler-case (close-tcp socket)
        (error ()
          nil))))
  nil)

(-> remote--h2-preface-p (http-request) boolean)
(defun remote--h2-preface-p (request)
  "Return whether REQUEST is the first line of the HTTP/2 connection preface."
  (and (string= (http-request-method request) "PRI")
       (string= (http-request-target request) "*")
       (string= (http-request-version request) "HTTP/2.0")
       t))

(-> remote--serve-h2 (acp-remote-server t &key (:preface keyword) (:upgrade-request (or null http-request)))
    null)
(defun remote--serve-h2 (server socket &key (preface ':full) upgrade-request)
  "Serve HTTP/2 on SOCKET until the client leaves, after its preface line or an h2c upgrade."
  (let ((connection (make-h2-server-connection
                     (tcp-stream socket)
                     :handler           (lambda (h2-stream) (remote--handle-h2-stream server h2-stream))
                     :preface           preface
                     :shutdown-function (lambda () (remote--shut-down-socket socket))
                     :log-function      (lambda (line) (remote--log server "~A" line)))))
    (if upgrade-request
        (h2-connection-serve connection
                             :upgrade-headers  (remote--upgrade-fields upgrade-request)
                             :upgrade-body     (http-request-body upgrade-request)
                             :upgrade-settings (http-request-header upgrade-request "http2-settings"))
        (h2-connection-serve connection)))
  nil)

(-> remote--upgrade-fields (http-request) list)
(defun remote--upgrade-fields (request)
  "Return the HTTP/1.1 REQUEST as HTTP/2 fields for an h2c upgrade."
  (append (list (cons ":method" (http-request-method request))
                (cons ":scheme" "http")
                (cons ":authority" (or (http-request-header request "host") ""))
                (cons ":path" (http-request-target request)))
          (loop for (name . value) in (http-request-headers request)
                unless (member name '("host" "connection" "upgrade" "http2-settings" "keep-alive"
                                      "proxy-connection" "transfer-encoding" "te")
                               :test #'string=)
                  collect (cons name value))))

(-> remote--handle-h2-stream (acp-remote-server h2-stream) null)
(defun remote--handle-h2-stream (server h2-stream)
  "Handle one HTTP/2 request stream."
  (let ((exchange nil))
    (handler-case
        (let* ((fields  (h2-stream-headers h2-stream))
               (body    (h2-stream-read-body h2-stream
                                             :limit (* 4 (acp-remote-server-maximum-message-characters
                                                          server))))
               (request (make-http-request
                         :method  (h2-stream-method h2-stream)
                         :target  (h2-stream-path h2-stream)
                         :version "HTTP/2"
                         :headers (cons (cons "host" (or (h2-stream-authority h2-stream) ""))
                                        (loop for field in fields
                                              unless (char= (char (first field) 0) #\:)
                                                collect field))
                         :body    body)))
          (setf exchange (make-instance 'acp-remote-h2-exchange :request   request
                                                                :h2-stream h2-stream))
          (remote--handle server exchange))
      (http-request-error (condition)
        (h2-stream-respond h2-stream (http-request-error-status condition)
                           :body (string-to-octets (acp-error-message condition) :encoding ':utf-8)))
      (h2-error ()
        nil)))
  nil)


;;;; -- Request Routing --

(-> remote--handle (acp-remote-server acp-remote-exchange) boolean)
(defun remote--handle (server exchange)
  "Answer the request of EXCHANGE; return whether the HTTP/1.1 connection may carry another."
  (let* ((request    (acp-remote-exchange-request exchange))
         (method     (http-request-method request))
         (transports (acp-remote-server-transports server))
         (http-p     (member ':http transports))
         (websocket  (and (member ':websocket transports)
                          (typep exchange 'acp-remote-http1-exchange)
                          (websocket-upgrade-request-p request))))
    (block nil
      (unless (string= (http-request-path request) *acp-remote-path*)
        (remote--respond exchange 404 "There is no ACP endpoint at this path.")
        (return t))
      (let ((origin (http-request-header request "origin")))
        (when (and origin (not (member origin (acp-remote-server-allowed-origins server) :test #'string=)))
          (remote--respond exchange 403 "Requests from web pages are refused.")
          (return t)))
      (unless (remote--authorized-p server request)
        (remote--respond exchange 401 "The request lacks the bearer token."
                         :headers '(("WWW-Authenticate" . "Bearer")))
        (return t))
      (cond
        (websocket
         (remote--serve-websocket server exchange)
         nil)
        ((not http-p)
         (if (string= method "GET")
             (remote--respond exchange 426 "Connect with a WebSocket upgrade."
                              :headers '(("Upgrade" . "websocket")))
             (remote--respond exchange 405 "Connect with a WebSocket upgrade."
                              :headers '(("Allow" . "GET"))))
         t)
        ((string= method "POST")
         (remote--post server exchange)
         t)
        ((string= method "GET")
         (remote--open-stream server exchange)
         nil)
        ((string= method "DELETE")
         (remote--delete server exchange)
         t)
        (t
         (remote--respond exchange 405 "The ACP endpoint takes GET, POST, and DELETE."
                          :headers '(("Allow" . "GET, POST, DELETE")))
         t)))))

(-> remote--respond (acp-remote-exchange (integer 100 999) string &key (:headers list)) null)
(defun remote--respond (exchange status message &key headers)
  "Answer EXCHANGE with STATUS and the plain-text MESSAGE."
  (remote-exchange-respond exchange status
                           :headers (cons (cons "Content-Type" "text/plain; charset=utf-8") headers)
                           :body    (format nil "~A~%" message))
  nil)

(-> remote--authorized-p (acp-remote-server http-request) boolean)
(defun remote--authorized-p (server request)
  "Return whether REQUEST carries the server's token, or the server needs none."
  (let ((token (acp-remote-server-token server)))
    (or (null token)
        (let ((authorization (http-request-header request "authorization"))
              (query         (rest (assoc "token" (http-request-query request) :test #'string=))))
          (or (and authorization
                   (> (length authorization) 7)
                   (string-equal "Bearer " authorization :end2 7)
                   (remote--same-secret-p (subseq authorization 7) token))
              (and query (remote--same-secret-p query token))))
        nil)))

(-> remote--same-secret-p (string string) boolean)
(defun remote--same-secret-p (given expected)
  "Return whether the secret GIVEN equals EXPECTED, taking time independent of where they differ."
  (and (constant-time-equal (string-to-octets given :encoding ':utf-8)
                            (string-to-octets expected :encoding ':utf-8))
       t))

(-> remote--serve-websocket (acp-remote-server acp-remote-http1-exchange) null)
(defun remote--serve-websocket (server exchange)
  "Accept the WebSocket upgrade of EXCHANGE and serve one ACP connection over it."
  (let ((websocket (handler-case
                       (websocket-accept (acp-remote-http1-exchange-stream exchange)
                                         (acp-remote-exchange-request exchange)
                                         :headers (list (cons "Acp-Connection-Id" (remote--new-id)))
                                         :socket  (acp-remote-http1-exchange-socket exchange))
                     (http-request-error (condition)
                       (remote--respond exchange (http-request-error-status condition)
                                        (acp-error-message condition)
                                        :headers (http-request-error-headers condition))
                       nil))))
    (when websocket
      (funcall (acp-remote-server-serve-function server)
               (make-acp-websocket-channel
                websocket
                :maximum-message-characters (acp-remote-server-maximum-message-characters server)))))
  nil)

(-> remote--new-id () string)
(defun remote--new-id ()
  "Return a fresh random connection identifier."
  (format nil "~(~{~2,'0x~}~)" (coerce (random-data 16) 'list)))


;;;; -- Streamable HTTP --

(-> remote--connection (acp-remote-server acp-remote-exchange) (or null acp-remote-connection))
(defun remote--connection (server exchange)
  "Return the live connection EXCHANGE names, answering 400 or 404 and returning NIL otherwise."
  (let ((id (http-request-header (acp-remote-exchange-request exchange) "acp-connection-id")))
    (if (null id)
        (progn
          (remote--respond exchange 400 "The request needs an Acp-Connection-Id header.")
          nil)
        (let ((connection (bt2:with-lock-held ((acp-remote-server--lock server))
                            (gethash id (acp-remote-server--connections server)))))
          (unless connection
            (remote--respond exchange 404 "The ACP connection is unknown or ended."))
          connection))))

(-> remote--post (acp-remote-server acp-remote-exchange) null)
(defun remote--post (server exchange)
  "Deliver the JSON-RPC message a client POSTs."
  (let* ((request      (acp-remote-exchange-request exchange))
         (content-type (or (http-request-header request "content-type") "")))
    (block nil
      (unless (search "application/json" content-type :test #'char-equal)
        (remote--respond exchange 415 "The body must be application/json.")
        (return))
      (multiple-value-bind (text message) (remote--decode-message server (http-request-body request))
        (cond
          ((null text)
           (remote--respond exchange 400 "The body is not one JSON-RPC message."))
          ((vectorp message)
           (remote--respond exchange 501 "Batch requests are not supported."))
          ((not (http-request-header request "acp-connection-id"))
           (remote--initialize server exchange :text text :message message))
          (t
           (let ((connection (remote--connection server exchange)))
             (when connection
               (remote--deliver server exchange connection :text text :message message))))))))
  nil)

(-> remote--decode-message (acp-remote-server (vector (unsigned-byte 8))) (values (or null string) t))
(defun remote--decode-message (server octets)
  "Return the message text of OCTETS on one line and its decoded JSON, or NIL."
  (handler-case
      (let* ((text  (octets-to-string octets :encoding ':utf-8))
             (value (json-decode text :limit (acp-remote-server-maximum-message-characters server))))
        (if (or (hash-table-p value) (vectorp value))
            (values (json-encode value :limit (acp-remote-server-maximum-message-characters server)) value)
            (values nil nil)))
    (error ()
      (values nil nil))))

(-> remote--initialize (acp-remote-server acp-remote-exchange &key (:text string) (:message hash-table)) null)
(defun remote--initialize (server exchange &key text message)
  "Open a connection for the initialize request MESSAGE and answer with the agent's response."
  (let ((id (gethash "id" message)))
    (unless (and (equal (json-get message "method") "initialize") id)
      (remote--respond exchange 400 "A request without Acp-Connection-Id must be initialize.")
      (return-from remote--initialize nil))
    (multiple-value-bind (connection agent-channel) (remote--open-connection server)
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (setf (gethash id (acp-remote-connection--waiters connection)) ':pending))
      (remote--spawn server (lambda () (remote--run-agent server connection agent-channel))
                     "agentcomms remote agent")
      (remote--spawn server (lambda () (remote--pump server connection))
                     "agentcomms remote pump")
      (handler-case (channel-write-message (acp-remote-connection--channel connection) text)
        (acp-error ()
          nil))
      (let ((response nil))
        (bt2:with-lock-held ((acp-remote-server--lock server))
          (let ((deadline (+ (get-universal-time) *acp-remote-initialize-seconds*)))
            (loop while (and (eq (gethash id (acp-remote-connection--waiters connection)) ':pending)
                             (not (acp-remote-connection--closed-p connection))
                             (< (get-universal-time) deadline))
                  do (bt2:condition-wait (acp-remote-server--condition server)
                                         (acp-remote-server--lock server)
                                         :timeout 1)))
          (let ((value (gethash id (acp-remote-connection--waiters connection))))
            (unless (eq value ':pending)
              (setf response value)))
          (remhash id (acp-remote-connection--waiters connection)))
        (if response
            (remote-exchange-respond exchange 200
                                     :headers (list (cons "Content-Type" "application/json")
                                                    (cons "Acp-Connection-Id" (acp-remote-connection-id connection)))
                                     :body    response)
            (progn
              (remote--end-connection server connection)
              (remote--respond exchange 504 "The agent did not answer initialize."))))))
  nil)

(-> remote--open-connection (acp-remote-server) (values acp-remote-connection acp-channel))
(defun remote--open-connection (server)
  "Register a new HTTP connection; return it and the channel end its agent serves."
  (multiple-value-bind (agent-channel transport-channel)
      (make-acp-channel-pair :maximum-message-characters (acp-remote-server-maximum-message-characters server))
    (let ((connection (make-instance 'acp-remote-connection :id      (remote--new-id)
                                                            :channel transport-channel)))
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (setf (gethash (acp-remote-connection-id connection) (acp-remote-server--connections server))
              connection))
      (values connection agent-channel))))

(-> remote--run-agent (acp-remote-server acp-remote-connection acp-channel) null)
(defun remote--run-agent (server connection channel)
  "Serve CONNECTION's agent on CHANNEL, ending the connection when the agent stops."
  (unwind-protect
       (handler-case (funcall (acp-remote-server-serve-function server) channel)
         (error (condition)
           (remote--log server "The ACP agent of ~A failed: ~A" (acp-remote-connection-id connection) condition)))
    (channel-close channel)
    (remote--end-connection server connection))
  nil)

(-> remote--deliver (acp-remote-server acp-remote-exchange acp-remote-connection &key (:text string) (:message hash-table))
    null)
(defun remote--deliver (server exchange connection &key text message)
  "Pass a client MESSAGE on an established CONNECTION to its agent and answer 202."
  (let* ((method     (json-get message "method"))
         (id         (gethash "id" message))
         (session-id (json-get (json-get message "params") "sessionId"))
         (header     (http-request-header (acp-remote-exchange-request exchange) "acp-session-id")))
    (block nil
      (when (equal method "initialize")
        (remote--respond exchange 400 "The connection is already initialized.")
        (return))
      (when (and header (stringp session-id) (string/= header session-id))
        (remote--respond exchange 400 "Acp-Session-Id differs from the message's sessionId.")
        (return))
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (setf (acp-remote-connection--last-active connection) (get-universal-time))
        (when (and method id)
          (setf (gethash id (acp-remote-connection--routes connection))
                (if (and (stringp session-id)
                         (not (member method *acp-remote-session-response-methods* :test #'string=)))
                    session-id
                    ':connection))))
      (handler-case
          (progn
            (channel-write-message (acp-remote-connection--channel connection) text)
            (remote-exchange-respond exchange 202))
        (acp-error ()
          (remote--respond exchange 404 "The ACP connection ended.")))))
  nil)

(-> remote--delete (acp-remote-server acp-remote-exchange) null)
(defun remote--delete (server exchange)
  "End the connection a client DELETEs."
  (let ((connection (remote--connection server exchange)))
    (when connection
      (remote--end-connection server connection)
      (remote-exchange-respond exchange 202)))
  nil)

(-> remote--end-connection (acp-remote-server acp-remote-connection) null)
(defun remote--end-connection (server connection)
  "End CONNECTION: close the agent's channel and finish its event streams."
  (let ((close-p nil))
    (bt2:with-lock-held ((acp-remote-server--lock server))
      (unless (acp-remote-connection--closed-p connection)
        (setf (acp-remote-connection--closed-p connection) t
              close-p                                      t)
        (remhash (acp-remote-connection-id connection) (acp-remote-server--connections server)))
      (bt2:condition-broadcast (acp-remote-server--condition server)))
    (when close-p
      (channel-close (acp-remote-connection--channel connection))))
  nil)


;;;; -- Event Streams --

(-> remote--pump (acp-remote-server acp-remote-connection) null)
(defun remote--pump (server connection)
  "Route each message the agent sends to the waiting POST or the event stream it belongs to."
  (let ((channel (acp-remote-connection--channel connection)))
    (handler-case
        (loop for text = (channel-read-message channel)
              while text
              do (remote--route server connection text))
      (error (condition)
        (remote--log server "Routing ACP messages for ~A failed: ~A"
                     (acp-remote-connection-id connection) condition))))
  (remote--end-connection server connection)
  nil)

(-> remote--route (acp-remote-server acp-remote-connection string) null)
(defun remote--route (server connection text)
  "Queue the agent's message TEXT for its stream, or hand it to the POST waiting for it."
  (let* ((message    (json-decode text))
         (method     (json-get message "method"))
         (id         (gethash "id" message))
         (overflow-p nil))
    (bt2:with-lock-held ((acp-remote-server--lock server))
      (let ((waiters (acp-remote-connection--waiters connection))
            (routes  (acp-remote-connection--routes connection)))
        (if (and (null method) id (eq (gethash id waiters) ':pending))
            (setf (gethash id waiters) text)
            (let* ((key  (cond
                           (method
                            (let ((session-id (json-get (json-get message "params") "sessionId")))
                              (if (stringp session-id) session-id ':connection)))
                           (t
                            (prog1 (gethash id routes ':connection)
                              (remhash id routes)))))
                   (slot (remote--slot connection key)))
              (setf (acp-remote-slot--messages slot)
                    (nconc (acp-remote-slot--messages slot) (list text)))
              (when (> (incf (acp-remote-connection--queued connection)) *acp-remote-queue-limit*)
                (setf overflow-p t)))))
      (bt2:condition-broadcast (acp-remote-server--condition server)))
    (when overflow-p
      (remote--log server "ACP connection ~A queued too many messages for unopened streams."
                   (acp-remote-connection-id connection))
      (remote--end-connection server connection)))
  nil)

(-> remote--slot (acp-remote-connection (or keyword string)) acp-remote-slot)
(defun remote--slot (connection key)
  "Return the event stream slot for KEY, creating it; hold the server lock."
  (let ((slots (acp-remote-connection--slots connection)))
    (or (gethash key slots)
        (setf (gethash key slots) (make-instance 'acp-remote-slot)))))

(-> remote--open-stream (acp-remote-server acp-remote-exchange) null)
(defun remote--open-stream (server exchange)
  "Stream the messages for a connection or one of its sessions as Server-Sent Events."
  (let ((request (acp-remote-exchange-request exchange)))
    (block nil
      (unless (search "text/event-stream" (or (http-request-header request "accept") "") :test #'char-equal)
        (remote--respond exchange 406 "An event stream needs Accept: text/event-stream.")
        (return))
      (let* ((connection (or (remote--connection server exchange)
                             (return)))
             (key        (or (http-request-header request "acp-session-id") ':connection))
             (slot       (bt2:with-lock-held ((acp-remote-server--lock server))
                           (let ((slot (remote--slot connection key)))
                             (setf (acp-remote-slot--owner slot) exchange)
                             (bt2:condition-broadcast (acp-remote-server--condition server))
                             slot))))
        (unwind-protect
             (handler-case
                 (progn
                   (remote-exchange-start-stream
                    exchange 200 :headers (list (cons "Content-Type" "text/event-stream")
                                                (cons "Cache-Control" "no-cache")
                                                (cons "Acp-Connection-Id" (acp-remote-connection-id connection))))
                   (remote--stream-events server connection :slot slot :exchange exchange)
                   (remote-exchange-finish exchange))
               (acp-connection-closed ()
                 nil))
          (bt2:with-lock-held ((acp-remote-server--lock server))
            (when (eq (acp-remote-slot--owner slot) exchange)
              (setf (acp-remote-slot--owner slot) nil)))))))
  nil)

(-> remote--stream-events (acp-remote-server acp-remote-connection &key (:slot acp-remote-slot) (:exchange acp-remote-exchange))
    null)
(defun remote--stream-events (server connection &key slot exchange)
  "Send SLOT's messages on EXCHANGE until the connection ends or another stream replaces it."
  (loop
    (let ((messages nil))
      (bt2:with-lock-held ((acp-remote-server--lock server))
        (loop until (or (acp-remote-slot--messages slot)
                        (acp-remote-connection--closed-p connection)
                        (not (eq (acp-remote-slot--owner slot) exchange))
                        (not (bt2:condition-wait (acp-remote-server--condition server)
                                                 (acp-remote-server--lock server)
                                                 :timeout *acp-remote-keepalive-seconds*))))
        (when (or (acp-remote-connection--closed-p connection)
                  (not (eq (acp-remote-slot--owner slot) exchange)))
          (return))
        (setf messages                                 (acp-remote-slot--messages slot)
              (acp-remote-slot--messages slot)         nil)
        (decf (acp-remote-connection--queued connection) (length messages)))
      (handler-case
          (remote-exchange-send exchange
                                (string-to-octets
                                 (if messages
                                     (format nil "~{data: ~A~%~%~}" messages)
                                     (format nil ": keepalive~%~%"))
                                 :encoding ':utf-8))
        (acp-connection-closed (condition)
          ;; A later stream for the same key delivers what this one could not.
          (bt2:with-lock-held ((acp-remote-server--lock server))
            (setf (acp-remote-slot--messages slot)
                  (append messages (acp-remote-slot--messages slot)))
            (incf (acp-remote-connection--queued connection) (length messages)))
          (error condition)))))
  nil)
