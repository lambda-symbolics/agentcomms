(in-package #:agentcomms)

;;;; -- Remote Server Fixtures --

(-> test-call-with-remote-server (function &key (:transports list) (:token (or null string))) t)
(defun test-call-with-remote-server (function &key (transports '(:http :websocket)) token)
  "Serve a fresh tooling agent per client on a loopback port and call FUNCTION with the server."
  (let ((server (acp-remote-server-start (lambda (channel)
                                           (acp-agent-serve (make-instance 'test-tooling-agent) channel))
                                         :port       0
                                         :transports transports
                                         :token      token)))
    (unwind-protect (funcall function server)
      (acp-remote-server-stop server))))

(-> test-remote-websocket-url (acp-remote-server &key (:query (or null string))) string)
(defun test-remote-websocket-url (server &key query)
  "Return the WebSocket URL of SERVER with an optional QUERY."
  (format nil "~A~@[?~A~]" (acp-remote-server-url server :scheme "ws") query))

(-> test-remote-websocket-session (acp-channel) null)
(defun test-remote-websocket-session (channel)
  "Run one prompt through a client on CHANNEL and check the agent's reply."
  (let ((client (make-instance 'test-client)))
    (acp-client-connect client channel :name "remote client")
    (unwind-protect
         (progn
           (client-initialize client)
           (test-equal "sess-1" (client-new-session client "/w"))
           (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "hello"))))
           (test-equal '("Hi there") (test-client-texts client)))
      (connection-close (acp-client-connection client))))
  nil)

(-> test-h2-client (acp-remote-server) llh2:h2-connection)
(defun test-h2-client (server)
  "Return an HTTP/2 client connection to SERVER."
  (let ((socket (tcp-connect "127.0.0.1" (acp-remote-server-port server))))
    (llh2:make-h2-client-connection (tcp-stream socket)
                                    :shutdown-function (lambda () (socket-shutdown socket ':io)))))

(-> test-h2-post (llh2:h2-connection hash-table &key (:headers list)) llh2:h2-stream)
(defun test-h2-post (client message &key headers)
  "POST the JSON-RPC MESSAGE to the ACP endpoint and return the stream once answered."
  (let ((stream (llh2:h2-request client
                                 :method  "POST"
                                 :path    *acp-remote-path*
                                 :headers (cons '("content-type" . "application/json") headers)
                                 :body    (string-to-octets (json-encode message) :encoding ':utf-8))))
    (llh2:h2-stream-wait-headers stream :timeout 30)
    stream))

(-> test-h2-events (llh2:h2-connection &key (:headers list)) llh2:h2-stream)
(defun test-h2-events (client &key headers)
  "Open an event stream on the ACP endpoint with HEADERS and return it once answered."
  (let ((stream (llh2:h2-request client :path    *acp-remote-path*
                                        :headers (cons '("accept" . "text/event-stream") headers))))
    (llh2:h2-stream-wait-headers stream :timeout 30)
    stream))

(defclass test-event-reader ()
  ((stream
    :initarg       :stream
    :reader        test-event-reader-stream
    :documentation "The HTTP/2 stream carrying Server-Sent Events.")
   (pending
    :initform      ""
    :accessor      test-event-reader-pending
    :type          string
    :documentation "Received text not yet split into events."))
  (:documentation "Splits the Server-Sent Events of one HTTP/2 stream into decoded messages."))

(-> test-next-event (test-event-reader) (or null hash-table))
(defun test-next-event (reader)
  "Return the next data event of READER as decoded JSON, skipping comments, or NIL at the end."
  (loop
    (let ((end (search (format nil "~%~%") (test-event-reader-pending reader))))
      (if end
          (let ((event (subseq (test-event-reader-pending reader) 0 end)))
            (setf (test-event-reader-pending reader) (subseq (test-event-reader-pending reader) (+ end 2)))
            (when (and (> (length event) 6) (string= "data: " event :end2 6))
              (return (json-decode (subseq event 6)))))
          (let ((chunk (llh2:h2-stream-read-data (test-event-reader-stream reader) :timeout 30)))
            (unless chunk
              (return nil))
            (setf (test-event-reader-pending reader)
                  (concatenate 'string (test-event-reader-pending reader)
                               (octets-to-string chunk :encoding ':utf-8))))))))

(-> test-request-message (integer string &rest t) hash-table)
(defun test-request-message (id method &rest params)
  "Return a JSON-RPC request with ID, METHOD, and the alternating keys and values of PARAMS."
  (json-object "jsonrpc" "2.0" "id" id "method" method "params" (apply #'json-object params)))

(-> test-http1-exchange (acp-remote-server string) (values integer list string))
(defun test-http1-exchange (server text)
  "Send the raw HTTP/1.1 request TEXT to SERVER; return the status, headers, and body of the answer."
  (let ((socket (tcp-connect "127.0.0.1" (acp-remote-server-port server))))
    (unwind-protect
         (let ((stream (tcp-stream socket)))
           (write-sequence (string-to-octets text :encoding ':utf-8) stream)
           (force-output stream)
           (socket-shutdown socket ':output)
           (let* ((octets (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                                      :adjustable   t
                                                      :fill-pointer 0)))
                            (loop for octet = (read-byte stream nil nil)
                                  while octet
                                  do (vector-push-extend octet buffer))
                            buffer))
                  (text   (octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                            :encoding ':utf-8))
                  (split  (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) text))
                  (lines  (test-split-lines (subseq text 0 split))))
             (values (parse-integer (first lines) :start 9 :end 12)
                     (loop for line in (rest lines)
                           for colon = (position #\: line)
                           collect (cons (string-downcase (subseq line 0 colon))
                                         (string-trim " " (subseq line (1+ colon)))))
                     (subseq text (+ split 4)))))
      (close-tcp socket))))

(-> test-split-lines (string) list)
(defun test-split-lines (text)
  "Return the CRLF-separated lines of TEXT."
  (loop with start = 0
        for end = (search (format nil "~C~C" #\Return #\Newline) text :start2 start)
        collect (subseq text start end)
        while end
        do (setf start (+ end 2))))

(-> test-http1-request (string &key (:method string) (:headers list) (:body string)) string)
(defun test-http1-request (path &key (method "GET") headers (body ""))
  "Return the text of an HTTP/1.1 request for PATH with HEADERS and BODY that closes its connection."
  (format nil "~A ~A HTTP/1.1~C~CHost: 127.0.0.1~C~CConnection: close~C~C~{~A: ~A~C~C~}Content-Length: ~D~C~C~C~C~A"
          method path #\Return #\Newline #\Return #\Newline #\Return #\Newline
          (loop for (name . value) in headers
                append (list name value #\Return #\Newline))
          (length (string-to-octets body :encoding ':utf-8))
          #\Return #\Newline #\Return #\Newline body))


;;;; -- WebSocket Transport Tests --

(define-test remote-websocket-session
  (test-call-with-remote-server
   (lambda (server)
     (test-remote-websocket-session (acp-websocket-connect (test-remote-websocket-url server))))
   :transports '(:websocket)))

(define-test remote-websocket-token
  (test-call-with-remote-server
   (lambda (server)
     (test-equal 401 (websocket-handshake-error-status
                      (test-signals websocket-handshake-error
                        (acp-websocket-connect (test-remote-websocket-url server)))))
     (test-equal 401 (websocket-handshake-error-status
                      (test-signals websocket-handshake-error
                        (acp-websocket-connect (test-remote-websocket-url server :query "token=wrong")))))
     (test-remote-websocket-session
      (acp-websocket-connect (test-remote-websocket-url server :query "token=s3cret")))
     (test-remote-websocket-session
      (acp-websocket-connect (test-remote-websocket-url server)
                             :headers '(("Authorization" . "Bearer s3cret")))))
   :token "s3cret"))

(define-test remote-websocket-refuses-web-pages
  (test-call-with-remote-server
   (lambda (server)
     (test-equal 403 (websocket-handshake-error-status
                      (test-signals websocket-handshake-error
                        (acp-websocket-connect (test-remote-websocket-url server)
                                               :headers '(("Origin" . "https://example.com")))))))))


;;;; -- Streamable HTTP Tests --

(define-test remote-http2-session
  (test-call-with-remote-server
   (lambda (server)
     (let ((client (test-h2-client server)))
       (unwind-protect
            (let* ((initialize    (test-h2-post client (test-request-message 0 "initialize" "protocolVersion" 1)))
                   (connection-id (llh2:h2-stream-header initialize "acp-connection-id"))
                   (identity      (list (cons "acp-connection-id" connection-id))))
              (test-equal 200 (llh2:h2-stream-status initialize))
              (test-assert connection-id)
              (test-equal "test-agent"
                          (json-get (json-get (json-get (json-decode (octets-to-string
                                                                      (llh2:h2-stream-read-body initialize)
                                                                      :encoding ':utf-8))
                                                        "result")
                                              "agentInfo")
                                    "name"))
              (let ((events (make-instance 'test-event-reader
                                           :stream (test-h2-events client :headers identity))))
                (test-equal 202 (llh2:h2-stream-status
                                 (test-h2-post client (test-request-message 1 "session/new"
                                                                            "cwd" "/w" "mcpServers" #())
                                               :headers identity)))
                (let ((response (test-next-event events)))
                  (test-equal 1 (json-get response "id"))
                  (test-equal "sess-1" (json-get (json-get response "result") "sessionId")))
                (let ((session (make-instance 'test-event-reader
                                              :stream (test-h2-events
                                                       client :headers (cons '("acp-session-id" . "sess-1")
                                                                             identity)))))
                  (test-equal 202 (llh2:h2-stream-status
                                   (test-h2-post client (test-request-message
                                                         2 "session/prompt" "sessionId" "sess-1"
                                                         "prompt" (vector (acp-text-content "hello")))
                                                 :headers (cons '("acp-session-id" . "sess-1") identity))))
                  (let* ((events   (loop for event = (test-next-event session)
                                             while event
                                             collect event
                                             do (when (equal (json-get event "method") "session/request_permission")
                                                  (test-equal 202 (llh2:h2-stream-status
                                                                   (test-h2-post
                                                                    client
                                                                    (json-object
                                                                     "jsonrpc" "2.0"
                                                                     "id"      (json-get event "id")
                                                                     "result"  (json-object
                                                                                "outcome" (json-object "outcome"  "selected"
                                                                                                       "optionId" "allow")))
                                                                    :headers (cons '("acp-session-id" . "sess-1")
                                                                                   identity)))))
                                             until (and (json-get event "id") (not (json-get event "method")))))
                         (updates  (loop for event in (butlast events)
                                         for update = (json-get (json-get event "params") "update")
                                         when update
                                           collect update))
                         (response (first (last events))))
                    (test-equal "Hi there" (json-get (json-get (first updates) "content") "text"))
                    (test-equal "completed" (json-get (first (last updates)) "status"))
                    (test-equal 2 (json-get response "id"))
                    (test-equal "end_turn" (json-get (json-get response "result") "stopReason")))
                  (let ((delete (llh2:h2-request client :method "DELETE" :path *acp-remote-path* :headers identity)))
                    (llh2:h2-stream-wait-headers delete :timeout 30)
                    (test-equal 202 (llh2:h2-stream-status delete)))
                  (test-equal nil (test-next-event session))
                  (test-equal nil (test-next-event events))
                  (test-equal 404 (llh2:h2-stream-status
                                   (test-h2-post client (test-request-message 3 "session/list")
                                                 :headers identity))))))
         (llh2:h2-connection-close client))))))

(define-test remote-http1-requests
  (test-call-with-remote-server
   (lambda (server)
     (multiple-value-bind (status headers body)
         (test-http1-exchange server (test-http1-request
                                      "/acp" :method  "POST"
                                             :headers '(("Content-Type" . "application/json"))
                                             :body    (json-encode (test-request-message 0 "initialize"
                                                                                         "protocolVersion" 1))))
       (test-equal 200 status)
       (test-assert (rest (assoc "acp-connection-id" headers :test #'string=)))
       (test-equal 0 (json-get (json-decode body) "id")))
     (dolist (case `((404 ,(test-http1-request "/other"))
                     (415 ,(test-http1-request "/acp" :method "POST" :body "{}"))
                     (400 ,(test-http1-request "/acp" :method  "POST"
                                                      :headers '(("Content-Type" . "application/json"))
                                                      :body    (json-encode (test-request-message 0 "session/list"))))
                     (501 ,(test-http1-request "/acp" :method  "POST"
                                                      :headers '(("Content-Type" . "application/json"))
                                                      :body    "[]"))
                     (404 ,(test-http1-request "/acp" :method  "POST"
                                                      :headers '(("Content-Type" . "application/json")
                                                                 ("Acp-Connection-Id" . "missing"))
                                                      :body    (json-encode (test-request-message 0 "session/list"))))
                     (406 ,(test-http1-request "/acp" :headers '(("Acp-Connection-Id" . "missing"))))
                     (400 ,(test-http1-request "/acp" :headers '(("Accept" . "text/event-stream"))))
                     (405 ,(test-http1-request "/acp" :method "PUT"))
                     (403 ,(test-http1-request "/acp" :headers '(("Origin" . "https://example.com"))))))
       (destructuring-bind (expected request) case
         (test-equal expected (test-http1-exchange server request)))))))

(define-test remote-http-token-and-transports
  (test-call-with-remote-server
   (lambda (server)
     (test-equal 401 (test-http1-exchange server (test-http1-request "/acp")))
     (test-equal 404 (test-http1-exchange server (test-http1-request "/other?token=s3cret")))
     (test-equal 406 (test-http1-exchange server (test-http1-request "/acp?token=s3cret")))
     (test-equal 406 (test-http1-exchange server (test-http1-request
                                                  "/acp" :headers '(("Authorization" . "Bearer s3cret"))))))
   :token "s3cret")
  (test-call-with-remote-server
   (lambda (server)
     (test-equal 426 (test-http1-exchange server (test-http1-request "/acp")))
     (test-equal 405 (test-http1-exchange server (test-http1-request "/acp" :method "POST"))))
   :transports '(:websocket)))
