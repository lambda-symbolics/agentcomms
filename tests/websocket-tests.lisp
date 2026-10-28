(in-package #:agentcomms)

;;;; -- WebSocket Test Support --

(-> test-call-with-socket-pair (function) t)
(defun test-call-with-socket-pair (function)
  "Call FUNCTION with a connected client socket and server socket on the loopback interface.

Every socket is closed on exit."
  (let ((listener (tcp-listen "127.0.0.1" 0))
        (client   nil)
        (server   nil))
    (unwind-protect
         (progn
           (setf client (tcp-connect "127.0.0.1" (tcp-local-port listener))
                 server (tcp-accept listener))
           (funcall function client server))
      (dolist (socket (list client server listener))
        (when socket
          ; mallet:suppress no-ignore-errors -- the test may have closed the socket already
          (ignore-errors (close-tcp socket)))))))

(-> test-websocket (t (member :client :server)) websocket-connection)
(defun test-websocket (socket role)
  "Return a WebSocket connection with ROLE over the connected SOCKET."
  (make-instance 'websocket-connection :stream (tcp-stream socket)
                                       :socket socket
                                       :role   role))

(-> test-send-frames (t list) null)
(defun test-send-frames (socket frames)
  "Write FRAMES to SOCKET, each (OPCODE OCTETS &key FIN-P MASK-P) with masking by default.

OCTETS is a string sent as UTF-8 or a sequence of octets."
  (let ((stream (tcp-stream socket)))
    (dolist (frame frames)
      (destructuring-bind (opcode octets &key (fin-p t) (mask-p t)) frame
        (websocket--write-frame stream :opcode  opcode
                                       :payload (test-frame-octets octets)
                                       :fin-p   fin-p
                                       :mask-p  mask-p)))
    (finish-output stream))
  nil)

(-> test-frame-octets (sequence) octet-vector)
(defun test-frame-octets (octets)
  "Return OCTETS, a string sent as UTF-8 or a sequence of octets, as an octet vector."
  (if (stringp octets)
      (string-to-octets octets :encoding ':utf-8)
      (coerce octets 'octet-vector)))

(-> test-receive-frame (t) (or null websocket-frame))
(defun test-receive-frame (socket)
  "Read one frame from SOCKET, or NIL at end of input."
  (websocket--read-frame (tcp-stream socket)))

(-> test-close-frame-code (websocket-frame) (or null integer))
(defun test-close-frame-code (frame)
  "Return the status code carried by the close FRAME, or NIL when it has none."
  (test-equal ':close (websocket-frame-opcode frame))
  (let ((payload (websocket-frame-payload frame)))
    (if (>= (length payload) 2)
        (websocket--octets-integer (subseq payload 0 2))
        nil)))


;;;; -- WebSocket Framing Tests --

(define-test websocket-frame-round-trips
  (dolist (case '((0 2) (125 2) (126 4) (65535 4) (65536 10)))
    (destructuring-bind (length header-length) case
      (dolist (mask-p '(nil t))
        (let* ((payload (let ((octets (make-octet-vector length)))
                          (dotimes (index length octets)
                            (setf (aref octets index) (mod (* index 7) 256)))))
               (octets  (test-octets-written
                         (lambda (stream)
                           (websocket--write-frame stream :opcode  ':binary
                                                          :payload payload
                                                          :fin-p   (evenp length)
                                                          :mask-p  mask-p)))))
          (test-equal (+ header-length (if mask-p
                                           4
                                           0)
                         length)
                      (length octets))
          (test-call-with-octet-input
           octets
           (lambda (stream)
             (let ((frame (websocket--read-frame stream)))
               (test-equal ':binary (websocket-frame-opcode frame))
               (test-equal (evenp length) (websocket-frame-fin-p frame))
               (test-equal mask-p (websocket-frame-masked-p frame))
               (test-equal payload (websocket-frame-payload frame) :test #'equalp)
               (test-equal nil (websocket--read-frame stream))))))))))

(define-test websocket-frame-violations
  (dolist (case '((1002 (#xC1 #x00))
                  (1002 (#x83 #x00))
                  (1002 (#x09 #x00))
                  (1002 (#x89 #x7E #x00 #x7E))
                  (1002 (#x82 #x7F #x80 0 0 0 0 0 0 0))
                  (1009 (#x82 #x05 1 2 3 4 5))
                  (nil (#x82 #x05 1 2))))
    (destructuring-bind (code octets) case
      (test-call-with-octet-input
       (coerce octets 'octet-vector)
       (lambda (stream)
         (if code
             (test-equal code
                         (websocket-error-code
                          (test-signals websocket-error
                            (websocket--read-frame stream :maximum-payload-octets 4))))
             (test-signals end-of-file
               (websocket--read-frame stream))))))))


;;;; -- WebSocket Connection Tests --

(define-test websocket-server-messages-and-close-handshake
  (test-call-with-socket-pair
   (lambda (client server)
     (let ((websocket (test-websocket server ':server)))
       (test-send-frames client '((:text "Hel" :fin-p nil)
                                  (:ping "p")
                                  (:continuation "lo ∑")
                                  (:binary (1 2 3))))
       (test-equal '("Hello ∑" :text)
                   (multiple-value-list (websocket-read-message websocket)))
       (let ((pong (test-receive-frame client)))
         (test-equal ':pong (websocket-frame-opcode pong))
         (test-equal nil (websocket-frame-masked-p pong))
         (test-equal "p" (octets-to-string (websocket-frame-payload pong))))
       (multiple-value-bind (octets kind) (websocket-read-message websocket)
         (test-equal ':binary kind)
         (test-equal #(1 2 3) octets :test #'equalp))
       (websocket-send-text websocket "back")
       (let ((frame (test-receive-frame client)))
         (test-equal ':text (websocket-frame-opcode frame))
         (test-equal nil (websocket-frame-masked-p frame))
         (test-equal "back" (octets-to-string (websocket-frame-payload frame))))
       (test-assert (websocket-open-p websocket))
       (test-send-frames client (list (list ':close (websocket--close-payload 1000 "bye"))))
       (test-equal nil (websocket-read-message websocket))
       (test-equal 1000 (websocket-close-code websocket))
       (test-equal "bye" (websocket-close-reason websocket))
       (test-equal 1000 (test-close-frame-code (test-receive-frame client)))
       (test-equal nil (test-receive-frame client))
       (test-assert (not (websocket-open-p websocket)))
       (test-equal nil (websocket-read-message websocket))
       (test-signals acp-connection-closed (websocket-send-text websocket "late"))
       (websocket-close websocket)))))

(define-test websocket-fails-protocol-violations
  (dolist (case '((1002 ((:text "un" :mask-p nil)))
                  (1002 ((:continuation "or")))
                  (1002 ((:text "o" :fin-p nil) (:text "t")))
                  (1007 ((:text (#xC0 #x80))))
                  (1007 ((:text (#xE2 #x82) :fin-p nil) (:continuation (#xAC #xFF))))
                  (1002 ((:close (3))))
                  (1002 ((:close (3 237))))
                  (1002 ((:close (3 231))))
                  (1002 ((:close (19 136))))
                  (1007 ((:close (3 232 #xFF))))
                  (1009 ((:text "abc" :fin-p nil) (:continuation "def")))))
    (destructuring-bind (code frames) case
      (test-call-with-socket-pair
       (lambda (client server)
         (let ((websocket (test-websocket server ':server)))
           (test-send-frames client frames)
           (test-equal code
                       (websocket-error-code
                        (test-signals websocket-error
                          (websocket-read-message websocket :maximum-octets 4))))
           (test-equal code (websocket-close-code websocket))
           (test-equal code (test-close-frame-code (test-receive-frame client)))
           (test-equal nil (websocket-read-message websocket))))))))

(define-test websocket-client-masks-and-rejects-masked-frames
  (test-call-with-socket-pair
   (lambda (client server)
     (let ((websocket (test-websocket client ':client)))
       (websocket-send-text websocket "masked")
       (let ((frame (test-receive-frame server)))
         (test-assert (websocket-frame-masked-p frame))
         (test-equal "masked" (octets-to-string (websocket-frame-payload frame))))
       (test-send-frames server '((:text "masked by a server")))
       (test-equal 1002
                   (websocket-error-code
                    (test-signals websocket-error (websocket-read-message websocket))))
       (let ((frame (test-receive-frame server)))
         (test-assert (websocket-frame-masked-p frame))
         (test-equal 1002 (test-close-frame-code frame)))))))

(define-test websocket-close-while-another-thread-reads
  (dolist (reply-p '(t nil))
    (test-call-with-socket-pair
     (lambda (client server)
       (let* ((websocket (test-websocket server ':server))
              (reader    (make-thread (lambda ()
                                        (multiple-value-list (websocket-read-message websocket)))))
              (peer      (make-thread
                          (lambda ()
                            (let ((frame (test-receive-frame client)))
                              (when reply-p
                                (test-send-frames client (list (list ':close (websocket-frame-payload frame)))))
                              frame))))
              (start     (get-internal-real-time)))
         (unwind-protect
              (progn
                (test-assert (test-wait-until (lambda () (websocket-connection-reading-p websocket))))
                (let ((*websocket-close-seconds* 1))
                  (websocket-close websocket :code 1001 :reason "going away"))
                (test-equal '(nil nil) (join-thread reader))
                (let ((frame (join-thread peer)))
                  (test-equal 1001 (test-close-frame-code frame))
                  (test-equal "going away"
                              (octets-to-string (websocket-frame-payload frame) :start 2)))
                (test-equal 1001 (websocket-close-code websocket))
                (when reply-p
                  (test-assert (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second))
                               "the reply ends the wait"))
                (test-assert (websocket-connection-stream-closed-p websocket))
                (test-equal nil (test-receive-frame client)))
           (websocket-close websocket)
           (join-thread reader)
           (join-thread peer)))))))


;;;; -- WebSocket Handshake Test Support --

(-> test-call-with-websocket-server (function function) t)
(defun test-call-with-websocket-server (handler function)
  "Serve one loopback connection with HANDLER on a thread while calling FUNCTION with its URL.

HANDLER receives the accepted socket. The listener, the socket, and the
thread are cleaned up on every exit."
  (let ((listener (tcp-listen "127.0.0.1" 0))
        (socket   nil)
        (thread   nil))
    (unwind-protect
         (progn
           (setf thread (make-thread
                         (lambda ()
                           (setf socket (tcp-accept listener))
                           (funcall handler socket))
                         :name "agentcomms websocket test server"))
           (funcall function (format nil "ws://127.0.0.1:~D/acp?token=t" (tcp-local-port listener))))
      (unless (test-wait-until (lambda () (not (thread-alive-p thread))) :seconds 10)
        (destroy-thread thread))
      ; mallet:suppress no-ignore-errors -- a destroyed server thread may fail to join
      (ignore-errors (join-thread thread))
      (dolist (open-socket (list socket listener))
        (when open-socket
          ; mallet:suppress no-ignore-errors -- the server may have closed the socket already
          (ignore-errors (close-tcp open-socket)))))))

(-> test-upgrade-request (&key (:method string) (:version string) (:headers list)) http-request)
(defun test-upgrade-request (&key (method "GET") (version "HTTP/1.1") headers)
  "Return the RFC 6455 sample upgrade request with HEADERS replacing its fields by name."
  (make-http-request
   :method  method
   :target  "/chat"
   :version version
   :headers (let ((fields (list (cons "host" "server.example.com")
                                (cons "upgrade" "websocket")
                                (cons "connection" "keep-alive, Upgrade")
                                (cons "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ==")
                                (cons "sec-websocket-version" "13"))))
              (dolist (header headers fields)
                (setf fields (remove (first header) fields :key #'first :test #'string-equal))
                (when (rest header)
                  (setf fields (append fields (list header))))))))


;;;; -- WebSocket Handshake Tests --

(define-test websocket-server-handshake
  (test-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" (websocket--accept-key "dGhlIHNhbXBsZSBub25jZQ=="))
  (let ((request (test-upgrade-request)))
    (test-assert (websocket-upgrade-request-p request))
    (test-equal (test-http-octets "HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
Acp-Connection-Id: conn-1

")
                (test-octets-written
                 (lambda (stream)
                   (test-equal ':server
                               (websocket-connection-role
                                (websocket-accept stream request
                                                  :headers '(("Acp-Connection-Id" . "conn-1")))))))
                :test #'equalp))
  (dolist (case '((400 :method "POST")
                  (400 :headers (("upgrade")))
                  (400 :headers (("connection" . "keep-alive")))
                  (400 :version "HTTP/1.0")
                  (426 :headers (("sec-websocket-version" . "8")))
                  (400 :headers (("sec-websocket-key")))
                  (400 :headers (("sec-websocket-key" . "c2hvcnQ=")))
                  (400 :headers (("sec-websocket-key" . "AAAAAAAAAAAAAAAAAAAAAAAA")))
                  (400 :headers (("sec-websocket-key" . "dGhlIHNhbXBsZSBub25jZQ$=")))))
    (destructuring-bind (status &rest options) case
      (let ((written nil)
            (failure nil))
        (setf written (test-octets-written
                       (lambda (stream)
                         (setf failure (test-signals http-request-error
                                         (websocket-accept stream (apply #'test-upgrade-request options)))))))
        (test-equal status (http-request-error-status failure))
        (test-equal 0 (length written))
        (test-equal (if (= status 426)
                        '(("Sec-WebSocket-Version" . "13"))
                        nil)
                    (http-request-error-headers failure))))))

(define-test websocket-url-parsing
  (dolist (case '(("ws://example.com/acp?x=1" "example.com" 80 "/acp?x=1" "example.com")
                  ("ws://127.0.0.1:8080" "127.0.0.1" 8080 "/" "127.0.0.1:8080")
                  ("WS://[::1]:9/p" "::1" 9 "/p" "[::1]:9")
                  ("ws://[::1]" "::1" 80 "/" "[::1]")
                  ("ws://h?q=1" "h" 80 "/?q=1" "h")))
    (destructuring-bind (url &rest expected) case
      (test-equal expected (multiple-value-list (websocket--parse-url url)))))
  (dolist (url '("wss://example.com/acp" "http://example.com/" "example.com" "ws://h/#f"
                 "ws://user@h/" "ws://h:0/" "ws://h:x/" "ws://h:/" "ws:///p" "ws://[::1/" "ws://[::1]x/"))
    (test-signals websocket-error (websocket--parse-url url)))
  (test-assert (search "TLS" (acp-error-message (test-signals websocket-error
                                                  (websocket-connect "wss://example.com/acp"))))))

(define-test websocket-client-handshake-and-echo
  (let ((authorization nil))
    (test-call-with-websocket-server
     (lambda (socket)
       (let* ((stream    (tcp-stream socket))
              (request   (http1-read-request stream))
              (websocket (websocket-accept stream request :headers '(("Acp-Connection-Id" . "conn-1"))
                                                          :socket  socket)))
         (setf authorization (http-request-header request "authorization"))
         (loop for message = (websocket-read-message websocket)
               while message
               do (websocket-send-text websocket (format nil "echo ~A" message)))))
     (lambda (url)
       (let ((websocket (websocket-connect url :headers '(("Authorization" . "Bearer secret")))))
         (unwind-protect
              (progn
                (test-equal "conn-1" (http--header-value (websocket-handshake-headers websocket)
                                                         "acp-connection-id"))
                (websocket-send-text websocket "ping ∑")
                (test-equal "echo ping ∑" (websocket-read-message websocket))
                (test-equal "Bearer secret" authorization))
           (websocket-close websocket))
         (test-equal 1000 (websocket-close-code websocket))))))
  (dolist (case `((401 ,(lambda (stream request)
                          (declare (ignore request))
                          (http1-write-response stream 401 :body "no")))
                  (101 ,(lambda (stream request)
                          (declare (ignore request))
                          (http1-write-response-head stream 101
                                                     :headers '(("Upgrade" . "websocket")
                                                                ("Connection" . "Upgrade")
                                                                ("Sec-WebSocket-Accept" . "wrong")))))
                  (101 ,(lambda (stream request)
                          (websocket-accept stream request
                                            :headers '(("Sec-WebSocket-Extensions" . "permessage-deflate")))))
                  (nil ,(lambda (stream request)
                          (declare (ignore request))
                          (write-sequence (test-http-octets "garbage
") stream)
                          (finish-output stream)))))
    (destructuring-bind (status answer) case
      (test-call-with-websocket-server
       (lambda (socket)
         (let ((stream (tcp-stream socket)))
           (funcall answer stream (http1-read-request stream))))
       (lambda (url)
         (test-equal status
                     (websocket-handshake-error-status
                      (test-signals websocket-handshake-error (websocket-connect url)))))))))


;;;; -- ACP over WebSocket Tests --

(define-test websocket-channel-message-bounds
  (test-call-with-socket-pair
   (lambda (client server)
     (let ((channel (make-acp-websocket-channel (test-websocket server ':server)
                                                :maximum-message-characters 4)))
       (test-send-frames client '((:binary (1 2 3)) (:text "abcde") (:text "ok") (:text "∑∑∑∑")))
       (test-equal 4 (acp-message-too-large-limit
                      (test-signals acp-message-too-large (channel-read-message channel))))
       (test-equal "ok" (channel-read-message channel))
       (test-equal "∑∑∑∑" (channel-read-message channel))
       (test-signals acp-message-too-large (channel-write-message channel "large"))
       (test-send-frames client '((:text "0123456789abcdefg")))
       (test-signals acp-message-too-large (channel-read-message channel))
       (test-equal 1009 (test-close-frame-code (test-receive-frame client)))
       (test-equal nil (channel-read-message channel))
       (test-assert (not (channel-open-p channel)))
       (test-signals acp-connection-closed (channel-write-message channel "late"))
       (channel-close channel)
       (channel-close channel)))))

(define-test websocket-acp-agent-and-client
  (let ((agent        (make-instance 'test-tooling-agent))
        (client       (make-instance 'test-client))
        (agent-closed nil))
    (test-call-with-websocket-server
     (lambda (socket)
       (let* ((stream  (tcp-stream socket))
              (request (http1-read-request stream)))
         (acp-agent-serve agent
                          (make-acp-websocket-channel
                           (websocket-accept stream request :headers '(("Acp-Connection-Id" . "conn-1"))
                                                            :socket  socket)))
         (setf agent-closed t)))
     (lambda (url)
       (let ((channel (acp-websocket-connect url)))
         (test-equal "conn-1"
                     (http--header-value (websocket-handshake-headers (acp-websocket-channel-websocket channel))
                                         "acp-connection-id"))
         (acp-client-connect client channel :name "websocket client")
         (unwind-protect
              (progn
                (client-initialize client)
                (test-equal "test-agent" (json-get (acp-client-agent-info client) "name"))
                (test-equal "sess-1" (client-new-session client "/w"))
                (test-equal ':refusal (client-prompt client "sess-1" (list (acp-text-content "nothing ∑"))))
                (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "hello"))))
                (test-equal '("Hi there") (test-client-texts client))
                (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "write"))))
                (test-equal '(("/w/out.txt" . "written")) (test-client-writes client))
                (let ((thread (make-thread
                               (lambda ()
                                 (client-prompt client "sess-1" (list (acp-text-content "slow")))))))
                  (test-assert (test-wait-until
                                (lambda ()
                                  (acp-agent-session-prompt-active-p (acp-agent-session agent "sess-1")))))
                  (client-cancel client "sess-1")
                  (test-equal ':cancelled (join-thread thread))))
           (connection-close (acp-client-connection client)))
         (test-assert (test-wait-until (lambda () agent-closed))
                      "the agent's connection ends when the client closes")
         (test-equal 1000 (websocket-close-code (acp-websocket-channel-websocket channel))))))))
