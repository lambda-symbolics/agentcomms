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
                (test-equal (if reply-p
                                1001
                                1006)
                            (websocket-close-code websocket))
                (when reply-p
                  (test-assert (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second))
                               "the reply ends the wait"))
                (test-assert (websocket-connection-stream-closed-p websocket))
                (test-equal nil (test-receive-frame client)))
           (websocket-close websocket)
           (join-thread reader)
           (join-thread peer)))))))
