(in-package #:agentcomms)

;;;; -- WebSocket Parameters --

(defparameter *websocket-maximum-message-octets* (* 64 1024 1024)
  "The longest message WEBSOCKET-READ-MESSAGE accepts by default, in octets.")

(defparameter *websocket-close-seconds* 2
  "Seconds WEBSOCKET-CLOSE waits for the peer's close frame while another thread reads.")

(defparameter *websocket-opcodes*
  '((0 . :continuation)
    (1 . :text)
    (2 . :binary)
    (8 . :close)
    (9 . :ping)
    (10 . :pong))
  "The frame opcodes RFC 6455 defines and the keywords that name them.")

(defvar *websocket-random-lock* (make-lock "agentcomms websocket random data")
  "The lock serializing reads from the shared random source used for masking keys.")


;;;; -- Frames --

(defclass websocket-frame ()
  ((fin-p
    :initarg       :fin-p
    :reader        websocket-frame-fin-p
    :type          boolean
    :documentation "Whether this frame ends its message.")
   (opcode
    :initarg       :opcode
    :reader        websocket-frame-opcode
    :type          keyword
    :documentation "The frame kind, a keyword from *WEBSOCKET-OPCODES*.")
   (masked-p
    :initarg       :masked-p
    :reader        websocket-frame-masked-p
    :type          boolean
    :documentation "Whether the sender masked the payload.")
   (payload
    :initarg       :payload
    :reader        websocket-frame-payload
    :type          octet-vector
    :documentation "The unmasked payload."))
  (:documentation "One WebSocket frame as read from the wire."))

(-> websocket--control-opcode-p (keyword) boolean)
(defun websocket--control-opcode-p (opcode)
  "Return whether OPCODE names a control frame."
  (and (member opcode '(:close :ping :pong)) t))

(-> websocket--random-octets ((integer 0)) octet-vector)
(defun websocket--random-octets (count)
  "Return COUNT octets from the operating system's random source."
  (with-lock-held (*websocket-random-lock*)
    (coerce (random-data count) 'octet-vector)))

(-> websocket--mask (octet-vector octet-vector) octet-vector)
(defun websocket--mask (payload key)
  "Return a copy of PAYLOAD masked, or unmasked, with the four-octet KEY."
  (let ((result (make-octet-vector (length payload))))
    (dotimes (index (length payload) result)
      (setf (aref result index)
            (logxor (aref payload index) (aref key (mod index 4)))))))

(-> websocket--write-frame
    (stream &key (:opcode keyword) (:payload octet-vector) (:fin-p boolean) (:mask-p boolean))
    null)
(defun websocket--write-frame (stream &key opcode payload (fin-p t) mask-p)
  "Write one frame of OPCODE carrying PAYLOAD to STREAM without flushing it.

MASK-P masks the payload with a fresh random key, as clients must."
  (let ((length (length payload))
        (header (make-array 14 :element-type 'octet :fill-pointer 0))
        (mask   (if mask-p
                    #x80
                    0)))
    (vector-push (logior (if fin-p
                             #x80
                             0)
                         (first (rassoc opcode *websocket-opcodes*)))
                 header)
    (cond
      ((< length 126)
       (vector-push (logior mask length) header))
      ((< length 65536)
       (vector-push (logior mask 126) header)
       (vector-push (ldb (byte 8 8) length) header)
       (vector-push (ldb (byte 8 0) length) header))
      (t
       (vector-push (logior mask 127) header)
       (loop for shift from 56 downto 0 by 8
             do (vector-push (ldb (byte 8 shift) length) header))))
    (cond
      (mask-p
       (let ((key (websocket--random-octets 4)))
         (loop for octet across key
               do (vector-push octet header))
         (write-sequence header stream)
         (write-sequence (websocket--mask payload key) stream)))
      (t
       (write-sequence header stream)
       (write-sequence payload stream))))
  nil)

(-> websocket--read-frame (stream &key (:maximum-payload-octets (integer 0)))
    (or null websocket-frame))
(defun websocket--read-frame (stream &key (maximum-payload-octets *websocket-maximum-message-octets*))
  "Read one frame from STREAM, or return NIL at end of input before its first octet.

Signal WEBSOCKET-ERROR with code 1002 for reserved bits, an unknown
opcode, a fragmented or long control frame, or a malformed length, and
with code 1009 for a data payload longer than MAXIMUM-PAYLOAD-OCTETS.
Signal END-OF-FILE when the input ends inside the frame."
  (let ((first-octet (read-byte stream nil nil)))
    (if (null first-octet)
        nil
        (let* ((second-octet (read-byte stream))
               (fin-p        (logbitp 7 first-octet))
               (opcode       (rest (assoc (ldb (byte 4 0) first-octet) *websocket-opcodes*)))
               (masked-p     (logbitp 7 second-octet))
               (length       (ldb (byte 7 0) second-octet)))
          (unless (zerop (ldb (byte 3 4) first-octet))
            (websocket--protocol-error 1002 "A frame sets reserved bits."))
          (unless opcode
            (websocket--protocol-error 1002 "A frame has an unknown opcode."))
          (when (and (websocket--control-opcode-p opcode)
                     (or (not fin-p) (> length 125)))
            (websocket--protocol-error 1002 "A control frame is fragmented or too long."))
          (setf length (websocket--read-extended-length stream length))
          (when (and (not (websocket--control-opcode-p opcode))
                     (> length maximum-payload-octets))
            (websocket--protocol-error
             1009 (format nil "The message exceeds ~D octets." maximum-payload-octets)))
          (let* ((key     (and masked-p (websocket--read-octets stream 4)))
                 (payload (websocket--read-octets stream length)))
            (make-instance 'websocket-frame
                           :fin-p    fin-p
                           :opcode   opcode
                           :masked-p masked-p
                           :payload  (if key
                                         (websocket--mask payload key)
                                         payload)))))))

(-> websocket--read-extended-length (stream (integer 0 127)) (integer 0))
(defun websocket--read-extended-length (stream length)
  "Return the payload length whose seven-bit field is LENGTH, reading its extension.

Signal WEBSOCKET-ERROR with code 1002 when a 64-bit length sets its top bit."
  (case length
    (126
     (websocket--octets-integer (websocket--read-octets stream 2)))
    (127
     (let ((value (websocket--octets-integer (websocket--read-octets stream 8))))
       (when (logbitp 63 value)
         (websocket--protocol-error 1002 "A frame length sets its most significant bit."))
       value))
    (t
     length)))

(-> websocket--octets-integer (octet-vector) (integer 0))
(defun websocket--octets-integer (octets)
  "Return the unsigned big-endian integer that OCTETS encode."
  (reduce (lambda (value octet) (logior (ash value 8) octet)) octets :initial-value 0))

(-> websocket--read-octets (stream (integer 0)) octet-vector)
(defun websocket--read-octets (stream count)
  "Read exactly COUNT octets from STREAM, signalling END-OF-FILE when it ends first."
  (let ((octets (make-octet-vector count)))
    (when (< (read-sequence octets stream) count)
      ; mallet:suppress error-without-custom-condition -- a truncated frame ends input like any stream
      (error 'end-of-file :stream stream))
    octets))

(-> websocket--protocol-error (integer string) nil)
(defun websocket--protocol-error (code message)
  "Signal WEBSOCKET-ERROR with close status CODE and MESSAGE."
  (error 'websocket-error :code code :message message))


;;;; -- Connections --

(defclass websocket-connection ()
  ((stream
    :initarg       :stream
    :reader        websocket-connection-stream
    :type          stream
    :documentation "The bidirectional octet stream carrying frames.")
   (socket
    :initarg       :socket
    :initform      nil
    :reader        websocket-connection-socket
    :type          t
    :documentation "The USOCKET socket under the stream, or NIL when unknown.")
   (role
    :initarg       :role
    :reader        websocket-connection-role
    :type          (member :client :server)
    :documentation "Which end of the connection this is; clients mask their frames.")
   (handshake-headers
    :initarg       :handshake-headers
    :initform      nil
    :reader        websocket-handshake-headers
    :type          list
    :documentation "The header fields of the opening handshake response.")
   (close-code
    :initform      nil
    :accessor      websocket-close-code
    :type          (or null integer)
    :documentation "The peer's close status, else the one this side sent or failed with,
else 1006 for a connection that ended without a close frame; NIL while open.")
   (close-reason
    :initform      nil
    :accessor      websocket-close-reason
    :type          (or null string)
    :documentation "The reason that came with the close status.")
   (close-sent-p
    :initform      nil
    :accessor      websocket-connection-close-sent-p
    :type          boolean
    :documentation "Whether this side sent its close frame.")
   (close-received-p
    :initform      nil
    :accessor      websocket-connection-close-received-p
    :type          boolean
    :documentation "Whether the peer's close frame arrived.")
   (reading-p
    :initform      nil
    :accessor      websocket-connection-reading-p
    :type          boolean
    :documentation "Whether a thread is reading from the stream.")
   (released-p
    :initform      nil
    :accessor      websocket-connection-released-p
    :type          boolean
    :documentation "Whether the connection was released and its socket shut down.")
   (stream-closed-p
    :initform      nil
    :accessor      websocket-connection-stream-closed-p
    :type          boolean
    :documentation "Whether the stream is closed.")
   (lock
    :initform      (make-lock "agentcomms websocket state")
    :reader        websocket-connection-lock
    :type          t
    :documentation "The lock guarding the reading and release state.")
   (condition
    :initform      (make-condition-variable :name "agentcomms websocket state")
    :reader        websocket-connection-condition
    :type          t
    :documentation "Signaled when the close frame arrives, reading stops, or release starts.")
   (read-lock
    :initform      (make-lock "agentcomms websocket reader")
    :reader        websocket-connection-read-lock
    :type          t
    :documentation "The lock admitting one reader at a time.")
   (write-lock
    :initform      (make-lock "agentcomms websocket writer")
    :reader        websocket-connection-write-lock
    :type          t
    :documentation "The lock serializing frames and the closing of the stream."))
  (:documentation
   "An open WebSocket connection after its opening handshake.

One thread reads messages while others send them. Closing from any
thread shuts the socket down, which wakes a blocked reader, and the last
thread to leave the stream closes it."))

(-> websocket-open-p (websocket-connection) boolean)
(defun websocket-open-p (connection)
  "Return whether CONNECTION can still send and receive messages."
  (not (or (websocket-connection-close-sent-p connection)
           (websocket-connection-close-received-p connection)
           (websocket-connection-released-p connection))))

(-> websocket-read-message (websocket-connection &key (:maximum-octets (integer 0)))
    (values (or null string octet-vector) (member nil :text :binary)))
(defun websocket-read-message (connection &key (maximum-octets *websocket-maximum-message-octets*))
  "Block until the next complete data message arrives on CONNECTION.

Return a string and :TEXT for a text message, or octets and :BINARY.
Pings are answered, pongs ignored, and the close handshake completed on
the way. Return NIL once the connection has closed. When the peer breaks
the protocol, sends invalid UTF-8 text, or sends a message longer than
MAXIMUM-OCTETS, fail the connection and signal WEBSOCKET-ERROR."
  (with-lock-held ((websocket-connection-read-lock connection))
    (with-lock-held ((websocket-connection-lock connection))
      (when (websocket-connection-released-p connection)
        (return-from websocket-read-message (values nil nil)))
      (setf (websocket-connection-reading-p connection) t))
    (unwind-protect
         (handler-case (websocket--read-data-message connection maximum-octets)
           (stream-error ()
             (websocket--end connection 1006)
             (values nil nil)))
      (websocket--finish-reading connection))))

(-> websocket-send-text (websocket-connection string) null)
(defun websocket-send-text (connection text)
  "Send TEXT as one text frame on CONNECTION.

Signal ACP-CONNECTION-CLOSED once the closing handshake has started."
  (websocket--send-frame connection :opcode ':text :payload (string-to-octets text :encoding ':utf-8)))

(-> websocket-send-binary (websocket-connection (vector (unsigned-byte 8))) null)
(defun websocket-send-binary (connection octets)
  "Send OCTETS as one binary frame on CONNECTION.

Signal ACP-CONNECTION-CLOSED once the closing handshake has started."
  (websocket--send-frame connection :opcode ':binary :payload (coerce octets 'octet-vector)))

(-> websocket-send-ping (websocket-connection &key (:payload (vector (unsigned-byte 8)))) null)
(defun websocket-send-ping (connection &key (payload (make-octet-vector 0)))
  "Send a ping carrying PAYLOAD, at most 125 octets, on CONNECTION.

The peer answers with a pong, which WEBSOCKET-READ-MESSAGE discards."
  (when (> (length payload) 125)
    (error 'acp-error :message "A ping payload is limited to 125 octets."))
  (websocket--send-frame connection :opcode ':ping :payload (coerce payload 'octet-vector)))

(-> websocket-close (websocket-connection &key (:code integer) (:reason string)) null)
(defun websocket-close (connection &key (code 1000) (reason ""))
  "Send a close frame with CODE and REASON on CONNECTION, then release it.

While another thread is reading, wait up to *WEBSOCKET-CLOSE-SECONDS*
for the peer's close frame before shutting the socket down. Closing
twice is harmless. REASON is cut to the 123 octets a close frame holds."
  (unless (websocket--close-code-p code)
    (error 'acp-error :message (format nil "~D is not a close status an endpoint may send." code)))
  (handler-case (websocket--send-frame connection :opcode  ':close
                                                  :payload (websocket--close-payload code reason))
    (error ()
      nil))
  (let ((lock     (websocket-connection-lock connection))
        (deadline (+ (get-internal-real-time)
                     (* *websocket-close-seconds* internal-time-units-per-second))))
    (with-lock-held (lock)
      (loop while (and (websocket-connection-reading-p connection)
                       (not (websocket-connection-close-received-p connection))
                       (not (websocket-connection-released-p connection))
                       (< (get-internal-real-time) deadline))
            do (condition-wait (websocket-connection-condition connection) lock
                               :timeout (/ (- deadline (get-internal-real-time))
                                           internal-time-units-per-second)))
      (unless (websocket-close-code connection)
        (setf (websocket-close-code connection)   code
              (websocket-close-reason connection) reason))))
  (websocket--release connection)
  nil)

(-> websocket--read-data-message (websocket-connection (integer 0))
    (values (or null string octet-vector) (member nil :text :binary)))
(defun websocket--read-data-message (connection maximum-octets)
  "Read frames from CONNECTION until a data message completes or the connection closes."
  (let ((stream  (websocket-connection-stream connection))
        (kind    nil)
        (message nil))
    (loop
      (let ((frame (handler-case
                       (websocket--read-frame stream
                                              :maximum-payload-octets
                                              (- maximum-octets (if message
                                                                    (length message)
                                                                    0)))
                     (websocket-error (condition)
                       (websocket--fail connection
                                        :code    (websocket-error-code condition)
                                        :message (acp-error-message condition))))))
        (when (null frame)
          (websocket--end connection 1006)
          (return (values nil nil)))
        (unless (eq (websocket-frame-masked-p frame)
                    (eq (websocket-connection-role connection) ':server))
          (websocket--fail connection
                           :code    1002
                           :message "Clients must mask their frames and servers must not."))
        (let ((payload (websocket-frame-payload frame))
              (fin-p   (websocket-frame-fin-p frame)))
          (ecase (websocket-frame-opcode frame)
            (:ping
             (websocket--send-frame connection :opcode ':pong :payload payload))
            (:pong
             nil)
            (:close
             (websocket--receive-close connection payload)
             (return (values nil nil)))
            (:continuation
             (unless message
               (websocket--fail connection
                                :code    1002
                                :message "A continuation frame arrived outside a message."))
             (let ((start (length message)))
               (adjust-array message (+ start (length payload)) :fill-pointer (+ start (length payload)))
               (replace message payload :start1 start))
             (when fin-p
               (return (websocket--complete-message connection kind message))))
            ((:text :binary)
             (when message
               (websocket--fail connection
                                :code    1002
                                :message "A new message began before the previous one ended."))
             (if fin-p
                 (return (websocket--complete-message connection (websocket-frame-opcode frame) payload))
                 (setf kind    (websocket-frame-opcode frame)
                       message (make-array (length payload) :element-type     'octet
                                                            :adjustable       t
                                                            :fill-pointer     (length payload)
                                                            :initial-contents payload))))))))))

(-> websocket--complete-message (websocket-connection (member :text :binary) (vector (unsigned-byte 8)))
    (values (or string octet-vector) (member :text :binary)))
(defun websocket--complete-message (connection kind octets)
  "Return the finished message OCTETS of KIND as the caller receives it.

Fail CONNECTION with code 1007 when a text message is not valid UTF-8."
  (let ((octets (coerce octets 'octet-vector)))
    (if (eq kind ':text)
        (values (handler-case (octets-to-string octets :encoding ':utf-8)
                  (character-coding-error ()
                    (websocket--fail connection :code 1007 :message "A text message is not valid UTF-8.")))
                ':text)
        (values octets ':binary))))

(-> websocket--receive-close (websocket-connection octet-vector) null)
(defun websocket--receive-close (connection payload)
  "Record the peer's close frame PAYLOAD, answer it, and release CONNECTION.

Fail the connection with code 1002 for a malformed status and 1007 for
a reason that is not UTF-8."
  (when (= (length payload) 1)
    (websocket--fail connection :code 1002 :message "A close frame has a one-octet payload."))
  (let ((code (if (>= (length payload) 2)
                  (websocket--octets-integer (subseq payload 0 2))
                  nil)))
    (when (and code (not (websocket--close-code-p code)))
      (websocket--fail connection :code 1002 :message (format nil "The close status ~D is not valid." code)))
    (let ((reason (handler-case (octets-to-string payload :start    (min 2 (length payload))
                                                          :encoding ':utf-8)
                    (character-coding-error ()
                      (websocket--fail connection :code 1007 :message "A close reason is not valid UTF-8.")))))
      (with-lock-held ((websocket-connection-lock connection))
        (setf (websocket-connection-close-received-p connection) t
              (websocket-close-code connection)                  (or code 1005)
              (websocket-close-reason connection)                reason)
        (condition-notify (websocket-connection-condition connection)))
      (handler-case (websocket--send-frame connection :opcode  ':close
                                                      :payload (if code
                                                                   (subseq payload 0 2)
                                                                   (make-octet-vector 0)))
        (error ()
          nil))
      (websocket--release connection)))
  nil)

(-> websocket--close-code-p (integer) boolean)
(defun websocket--close-code-p (code)
  "Return whether CODE is a close status that may appear in a close frame."
  (and (or (<= 1000 code 1003)
           (<= 1007 code 1014)
           (<= 3000 code 4999))
       t))

(-> websocket--close-payload (integer string) octet-vector)
(defun websocket--close-payload (code reason)
  "Return the close frame payload for CODE and REASON, cutting REASON to whole characters."
  (let* ((text (string-to-octets reason :encoding ':utf-8))
         (end  (length text)))
    (when (> end 123)
      (setf end 123)
      (loop while (= (ldb (byte 2 6) (aref text end)) #b10)
            do (decf end)))
    (concatenate 'octet-vector
                 (vector (ldb (byte 8 8) code) (ldb (byte 8 0) code))
                 (subseq text 0 end))))

(-> websocket--send-frame (websocket-connection &key (:opcode keyword) (:payload octet-vector)) null)
(defun websocket--send-frame (connection &key opcode payload)
  "Write one complete frame of OPCODE with PAYLOAD to CONNECTION and flush it.

After the close frame, control frames are dropped and data frames and
pings signal ACP-CONNECTION-CLOSED."
  (with-lock-held ((websocket-connection-write-lock connection))
    (cond
      ((or (websocket-connection-close-sent-p connection)
           (websocket-connection-stream-closed-p connection))
       (when (member opcode '(:text :binary :ping))
         (error 'acp-connection-closed :message "The WebSocket connection is closed.")))
      (t
       (when (eq opcode ':close)
         (setf (websocket-connection-close-sent-p connection) t))
       (let ((stream (websocket-connection-stream connection)))
         (websocket--write-frame stream :opcode  opcode
                                        :payload payload
                                        :mask-p  (eq (websocket-connection-role connection) ':client))
         (finish-output stream)))))
  nil)

(-> websocket--fail (websocket-connection &key (:code integer) (:message string)) nil)
(defun websocket--fail (connection &key code message)
  "Fail CONNECTION: send a close frame with CODE, release it, and signal WEBSOCKET-ERROR."
  (handler-case (websocket--send-frame connection :opcode  ':close
                                                  :payload (websocket--close-payload code ""))
    (error ()
      nil))
  (with-lock-held ((websocket-connection-lock connection))
    (unless (websocket-close-code connection)
      (setf (websocket-close-code connection)   code
            (websocket-close-reason connection) message)))
  (websocket--release connection)
  (error 'websocket-error :code code :message message))

(-> websocket--end (websocket-connection integer) null)
(defun websocket--end (connection code)
  "Record CODE unless a close status is known, and release CONNECTION."
  (with-lock-held ((websocket-connection-lock connection))
    (unless (websocket-close-code connection)
      (setf (websocket-close-code connection)   code
            (websocket-close-reason connection) "")))
  (websocket--release connection))

(-> websocket--release (websocket-connection) null)
(defun websocket--release (connection)
  "Shut CONNECTION's socket down once and close its stream unless a reader is inside it.

A reader closes the stream itself when it leaves."
  (let ((first-p        nil)
        (close-stream-p nil))
    (with-lock-held ((websocket-connection-lock connection))
      (unless (websocket-connection-released-p connection)
        (setf (websocket-connection-released-p connection) t
              first-p                                      t)
        (condition-notify (websocket-connection-condition connection)))
      (setf close-stream-p (not (websocket-connection-reading-p connection))))
    (let ((socket (websocket-connection-socket connection)))
      (when (and first-p socket)
        ; mallet:suppress no-ignore-errors -- the peer may have reset the socket already
        (ignore-errors (socket-shutdown socket :io))))
    (when close-stream-p
      (websocket--close-stream connection)))
  nil)

(-> websocket--finish-reading (websocket-connection) null)
(defun websocket--finish-reading (connection)
  "Mark CONNECTION's reader gone and close the stream when the connection was released."
  (let ((close-p nil))
    (with-lock-held ((websocket-connection-lock connection))
      (setf (websocket-connection-reading-p connection) nil
            close-p                                     (websocket-connection-released-p connection))
      (condition-notify (websocket-connection-condition connection)))
    (when close-p
      (websocket--close-stream connection)))
  nil)

(-> websocket--close-stream (websocket-connection) null)
(defun websocket--close-stream (connection)
  "Close CONNECTION's socket, or its stream when there is no socket, once."
  (with-lock-held ((websocket-connection-write-lock connection))
    (unless (websocket-connection-stream-closed-p connection)
      (setf (websocket-connection-stream-closed-p connection) t)
      (let ((socket (websocket-connection-socket connection)))
        ; mallet:suppress no-ignore-errors -- the connection closes even when the socket fails to close
        (ignore-errors
          (if socket
              (close-tcp socket)
              (close (websocket-connection-stream connection)))))))
  nil)
