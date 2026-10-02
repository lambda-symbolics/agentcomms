(in-package #:agentcomms)

;;;; -- Message Channels --

(defclass acp-channel ()
  ((maximum-message-characters
    :initarg :maximum-message-characters
    :initform *acp-maximum-message-characters*
    :accessor channel-maximum-message-characters
    :type integer
    :documentation "The longest message this channel accepts or sends."))
  (:documentation
   "A bidirectional carrier of newline-delimited JSON-RPC messages.

Channels move complete message texts without interpreting them. The
connection layer owns framing semantics above this protocol."))

(defgeneric channel-read-message (channel)
  (:documentation
   "Block until one complete message text arrives on CHANNEL and return it.

Return NIL once the peer has closed the channel and no messages remain.
Signal ACP-MESSAGE-TOO-LARGE after discarding a message longer than the
channel's maximum message characters."))

(defgeneric channel-write-message (channel text)
  (:documentation "Deliver the complete message TEXT, one line, on CHANNEL."))

(defgeneric channel-close (channel)
  (:documentation "Close CHANNEL, releasing its resources. Closing twice is harmless."))

(defgeneric channel-open-p (channel)
  (:documentation "Return true while CHANNEL can still carry messages."))


(-> channel--check-length (acp-channel string) null)
(defun channel--check-length (channel text)
  "Signal ACP-MESSAGE-TOO-LARGE when TEXT exceeds CHANNEL's limit."
  (let ((limit (channel-maximum-message-characters channel)))
    (when (> (length text) limit)
      (error 'acp-message-too-large
             :message (format nil "The message exceeds ~D characters." limit)
             :limit limit)))
  nil)


;;;; -- Stream Channels --

(defclass acp-stream-channel (acp-channel)
  ((input
    :initarg :input
    :reader acp-stream-channel-input
    :type stream
    :documentation "The character input stream carrying peer messages.")
   (output
    :initarg :output
    :reader acp-stream-channel-output
    :type stream
    :documentation "The character output stream carrying this side's messages.")
   (close-function
    :initarg :close-function
    :initform nil
    :reader acp-stream-channel-close-function
    :type (or null function)
    :documentation "An optional function run once when the channel closes.")
   (open-p
    :initform t
    :accessor acp-stream-channel-open-p
    :type boolean
    :documentation "Whether the channel still accepts messages.")
   (lock
    :initform (make-lock "agentcomms stream channel")
    :reader acp-stream-channel-lock
    :type t
    :documentation "The lock serializing state changes and output."))
  (:documentation
   "A channel over a pair of UTF-8 character streams, one line per message."))

(-> make-acp-stream-channel
    (&key (:input stream) (:output stream) (:close-function (or null function))
          (:maximum-message-characters integer))
    acp-stream-channel)
(defun make-acp-stream-channel
    (&key input output close-function
       (maximum-message-characters *acp-maximum-message-characters*))
  "Return a channel reading messages from INPUT and writing them to OUTPUT.

CLOSE-FUNCTION, when supplied, runs once after the streams close."
  (make-instance 'acp-stream-channel
                 :input input
                 :output output
                 :close-function close-function
                 :maximum-message-characters maximum-message-characters))

(-> channel--read-bounded-line (stream integer) (or null string))
(defun channel--read-bounded-line (stream limit)
  "Read one line from STREAM within LIMIT characters, or NIL at end of input.

A longer line is consumed through its newline and then rejected so the
stream stays aligned on message boundaries."
  (let ((buffer (make-array 256 :element-type 'character
                                :adjustable t
                                :fill-pointer 0))
        (overflow-p nil))
    (loop
      (let ((character (read-char stream nil nil)))
        (cond
          ((null character)
           (return
             (cond
               (overflow-p
                (error 'acp-message-too-large
                       :message (format nil "The incoming message exceeds ~D characters."
                                        limit)
                       :limit limit))
               ((zerop (length buffer))
                nil)
               (t
                (coerce buffer 'simple-string)))))
          ((char= character #\Newline)
           (when overflow-p
             (error 'acp-message-too-large
                    :message (format nil "The incoming message exceeds ~D characters."
                                     limit)
                    :limit limit))
           (return (string-right-trim '(#\Return) (coerce buffer 'simple-string))))
          (overflow-p
           nil)
          ((>= (length buffer) limit)
           (setf overflow-p t))
          (t
           (vector-push-extend character buffer)))))))

(defmethod channel-read-message ((channel acp-stream-channel))
  "Read the next non-blank line from the channel's input stream."
  (loop
    (let ((line (handler-case
                    (channel--read-bounded-line (acp-stream-channel-input channel)
                                                (channel-maximum-message-characters channel))
                  (acp-error (condition)
                    (error condition))
                  (stream-error ()
                    (return nil))
                  (error (cause)
                    (if (acp-stream-channel-open-p channel)
                        (error 'acp-protocol-error
                               :message (format nil "Reading the channel failed: ~A" cause))
                        (return nil))))))
      (cond
        ((null line)
         (return nil))
        ((every (lambda (character) (member character '(#\Space #\Tab #\Return))) line)
         nil)
        (t
         (return line))))))

(defmethod channel-write-message ((channel acp-stream-channel) text)
  "Write TEXT and a newline to the channel's output stream, then flush."
  (channel--check-length channel text)
  (with-lock-held ((acp-stream-channel-lock channel))
    (unless (acp-stream-channel-open-p channel)
      (error 'acp-connection-closed))
    (let ((output (acp-stream-channel-output channel)))
      (write-string text output)
      (write-char #\Newline output)
      (finish-output output)))
  nil)

(defmethod channel-open-p ((channel acp-stream-channel))
  "Return whether the stream channel is open."
  (acp-stream-channel-open-p channel))

(defmethod channel-close ((channel acp-stream-channel))
  "Close both streams once and run the close function."
  (let ((close-p nil))
    (with-lock-held ((acp-stream-channel-lock channel))
      (when (acp-stream-channel-open-p channel)
        (setf (acp-stream-channel-open-p channel) nil
              close-p t)))
    (when close-p
      (ignore-errors (close (acp-stream-channel-output channel)))
      (ignore-errors (close (acp-stream-channel-input channel)))
      (let ((function (acp-stream-channel-close-function channel)))
        (when function
          (funcall function)))))
  nil)


;;;; -- In-Memory Channel Pairs --

(defclass acp-pipe-channel (acp-channel)
  ((peer
    :initform nil
    :accessor acp-pipe-channel-peer
    :type (or null acp-pipe-channel)
    :documentation "The channel receiving what this one writes.")
   (incoming
    :initform nil
    :accessor acp-pipe-channel-incoming
    :type list
    :documentation "Delivered message texts awaiting a reader, oldest first.")
   (input-ended-p
    :initform nil
    :accessor acp-pipe-channel-input-ended-p
    :type boolean
    :documentation "Whether the peer closed, ending input after the queue drains.")
   (open-p
    :initform t
    :accessor acp-pipe-channel-open-p
    :type boolean
    :documentation "Whether this end still accepts writes.")
   (lock
    :initform (make-lock "agentcomms pipe channel")
    :reader acp-pipe-channel-lock
    :type t
    :documentation "The lock guarding the queue and flags.")
   (condition
    :initform (make-condition-variable :name "agentcomms pipe channel")
    :reader acp-pipe-channel-condition
    :type t
    :documentation "Signaled when a message arrives or input ends."))
  (:documentation
   "One end of an in-process channel pair for tests and embedded peers."))

(-> make-acp-channel-pair (&key (:maximum-message-characters integer))
    (values acp-pipe-channel acp-pipe-channel))
(defun make-acp-channel-pair
    (&key (maximum-message-characters *acp-maximum-message-characters*))
  "Return two connected in-memory channels; each reads what the other writes."
  (let ((left (make-instance 'acp-pipe-channel
                             :maximum-message-characters maximum-message-characters))
        (right (make-instance 'acp-pipe-channel
                              :maximum-message-characters maximum-message-characters)))
    (setf (acp-pipe-channel-peer left) right
          (acp-pipe-channel-peer right) left)
    (values left right)))

(-> pipe-channel--deliver (acp-pipe-channel (or null string)) null)
(defun pipe-channel--deliver (channel text)
  "Append TEXT to CHANNEL's queue, or mark its input ended when TEXT is NIL."
  (with-lock-held ((acp-pipe-channel-lock channel))
    (if text
        (setf (acp-pipe-channel-incoming channel)
              (nconc (acp-pipe-channel-incoming channel) (list text)))
        (setf (acp-pipe-channel-input-ended-p channel) t))
    (condition-notify (acp-pipe-channel-condition channel)))
  nil)

(defmethod channel-read-message ((channel acp-pipe-channel))
  "Wait for the next delivered message, or NIL once the peer has closed."
  (with-lock-held ((acp-pipe-channel-lock channel))
    (loop
      (let ((incoming (acp-pipe-channel-incoming channel)))
        (cond
          (incoming
           (setf (acp-pipe-channel-incoming channel) (rest incoming))
           (let ((text (first incoming)))
             (channel--check-length channel text)
             (return text)))
          ((or (acp-pipe-channel-input-ended-p channel)
               (not (acp-pipe-channel-open-p channel)))
           (return nil))
          (t
           (condition-wait (acp-pipe-channel-condition channel)
                           (acp-pipe-channel-lock channel))))))))

(defmethod channel-write-message ((channel acp-pipe-channel) text)
  "Deliver TEXT to the peer end."
  (channel--check-length channel text)
  (let ((peer (with-lock-held ((acp-pipe-channel-lock channel))
                (unless (acp-pipe-channel-open-p channel)
                  (error 'acp-connection-closed))
                (acp-pipe-channel-peer channel))))
    (pipe-channel--deliver peer text))
  nil)

(defmethod channel-open-p ((channel acp-pipe-channel))
  "Return whether this end is open."
  (acp-pipe-channel-open-p channel))

(defmethod channel-close ((channel acp-pipe-channel))
  "Close this end and end the peer's input once its queue drains."
  (let ((peer nil))
    (with-lock-held ((acp-pipe-channel-lock channel))
      (when (acp-pipe-channel-open-p channel)
        (setf (acp-pipe-channel-open-p channel) nil
              peer (acp-pipe-channel-peer channel))
        (condition-notify (acp-pipe-channel-condition channel))))
    (when peer
      (pipe-channel--deliver peer nil)))
  nil)
