(in-package #:agentcomms)

;;;; -- Connection Test Peers --

(defclass test-echo-peer (acp-peer)
  ((notifications
    :initform nil
    :accessor test-echo-peer-notifications
    :type list
    :documentation "Notifications received, oldest first, as (METHOD . PARAMS).")
   (close-reasons
    :initform nil
    :accessor test-echo-peer-close-reasons
    :type list
    :documentation "Close reasons observed, newest first; NIL marks a clean close.")
   (closed-count
    :initform 0
    :accessor test-echo-peer-closed-count
    :type (integer 0)
    :documentation "How many times the close hook ran.")
   (lock
    :initform (make-lock "agentcomms test peer")
    :reader test-echo-peer-lock
    :type t
    :documentation "The lock guarding the recorded notifications."))
  (:documentation "A peer with echo, failure, and cancellation-aware methods."))

(defmethod peer-handle-request ((peer test-echo-peer) connection method params)
  "Serve the scripted test methods."
  (declare (ignore connection))
  (cond
    ((string= method "echo")
     params)
    ((string= method "empty")
     nil)
    ((string= method "fail")
     (error 'acp-method-error
            :code -32001
            :message "scripted failure"
            :data (json-object "detail" "from the test peer")))
    ((string= method "crash")
     (error "an unexpected Lisp error"))
    ((string= method "wait-for-cancel")
     (loop repeat 400
           do (acp-check-cancelled)
              (sleep 0.01))
     (json-object "outcome" "never cancelled"))
    ((string= method "slow")
     (sleep 0.5)
     (json-object "outcome" "slow result"))
    (t
     (call-next-method))))

(defmethod peer-handle-notification ((peer test-echo-peer) connection method params)
  "Record every notification."
  (declare (ignore connection))
  (with-lock-held ((test-echo-peer-lock peer))
    (setf (test-echo-peer-notifications peer)
          (nconc (test-echo-peer-notifications peer) (list (cons method params)))))
  (when (string= method "explode")
    (error "notification handlers may fail without consequence"))
  nil)

(defmethod peer-connection-closed ((peer test-echo-peer) connection reason)
  "Record the close."
  (declare (ignore connection))
  (with-lock-held ((test-echo-peer-lock peer))
    (incf (test-echo-peer-closed-count peer))
    (push reason (test-echo-peer-close-reasons peer)))
  nil)

(-> test-connection-pair () (values acp-connection acp-connection test-echo-peer test-echo-peer))
(defun test-connection-pair ()
  "Return two connected connections and their peers."
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((left-peer (make-instance 'test-echo-peer))
          (right-peer (make-instance 'test-echo-peer)))
      (values (make-acp-connection :channel left :peer left-peer :name "left")
              (make-acp-connection :channel right :peer right-peer :name "right")
              left-peer
              right-peer))))

(-> test-wait-until (function &key (:seconds real)) boolean)
(defun test-wait-until (predicate &key (seconds 5))
  "Poll PREDICATE for up to SECONDS seconds and return whether it became true."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.01))))


;;;; -- Connection Tests --

(define-test connection-requests-and-notifications
  (multiple-value-bind (left right left-peer right-peer)
      (test-connection-pair)
    (declare (ignore left-peer))
    (unwind-protect
         (progn
           (test-equal "hello"
                       (json-get (connection-request left "echo" (json-object "text" "hello"))
                                 "text"))
           (test-assert (json-object-p (connection-request left "empty" nil))
                        "a NIL result is sent as an empty object")
           (test-assert (zerop (hash-table-count (connection-request left "empty" nil))))
           (test-equal "hello back"
                       (json-get (connection-request right "echo" (json-object "text" "hello back"))
                                 "text")
                       :test #'equal)
           (let ((condition (test-signals acp-remote-error
                              (connection-request left "fail" nil))))
             (test-equal -32001 (acp-method-error-code condition))
             (test-equal "scripted failure" (acp-error-message condition))
             (test-equal "fail" (acp-remote-error-method condition))
             (test-equal "from the test peer"
                         (json-get (acp-method-error-data condition) "detail")))
           (let ((condition (test-signals acp-remote-error
                              (connection-request left "crash" nil))))
             (test-equal -32603 (acp-method-error-code condition))
             (test-assert (search "unexpected Lisp error" (acp-error-message condition))))
           (test-equal -32601
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request left "no/such_method" nil))))
           (connection-notify left "note" (json-object "n" 1))
           (connection-notify left "explode")
           (connection-notify left "note" (json-object "n" 2))
           (test-assert (test-wait-until
                         (lambda ()
                           (with-lock-held ((test-echo-peer-lock right-peer))
                             (= 3 (length (test-echo-peer-notifications right-peer))))))
                        "notifications arrive in order despite a failing handler")
           (test-equal '("note" "explode" "note")
                       (mapcar #'first (test-echo-peer-notifications right-peer)))
           (test-equal 2 (json-get (rest (third (test-echo-peer-notifications right-peer))) "n")))
      (connection-close left)
      (connection-close right))))

(define-test connection-concurrent-requests
  (multiple-value-bind (left right)
      (test-connection-pair)
    (unwind-protect
         (let* ((results (make-array 4 :initial-element nil))
                (threads
                  (loop for index from 0 below 4
                        collect (let ((index index))
                                  (make-thread
                                   (lambda ()
                                     (setf (aref results index)
                                           (json-get (connection-request
                                                      left "echo"
                                                      (json-object "index" index))
                                                     "index"))))))))
           (dolist (thread threads)
             (join-thread thread))
           (test-equal '(0 1 2 3) (coerce results 'list)))
      (connection-close left)
      (connection-close right))))

(define-test connection-timeout-cancels-the-request
  (multiple-value-bind (left right)
      (test-connection-pair)
    (unwind-protect
         (let ((started (get-internal-real-time)))
           (let ((condition (test-signals acp-timeout
                              (connection-request left "wait-for-cancel" nil :timeout 0.2))))
             (test-equal 0.2 (acp-timeout-seconds condition)))
           (test-assert (< (- (get-internal-real-time) started)
                           (* 2 internal-time-units-per-second))
                        "the timeout returns promptly")
           (test-assert (test-wait-until
                         (lambda ()
                           (with-lock-held ((acp-connection-lock right))
                             (zerop (hash-table-count (acp-connection-inbound right))))))
                        "the cancelled handler observes the cancellation and finishes")
           (test-equal "slow result"
                       (json-get (connection-request left "slow" nil :timeout 5) "outcome")))
      (connection-close left)
      (connection-close right))))

(define-test connection-cancelled-handler-answers-with-the-cancelled-code
  (multiple-value-bind (left right)
      (test-connection-pair)
    (unwind-protect
         (let* ((outcome nil)
                (thread (make-thread
                         (lambda ()
                           (setf outcome
                                 (handler-case
                                     (connection-request left "wait-for-cancel" nil)
                                   (acp-remote-error (condition)
                                     (acp-method-error-code condition))))))))
           (test-assert (test-wait-until
                         (lambda ()
                           (with-lock-held ((acp-connection-lock right))
                             (plusp (hash-table-count (acp-connection-inbound right))))))
                        "the request is registered on the serving side")
           (let ((identifier (with-lock-held ((acp-connection-lock right))
                               (loop for key being the hash-keys of (acp-connection-inbound right)
                                     return key))))
             (connection-cancel-request left identifier))
           (join-thread thread)
           (test-equal -32800 outcome))
      (connection-close left)
      (connection-close right))))

(define-test connection-close-fails-pending-requests
  (multiple-value-bind (left right left-peer right-peer)
      (test-connection-pair)
    (let* ((outcome nil)
           (thread (make-thread
                    (lambda ()
                      (setf outcome
                            (handler-case
                                (connection-request left "wait-for-cancel" nil)
                              (acp-connection-closed ()
                                ':closed)))))))
      (test-assert (test-wait-until
                    (lambda ()
                      (with-lock-held ((acp-connection-lock left))
                        (plusp (hash-table-count (acp-connection-pending left)))))))
      (connection-close left)
      (join-thread thread)
      (test-equal ':closed outcome)
      (test-assert (not (connection-open-p left)))
      (test-assert (test-wait-until (lambda () (not (connection-open-p right))))
                   "closing one end ends the other end's input")
      (test-equal nil (connection-run right))
      (test-signals acp-connection-closed (connection-request left "echo" nil))
      (test-signals acp-connection-closed (connection-notify right "note"))
      (connection-close left)
      (test-equal 1 (test-echo-peer-closed-count left-peer))
      (test-equal 1 (test-echo-peer-closed-count right-peer))
      (test-equal '(nil) (test-echo-peer-close-reasons right-peer)))))

(define-test connection-rejects-malformed-messages
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((connection (make-acp-connection :channel left :peer (make-instance 'test-echo-peer))))
      (unwind-protect
           (flet ((exchange (text)
                    (channel-write-message right text)
                    (json-decode (channel-read-message right))))
             (let ((response (exchange "{not json")))
               (test-equal -32700 (json-get (json-get response "error") "code"))
               (multiple-value-bind (identifier present-p) (gethash "id" response)
                 (test-assert (and present-p (null identifier)) "a parse error answers with a null id")))
             (let ((response (exchange "[1, 2]")))
               (test-equal -32600 (json-get (json-get response "error") "code")))
             (let ((response (exchange "{\"jsonrpc\": \"1.0\", \"id\": 9, \"method\": \"echo\"}")))
               (test-equal -32600 (json-get (json-get response "error") "code"))
               (test-equal 9 (json-get response "id")))
             (let ((response (exchange "{\"jsonrpc\": \"2.0\", \"id\": {\"bad\": true}, \"method\": \"echo\"}")))
               (test-equal -32600 (json-get (json-get response "error") "code")))
             (let ((response (exchange "{\"jsonrpc\": \"2.0\", \"id\": 4}")))
               (test-equal -32600 (json-get (json-get response "error") "code")))
             (let ((response (exchange "{\"jsonrpc\": \"2.0\", \"id\": \"s-1\", \"method\": \"echo\", \"params\": {\"k\": 1}}")))
               (test-equal "s-1" (json-get response "id"))
               (test-equal 1 (json-get (json-get response "result") "k")))
             (setf (channel-maximum-message-characters left) 200)
             (let ((response (exchange (make-string 300 :initial-element #\a))))
               (test-equal -32700 (json-get (json-get response "error") "code"))
               (test-assert (search "exceeds 200" (json-get (json-get response "error") "message"))))
             (setf (channel-maximum-message-characters left) *acp-maximum-message-characters*)
             (channel-write-message right "{\"jsonrpc\": \"2.0\", \"id\": 77, \"result\": {}}")
             (test-equal "x"
                         (json-get (json-get (exchange "{\"jsonrpc\": \"2.0\", \"id\": 5, \"method\": \"echo\", \"params\": {\"k\": \"x\"}}")
                                             "result")
                                   "k")
                         :test #'equal))
        (connection-close connection)
        (channel-close right)))))

(define-test stream-channel-frames-lines
  (let* ((input (make-string-input-stream
                 (format nil "~%{\"a\": 1}~C~%   ~%{\"b\": 2}" #\Return)))
         (output (make-string-output-stream))
         (channel (make-acp-stream-channel :input input :output output)))
    (test-equal "{\"a\": 1}" (channel-read-message channel))
    (test-equal "{\"b\": 2}" (channel-read-message channel))
    (test-equal nil (channel-read-message channel))
    (channel-write-message channel "{\"c\": 3}")
    (test-equal (format nil "{\"c\": 3}~%") (get-output-stream-string output))
    (channel-close channel)
    (channel-close channel)
    (test-assert (not (channel-open-p channel)))
    (test-signals acp-connection-closed (channel-write-message channel "late")))
  (let ((channel (make-acp-stream-channel
                  :input (make-string-input-stream (format nil "abcdefgh~%ok~%"))
                  :output (make-string-output-stream)
                  :maximum-message-characters 4)))
    (test-signals acp-message-too-large (channel-read-message channel))
    (test-equal "ok" (channel-read-message channel))
    (test-signals acp-message-too-large (channel-write-message channel "too long"))))
