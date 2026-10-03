(in-package #:agentcomms)

;;;; -- Buffered Thought Updates --

(defclass acp-update-buffer ()
  ((sender :initarg :sender :reader update-buffer-sender :type function
           :documentation "The synchronous notification sender, called under the update lock.")
   (validator :initarg :validator :reader update-buffer-validator :type function
              :documentation "The preflight validator for one complete outgoing update.")
   (thought-batch-size :initarg :thought-batch-size :reader update-buffer-thought-batch-size
                       :type (integer 1 *)
                       :documentation "The character threshold for emitting a text thought batch.")
   (template :initform nil :accessor update-buffer-template :type (or null hash-table)
             :documentation "A private snapshot of the pending update, excluding its text.")
   (signature :initform nil :accessor update-buffer-signature :type (or null string)
              :documentation "The encoded non-text fields qualifying compatible fragments.")
   (text :initform "" :accessor update-buffer-text :type string
         :documentation "Pending text, bounded by twice the batch threshold.")
   (lock :initform (make-lock "agentcomms update buffer") :reader update-buffer-lock
         :documentation "The lock ordering fragment accumulation and notification delivery."))
  (:documentation "An opt-in, ordered update sender for one session or prompt."))

(-> make-acp-update-buffer
    (function &key (:thought-batch-size (integer 1 *)) (:validator function)) acp-update-buffer)
(defun make-acp-update-buffer (sender &key (thought-batch-size 800) (validator #'json-encode))
  "Create an ordered update buffer calling SENDER with each emitted update.

Only compatible text thoughts coalesce. Other updates flush pending thoughts.
Flush before permission requests, replies, and stream boundaries. SENDER must
not reenter the buffer or wait for a client reply. VALIDATOR preflights each
candidate; signal ACP-MESSAGE-TOO-LARGE to split a batch before its wire limit."
  (check-type thought-batch-size (integer 1 *))
  (make-instance 'acp-update-buffer :sender sender :validator validator
                                  :thought-batch-size thought-batch-size))

(-> update-buffer--copy-object (hash-table) hash-table)
(defun update-buffer--copy-object (object)
  "Copy OBJECT's fields without changing its JSON values."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) object)
    copy))

(-> update-buffer--thought (hash-table) (values (or null string) (or null string)))
(defun update-buffer--thought (update)
  "Return a text thought's non-text signature and text, or two NIL values."
  (let ((content (json-get update "content")))
    (if (and (equal (json-get update "sessionUpdate") "agent_thought_chunk")
             (json-object-p content)
             (equal (json-get content "type") "text")
             (stringp (json-get content "text")))
        (let ((template (update-buffer--copy-object update))
              (body (update-buffer--copy-object content)))
          (remhash "text" body)
          (setf (gethash "content" template) body)
          (values (json-encode template) (json-get content "text")))
        (values nil nil))))

(-> update-buffer--flush (acp-update-buffer) null)
(defun update-buffer--flush (buffer)
  "Publish pending text under BUFFER's lock, retaining it if SENDER fails."
  (let ((template (update-buffer-template buffer)))
    (when template
      (setf (gethash "text" (gethash "content" template)) (update-buffer-text buffer))
      (funcall (update-buffer-sender buffer) template)
      (setf (update-buffer-template buffer) nil
            (update-buffer-signature buffer) nil
            (update-buffer-text buffer) "")))
  nil)

(-> update-buffer--candidate-p (acp-update-buffer string) boolean)
(defun update-buffer--candidate-p (buffer text)
  "Return whether BUFFER's pending update can carry TEXT within its wire limit."
  (let* ((candidate (update-buffer--copy-object (update-buffer-template buffer)))
         (content (update-buffer--copy-object (gethash "content" candidate))))
    (setf (gethash "text" content) text
          (gethash "content" candidate) content)
    (handler-case
        (progn (funcall (update-buffer-validator buffer) candidate) t)
      (acp-message-too-large () nil))))

(-> update-buffer-send (acp-update-buffer hash-table) null)
(defun update-buffer-send (buffer update)
  "Send UPDATE, batching compatible text thoughts without mutating caller data.

Large fragments pass through after pending text. A failed sender leaves the
pending batch available for explicit retry; it cannot grow without bound."
  (with-lock-held ((update-buffer-lock buffer))
    (funcall (update-buffer-validator buffer) update)
    (multiple-value-bind (signature text) (update-buffer--thought update)
      (let ((limit (update-buffer-thought-batch-size buffer)))
        (cond
          ((or (null text) (zerop (length text)) (>= (length text) limit))
           (update-buffer--flush buffer)
           (funcall (update-buffer-sender buffer) update))
          (t
           (when (or (>= (length (update-buffer-text buffer)) limit)
                     (and (update-buffer-template buffer)
                          (not (equal signature (update-buffer-signature buffer)))))
             (update-buffer--flush buffer))
           (let ((joined (concatenate 'string (update-buffer-text buffer) text)))
             (when (and (update-buffer-template buffer)
                        (not (update-buffer--candidate-p buffer joined)))
               (update-buffer--flush buffer)
               (setf joined text))
             (unless (update-buffer-template buffer)
               (setf (update-buffer-template buffer) (json-decode signature)
                     (update-buffer-signature buffer) signature))
             (setf (update-buffer-text buffer) joined))
           (when (>= (length (update-buffer-text buffer)) limit)
             (update-buffer--flush buffer)))))))
  nil)

(-> update-buffer-flush (acp-update-buffer) null)
(defun update-buffer-flush (buffer)
  "Publish pending thoughts before a permission request, reply, or stream boundary."
  (with-lock-held ((update-buffer-lock buffer))
    (update-buffer--flush buffer)))
