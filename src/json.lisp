(in-package #:agentcomms)

;;;; -- Bounded JSON Documents --

;;; argo owns the JSON value model: objects are EQUAL hash tables with string
;;; keys, arrays vectors, true T, false argo's marker, which JSON-GET reports
;;; as NIL, and null NIL. Writing :NULL also produces null, which is how an
;;; explicit null is spelled in JSON-OBJECT, since that builder omits NIL. This
;;; file adds the protocol's message bounds and conditions at the boundary.

(defparameter *acp-maximum-message-characters* (* 16 1024 1024)
  "The largest JSON-RPC message accepted or produced, in characters.")

(defparameter *json-maximum-depth* 64
  "The deepest container nesting accepted in one document.")

(defparameter *json-maximum-nodes* 100000
  "The most values, keys included, accepted in one document.")

(defparameter *json-diagnostic-limit* 512
  "The longest payload excerpt carried by a protocol condition.")


;;;; -- Construction and Access --

(-> json-object (&rest t) hash-table)
(defun json-object (&rest pairs)
  "Return an EQUAL hash table populated from alternating string keys and values.

A value of NIL omits its key, so optional fields can be passed directly;
write :NULL for an explicit JSON null."
  (unless (evenp (length pairs))
    (error 'acp-protocol-error
           :message "JSON object construction requires key and value pairs."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (unless (stringp key)
               (error 'acp-protocol-error
                      :message "JSON object keys must be strings."))
             (when value
               (setf (gethash key object) value)))
    object))

(-> json-get (t string &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when absent or when OBJECT is no object.

JSON false and null both read as NIL; use GETHASH to tell them apart."
  (if (hash-table-p object)
      (multiple-value-bind (value present-p)
          (gethash key object)
        (cond
          ((not present-p)
           default)
          ((json-false-p value)
           nil)
          (t
           value)))
      default))

(-> json-sequence->list (t) list)
(defun json-sequence->list (value)
  "Return JSON array VALUE as a fresh list, treating absence and null as empty."
  (cond
    ((null value)
     nil)
    ((and (vectorp value) (not (stringp value)))
     (coerce value 'list))
    ((listp value)
     (copy-list value))
    (t
     (error 'acp-protocol-error
            :message "A JSON array was required."
            :payload (bounded-diagnostic value)))))


;;;; -- Encoding and Decoding --

(-> bounded-diagnostic (t &key (:limit integer)) string)
(defun bounded-diagnostic (value &key (limit *json-diagnostic-limit*))
  "Return a bounded printed representation of VALUE."
  (let ((text (if (stringp value)
                  value
                  (handler-case
                      (with-output-to-string (stream)
                        (let ((*print-readably* nil))
                          (prin1 value stream)))
                    (error ()
                      "#<unprintable value>")))))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(-> json--limits () json-limits)
(defun json--limits ()
  "Return the structural bounds currently configured for one document."
  (make-json-limits :maximum-depth *json-maximum-depth*
                    :maximum-nodes *json-maximum-nodes*))

(-> json-encode (t &key (:limit integer)) string)
(defun json-encode (value &key (limit *acp-maximum-message-characters*))
  "Return VALUE as one compact JSON line within LIMIT characters.

VALUE follows argo's value model. A value with no JSON form, or one beyond
the structural bounds, signals ACP-PROTOCOL-ERROR, and a result longer than
LIMIT signals ACP-MESSAGE-TOO-LARGE."
  (let ((text (handler-case
                  (argo:json-encode value :limits (json--limits))
                (json-error (condition)
                  (error 'acp-protocol-error
                         :message (format nil "Could not encode JSON: ~A" condition)
                         :payload (bounded-diagnostic value))))))
    (when (> (length text) limit)
      (error 'acp-message-too-large
             :message (format nil "The outgoing message exceeds ~D characters." limit)
             :limit limit))
    text))

(-> json-decode (string &key (:limit integer)) t)
(defun json-decode (source &key (limit *acp-maximum-message-characters*))
  "Decode exactly one JSON document from SOURCE within LIMIT characters.

The result follows argo's value model. A SOURCE longer than LIMIT signals
ACP-MESSAGE-TOO-LARGE; malformed JSON, trailing text, and documents beyond
the structural bounds signal ACP-PROTOCOL-ERROR."
  (when (> (length source) limit)
    (error 'acp-message-too-large
           :message (format nil "The incoming message exceeds ~D characters." limit)
           :limit limit))
  (handler-case
      (argo:json-decode source :limits (json--limits))
    (json-error (condition)
      (error 'acp-protocol-error
             :message (format nil "Could not decode JSON: ~A" condition)
             :payload (bounded-diagnostic source)))))
