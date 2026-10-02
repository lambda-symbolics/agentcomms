(in-package #:agentcomms)

;;;; -- Bounded JSON Documents --

(defparameter *acp-maximum-message-characters* (* 16 1024 1024)
  "The largest JSON-RPC message accepted or produced, in characters.")

(defparameter *json-maximum-depth* 64
  "The deepest container nesting accepted in one decoded document.")

(defparameter *json-maximum-nodes* 100000
  "The most values, keys included, accepted in one decoded document.")

(defparameter *json-diagnostic-limit* 512
  "The longest payload excerpt carried by a protocol condition.")


;;;; -- Canonical Values --

(-> json-true-value () t)
(defun json-true-value ()
  "Return the value representing JSON true."
  'yason:true)

(-> json-false-value () t)
(defun json-false-value ()
  "Return the value representing JSON false."
  'yason:false)

(-> json-null-value () keyword)
(defun json-null-value ()
  "Return the value representing JSON null."
  ':null)

(-> json-true-p (t) boolean)
(defun json-true-p (value)
  "Return true exactly when VALUE is JSON true."
  (eq value 'yason:true))

(-> json-boolean-p (t) boolean)
(defun json-boolean-p (value)
  "Return true when VALUE is one of the JSON boolean symbols."
  (and (or (eq value 'yason:true) (eq value 'yason:false)) t))

(-> json-null-p (t) boolean)
(defun json-null-p (value)
  "Return true when VALUE is JSON null."
  (eq value ':null))

(-> json-object-p (t) boolean)
(defun json-object-p (value)
  "Return true when VALUE is a decoded JSON object."
  (and (hash-table-p value) t))


;;;; -- Construction and Access --

(-> json-object (&rest t) hash-table)
(defun json-object (&rest pairs)
  "Return an EQUAL hash table populated from alternating string keys and values.

A value of NIL omits its key, so optional fields can be passed directly."
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
  "Return KEY from JSON OBJECT, or DEFAULT when absent or when OBJECT is no object."
  (if (hash-table-p object)
      (multiple-value-bind (value present-p)
          (gethash key object)
        (if present-p value default))
      default))

(-> json-sequence->list (t) list)
(defun json-sequence->list (value)
  "Return JSON array VALUE as a fresh list, treating absence as empty."
  (cond
    ((null value)
     nil)
    ((vectorp value)
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

(-> json--measure (t integer) integer)
(defun json--measure (value depth)
  "Return the node count of VALUE while enforcing the depth and node bounds."
  (when (> depth *json-maximum-depth*)
    (error 'acp-protocol-error
           :message (format nil "The JSON document nests deeper than ~D levels."
                            *json-maximum-depth*)))
  (let ((count 1))
    (flet ((add (nodes)
             (incf count nodes)
             (when (> count *json-maximum-nodes*)
               (error 'acp-protocol-error
                      :message (format nil "The JSON document has more than ~D nodes."
                                       *json-maximum-nodes*)))))
      (cond
        ((hash-table-p value)
         (maphash (lambda (key element)
                    (declare (ignore key))
                    (add (1+ (json--measure element (1+ depth)))))
                  value))
        ((and (vectorp value) (not (stringp value)))
         (loop for element across value
               do (add (json--measure element (1+ depth)))))
        ((consp value)
         (dolist (element value)
           (add (json--measure element (1+ depth))))))
      count)))

(-> json-encode (t &key (:limit integer)) string)
(defun json-encode (value &key (limit *acp-maximum-message-characters*))
  "Return VALUE as one compact JSON line without embedded newlines.

Lists encode as arrays, hash tables as objects, and the canonical boolean
and null values as their JSON literals. The result must fit within LIMIT
characters."
  (json--measure value 0)
  (let ((text (with-output-to-string (stream)
                (let ((yason:*list-encoder* #'yason:encode-plain-list-to-array))
                  (yason:encode (json--prepare value) stream)))))
    (when (> (length text) limit)
      (error 'acp-message-too-large
             :message (format nil "The outgoing message exceeds ~D characters." limit)
             :limit limit))
    (when (find #\Newline text)
      (error 'acp-protocol-error
             :message "The encoded JSON contains a raw newline."))
    text))

(-> json--prepare (t) t)
(defun json--prepare (value)
  "Return VALUE with NIL spelled as an empty array and containers prepared.

Lists are arrays in this encoding, so NIL is the empty array. Booleans use
the canonical symbols and null the :NULL keyword, which Yason encodes
natively."
  (cond
    ((null value)
     (vector))
    ((stringp value)
     value)
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key element)
                  (setf (gethash key copy) (json--prepare element)))
                value)
       copy))
    ((vectorp value)
     (map 'vector #'json--prepare value))
    ((consp value)
     (mapcar #'json--prepare value))
    (t
     value)))

(-> json-decode (string &key (:limit integer)) t)
(defun json-decode (source &key (limit *acp-maximum-message-characters*))
  "Decode one JSON document from SOURCE within LIMIT characters.

Objects become EQUAL hash tables, arrays vectors, booleans the canonical
symbols, and null the :NULL keyword. Trailing non-blank text is an error."
  (when (> (length source) limit)
    (error 'acp-message-too-large
           :message (format nil "The incoming message exceeds ~D characters." limit)
           :limit limit))
  (let ((value
          (handler-case
              (with-input-from-string (stream source)
                (let ((decoded (yason:parse stream
                                            :json-arrays-as-vectors t
                                            :json-booleans-as-symbols t
                                            :json-nulls-as-keyword t)))
                  (loop for character = (read-char stream nil nil)
                        while character
                        unless (member character '(#\Space #\Tab #\Return #\Newline))
                          do (error "Unexpected text follows the JSON document."))
                  decoded))
            (acp-error (condition)
              (error condition))
            (error (cause)
              (error 'acp-protocol-error
                     :message (format nil "Could not decode JSON: ~A" cause)
                     :payload (bounded-diagnostic source))))))
    (json--measure value 0)
    value))
