(in-package #:agentcomms)

;;;; -- HTTP Limits --

(defparameter *http-maximum-header-octets* 65536
  "The most octets accepted in a request line and its header section.")

(defparameter *http-maximum-body-octets* (* 8 1024 1024)
  "The most octets accepted in one request body.")

(defparameter *http-maximum-chunk-line-octets* 4096
  "The most octets accepted in one chunk size line, extensions included.")

(defparameter *http-reason-phrases*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (204 . "No Content")
    (206 . "Partial Content")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (413 . "Content Too Large")
    (414 . "URI Too Long")
    (415 . "Unsupported Media Type")
    (417 . "Expectation Failed")
    (421 . "Misdirected Request")
    (422 . "Unprocessable Content")
    (426 . "Upgrade Required")
    (428 . "Precondition Required")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported"))
  "The reason phrase written after each known status code.")


;;;; -- Requests --

(defclass http-request ()
  ((method
    :initarg       :method
    :reader        http-request-method
    :type          string
    :documentation "The request method, such as \"GET\".")
   (target
    :initarg       :target
    :reader        http-request-target
    :type          string
    :documentation "The raw request target, query included.")
   (path
    :initarg       :path
    :reader        http-request-path
    :type          string
    :documentation "The raw request target without its query.")
   (query
    :initarg       :query
    :reader        http-request-query
    :type          list
    :documentation "The decoded query parameters as (NAME . VALUE) strings, in order.")
   (version
    :initarg       :version
    :reader        http-request-version
    :type          string
    :documentation "The protocol version, such as \"HTTP/1.1\".")
   (headers
    :initarg       :headers
    :reader        http-request-headers
    :type          list
    :documentation "The header fields as (LOWERCASE-NAME . VALUE) strings, in arrival order.")
   (body
    :initarg       :body
    :reader        http-request-body
    :type          octet-vector
    :documentation "The request content, empty when there is none."))
  (:documentation
   "An HTTP request independent of the protocol version that carried it."))

(defmethod print-object ((request http-request) stream)
  "Print REQUEST with its method and target."
  (print-unreadable-object (request stream :type t :identity t)
    (format stream "~A ~A" (http-request-method request) (http-request-target request))))

(-> make-http-request
    (&key (:method string) (:target string) (:version string) (:headers list)
          (:body (or null (vector (unsigned-byte 8)))))
    http-request)
(defun make-http-request (&key method target (version "HTTP/1.1") headers body)
  "Return a request for METHOD and TARGET with HEADERS and BODY.

The path and decoded query come from TARGET. Header names are stored in
lowercase. Signal HTTP-REQUEST-ERROR with status 400 when the query is
malformed."
  (multiple-value-bind (path query) (http--split-target target)
    (make-instance 'http-request
                   :method  method
                   :target  target
                   :path    path
                   :query   (if query
                                (http--parse-query query)
                                nil)
                   :version version
                   :headers (mapcar (lambda (header)
                                      (cons (string-downcase (first header)) (rest header)))
                                    headers)
                   :body    (coerce (or body #()) 'octet-vector))))

(-> http-request-header (http-request string) (or null string))
(defun http-request-header (request name)
  "Return the value of header NAME in REQUEST, or NIL when it is absent.

Name matching ignores case. Repeated fields are joined with \", \"."
  (http--header-value (http-request-headers request) name))

(-> http-request-header-token-p (http-request string string) boolean)
(defun http-request-header-token-p (request name token)
  "Return whether the comma-separated header NAME in REQUEST lists TOKEN, ignoring case."
  (http--header-token-p (http-request-headers request) name token))

(-> http-request-keep-alive-p (http-request) boolean)
(defun http-request-keep-alive-p (request)
  "Return whether the connection stays open after the response to REQUEST.

HTTP/1.1 connections stay open unless the request has Connection: close.
HTTP/1.0 connections stay open only with Connection: keep-alive."
  (cond
    ((http-request-header-token-p request "connection" "close")
     nil)
    ((string= (http-request-version request) "HTTP/1.0")
     (http-request-header-token-p request "connection" "keep-alive"))
    (t
     t)))

(-> http--header-value (list string) (or null string))
(defun http--header-value (headers name)
  "Return the value of field NAME in the alist HEADERS, joining repeats with \", \"."
  (let ((values (loop for (field-name . value) in headers
                      when (string-equal field-name name)
                        collect value)))
    (if values
        (string-join values ", ")
        nil)))

(-> http--header-token-p (list string string) boolean)
(defun http--header-token-p (headers name token)
  "Return whether field NAME in the alist HEADERS lists TOKEN, ignoring case."
  (let ((value (http--header-value headers name)))
    (and value
         (member token (http--split-list value) :test #'string-equal)
         t)))

(-> http--split-list (string) list)
(defun http--split-list (value)
  "Return the non-empty comma-separated elements of VALUE without surrounding whitespace."
  (remove "" (mapcar #'http--trim (split-sequence #\, value)) :test #'string=))

(-> http--trim (string) string)
(defun http--trim (string)
  "Return STRING without surrounding spaces and tabs."
  (string-trim '(#\Space #\Tab) string))

(-> http--split-target (string) (values string (or null string)))
(defun http--split-target (target)
  "Return the path of TARGET and its query text, or NIL when it has no query."
  (let ((mark (position #\? target)))
    (if mark
        (values (subseq target 0 mark) (subseq target (1+ mark)))
        (values target nil))))

(-> http--parse-query (string) list)
(defun http--parse-query (query)
  "Decode QUERY, form-urlencoded text, into (NAME . VALUE) strings in order.

A plus sign decodes to a space. Empty fields are skipped and a field
without an equals sign has an empty value."
  (loop for field in (split-sequence #\& query)
        unless (string= field "")
          collect (let ((mark (position #\= field)))
                    (if mark
                        (cons (http--percent-decode (subseq field 0 mark))
                              (http--percent-decode (subseq field (1+ mark))))
                        (cons (http--percent-decode field) "")))))

(-> http--percent-decode (string) string)
(defun http--percent-decode (text)
  "Decode the percent escapes and plus signs of TEXT as UTF-8.

Signal HTTP-REQUEST-ERROR with status 400 for a malformed escape or
invalid UTF-8."
  (let ((octets (make-array (length text) :element-type 'octet :fill-pointer 0))
        (index  0))
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 ((char= character #\%)
                  (let ((value (and (<= (+ index 3) (length text))
                                    (digit-char-p (char text (+ index 1)) 16)
                                    (digit-char-p (char text (+ index 2)) 16)
                                    (parse-integer text :start (1+ index) :end (+ index 3) :radix 16))))
                    (unless value
                      (http--fail 400 "The query has a malformed percent escape."))
                    (vector-push value octets)
                    (incf index 3)))
                 ((char= character #\+)
                  (vector-push 32 octets)
                  (incf index))
                 ((< (char-code character) 128)
                  (vector-push (char-code character) octets)
                  (incf index))
                 (t
                  (http--fail 400 "The query contains a character outside ASCII.")))))
    (handler-case (octets-to-string octets :encoding ':utf-8)
      (character-coding-error ()
        (http--fail 400 "The query is not valid UTF-8.")))))

(-> http--fail (integer string &key (:headers list)) nil)
(defun http--fail (status message &key headers)
  "Signal HTTP-REQUEST-ERROR with STATUS, MESSAGE, and response HEADERS."
  (error 'http-request-error :status status :message message :headers headers))


;;;; -- HTTP/1.1 Request Reading --

(-> http1-read-request
    (stream &key (:maximum-header-octets (integer 0)) (:maximum-body-octets (integer 0)))
    (or null http-request))
(defun http1-read-request (stream &key (maximum-header-octets *http-maximum-header-octets*)
                                       (maximum-body-octets *http-maximum-body-octets*))
  "Read one HTTP/1.0 or HTTP/1.1 request from the octet STREAM.

Return NIL at end of input before the first octet. The request line and
header section must fit in MAXIMUM-HEADER-OCTETS and the body, framed by
Content-Length or chunked Transfer-Encoding, in MAXIMUM-BODY-OCTETS.
When the request expects 100-continue and STREAM is also an output
stream, write the interim response before reading the body.

The HTTP/2 connection preface is returned as soon as its request line and
the empty line after it are read, with method PRI, target *, and version
HTTP/2.0; the caller reads the rest of the preface.

Signal HTTP-REQUEST-ERROR carrying the status to answer with: 400 for a
malformed request, 413 for an oversized body, 431 for an oversized head,
501 for an unsupported transfer coding, and 505 for another version."
  (let ((remaining maximum-header-octets)
        (line      nil))
    (loop
      (multiple-value-bind (text length) (http1--read-line stream remaining)
        (when (null text)
          (if (= remaining maximum-header-octets)
              (return-from http1-read-request nil)
              (http--fail 400 "The request ended before its request line.")))
        (decf remaining length)
        (unless (string= text "")
          (setf line text)
          (return))))
    (multiple-value-bind (method target version) (http1--parse-request-line line)
      (multiple-value-bind (headers remaining) (http1--read-header-section stream remaining)
        (declare (ignore remaining))
        (cond
          ((string= version "HTTP/2.0")
           (when headers
             (http--fail 400 "The HTTP/2 connection preface has header fields."))
           (make-http-request :method method :target target :version version))
          (t
           (http1--check-host headers version)
           (let ((body (http1--read-body stream
                                         :headers               headers
                                         :version               version
                                         :maximum-body-octets   maximum-body-octets
                                         :maximum-header-octets maximum-header-octets)))
             (make-http-request :method  method
                                :target  target
                                :version version
                                :headers headers
                                :body    body))))))))

(-> http1--read-line (stream (integer 0) &key (:overflow-status integer))
    (values (or null string) (integer 0)))
(defun http1--read-line (stream limit &key (overflow-status 431))
  "Read one line of at most LIMIT octets, terminator included, from STREAM.

Return the line without its CRLF or bare LF and the octets consumed, or
NIL and 0 at end of input before the first octet. Octets map to
characters as ISO-8859-1. Signal HTTP-REQUEST-ERROR with OVERFLOW-STATUS
for a longer line and 400 for input ending inside the line, a bare CR, or
a NUL."
  (let ((line  (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
        (count 0))
    (loop
      (let ((octet (read-byte stream nil nil)))
        (cond
          ((null octet)
           (if (zerop count)
               (return-from http1--read-line (values nil 0))
               (http--fail 400 "The input ended inside a line.")))
          ((>= count limit)
           (http--fail overflow-status "The request head is too large."))
          ((= octet 10)
           (incf count)
           (return))
          (t
           (incf count)
           (vector-push-extend (code-char octet) line)))))
    (when (and (plusp (length line)) (char= (char line (1- (length line))) #\Return))
      (decf (fill-pointer line)))
    (when (find-if (lambda (character) (member character '(#\Return #\Nul))) line)
      (http--fail 400 "A line contains a bare carriage return or a NUL."))
    (values (coerce line 'simple-string) count)))

(-> http1--parse-request-line (string) (values string string string))
(defun http1--parse-request-line (line)
  "Return the method, target, and version of request LINE.

Signal HTTP-REQUEST-ERROR with status 400 when LINE is malformed and 505
when its version is neither HTTP/1.0, HTTP/1.1, nor the HTTP/2 preface."
  (let ((parts (split-sequence #\Space line)))
    (unless (= (length parts) 3)
      (http--fail 400 "The request line is malformed."))
    (destructuring-bind (method target version) parts
      (unless (http--token-p method)
        (http--fail 400 "The request method is not a token."))
      (unless (and (plusp (length target))
                   (every (lambda (character) (char<= #\! character #\~)) target))
        (http--fail 400 "The request target is malformed."))
      (unless (and (= (length version) 8)
                   (string= "HTTP/" version :end2 5)
                   (digit-char-p (char version 5))
                   (char= (char version 6) #\.)
                   (digit-char-p (char version 7)))
        (http--fail 400 "The request line has a malformed protocol version."))
      (unless (or (member version '("HTTP/1.0" "HTTP/1.1") :test #'string=)
                  (and (string= version "HTTP/2.0")
                       (string= method "PRI")
                       (string= target "*")))
        (http--fail 505 (format nil "The protocol version ~A is not supported." version)))
      (values method target version))))

(-> http1--read-header-section (stream (integer 0) &key (:overflow-status integer))
    (values list (integer 0)))
(defun http1--read-header-section (stream remaining &key (overflow-status 431))
  "Read header lines up to the empty line within REMAINING octets.

Return the header alist in arrival order and the octets left. Signal
HTTP-REQUEST-ERROR with OVERFLOW-STATUS when the section does not fit."
  (let ((headers nil))
    (loop
      (multiple-value-bind (line length)
          (http1--read-line stream remaining :overflow-status overflow-status)
        (when (null line)
          (http--fail 400 "The input ended inside the header section."))
        (decf remaining length)
        (when (string= line "")
          (return (values (nreverse headers) remaining)))
        (push (http1--parse-header-line line) headers)))))

(-> http1--parse-header-line (string) cons)
(defun http1--parse-header-line (line)
  "Return header LINE as (LOWERCASE-NAME . TRIMMED-VALUE).

Signal HTTP-REQUEST-ERROR with status 400 for obsolete line folding, a
name that is not a token, or a control character in the value."
  (when (member (char line 0) '(#\Space #\Tab))
    (http--fail 400 "Obsolete header line folding is not accepted."))
  (let ((colon (position #\: line)))
    (unless (and colon (http--token-p (subseq line 0 colon)))
      (http--fail 400 "A header field name is malformed."))
    (let ((value (http--trim (subseq line (1+ colon)))))
      (unless (every #'http--field-value-character-p value)
        (http--fail 400 "A header field value contains a control character."))
      (cons (string-downcase (subseq line 0 colon)) value))))

(-> http1--check-host (list string) null)
(defun http1--check-host (headers version)
  "Signal HTTP-REQUEST-ERROR with status 400 unless HEADERS has one Host, as VERSION requires."
  (let ((count (count "host" headers :key #'first :test #'string=)))
    (when (or (> count 1)
              (and (zerop count) (string= version "HTTP/1.1")))
      (http--fail 400 "An HTTP/1.1 request needs exactly one Host header field.")))
  nil)

(-> http1--read-body
    (stream &key (:headers list) (:version string) (:maximum-body-octets (integer 0))
            (:maximum-header-octets (integer 0)))
    octet-vector)
(defun http1--read-body (stream &key headers version maximum-body-octets maximum-header-octets)
  "Read the body framed by HEADERS from STREAM.

Content-Length gives an exact length and chunked Transfer-Encoding a
sequence of chunks whose trailer fields fit in MAXIMUM-HEADER-OCTETS.
Without either the body is empty."
  (let ((transfer-encoding (http--header-value headers "transfer-encoding"))
        (content-length    (http1--content-length headers)))
    (when (and transfer-encoding content-length)
      (http--fail 400 "The request has both Transfer-Encoding and Content-Length."))
    (cond
      (transfer-encoding
       (http1--check-transfer-coding transfer-encoding version)
       (http1--send-continue stream headers version)
       (http1--read-chunked-body stream
                                 :maximum-body-octets   maximum-body-octets
                                 :maximum-header-octets maximum-header-octets))
      ((and content-length (plusp content-length))
       (when (> content-length maximum-body-octets)
         (http--fail 413 (format nil "The request body exceeds ~D octets." maximum-body-octets)))
       (http1--send-continue stream headers version)
       (let* ((body (make-octet-vector content-length))
              (end  (read-sequence body stream)))
         (when (< end content-length)
           (http--fail 400 "The input ended inside the request body."))
         body))
      (t
       (make-octet-vector 0)))))

(-> http1--content-length (list) (or null (integer 0)))
(defun http1--content-length (headers)
  "Return the Content-Length in HEADERS, or NIL when there is none.

Repeated identical values are accepted. Signal HTTP-REQUEST-ERROR with
status 400 for differing or malformed values."
  (let ((value (http--header-value headers "content-length")))
    (if (null value)
        nil
        (let ((lengths (mapcar #'http--trim (split-sequence #\, value))))
          (unless (every (lambda (length)
                           (and (plusp (length length)) (every #'digit-char-p length)))
                         lengths)
            (http--fail 400 "The Content-Length is malformed."))
          (let ((numbers (mapcar #'parse-integer lengths)))
            (unless (every (partial #'= (first numbers)) numbers)
              (http--fail 400 "The request has differing Content-Length values."))
            (first numbers))))))

(-> http1--check-transfer-coding (string string) null)
(defun http1--check-transfer-coding (value version)
  "Accept VALUE, a Transfer-Encoding field of a VERSION request, only as plain chunked.

Signal HTTP-REQUEST-ERROR with status 400 when the body length cannot be
determined and 501 for a coding other than chunked."
  (let ((codings (mapcar (lambda (coding)
                           (http--trim (subseq coding 0 (position #\; coding))))
                         (http--split-list value))))
    (cond
      ((string= version "HTTP/1.0")
       (http--fail 400 "An HTTP/1.0 request cannot use Transfer-Encoding."))
      ((or (null codings)
           (not (string-equal (first (last codings)) "chunked"))
           (> (count "chunked" codings :test #'string-equal) 1))
       (http--fail 400 "The request body must end with one chunked transfer coding."))
      ((rest codings)
       (http--fail 501 (format nil "The transfer coding ~A is not implemented."
                               (first codings))))))
  nil)

(-> http1--send-continue (stream list string) null)
(defun http1--send-continue (stream headers version)
  "Write a 100 Continue response when a VERSION request with HEADERS expects one.

Nothing is written unless STREAM is also an output stream."
  (when (and (string= version "HTTP/1.1")
             (output-stream-p stream)
             (let ((expect (http--header-value headers "expect")))
               (and expect (string-equal expect "100-continue"))))
    (http1-write-response-head stream 100))
  nil)

(-> http1--read-chunked-body
    (stream &key (:maximum-body-octets (integer 0)) (:maximum-header-octets (integer 0)))
    octet-vector)
(defun http1--read-chunked-body (stream &key maximum-body-octets maximum-header-octets)
  "Read a chunked body from STREAM and discard its trailer fields.

Signal HTTP-REQUEST-ERROR with status 400 for malformed framing and 413
when the chunks exceed MAXIMUM-BODY-OCTETS."
  (let ((body (make-array 0 :element-type 'octet :adjustable t :fill-pointer 0)))
    (loop
      (let ((size (http1--read-chunk-size stream)))
        (when (zerop size)
          (http1--read-header-section stream maximum-header-octets)
          (return (coerce body 'octet-vector)))
        (when (> (+ (length body) size) maximum-body-octets)
          (http--fail 413 (format nil "The request body exceeds ~D octets." maximum-body-octets)))
        (let* ((start (length body))
               (end   (+ start size)))
          (adjust-array body end :fill-pointer end)
          (when (< (read-sequence body stream :start start) end)
            (http--fail 400 "The input ended inside a chunk.")))
        (let ((line (http1--read-line stream 2 :overflow-status 400)))
          (unless (equal line "")
            (http--fail 400 "A chunk is not followed by a line break.")))))))

(-> http1--read-chunk-size (stream) (integer 0))
(defun http1--read-chunk-size (stream)
  "Read one chunk size line from STREAM and return the size, ignoring extensions."
  (let ((line (http1--read-line stream *http-maximum-chunk-line-octets* :overflow-status 400)))
    (when (null line)
      (http--fail 400 "The input ended before a chunk size."))
    (let* ((end    (or (position-if-not (lambda (character) (digit-char-p character 16)) line)
                       (length line)))
           (suffix (string-left-trim '(#\Space #\Tab) (subseq line end))))
      (unless (and (plusp end)
                   (or (string= suffix "")
                       (char= (char suffix 0) #\;)))
        (http--fail 400 "A chunk size line is malformed."))
      (parse-integer line :end end :radix 16))))


;;;; -- HTTP/1.1 Response Writing --

(-> http1-write-response
    (stream (integer 100 999) &key (:headers list) (:body (or null string (vector (unsigned-byte 8)))))
    null)
(defun http1-write-response (stream status &key headers body)
  "Write a complete HTTP/1.1 response with STATUS, HEADERS, and BODY to STREAM.

HEADERS is a list of (NAME . VALUE) strings. BODY is octets or a string
sent as UTF-8. Content-Length is computed, so HEADERS must not carry it
or Transfer-Encoding; responses with status 1xx, 204, or 304 carry no
body and no length."
  (let ((octets (http--body-octets body)))
    (when (or (http--header-value headers "content-length")
              (http--header-value headers "transfer-encoding"))
      (error 'acp-error
             :message "HTTP1-WRITE-RESPONSE computes the message length itself."))
    (cond
      ((or (< status 200) (= status 204) (= status 304))
       (when (plusp (length octets))
         (error 'acp-error
                :message (format nil "A response with status ~D has no body." status)))
       (http1--write-head stream (http1--status-line status) headers))
      (t
       (http1--write-head stream (http1--status-line status)
                          (append headers
                                  (list (cons "Content-Length"
                                              (princ-to-string (length octets))))))
       (write-sequence octets stream))))
  (force-output stream)
  nil)

(-> http1-write-response-head (stream (integer 100 999) &key (:headers list)) null)
(defun http1-write-response-head (stream status &key headers)
  "Write the status line for STATUS, HEADERS, and the empty line to STREAM.

The body, when there is one, follows on STREAM and ends when the
connection closes, as Server-Sent Events and switched protocols do."
  (http1--write-head stream (http1--status-line status) headers)
  (force-output stream)
  nil)

(-> http1--status-line (integer) string)
(defun http1--status-line (status)
  "Return the HTTP/1.1 status line for STATUS with its reason phrase."
  (format nil "HTTP/1.1 ~D ~A" status (or (rest (assoc status *http-reason-phrases*)) "")))

(-> http1--write-head (stream string list) null)
(defun http1--write-head (stream start-line headers)
  "Write START-LINE, the HEADERS alist, and the closing empty line to STREAM.

Signal ACP-ERROR when a name is not a token or a value contains a
character that cannot appear in a header field."
  (let ((text (with-output-to-string (output)
                (write-string start-line output)
                (write-string (http--line-break) output)
                (loop for (name . value) in headers
                      do (unless (and (stringp name) (http--token-p name))
                           (error 'acp-error
                                  :message (format nil "The header name ~S is not a token." name)))
                         (unless (and (stringp value) (every #'http--field-value-character-p value))
                           (error 'acp-error
                                  :message (format nil "The value of header ~A is not valid." name)))
                         (write-string name output)
                         (write-string ": " output)
                         (write-string value output)
                         (write-string (http--line-break) output))
                (write-string (http--line-break) output))))
    (write-sequence (string-to-octets text :encoding ':latin-1) stream))
  nil)

(-> http--line-break () string)
(defun http--line-break ()
  "Return the CRLF sequence that ends HTTP/1.1 lines."
  (coerce '(#\Return #\Linefeed) 'string))

(-> http--body-octets ((or null string (vector (unsigned-byte 8)))) octet-vector)
(defun http--body-octets (body)
  "Return BODY as octets, encoding a string as UTF-8."
  (etypecase body
    (null
     (make-octet-vector 0))
    (string
     (string-to-octets body :encoding ':utf-8))
    (vector
     (coerce body 'octet-vector))))

(-> http--token-p (string) boolean)
(defun http--token-p (string)
  "Return whether STRING is a non-empty HTTP token."
  (and (plusp (length string))
       (every (lambda (character)
                (or (char<= #\a character #\z)
                    (char<= #\A character #\Z)
                    (char<= #\0 character #\9)
                    (find character "!#$%&'*+-.^_`|~")))
              string)
       t))

(-> http--field-value-character-p (character) boolean)
(defun http--field-value-character-p (character)
  "Return whether CHARACTER may appear in a header field value."
  (let ((code (char-code character)))
    (or (= code 9)
        (<= 32 code 126)
        (<= 128 code 255))))
