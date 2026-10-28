(in-package #:agentcomms)

;;;; -- HTTP Test Support --

(-> test-http-octets (string &key (:line-break (member :crlf :lf))) octet-vector)
(defun test-http-octets (text &key (line-break ':crlf))
  "Return TEXT as UTF-8 octets, writing each newline as LINE-BREAK."
  (string-to-octets
   (if (eq line-break ':crlf)
       (with-output-to-string (output)
         (loop for character across text
               do (if (char= character #\Newline)
                      (write-string (http--line-break) output)
                      (write-char character output))))
       text)
   :encoding ':utf-8))

(-> test-call-with-octet-input (octet-vector function) t)
(defun test-call-with-octet-input (octets function)
  "Call FUNCTION with a binary input stream holding OCTETS and return its values."
  (uiop:with-temporary-file (:pathname pathname)
    (with-open-file (output pathname :direction    ':output
                                     :element-type 'octet
                                     :if-exists    ':supersede)
      (write-sequence octets output))
    (with-open-file (input pathname :element-type 'octet)
      (funcall function input))))

(-> test-octets-written (function) octet-vector)
(defun test-octets-written (function)
  "Call FUNCTION with a binary output stream and return the octets it wrote."
  (uiop:with-temporary-file (:pathname pathname)
    (with-open-file (output pathname :direction    ':output
                                     :element-type 'octet
                                     :if-exists    ':supersede)
      (funcall function output))
    (with-open-file (input pathname :element-type 'octet)
      (let ((octets (make-octet-vector (file-length input))))
        (read-sequence octets input)
        octets))))

(-> test-read-request (string &key (:line-break (member :crlf :lf))
                              (:maximum-header-octets (integer 0))
                              (:maximum-body-octets (integer 0)))
    (or null http-request))
(defun test-read-request (text &key (line-break ':crlf)
                                    (maximum-header-octets *http-maximum-header-octets*)
                                    (maximum-body-octets *http-maximum-body-octets*))
  "Read one request from TEXT with the given line breaks and limits."
  (test-call-with-octet-input
   (test-http-octets text :line-break line-break)
   (lambda (stream)
     (http1-read-request stream :maximum-header-octets maximum-header-octets
                                :maximum-body-octets   maximum-body-octets))))

(-> test-octets-text (octet-vector) string)
(defun test-octets-text (octets)
  "Return OCTETS decoded as UTF-8."
  (octets-to-string octets :encoding ':utf-8))


;;;; -- HTTP Request Reading Tests --

(define-test http-reads-valid-requests
  (dolist (case
           '(("GET /acp?token=a%20b&x=1+2&&flag HTTP/1.1
Host: example
Accept:  text/event-stream

"
              :method "GET" :target "/acp?token=a%20b&x=1+2&&flag" :path "/acp"
              :query   (("token" . "a b") ("x" . "1 2") ("flag" . ""))
              :headers (("host" . "example") ("accept" . "text/event-stream")))
             ("GET /%E2%88%91?q=%E2%88%91 HTTP/1.1
Host: example

"
              :line-break :lf :path "/%E2%88%91" :query (("q" . "∑"))
              :headers (("host" . "example")))
             ("

POST /acp HTTP/1.1
Host: example
Content-Length: 5, 5

hello"
              :method "POST" :body "hello"
              :headers (("host" . "example") ("content-length" . "5, 5")))
             ("POST /acp HTTP/1.1
Host: example
Transfer-Encoding: chunked

5;name=value
hello
6
 world
0
Trailer-Field: ignored

"
              :method "POST" :body "hello world"
              :headers (("host" . "example") ("transfer-encoding" . "chunked")))
             ("GET * HTTP/1.0

"
              :version "HTTP/1.0" :target "*" :path "*" :headers nil)))
    (destructuring-bind (text &key (line-break ':crlf) (method "GET") target path query
                                   (version "HTTP/1.1") headers (body ""))
        case
      (let ((request (test-read-request text :line-break line-break)))
        (test-equal method (http-request-method request))
        (when target
          (test-equal target (http-request-target request)))
        (when path
          (test-equal path (http-request-path request)))
        (test-equal query (http-request-query request))
        (test-equal version (http-request-version request))
        (test-equal headers (http-request-headers request))
        (test-equal body (test-octets-text (http-request-body request)))))))

(define-test http-rejects-invalid-requests
  (dolist (case '((400 "GET /
")
                  (400 "GET  / HTTP/1.1
Host: a

")
                  (400 "GET / HTTP/1.1

")
                  (400 "GET / HTTP/1.1
Host: a
Host: b

")
                  (505 "GET / HTTP/3.0
Host: a

")
                  (505 "GET / HTTP/2.0
Host: a

")
                  (505 "PRI / HTTP/2.0

")
                  (505 "GET / HTTP/0.9
")
                  (400 "GET / HTTPS/1.1
Host: a

")
                  (400 "G(T / HTTP/1.1
Host: a

")
                  (400 "GET / HTTP/1.1
Host: a
X-Folded: one
 two

")
                  (400 "GET / HTTP/1.1
Host : a

")
                  (400 "GET / HTTP/1.1
Host: a
Bad Name: value

")
                  (400 "POST / HTTP/1.1
Host: a
Content-Length: 5
Transfer-Encoding: chunked

5
hello
0

")
                  (400 "POST / HTTP/1.1
Host: a
Content-Length: five

")
                  (400 "POST / HTTP/1.1
Host: a
Content-Length: 5
Content-Length: 6

hello!")
                  (501 "POST / HTTP/1.1
Host: a
Transfer-Encoding: gzip, chunked

")
                  (400 "POST / HTTP/1.1
Host: a
Transfer-Encoding: chunked, gzip

")
                  (400 "POST / HTTP/1.0
Transfer-Encoding: chunked

0

")
                  (400 "POST / HTTP/1.1
Host: a
Transfer-Encoding: chunked

zz
")
                  (400 "POST / HTTP/1.1
Host: a
Transfer-Encoding: chunked

5
helloX
0

")
                  (400 "POST / HTTP/1.1
Host: a
Content-Length: 10

short")
                  (400 "GET /?q=%zz HTTP/1.1
Host: a

")
                  (400 "GET /?q=%FF HTTP/1.1
Host: a

")
                  (400 "GET / HTTP/1.1
Host: a
")
                  (413 "POST / HTTP/1.1
Host: a
Content-Length: 5

hello"
                   :maximum-body-octets 4)
                  (413 "POST / HTTP/1.1
Host: a
Transfer-Encoding: chunked

3
abc
3
def
0

"
                   :maximum-body-octets 4)
                  (431 "GET / HTTP/1.1
Host: a
X-Long: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

"
                   :maximum-header-octets 32)))
    (destructuring-bind (status text &rest options) case
      (test-equal status
                  (http-request-error-status
                   (test-signals http-request-error
                     (apply #'test-read-request text options)))))))

(define-test http-reads-connection-preface-and-pipelined-requests
  (test-call-with-octet-input
   (test-http-octets "PRI * HTTP/2.0

SM

")
   (lambda (stream)
     (let ((request (http1-read-request stream)))
       (test-equal '("PRI" "*" "HTTP/2.0")
                   (list (http-request-method request)
                         (http-request-target request)
                         (http-request-version request)))
       (let ((remainder (make-octet-vector 6)))
         (test-equal 6 (read-sequence remainder stream))
         (test-equal (test-http-octets (format nil "SM~%~%")) remainder :test #'equalp)))))
  (test-call-with-octet-input
   (test-http-octets "GET /one HTTP/1.1
Host: a

GET /two HTTP/1.1
Host: a

")
   (lambda (stream)
     (test-equal "/one" (http-request-path (http1-read-request stream)))
     (test-equal "/two" (http-request-path (http1-read-request stream)))
     (test-equal nil (http1-read-request stream))))
  (test-equal nil (test-read-request "")))

(define-test http-answers-expected-continue
  (let ((written (test-octets-written
                  (lambda (output)
                    (test-call-with-octet-input
                     (test-http-octets "POST / HTTP/1.1
Host: a
Expect: 100-continue
Content-Length: 2

ok")
                     (lambda (input)
                       (test-equal "ok"
                                   (test-octets-text
                                    (http-request-body
                                     (http1-read-request (make-two-way-stream input output)))))))))))
    (test-equal "HTTP/1.1 100 Continue

" (remove #\Return (test-octets-text written)))))


;;;; -- HTTP Request Object Tests --

(define-test http-request-headers-and-tokens
  (let ((request (make-http-request :method  "GET"
                                    :target  "/acp?a=1&a=2"
                                    :headers '(("Connection" . "keep-alive, Upgrade")
                                               ("X-Repeated" . "1")
                                               ("x-repeated" . "2")))))
    (test-equal "/acp" (http-request-path request))
    (test-equal '(("a" . "1") ("a" . "2")) (http-request-query request))
    (test-equal "1, 2" (http-request-header request "X-REPEATED"))
    (test-equal nil (http-request-header request "missing"))
    (test-equal t (http-request-header-token-p request "connection" "upgrade"))
    (test-equal nil (http-request-header-token-p request "connection" "close"))
    (test-equal 0 (length (http-request-body request))))
  (dolist (case '(("HTTP/1.1" nil t)
                  ("HTTP/1.1" "close" nil)
                  ("HTTP/1.1" "Upgrade, Close" nil)
                  ("HTTP/1.0" nil nil)
                  ("HTTP/1.0" "Keep-Alive" t)))
    (destructuring-bind (version connection expected) case
      (test-equal expected
                  (http-request-keep-alive-p
                   (make-http-request :method  "GET"
                                      :target  "/"
                                      :version version
                                      :headers (and connection
                                                    (list (cons "connection" connection)))))))))


;;;; -- HTTP Response Writing Tests --

(define-test http-writes-responses
  (dolist (case `((,(lambda (stream)
                      (http1-write-response stream 200 :headers '(("Content-Type" . "text/plain"))
                                                       :body    "∑"))
                   "HTTP/1.1 200 OK
Content-Type: text/plain
Content-Length: 3

∑")
                  (,(lambda (stream)
                      (http1-write-response stream 404))
                   "HTTP/1.1 404 Not Found
Content-Length: 0

")
                  (,(lambda (stream)
                      (http1-write-response stream 204 :headers '(("X-A" . "b"))))
                   "HTTP/1.1 204 No Content
X-A: b

")
                  (,(lambda (stream)
                      (http1-write-response stream 299 :body (string-to-octets "x")))
                   ,(format nil "HTTP/1.1 299 ~%Content-Length: 1~%~%x"))
                  (,(lambda (stream)
                      (http1-write-response-head stream 200 :headers '(("Content-Type" . "text/event-stream"))))
                   "HTTP/1.1 200 OK
Content-Type: text/event-stream

")))
    (destructuring-bind (function expected) case
      (test-equal (test-http-octets expected) (test-octets-written function) :test #'equalp)))
  (dolist (arguments '((200 :headers (("X-Bad" . "a
b")))
                       (200 :headers (("Bad Name" . "a")))
                       (200 :headers (("Content-Length" . "1")) :body "x")
                       (204 :body "x")
                       (200 :headers (("X-Wide" . "∑")))))
    (test-signals acp-error
      (test-octets-written
       (lambda (stream)
         (apply #'http1-write-response stream arguments))))))
