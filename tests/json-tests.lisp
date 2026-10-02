(in-package #:agentcomms)

;;;; -- JSON Tests --

(define-test json-round-trip
  (let* ((object (json-object "jsonrpc" "2.0"
                              "id" 7
                              "flag" (json-true-value)
                              "off" (json-false-value)
                              "nothing" (json-null-value)
                              "items" (vector 1 "two" (json-object "k" "v"))
                              "list" (list "a" "b")
                              "empty" (vector)
                              "absent" nil))
         (text (json-encode object))
         (decoded (json-decode text)))
    (test-assert (not (find #\Newline text)) "compact encoding has no newline")
    (test-assert (not (nth-value 1 (gethash "absent" object)))
                 "NIL values are omitted from constructed objects")
    (test-equal "2.0" (json-get decoded "jsonrpc"))
    (test-equal 7 (json-get decoded "id"))
    (test-assert (json-true-p (json-get decoded "flag")))
    (test-assert (json-boolean-p (json-get decoded "off")))
    (test-assert (not (json-true-p (json-get decoded "off"))))
    (test-assert (json-null-p (json-get decoded "nothing")))
    (test-equal '("a" "b") (json-sequence->list (json-get decoded "list")))
    (test-equal nil (json-sequence->list (json-get decoded "empty")))
    (test-equal "[[],true]" (json-encode (list nil t)))
    (test-equal "v" (json-get (elt (json-get decoded "items") 2) "k"))
    (test-equal ':missing (json-get decoded "unknown" ':missing))
    (test-equal ':missing (json-get "not an object" "key" ':missing))))

(define-test json-decoding-rejects-malformed-documents
  (test-signals acp-protocol-error (json-decode "{\"a\": }"))
  (test-signals acp-protocol-error (json-decode "{} trailing"))
  (test-signals acp-protocol-error (json-sequence->list 42))
  (let ((*acp-maximum-message-characters* 8))
    (test-signals acp-message-too-large (json-decode "{\"abc\": 12345}"))
    (test-signals acp-message-too-large (json-encode (json-object "abc" 12345))))
  (let ((*json-maximum-depth* 2))
    (test-signals acp-protocol-error (json-decode "[[[1]]]")))
  (let ((*json-maximum-nodes* 3))
    (test-signals acp-protocol-error (json-decode "[1, 2, 3, 4]"))))
