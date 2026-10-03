(in-package #:agentcomms)

;;;; -- JSON Boundary Tests --

(define-test json-objects-omit-absent-fields
  (let ((object (json-object "id" 7 "absent" nil "nothing" ':null "off" (json-false))))
    (test-assert (not (nth-value 1 (gethash "absent" object)))
                 "NIL values are omitted from constructed objects")
    (test-equal "{\"id\":7,\"nothing\":null,\"off\":false}"
                (json-encode object))
    (test-equal ':missing (json-get object "unknown" ':missing))
    (test-equal ':missing (json-get "not an object" "key" ':missing))
    (test-equal nil (json-get (json-decode "{\"off\":false}") "off" ':missing))
    (test-equal '("a" "b") (json-sequence->list (json-decode "[\"a\",\"b\"]")))
    (test-equal nil (json-sequence->list nil))))

(define-test json-boundary-signals-protocol-conditions
  (test-signals acp-protocol-error (json-decode "{\"a\": }"))
  (test-signals acp-protocol-error (json-decode "{} trailing"))
  (test-signals acp-protocol-error (json-encode (json-object "f" #'car)))
  (test-signals acp-protocol-error (json-sequence->list 42))
  (let ((*acp-maximum-message-characters* 8))
    (test-signals acp-message-too-large (json-decode "{\"abc\": 12345}"))
    (test-signals acp-message-too-large (json-encode (json-object "abc" 12345))))
  (let ((*json-maximum-depth* 2))
    (test-signals acp-protocol-error (json-decode "[[[1]]]")))
  (let ((*json-maximum-nodes* 3))
    (test-signals acp-protocol-error (json-decode "[1, 2, 3, 4]"))))
