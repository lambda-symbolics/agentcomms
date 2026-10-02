(in-package #:agentcomms)

;;;; -- Connection Admission and Ownership --

(define-test connection-accepts-json-rpc-identifiers
    (multiple-value-bind (server-channel client-channel) (make-acp-channel-pair)
      (let ((connection (make-acp-connection :channel server-channel
                                             :peer (make-instance 'test-echo-peer))))
        (unwind-protect
             (dolist (identifier '("named-request" 7 1.25 :null))
               (channel-write-message
                client-channel
                (json-encode (json-object "jsonrpc" "2.0" "id" identifier
                                          "method" "echo" "params" (json-object "text" "value"))))
               (let ((response (json-decode (channel-read-message client-channel))))
                 (test-equal identifier (json-get response "id") :test #'equalp)
                 (test-equal "value" (json-get (json-get response "result") "text"))))
          (connection-close connection)
          (channel-close client-channel)))))

(define-test connection-bounds-and-cancels-owned-handlers
    (multiple-value-bind (server-channel client-channel)
        (make-acp-channel-pair)
      (let* ((peer (make-instance 'test-echo-peer))
             (connection
              (make-acp-connection :channel server-channel :peer peer :maximum-inbound-requests 1)))
        (unwind-protect
             (progn
               (channel-write-message client-channel
                                      "{\"jsonrpc\":\"2.0\",\"id\":\"active\",\"method\":\"wait-for-cancel\"}")
               (test-assert
                (test-wait-until
                 (lambda () (= 1 (hash-table-count (acp-connection-inbound connection))))))
               (dolist (identifier '("active" "excess"))
                 (channel-write-message client-channel
                                        (json-encode
                                         (json-object "jsonrpc" "2.0" "id" identifier "method" "echo")))
                 (let ((response (json-decode (channel-read-message client-channel))))
                   (test-equal identifier (json-get response "id"))
                   (test-equal
                    (if (equal identifier "active")
                        -32600
                        -32001)
                    (json-get (json-get response "error") "code"))))
               (channel-write-message client-channel
                                      "{\"jsonrpc\":\"2.0\",\"method\":\"$/cancel_request\",\"params\":{\"requestId\":\"active\"}}")
               (test-equal -32800
                           (json-get (json-get (json-decode (channel-read-message client-channel)) "error")
                                     "code"))
               (test-assert
                (test-wait-until
                 (lambda () (zerop (hash-table-count (acp-connection-inbound connection))))))
               (channel-write-message client-channel
                                      "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"wait-for-cancel\"}")
               (test-assert
                (test-wait-until
                 (lambda () (= 1 (hash-table-count (acp-connection-inbound connection))))))
               (let* ((request (gethash 2 (acp-connection-inbound connection)))
                      (thread (acp-inbound-request-thread request)))
                 (connection-close connection)
                 (test-assert (acp-inbound-request-cancelled-p request))
                 (test-assert (not (thread-alive-p thread)))
                 (test-equal 1 (test-echo-peer-closed-count peer))))
          (connection-close connection)
          (channel-close client-channel)))))

(define-test connection-wakes-the-correct-response-waiter
    (multiple-value-bind (server-channel raw-channel) (make-acp-channel-pair)
      (let ((connection (make-acp-connection :channel server-channel :peer (make-instance 'acp-peer)))
            (threads nil)
            (results (make-hash-table :test #'equal)))
        (unwind-protect
             (progn
               (dolist (method '("first" "second"))
                 (let ((name method))
                   (push (make-thread
                          (lambda ()
                            (setf (gethash name results)
                                  (handler-case (connection-request connection name nil)
                                    (acp-connection-closed () :closed)))))
                         threads)))
               (let* ((requests (loop repeat 2 collect (json-decode (channel-read-message raw-channel))))
                      (second (find "second" requests :key (lambda (message) (json-get message "method"))
                                    :test #'equal)))
                 (channel-write-message raw-channel
                                        (json-encode (json-object "jsonrpc" "2.0" "id" (json-get second "id") "result" "ready")))
                 (test-assert (test-wait-until (lambda () (gethash "second" results)) :seconds 1))
                 (test-equal "ready" (gethash "second" results))
                 (test-assert (not (gethash "first" results)))
                 (connection-close connection)
                 (mapc #'join-thread threads)
                 (test-equal :closed (gethash "first" results))))
          (connection-close connection)
          (channel-close raw-channel)
          (mapc #'join-thread threads)))))

(define-test connection-cleans-abandoned-outgoing-request
    (multiple-value-bind (server-channel raw-channel) (make-acp-channel-pair)
      (let* ((connection (make-acp-connection :channel server-channel :peer (make-instance 'acp-peer)))
             (thread (make-thread (lambda ()
                                    (catch 'abandon
                                      (connection-request connection "pending" nil))))))
        (unwind-protect
             (let* ((request (json-decode (channel-read-message raw-channel)))
                    (identifier (json-get request "id")))
               (bordeaux-threads:interrupt-thread thread (lambda () (throw 'abandon nil)))
               (join-thread thread)
               (let ((cancellation (json-decode (channel-read-message raw-channel))))
                 (test-equal "$/cancel_request" (json-get cancellation "method"))
                 (test-equal identifier (json-get (json-get cancellation "params") "requestId")))
               (test-assert (zerop (hash-table-count (acp-connection-pending connection)))))
          (connection-close connection)
          (channel-close raw-channel)
          (join-thread thread)))))

;;;; -- ACP Path and Prompt Contracts --

(define-test schema-absolute-path-validation
    (dolist (path '("/work/file" "C:/work/file" "D:\\work\\file" "\\\\server\\share\\file"))
      (test-equal path (acp-field (json-object "path" path) "path" :type ':absolute-path))
      (test-equal path (json-get (acp-tool-call-location path) "path")))
  (dolist (path '("" "." "file" "../work" "C:file" "\\file" "\\\\server"))
    (test-signals acp-method-error
                  (acp-field (json-object "path" path) "path" :type ':absolute-path))
    (test-signals acp-method-error (acp-diff-content path "content")))
  (test-signals acp-method-error
                (agent--session-setup-arguments (json-object "cwd" "relative" "mcpServers" #())))
  (test-signals acp-method-error
                (agent--session-setup-arguments
                 (json-object "cwd" "/work" "mcpServers" #() "additionalDirectories" #("relative")))))

(define-test agent-rejects-overlapping-prompts
    (let* ((agent (make-instance 'test-agent))
           (session (agent--register-session agent "session" "/work")))
      (setf (acp-agent-session-prompt-active-p session) t)
      (test-equal -32001
                  (acp-method-error-code
                   (test-signals acp-method-error
                                 (agent--run-prompt agent session (list (acp-text-content "unused")) (json-object)))))
      (test-assert (acp-agent-session-prompt-active-p session))
      (test-assert (null (test-agent-prompts agent)))))

(define-test agent-disconnect-marks-and-cancels-all-sessions
    (multiple-value-bind (channel remote) (make-acp-channel-pair)
      (let* ((agent (make-instance 'test-agent))
             (connection (make-acp-connection :channel channel :peer agent))
             (first (agent--register-session agent "first" "/work"))
             (second (agent--register-session agent "second" "/work")))
        (unwind-protect
             (progn
               (connection-close connection)
               (test-equal 2 (test-agent-cancellations agent))
               (test-assert (acp-agent-session-cancel-requested-p first))
               (test-assert (acp-agent-session-cancel-requested-p second)))
          (connection-close connection)
          (channel-close remote)))))

;;;; -- Session Teardown Races --

(defclass lifecycle-test-agent (test-agent)
  ((lock :initform (make-lock "lifecycle test") :reader lifecycle-test-lock)
   (condition :initform (make-condition-variable) :reader lifecycle-test-condition)
   (entered-p :initform nil :accessor lifecycle-test-entered-p)
   (release-p :initform nil :accessor lifecycle-test-release-p)
   (closed-p :initform nil :accessor lifecycle-test-closed-p)))

(defmethod agent-prompt ((agent lifecycle-test-agent) session-id prompt params)
  (declare (ignore session-id prompt params))
  (with-lock-held ((lifecycle-test-lock agent))
    (setf (lifecycle-test-entered-p agent) t)
    (loop until (lifecycle-test-release-p agent)
          do (condition-wait (lifecycle-test-condition agent) (lifecycle-test-lock agent))))
  ':end-turn)

(defmethod agent-close-session ((agent lifecycle-test-agent) session-id params)
  (declare (ignore params))
  (test-assert (not (acp-agent-session-prompt-active-p (acp-agent-session agent session-id))))
  (setf (lifecycle-test-closed-p agent) t)
  nil)

(define-test agent-close-waits-for-prompt-completion
    (let* ((agent (make-instance 'lifecycle-test-agent))
           (session (agent--register-session agent "session" "/work"))
           (outcome nil)
           (prompt-thread (make-thread
                           (lambda ()
                             (setf outcome (agent--run-prompt agent session nil (json-object))))))
           (close-thread nil))
      (unwind-protect
           (progn
             (test-assert (test-wait-until (lambda () (lifecycle-test-entered-p agent))))
             (setf close-thread (make-thread (lambda () (agent--close-session agent session (json-object)))))
             (test-assert (test-wait-until (lambda () (acp-agent-session-closing-p session))))
             (test-assert (not (lifecycle-test-closed-p agent)))
             (with-lock-held ((lifecycle-test-lock agent))
               (setf (lifecycle-test-release-p agent) t)
               (condition-notify (lifecycle-test-condition agent)))
             (join-thread prompt-thread)
             (join-thread close-thread)
             (test-equal "cancelled" (json-get outcome "stopReason"))
             (test-assert (lifecycle-test-closed-p agent))
             (test-assert (null (acp-agent-session agent "session"))))
        (with-lock-held ((lifecycle-test-lock agent))
          (setf (lifecycle-test-release-p agent) t)
          (condition-notify (lifecycle-test-condition agent)))
        (join-thread prompt-thread)
        (when close-thread (join-thread close-thread)))))

(define-test agent-rejects-duplicate-session-registration
    (multiple-value-bind (client agent peer) (test-agent-pair)
      (declare (ignore peer))
      (unwind-protect
           (progn
             (connection-request client "session/new" (json-object "cwd" "/work" "mcpServers" #()))
             (let ((session (acp-agent-session agent "sess-1")))
               (test-signals acp-remote-error
                             (connection-request client "session/new" (json-object "cwd" "/other" "mcpServers" #())))
               (test-assert (eq session (acp-agent-session agent "sess-1")))
               (test-signals acp-remote-error
                             (connection-request client "session/load" (json-object "sessionId" "sess-1"
                                                                                    "cwd" "/other" "mcpServers" #())))
               (test-assert (eq session (acp-agent-session agent "sess-1")))))
        (connection-close client))))
