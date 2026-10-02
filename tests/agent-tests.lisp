(in-package #:agentcomms)

;;;; -- Agent Test Doubles --

(defclass test-agent (acp-agent)
  ((prompts
    :initform nil
    :accessor test-agent-prompts
    :type list
    :documentation "Prompt texts received, newest first.")
   (cancellations
    :initform 0
    :accessor test-agent-cancellations
    :type (integer 0)
    :documentation "How many times AGENT-CANCEL ran.")
   (closed-sessions
    :initform nil
    :accessor test-agent-closed-sessions
    :type list
    :documentation "Session ids closed through session/close."))
  (:documentation "An agent scripted by the text of each prompt."))

(defmethod agent-implementation ((agent test-agent))
  "Name the test agent."
  (acp-implementation "test-agent" "9.9" :title "Test Agent"))

(defmethod agent-capabilities ((agent test-agent))
  "Advertise loading, closing, listing, and image prompts."
  (acp-agent-capabilities :load-session t :close t :list t :image t))

(defmethod agent-auth-methods ((agent test-agent))
  "Advertise one agent-driven login."
  (list (acp-auth-method "login" "Log in")))

(defmethod agent-authenticate ((agent test-agent) method-id params)
  "Accept the advertised login only."
  (declare (ignore params))
  (if (string= method-id "login")
      nil
      (call-next-method)))

(defmethod agent-new-session ((agent test-agent) &key cwd mcp-servers additional-directories params)
  "Create the fixed session, reporting its modes."
  (declare (ignore cwd mcp-servers additional-directories params))
  (values "sess-1"
          (json-object "modes" (acp-session-mode-state
                                "ask" (list (acp-session-mode "ask" "Ask")
                                            (acp-session-mode "code" "Code"))))))

(defmethod agent-load-session ((agent test-agent) &key session-id cwd mcp-servers additional-directories params)
  "Replay one user message before returning."
  (declare (ignore cwd mcp-servers additional-directories params))
  (agent-send-update agent session-id
                     (acp-update-user-message (acp-text-content "earlier question")
                                              :message-id "m-user-1"))
  nil)

(defmethod agent-list-sessions ((agent test-agent) &key cwd cursor params)
  "List the fixed session with a next cursor."
  (declare (ignore cursor params))
  (values (list (acp-session-info "sess-1" (or cwd "/work") :title "First"))
          "page-2"))

(defmethod agent-close-session ((agent test-agent) session-id params)
  "Record the close."
  (declare (ignore params))
  (push session-id (test-agent-closed-sessions agent))
  nil)

(defmethod agent-cancel ((agent test-agent) session-id)
  "Count the cancellation."
  (declare (ignore session-id))
  (incf (test-agent-cancellations agent))
  nil)

(defmethod agent-extension-request ((agent test-agent) method params)
  "Echo the params of the test extension method."
  (if (string= method "_test/echo")
      params
      (call-next-method)))

(defmethod agent-prompt ((agent test-agent) session-id prompt params)
  "Act on the first text block of PROMPT."
  (declare (ignore params))
  (let ((text (acp-content-text (first prompt))))
    (push text (test-agent-prompts agent))
    (cond
      ((string= text "hello")
       (agent-send-update agent session-id
                          (acp-update-agent-message (acp-text-content "Hi there") :message-id "m1"))
       (agent-send-update agent session-id
                          (acp-update-tool-call
                           (acp-tool-call "call-1" "Reading README" :kind ':read :status ':pending)))
       (multiple-value-bind (outcome option-id)
           (agent-request-permission agent session-id
                                     (acp-tool-call-update "call-1")
                                     (list (acp-permission-option "allow" "Allow" ':allow-once)
                                           (acp-permission-option "reject" "Reject" ':reject-once)))
         (agent-send-update agent session-id
                            (acp-update-tool-call-progress
                             (acp-tool-call-update "call-1"
                                                   :status (if (and (eq outcome ':selected)
                                                                    (string= option-id "allow"))
                                                               ':completed
                                                               ':failed)
                                                   :raw-output (json-object "outcome" (string-downcase
                                                                                        (symbol-name outcome)))))))
       ':end-turn)
      ((string= text "slow")
       (loop repeat 500
             do (agent-check-cancelled agent session-id)
                (sleep 0.01))
       ':end-turn)
      ((string= text "swallow")
       (loop repeat 500
             until (agent-session-cancelled-p agent session-id)
             do (sleep 0.01))
       ':max-tokens)
      ((string= text "read")
       (agent-send-update agent session-id
                          (acp-update-agent-message
                           (acp-text-content (agent-read-text-file agent session-id "/w/notes.txt"
                                                                   :line 2 :limit 5))))
       ':end-turn)
      ((string= text "boom")
       (error "kaboom"))
      (t
       ':refusal))))

(defclass test-client-peer (acp-peer)
  ((updates
    :initform nil
    :accessor test-client-peer-updates
    :type list
    :documentation "session/update params received, oldest first.")
   (permission-requests
    :initform nil
    :accessor test-client-peer-permission-requests
    :type list
    :documentation "session/request_permission params received, oldest first.")
   (lock
    :initform (make-lock "agentcomms test client peer")
    :reader test-client-peer-lock
    :type t
    :documentation "The lock guarding the records."))
  (:documentation "A raw client peer answering permission and file requests."))

(defmethod peer-handle-notification ((peer test-client-peer) connection method params)
  "Record session updates."
  (declare (ignore connection))
  (when (string= method "session/update")
    (with-lock-held ((test-client-peer-lock peer))
      (setf (test-client-peer-updates peer)
            (nconc (test-client-peer-updates peer) (list params)))))
  nil)

(defmethod peer-handle-request ((peer test-client-peer) connection method params)
  "Allow the first permission option and serve one fixed file."
  (declare (ignore connection))
  (cond
    ((string= method "session/request_permission")
     (with-lock-held ((test-client-peer-lock peer))
       (setf (test-client-peer-permission-requests peer)
             (nconc (test-client-peer-permission-requests peer) (list params))))
     (json-object "outcome" (json-object "outcome" "selected"
                                         "optionId" (json-get (elt (json-get params "options") 0)
                                                              "optionId"))))
    ((string= method "fs/read_text_file")
     (json-object "content" (format nil "line ~A of ~A" (json-get params "line")
                                    (json-get params "path"))))
    (t
     (call-next-method))))

(-> test-agent-pair (&key (:client-capabilities t))
    (values acp-connection test-agent test-client-peer))
(defun test-agent-pair (&key client-capabilities)
  "Return an initialized client connection to a fresh test agent and both peers."
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((agent (make-instance 'test-agent))
          (peer (make-instance 'test-client-peer)))
      (acp-agent-connect agent right :name "test agent")
      (let ((client (make-acp-connection :channel left :peer peer :name "raw client")))
        (connection-request client "initialize"
                            (json-object "protocolVersion" 1
                                         "clientCapabilities" client-capabilities))
        (values client agent peer)))))

(-> test-update-kinds (test-client-peer) list)
(defun test-update-kinds (peer)
  "Return the kinds of the session updates PEER received."
  (with-lock-held ((test-client-peer-lock peer))
    (mapcar (lambda (params) (acp-update-kind (json-get params "update")))
            (test-client-peer-updates peer))))


;;;; -- Agent Tests --

(define-test agent-initialization-and-gating
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let* ((agent (make-instance 'test-agent))
           (client (make-acp-connection :channel left :peer (make-instance 'test-client-peer))))
      (acp-agent-connect agent right)
      (unwind-protect
           (progn
             (test-equal -32600
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "session/new"
                                                (json-object "cwd" "/w" "mcpServers" (vector))))))
             (test-equal -32602
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "initialize" (json-object)))))
             (let ((result (connection-request client "initialize"
                                               (json-object "protocolVersion" 99
                                                            "clientInfo" (acp-implementation "zed" "1.0")))))
               (test-equal 1 (json-get result "protocolVersion"))
               (test-equal "test-agent" (json-get (json-get result "agentInfo") "name"))
               (test-assert (acp-capability-enabled-p (json-get result "agentCapabilities") "loadSession"))
               (test-assert (not (acp-capability-enabled-p (json-get result "agentCapabilities")
                                                           "sessionCapabilities.resume")))
               (test-equal "login" (json-get (elt (json-get result "authMethods") 0) "id")))
             (test-equal 1 (acp-agent-protocol-version agent))
             (test-equal "zed" (json-get (acp-agent-client-info agent) "name"))
             (test-equal 1 (json-get (connection-request client "initialize"
                                                         (json-object "protocolVersion" 1))
                                     "protocolVersion"))
             (test-assert (json-object-p (connection-request client "authenticate"
                                                             (json-object "methodId" "login"))))
             (test-equal -32602
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "authenticate" (json-object "methodId" "other")))))
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "session/resume"
                                                (json-object "sessionId" "s" "cwd" "/w")))))
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "logout" nil))))
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "bogus/method" nil))))
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request client "_other/extension" nil))))
             (test-equal "yes" (json-get (connection-request client "_test/echo" (json-object "ok" "yes"))
                                         "ok")))
        (connection-close client)))))

(define-test agent-sessions-and-prompt-turn
  (multiple-value-bind (client agent peer)
      (test-agent-pair)
    (unwind-protect
         (progn
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/new" (json-object "cwd" "/w")))))
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/new"
                                              (json-object "cwd" "/w"
                                                           "mcpServers" (vector (json-object "name" "x")))))))
           (let ((result (connection-request client "session/new"
                                             (json-object "cwd" "/w" "mcpServers" (vector)))))
             (test-equal "sess-1" (json-get result "sessionId"))
             (test-equal "ask" (json-get (json-get result "modes") "currentModeId")))
           (test-equal '("sess-1") (acp-agent-session-ids agent))
           (test-equal "/w" (acp-agent-session-working-directory (acp-agent-session agent "sess-1")))
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/prompt"
                                              (json-object "sessionId" "nope"
                                                           "prompt" (vector (acp-text-content "hi")))))))
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/prompt"
                                              (json-object "sessionId" "sess-1"
                                                           "prompt" (vector (json-object "type" "text")))))))
           (let ((result (connection-request client "session/prompt"
                                             (json-object "sessionId" "sess-1"
                                                          "prompt" (vector (acp-text-content "hello"))))))
             (test-equal "end_turn" (json-get result "stopReason")))
           (test-equal '(:agent-message-chunk :tool-call :tool-call-update) (test-update-kinds peer))
           (let ((request (first (test-client-peer-permission-requests peer))))
             (test-equal "sess-1" (json-get request "sessionId"))
             (test-equal "call-1" (json-get (json-get request "toolCall") "toolCallId"))
             (test-equal '("allow" "reject")
                         (map 'list (lambda (option) (json-get option "optionId"))
                              (json-get request "options"))))
           (let ((final (json-get (third (test-client-peer-updates peer)) "update")))
             (test-equal "completed" (json-get final "status"))
             (test-equal "selected" (json-get (json-get final "rawOutput") "outcome")))
           (test-equal "refusal"
                       (json-get (connection-request client "session/prompt"
                                                     (json-object "sessionId" "sess-1"
                                                                  "prompt" (vector (acp-text-content "whatever"))))
                                 "stopReason"))
           (let ((condition (test-signals acp-remote-error
                              (connection-request client "session/prompt"
                                                  (json-object "sessionId" "sess-1"
                                                               "prompt" (vector (acp-text-content "boom")))))))
             (test-equal -32603 (acp-method-error-code condition))
             (test-assert (search "kaboom" (acp-error-message condition))))
           (test-equal '("boom" "whatever" "hello") (test-agent-prompts agent))
           (let ((condition (test-signals acp-capability-error
                              (agent-read-text-file agent "sess-1" "/w/notes.txt"))))
             (test-equal "fs.readTextFile" (acp-capability-error-capability condition)))
           (test-signals acp-capability-error (agent-create-terminal agent "sess-1" "ls"))
           (test-equal -32603
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/set_mode"
                                              (json-object "sessionId" "sess-1" "modeId" "code"))))))
      (connection-close client))))

(define-test agent-cancellation-yields-the-cancelled-stop-reason
  (multiple-value-bind (client agent peer)
      (test-agent-pair)
    (declare (ignore peer))
    (unwind-protect
         (progn
           (connection-request client "session/new" (json-object "cwd" "/w" "mcpServers" (vector)))
           (dolist (text '("slow" "swallow"))
             (let* ((result nil)
                    (thread (make-thread
                             (lambda ()
                               (setf result
                                     (connection-request client "session/prompt"
                                                         (json-object "sessionId" "sess-1"
                                                                      "prompt" (vector (acp-text-content text)))))))))
               (test-assert (test-wait-until
                             (lambda ()
                               (acp-agent-session-prompt-active-p (acp-agent-session agent "sess-1"))))
                            "the prompt turn becomes active")
               (connection-notify client "session/cancel" (json-object "sessionId" "sess-1"))
               (join-thread thread)
               (test-equal "cancelled" (json-get result "stopReason"))
               (test-assert (not (agent-session-cancelled-p agent "sess-1"))
                            "the cancellation flag is cleared after the turn")))
           (test-equal 2 (test-agent-cancellations agent))
           (connection-notify client "session/cancel" (json-object "sessionId" "unknown"))
           (test-equal "end_turn"
                       (json-get (connection-request client "session/prompt"
                                                     (json-object "sessionId" "sess-1"
                                                                  "prompt" (vector (acp-text-content "slow"))))
                                 "stopReason"))
           (test-signals acp-timeout
             (connection-request client "session/prompt"
                                 (json-object "sessionId" "sess-1"
                                              "prompt" (vector (acp-text-content "slow")))
                                 :timeout 0.2))
           (test-assert (test-wait-until
                         (lambda ()
                           (not (acp-agent-session-prompt-active-p (acp-agent-session agent "sess-1")))))
                        "a protocol-level cancellation ends the prompt turn"))
      (connection-close client))))

(define-test agent-optional-session-methods
  (multiple-value-bind (client agent peer)
      (test-agent-pair :client-capabilities (acp-client-capabilities :read-text-file t))
    (unwind-protect
         (progn
           (test-assert (agent-client-capability-p agent "fs.readTextFile"))
           (test-assert (not (agent-client-capability-p agent "fs.writeTextFile")))
           (let ((result (connection-request client "session/load"
                                             (json-object "sessionId" "sess-9" "cwd" "/w"
                                                          "mcpServers" (vector)))))
             (test-assert (json-object-p result)))
           (test-equal '(:user-message-chunk) (test-update-kinds peer))
           (test-equal "m-user-1" (json-get (json-get (first (test-client-peer-updates peer)) "update")
                                            "messageId"))
           (test-equal '("sess-9") (acp-agent-session-ids agent))
           (test-equal "end_turn"
                       (json-get (connection-request client "session/prompt"
                                                     (json-object "sessionId" "sess-9"
                                                                  "prompt" (vector (acp-text-content "read"))))
                                 "stopReason"))
           (test-equal "line 2 of /w/notes.txt"
                       (acp-content-text (json-get (json-get (second (test-client-peer-updates peer)) "update")
                                                   "content")))
           (let ((result (connection-request client "session/list" (json-object "cwd" "/elsewhere"))))
             (test-equal "sess-1" (json-get (elt (json-get result "sessions") 0) "sessionId"))
             (test-equal "/elsewhere" (json-get (elt (json-get result "sessions") 0) "cwd"))
             (test-equal "page-2" (json-get result "nextCursor")))
           (test-assert (json-object-p (connection-request client "session/close"
                                                           (json-object "sessionId" "sess-9"))))
           (test-equal '("sess-9") (test-agent-closed-sessions agent))
           (test-equal 1 (test-agent-cancellations agent))
           (test-equal nil (acp-agent-session-ids agent))
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (connection-request client "session/close" (json-object "sessionId" "sess-9"))))))
      (connection-close client))))
