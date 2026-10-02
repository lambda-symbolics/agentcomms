(in-package #:agentcomms)

;;;; -- Client Test Doubles --

(defclass test-tooling-agent (test-agent)
  ()
  (:documentation "The test agent extended with prompts exercising client tooling."))

(defmethod agent-auth-methods ((agent test-tooling-agent))
  "Advertise an agent login and a terminal login."
  (list (acp-auth-method "login" "Log in")
        (acp-terminal-auth-method "terminal-login" "Log in from a terminal"
                                  :arguments '("--login"))))

(defmethod agent-prompt ((agent test-tooling-agent) session-id prompt params)
  "Exercise terminals, file writes, and elicitation on request."
  (let ((text (acp-content-text (first prompt))))
    (flet ((say (string)
             (agent-send-update agent session-id
                                (acp-update-agent-message (acp-text-content string)))))
      (cond
        ((string= text "terminal")
         (let ((terminal-id (agent-create-terminal agent session-id "make"
                                                   :arguments '("test")
                                                   :environment '(("CI" . "1"))
                                                   :cwd "/w"
                                                   :output-byte-limit 4096)))
           (multiple-value-bind (output truncated-p exited-p code signal)
               (agent-terminal-output agent session-id terminal-id)
             (say (format nil "~A|~A|~A|~A|~A" output truncated-p exited-p code signal)))
           (multiple-value-bind (code signal)
               (agent-wait-for-terminal-exit agent session-id terminal-id)
             (say (format nil "exit ~A ~A" code signal)))
           (agent-kill-terminal agent session-id terminal-id)
           (agent-release-terminal agent session-id terminal-id)
           ':end-turn))
        ((string= text "write")
         (agent-write-text-file agent session-id "/w/out.txt" "written")
         ':end-turn)
        ((string= text "elicit")
         (multiple-value-bind (action content)
             (agent-create-elicitation agent :mode ':form
                                             :message "Pick a strategy"
                                             :session-id session-id
                                             :schema (json-object "type" "object"))
           (say (format nil "~(~A~) ~A" action (and content (json-get content "strategy"))))
           ':end-turn))
        ((string= text "elicit-url")
         (multiple-value-bind (action content)
             (agent-create-elicitation agent :mode ':url
                                             :message "Authorize"
                                             :session-id session-id
                                             :url "https://example.test/auth"
                                             :elicitation-id "e-1")
           (declare (ignore content))
           (agent-complete-elicitation agent "e-1")
           (say (format nil "~(~A~)" action))
           ':end-turn))
        (t
         (call-next-method))))))

(defclass test-client (acp-client)
  ((updates
    :initform nil
    :accessor test-client-updates
    :type list
    :documentation "Session updates received, oldest first, as (SESSION-ID . UPDATE).")
   (permission-choice
    :initform "allow"
    :accessor test-client-permission-choice
    :type (or null string)
    :documentation "The option id to select, or NIL to answer cancelled.")
   (writes
    :initform nil
    :accessor test-client-writes
    :type list
    :documentation "File writes received as (PATH . CONTENT).")
   (terminal-calls
    :initform nil
    :accessor test-client-terminal-calls
    :type list
    :documentation "Terminal method names received, oldest first.")
   (completed-elicitations
    :initform nil
    :accessor test-client-completed-elicitations
    :type list
    :documentation "Elicitation ids reported complete.")
   (lock
    :initform (make-lock "agentcomms test client")
    :reader test-client-lock
    :type t
    :documentation "The lock guarding the records."))
  (:documentation "A client with fake files, terminals, and elicitation."))

(defmethod client-implementation ((client test-client))
  "Name the test client."
  (acp-implementation "test-client" "1.0"))

(defmethod client-capabilities ((client test-client))
  "Advertise files, terminals, and form elicitation."
  (acp-client-capabilities :read-text-file t :write-text-file t :terminal t
                           :elicitation-form t :boolean-config-options t))

(defmethod client-session-update ((client test-client) session-id update params)
  "Record the update."
  (declare (ignore params))
  (with-lock-held ((test-client-lock client))
    (setf (test-client-updates client)
          (nconc (test-client-updates client) (list (cons session-id update)))))
  nil)

(defmethod client-request-permission ((client test-client) session-id tool-call options params)
  "Select the configured option."
  (declare (ignore session-id tool-call options params))
  (let ((choice (test-client-permission-choice client)))
    (if choice
        (values ':selected choice)
        (values ':cancelled nil))))

(defmethod client-read-text-file ((client test-client) session-id path &key line limit params)
  "Serve a fixed file."
  (declare (ignore session-id params))
  (format nil "line ~A of ~A (~A)" line path limit))

(defmethod client-write-text-file ((client test-client) session-id path content params)
  "Record the write."
  (declare (ignore session-id params))
  (push (cons path content) (test-client-writes client))
  nil)

(defmethod client-create-terminal ((client test-client) session-id command
                                   &key arguments environment cwd output-byte-limit params)
  "Record the creation and return a fixed id."
  (declare (ignore session-id params))
  (push (format nil "create ~A ~{~A~} ~A ~A ~A" command arguments
                (rest (assoc "CI" environment :test #'string=)) cwd output-byte-limit)
        (test-client-terminal-calls client))
  "term-1")

(defmethod client-terminal-output ((client test-client) session-id terminal-id params)
  "Report running output."
  (declare (ignore session-id params))
  (push (format nil "output ~A" terminal-id) (test-client-terminal-calls client))
  (values "building..." t nil nil nil))

(defmethod client-wait-for-terminal-exit ((client test-client) session-id terminal-id params)
  "Report a signal exit."
  (declare (ignore session-id params))
  (push (format nil "wait ~A" terminal-id) (test-client-terminal-calls client))
  (values nil "SIGTERM"))

(defmethod client-kill-terminal ((client test-client) session-id terminal-id params)
  "Record the kill."
  (declare (ignore session-id params))
  (push (format nil "kill ~A" terminal-id) (test-client-terminal-calls client))
  nil)

(defmethod client-release-terminal ((client test-client) session-id terminal-id params)
  "Record the release."
  (declare (ignore session-id params))
  (push (format nil "release ~A" terminal-id) (test-client-terminal-calls client))
  nil)

(defmethod client-create-elicitation ((client test-client) params)
  "Accept forms with a fixed answer."
  (declare (ignore params))
  (values ':accept (json-object "strategy" "balanced")))

(defmethod client-elicitation-completed ((client test-client) elicitation-id params)
  "Record the completion."
  (declare (ignore params))
  (push elicitation-id (test-client-completed-elicitations client))
  nil)

(-> test-client-pair (&key (:agent-class symbol) (:client-class symbol))
    (values test-client acp-agent))
(defun test-client-pair (&key (agent-class 'test-tooling-agent) (client-class 'test-client))
  "Return a connected client and agent of the given classes, not yet initialized."
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((agent (make-instance agent-class))
          (client (make-instance client-class)))
      (acp-agent-connect agent right :name "test agent")
      (acp-client-connect client left :name "test client")
      (values client agent))))

(-> test-client-texts (test-client) list)
(defun test-client-texts (client)
  "Return the text of every agent message chunk CLIENT received."
  (with-lock-held ((test-client-lock client))
    (loop for (nil . update) in (test-client-updates client)
          when (eq (acp-update-kind update) ':agent-message-chunk)
            collect (acp-content-text (json-get update "content")))))


;;;; -- Client Tests --

(define-test client-initialization
  (multiple-value-bind (client agent)
      (test-client-pair)
    (unwind-protect
         (progn
           (test-signals acp-state-error (client-new-session client "/w"))
           (let ((result (client-initialize client)))
             (test-equal 1 (json-get result "protocolVersion"))
             (test-equal 1 (acp-client-protocol-version client))
             (test-equal "test-agent" (json-get (acp-client-agent-info client) "name"))
             (test-equal '("login" "terminal-login")
                         (mapcar (lambda (method) (json-get method "id"))
                                 (acp-client-auth-methods client)))
             (test-assert (client-agent-capability-p client "loadSession"))
             (test-assert (client-agent-capability-p client "promptCapabilities.image"))
             (test-assert (not (client-agent-capability-p client "sessionCapabilities.delete"))))
           (test-equal "test-client" (json-get (acp-agent-client-info agent) "name"))
           (test-assert (agent-client-capability-p agent "terminal"))
           (test-assert (agent-client-capability-p agent "session.configOptions.boolean"))
           (test-assert (json-object-p (client-authenticate client "login")))
           (test-signals acp-state-error (client-authenticate client "terminal-login"))
           (test-equal -32602
                       (acp-method-error-code
                        (test-signals acp-remote-error (client-authenticate client "unknown"))))
           (test-equal "sessionCapabilities.resume"
                       (acp-capability-error-capability
                        (test-signals acp-capability-error (client-resume-session client "s" "/w"))))
           (test-signals acp-capability-error (client-delete-session client "s"))
           (test-signals acp-capability-error (client-logout client))
           (test-signals acp-capability-error
             (client-new-session client "/w" :additional-directories '("/extra"))))
      (connection-close (acp-client-connection client))))
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((client (make-instance 'test-client)))
      (acp-client-connect client left)
      (let ((agent-side (make-acp-connection
                         :channel right
                         :peer (make-instance 'test-echo-peer))))
        (unwind-protect
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error (client-initialize client))))
          (connection-close agent-side))))))

(define-test client-rejects-unsupported-agent-versions
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let ((client (make-instance 'test-client)))
      (acp-client-connect client left)
      (unwind-protect
           (let ((thread (make-thread
                          (lambda ()
                            (let ((request (json-decode (channel-read-message right))))
                              (channel-write-message
                               right
                               (json-encode (json-object "jsonrpc" "2.0"
                                                         "id" (json-get request "id")
                                                         "result" (json-object "protocolVersion" 7)))))))))
             (let ((condition (test-signals acp-unsupported-version (client-initialize client))))
               (test-equal 7 (acp-unsupported-version-version condition)))
             (join-thread thread)
             (test-equal nil (acp-client-protocol-version client)))
        (connection-close (acp-client-connection client))
        (channel-close right)))))

(define-test client-session-lifecycle
  (multiple-value-bind (client agent)
      (test-client-pair)
    (unwind-protect
         (progn
           (client-initialize client)
           (multiple-value-bind (session-id result)
               (client-new-session client "/w" :mcp-servers (list (acp-mcp-server-stdio "fs" "/bin/fs")))
             (test-equal "sess-1" session-id)
             (test-equal "ask" (json-get (json-get result "modes") "currentModeId")))
           (multiple-value-bind (stop-reason result)
               (client-prompt client "sess-1" (list (acp-text-content "hello")))
             (test-equal ':end-turn stop-reason)
             (test-equal "end_turn" (json-get result "stopReason")))
           (test-equal '("Hi there") (test-client-texts client))
           (test-equal '(:agent-message-chunk :tool-call :tool-call-update)
                       (mapcar (lambda (entry) (acp-update-kind (rest entry)))
                               (test-client-updates client)))
           (test-equal "completed" (json-get (rest (third (test-client-updates client))) "status"))
           (setf (test-client-permission-choice client) nil)
           (client-prompt client "sess-1" (list (acp-text-content "hello")))
           (test-equal "failed" (json-get (rest (sixth (test-client-updates client))) "status"))
           (test-equal "cancelled" (json-get (json-get (rest (sixth (test-client-updates client))) "rawOutput")
                                             "outcome"))
           (test-equal ':refusal (client-prompt client "sess-1" (list (acp-text-content "nothing"))))
           (let ((thread (make-thread
                          (lambda ()
                            (client-prompt client "sess-1" (list (acp-text-content "slow")))))))
             (test-assert (test-wait-until
                           (lambda ()
                             (acp-agent-session-prompt-active-p (acp-agent-session agent "sess-1")))))
             (client-cancel client "sess-1")
             (test-equal ':cancelled (join-thread thread)))
           (test-equal -32603
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (client-set-config-option client "sess-1" "mode" "code"))))
           (test-equal -32603
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (client-set-mode client "sess-1" "code"))))
           (multiple-value-bind (sessions next-cursor)
               (client-list-sessions client :cwd "/w")
             (test-equal "sess-1" (json-get (first sessions) "sessionId"))
             (test-equal "page-2" next-cursor))
           (test-assert (json-object-p (client-load-session client "sess-2" "/w")))
           (test-equal '("sess-1" "sess-2") (sort (acp-agent-session-ids agent) #'string<))
           (test-equal ':user-message-chunk (acp-update-kind (rest (first (last (test-client-updates client))))))
           (test-assert (json-object-p (client-close-session client "sess-2")))
           (test-equal '("sess-1") (acp-agent-session-ids agent)))
      (connection-close (acp-client-connection client)))))

(define-test client-serves-tooling-requests
  (multiple-value-bind (client agent)
      (test-client-pair)
    (declare (ignore agent))
    (unwind-protect
         (progn
           (client-initialize client)
           (client-new-session client "/w")
           (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "read"))))
           (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "write"))))
           (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "terminal"))))
           (test-equal ':end-turn (client-prompt client "sess-1" (list (acp-text-content "elicit"))))
           (test-equal '("line 2 of /w/notes.txt (5)"
                         "building...|T|NIL|NIL|NIL"
                         "exit NIL SIGTERM"
                         "accept balanced")
                       (test-client-texts client))
           (test-equal '(("/w/out.txt" . "written")) (test-client-writes client))
           (test-equal '("create make test 1 /w 4096" "output term-1" "wait term-1"
                         "kill term-1" "release term-1")
                       (reverse (test-client-terminal-calls client)))
           (let ((condition (test-signals acp-remote-error
                              (client-prompt client "sess-1" (list (acp-text-content "elicit-url"))))))
             (test-equal -32603 (acp-method-error-code condition))
             (test-assert (search "elicitation.url" (acp-error-message condition)))))
      (connection-close (acp-client-connection client)))))

(define-test client-gates-unadvertised-methods
  (multiple-value-bind (left right)
      (make-acp-channel-pair)
    (let* ((client (make-instance 'acp-client))
           (raw (make-acp-connection :channel right :peer (make-instance 'test-echo-peer))))
      (acp-client-connect client left)
      (unwind-protect
           (progn
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error (connection-request raw "echo" nil))))
             (dolist (method '("fs/read_text_file" "fs/write_text_file" "terminal/create"
                               "terminal/output" "elicitation/create"))
               (test-equal -32601
                           (acp-method-error-code
                            (test-signals acp-remote-error
                              (connection-request raw method
                                                  (json-object "sessionId" "s" "path" "/p"
                                                               "content" "c" "command" "ls"
                                                               "terminalId" "t" "mode" "form"
                                                               "message" "m"))))))
             (test-equal -32603
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request raw "session/request_permission"
                                                (json-object "sessionId" "s"
                                                             "toolCall" (json-object "toolCallId" "c")
                                                             "options" (vector (acp-permission-option
                                                                                "a" "Allow" ':allow-once)))))))
             (test-equal -32602
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request raw "session/request_permission"
                                                (json-object "sessionId" "s" "options" (vector))))))
             (test-equal -32601
                         (acp-method-error-code
                          (test-signals acp-remote-error
                            (connection-request raw "_unknown/extension" nil)))))
        (connection-close raw)))))
