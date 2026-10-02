(in-package #:agentcomms)

;;;; -- Agent Role --

(defparameter *acp-agent-implementation-name* "agentcomms"
  "The implementation name an agent reports unless it overrides AGENT-IMPLEMENTATION.")

(defparameter *acp-agent-implementation-version* "0.1.0"
  "The implementation version an agent reports unless it overrides AGENT-IMPLEMENTATION.")

(define-condition acp-prompt-cancelled (acp-error)
  ()
  (:default-initargs :message "The prompt turn was cancelled.")
  (:documentation
   "The client cancelled the prompt turn in progress.

AGENT-CHECK-CANCELLED signals it, and AGENT-PROMPT implementations may
signal it themselves; the agent answers the prompt with the cancelled stop
reason either way."))


;;;; -- Sessions --

(defclass acp-agent-session ()
  ((identifier
    :initarg :identifier
    :reader acp-agent-session-identifier
    :type string
    :documentation "The session id the agent chose.")
   (working-directory
    :initarg :working-directory
    :reader acp-agent-session-working-directory
    :type string
    :documentation "The absolute working directory the client requested.")
   (cancel-requested-p
    :initform nil
    :accessor acp-agent-session-cancel-requested-p
    :type boolean
    :documentation "Whether session/cancel arrived during the current prompt turn.")
   (prompt-active-p
    :initform nil
    :accessor acp-agent-session-prompt-active-p
    :type boolean
    :documentation "Whether a prompt turn is in progress."))
  (:documentation "The library's bookkeeping for one agent session."))


;;;; -- Agent Peer --

(defclass acp-agent (acp-peer)
  ((connection
    :initform nil
    :accessor acp-agent-connection
    :type (or null acp-connection)
    :documentation "The connection to the client once attached.")
   (protocol-version
    :initform nil
    :accessor acp-agent-protocol-version
    :type (or null integer)
    :documentation "The negotiated protocol version, or NIL before initialize.")
   (client-capabilities
    :initform nil
    :accessor acp-agent-client-capabilities
    :type t
    :documentation "The client capabilities object received in initialize.")
   (client-info
    :initform nil
    :accessor acp-agent-client-info
    :type t
    :documentation "The client implementation descriptor, when provided.")
   (advertised-capabilities
    :initform nil
    :accessor acp-agent-advertised-capabilities
    :type t
    :documentation "The capabilities this agent reported in its initialize response.")
   (sessions
    :initform (make-hash-table :test #'equal)
    :reader acp-agent-sessions
    :type hash-table
    :documentation "Known sessions by id.")
   (lock
    :initform (make-lock "agentcomms agent")
    :reader acp-agent-lock
    :type t
    :documentation "The lock guarding negotiation state and the session table."))
  (:documentation
   "The agent side of an ACP connection.

Subclass it and specialize the AGENT- generic functions. The library
dispatches requests, validates parameters, negotiates the protocol
version, gates optional methods on advertised capabilities, and turns
cancellation into the cancelled stop reason."))

(-> acp-agent-connect
    (acp-agent acp-channel &key (:name string) (:log-function (or null function))
               (:request-timeout (or null real)))
    acp-connection)
(defun acp-agent-connect (agent channel &key (name "agentcomms agent") log-function
                                              (request-timeout *acp-default-request-timeout*))
  "Attach AGENT to CHANNEL and return the running connection."
  (let ((connection (make-acp-connection :channel channel
                                         :peer agent
                                         :name name
                                         :log-function log-function
                                         :request-timeout request-timeout)))
    (setf (acp-agent-connection agent) connection)
    connection))

(-> acp-agent-serve (acp-agent acp-channel &key (:name string) (:log-function (or null function)))
    (or null string))
(defun acp-agent-serve (agent channel &key (name "agentcomms agent") log-function)
  "Serve AGENT over CHANNEL until the client disconnects; return the close reason."
  (connection-run (acp-agent-connect agent channel :name name :log-function log-function)))

(-> agent--connection (acp-agent) acp-connection)
(defun agent--connection (agent)
  "Return AGENT's connection or signal that it is not attached."
  (or (acp-agent-connection agent)
      (error 'acp-state-error :message "The agent is not attached to a connection.")))


;;;; -- Generic Functions for Agent Implementations --

(defgeneric agent-implementation (agent)
  (:documentation "Return the implementation descriptor sent as agentInfo."))

(defgeneric agent-capabilities (agent)
  (:documentation
   "Return the agent capabilities object sent in the initialize response.

Build it with ACP-AGENT-CAPABILITIES. Optional methods are only dispatched
when the matching capability is advertised here."))

(defgeneric agent-auth-methods (agent)
  (:documentation "Return the list of advertised authentication methods."))

(defgeneric agent-authenticate (agent method-id params)
  (:documentation "Perform protocol-driven authentication METHOD-ID; return NIL or result extras."))

(defgeneric agent-new-session (agent &key cwd mcp-servers additional-directories params)
  (:documentation
   "Create a session for working directory CWD and return its id.

MCP-SERVERS is the validated list of server configurations and
ADDITIONAL-DIRECTORIES the extra workspace roots. A second value, when
returned, is a JSON object of response extras such as modes or
configOptions."))

(defgeneric agent-load-session (agent &key session-id cwd mcp-servers additional-directories params)
  (:documentation
   "Restore SESSION-ID, replaying its history through AGENT-SEND-UPDATE before returning.

Return NIL or a JSON object of response extras."))

(defgeneric agent-resume-session (agent &key session-id cwd mcp-servers additional-directories params)
  (:documentation "Restore SESSION-ID without replaying history; return NIL or response extras."))

(defgeneric agent-close-session (agent session-id params)
  (:documentation "Cancel SESSION-ID's work and release its resources."))

(defgeneric agent-prompt (agent session-id prompt params)
  (:documentation
   "Run one prompt turn for SESSION-ID over the content blocks PROMPT.

Stream progress with AGENT-SEND-UPDATE, call AGENT-CHECK-CANCELLED at safe
points, and return a stop reason keyword such as :END-TURN."))

(defgeneric agent-cancel (agent session-id)
  (:documentation
   "Abort SESSION-ID's prompt turn as fast as possible.

The library has already marked the session cancelled; specialize this to
interrupt provider requests or tool executions."))

(defgeneric agent-set-mode (agent session-id mode-id params)
  (:documentation "Switch SESSION-ID to MODE-ID."))

(defgeneric agent-set-config-option (agent session-id config-id value params)
  (:documentation
   "Set configuration option CONFIG-ID of SESSION-ID to VALUE.

VALUE is a string for select options or T or NIL for boolean ones. Return
the complete list of configuration options with their current values."))

(defgeneric agent-list-sessions (agent &key cwd cursor params)
  (:documentation
   "Return the list of session info objects, filtered by CWD when given.

Return a second value naming the next page cursor when more sessions exist."))

(defgeneric agent-delete-session (agent session-id params)
  (:documentation "Remove SESSION-ID from future session lists."))

(defgeneric agent-logout (agent params)
  (:documentation "End the authenticated state."))

(defgeneric agent-extension-request (agent method params)
  (:documentation "Answer the extension request METHOD, whose name begins with an underscore."))

(defgeneric agent-extension-notification (agent method params)
  (:documentation "React to the extension notification METHOD."))

(defmethod agent-implementation ((agent acp-agent))
  "Report the library's own name and version."
  (acp-implementation *acp-agent-implementation-name* *acp-agent-implementation-version*))

(defmethod agent-capabilities ((agent acp-agent))
  "Advertise only the baseline methods."
  (acp-agent-capabilities))

(defmethod agent-auth-methods ((agent acp-agent))
  "Advertise no authentication methods."
  nil)

(defmethod agent-authenticate ((agent acp-agent) method-id params)
  "Reject every method id, since none is advertised by default."
  (declare (ignore params))
  (acp-invalid-params "~A is not an advertised authentication method." method-id))

(-> agent--unimplemented (string) nil)
(defun agent--unimplemented (method)
  "Signal that baseline METHOD lacks an implementation."
  (error 'acp-method-error
         :code (acp-error-code ':internal-error)
         :message (format nil "This agent does not implement ~A." method)))

(defmethod agent-new-session ((agent acp-agent) &key cwd mcp-servers additional-directories params)
  "Signal that session creation is unimplemented."
  (declare (ignore cwd mcp-servers additional-directories params))
  (agent--unimplemented "session/new"))

(defmethod agent-load-session ((agent acp-agent) &key session-id cwd mcp-servers additional-directories params)
  "Signal that session loading is unimplemented."
  (declare (ignore session-id cwd mcp-servers additional-directories params))
  (agent--unimplemented "session/load"))

(defmethod agent-resume-session ((agent acp-agent) &key session-id cwd mcp-servers additional-directories params)
  "Signal that session resumption is unimplemented."
  (declare (ignore session-id cwd mcp-servers additional-directories params))
  (agent--unimplemented "session/resume"))

(defmethod agent-close-session ((agent acp-agent) session-id params)
  "Do nothing beyond the library's bookkeeping."
  (declare (ignore session-id params))
  nil)

(defmethod agent-prompt ((agent acp-agent) session-id prompt params)
  "Signal that prompting is unimplemented."
  (declare (ignore session-id prompt params))
  (agent--unimplemented "session/prompt"))

(defmethod agent-cancel ((agent acp-agent) session-id)
  "Rely on the cancellation flag the library already set."
  (declare (ignore session-id))
  nil)

(defmethod agent-set-mode ((agent acp-agent) session-id mode-id params)
  "Signal that modes are unimplemented."
  (declare (ignore session-id mode-id params))
  (agent--unimplemented "session/set_mode"))

(defmethod agent-set-config-option ((agent acp-agent) session-id config-id value params)
  "Signal that configuration options are unimplemented."
  (declare (ignore session-id config-id value params))
  (agent--unimplemented "session/set_config_option"))

(defmethod agent-list-sessions ((agent acp-agent) &key cwd cursor params)
  "Signal that session listing is unimplemented."
  (declare (ignore cwd cursor params))
  (agent--unimplemented "session/list"))

(defmethod agent-delete-session ((agent acp-agent) session-id params)
  "Signal that session deletion is unimplemented."
  (declare (ignore session-id params))
  (agent--unimplemented "session/delete"))

(defmethod agent-logout ((agent acp-agent) params)
  "Signal that logout is unimplemented."
  (declare (ignore params))
  (agent--unimplemented "logout"))

(defmethod agent-extension-request ((agent acp-agent) method params)
  "Answer unknown extension methods with Method Not Found."
  (declare (ignore params))
  (error 'acp-method-error
         :code (acp-error-code ':method-not-found)
         :message (format nil "Method not found: ~A" method)))

(defmethod agent-extension-notification ((agent acp-agent) method params)
  "Ignore unknown extension notifications."
  (declare (ignore method params))
  nil)


;;;; -- Session Bookkeeping --

(-> acp-agent-session (acp-agent string) (or null acp-agent-session))
(defun acp-agent-session (agent session-id)
  "Return the session record for SESSION-ID, or NIL when unknown."
  (with-lock-held ((acp-agent-lock agent))
    (gethash session-id (acp-agent-sessions agent))))

(-> acp-agent-session-ids (acp-agent) list)
(defun acp-agent-session-ids (agent)
  "Return the ids of every known session."
  (with-lock-held ((acp-agent-lock agent))
    (loop for identifier being the hash-keys of (acp-agent-sessions agent)
          collect identifier)))

(-> agent--register-session (acp-agent string string) acp-agent-session)
(defun agent--register-session (agent session-id cwd)
  "Record SESSION-ID with working directory CWD, replacing any stale record."
  (let ((session (make-instance 'acp-agent-session
                                :identifier session-id
                                :working-directory cwd)))
    (with-lock-held ((acp-agent-lock agent))
      (setf (gethash session-id (acp-agent-sessions agent)) session))
    session))

(-> agent--forget-session (acp-agent string) null)
(defun agent--forget-session (agent session-id)
  "Drop the record of SESSION-ID."
  (with-lock-held ((acp-agent-lock agent))
    (remhash session-id (acp-agent-sessions agent)))
  nil)

(-> agent--known-session (acp-agent string) acp-agent-session)
(defun agent--known-session (agent session-id)
  "Return the record for SESSION-ID or signal Invalid Params."
  (or (acp-agent-session agent session-id)
      (acp-invalid-params "Unknown session ~A." session-id)))

(-> agent-session-cancelled-p (acp-agent string) boolean)
(defun agent-session-cancelled-p (agent session-id)
  "Return true when the client cancelled SESSION-ID's current prompt turn."
  (let ((session (acp-agent-session agent session-id)))
    (and session (acp-agent-session-cancel-requested-p session) t)))

(-> agent-check-cancelled (acp-agent string) null)
(defun agent-check-cancelled (agent session-id)
  "Signal ACP-PROMPT-CANCELLED when SESSION-ID's turn or the request was cancelled."
  (when (or (agent-session-cancelled-p agent session-id)
            (acp-request-cancelled-p))
    (error 'acp-prompt-cancelled))
  nil)


;;;; -- Initialization --

(-> agent--negotiate-version (t) integer)
(defun agent--negotiate-version (requested)
  "Return the protocol version to answer a client REQUESTED version with."
  (unless (integerp requested)
    (acp-invalid-params "The protocolVersion must be an integer."))
  (if (member requested *acp-supported-protocol-versions*)
      requested
      (first *acp-supported-protocol-versions*)))

(-> agent--initialize (acp-agent hash-table) hash-table)
(defun agent--initialize (agent params)
  "Negotiate the version, record the client, and build the initialize response."
  (let ((version (agent--negotiate-version (json-get params "protocolVersion" ':null)))
        (client-capabilities (acp-field params "clientCapabilities" :type ':object))
        (client-info (acp-field params "clientInfo" :type ':object))
        (capabilities (agent-capabilities agent)))
    (with-lock-held ((acp-agent-lock agent))
      (setf (acp-agent-protocol-version agent) version
            (acp-agent-client-capabilities agent) (or client-capabilities (json-object))
            (acp-agent-client-info agent) client-info
            (acp-agent-advertised-capabilities agent) capabilities))
    (json-object "protocolVersion" version
                 "agentCapabilities" capabilities
                 "agentInfo" (agent-implementation agent)
                 "authMethods" (coerce (agent-auth-methods agent) 'vector))))

(-> agent--require-initialized (acp-agent string) null)
(defun agent--require-initialized (agent method)
  "Signal Invalid Request when METHOD arrives before initialize."
  (unless (acp-agent-protocol-version agent)
    (error 'acp-method-error
           :code (acp-error-code ':invalid-request)
           :message (format nil "~A requires initialize first." method)))
  nil)

(-> agent--require-capability (acp-agent string string) null)
(defun agent--require-capability (agent path method)
  "Signal Method Not Found when this agent never advertised capability PATH."
  (unless (acp-capability-enabled-p (acp-agent-advertised-capabilities agent) path)
    (error 'acp-method-error
           :code (acp-error-code ':method-not-found)
           :message (format nil "~A is not available; the agent does not advertise ~A."
                            method path)))
  nil)


;;;; -- Request Dispatch --

(-> agent--session-setup-arguments (hash-table &key (:mcp-servers-required-p boolean)) list)
(defun agent--session-setup-arguments (params &key (mcp-servers-required-p t))
  "Return the validated keyword arguments shared by session setup methods."
  (let ((cwd (acp-field params "cwd" :type ':string :required-p t))
        (servers (acp-validate-mcp-servers
                  (acp-field params "mcpServers" :type ':array
                                                 :required-p mcp-servers-required-p)))
        (directories (acp-field params "additionalDirectories" :type ':array)))
    (dolist (directory directories)
      (unless (stringp directory)
        (acp-invalid-params "Each additional directory must be a string.")))
    (list :cwd cwd
          :mcp-servers servers
          :additional-directories directories
          :params params)))

(-> agent--extras (t &rest t) hash-table)
(defun agent--extras (extras &rest pairs)
  "Return response EXTRAS, a JSON object or NIL, with PAIRS added."
  (let ((object (if (json-object-p extras)
                    extras
                    (json-object))))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(-> agent--run-prompt (acp-agent acp-agent-session list hash-table) hash-table)
(defun agent--run-prompt (agent session prompt params)
  "Run the prompt turn for SESSION and return the prompt response."
  (let ((session-id (acp-agent-session-identifier session)))
    (with-lock-held ((acp-agent-lock agent))
      (setf (acp-agent-session-cancel-requested-p session) nil
            (acp-agent-session-prompt-active-p session) t))
    (unwind-protect
         (let ((stop-reason
                 (handler-case
                     (agent-prompt agent session-id prompt params)
                   (acp-prompt-cancelled ()
                     ':cancelled)
                   (acp-request-cancelled ()
                     ':cancelled)
                   (acp-method-error (condition)
                     (if (acp-agent-session-cancel-requested-p session)
                         ':cancelled
                         (error condition)))
                   (error (condition)
                     (if (or (acp-agent-session-cancel-requested-p session)
                             (acp-request-cancelled-p))
                         ':cancelled
                         (error condition))))))
           (when (acp-agent-session-cancel-requested-p session)
             (setf stop-reason ':cancelled))
           (unless (keywordp stop-reason)
             (error 'acp-protocol-error
                    :message "AGENT-PROMPT must return a stop reason keyword."))
           (json-object "stopReason" (stop-reason-string stop-reason)))
      (with-lock-held ((acp-agent-lock agent))
        (setf (acp-agent-session-prompt-active-p session) nil
              (acp-agent-session-cancel-requested-p session) nil)))))

(-> agent--config-option-value (hash-table) t)
(defun agent--config-option-value (params)
  "Return the value of a set_config_option request as a string, T, or NIL."
  (let ((type (acp-field params "type" :type ':string))
        (value (json-get params "value" ':null)))
    (cond
      ((equal type "boolean")
       (unless (json-boolean-p value)
         (acp-invalid-params "A boolean option takes a boolean value."))
       (json-true-p value))
      ((stringp value)
       value)
      (t
       (acp-invalid-params "The value must be a string or a boolean.")))))

(defmethod peer-handle-request ((agent acp-agent) connection method params)
  "Dispatch an ACP request to the agent's generic functions."
  (declare (ignore connection))
  (let ((keyword (acp-method-keyword method)))
    (when (and keyword (not (eq keyword ':initialize)))
      (agent--require-initialized agent method))
    (unless (or (json-object-p params) (null params) (json-null-p params))
      (acp-invalid-params "The params must be an object."))
    (let ((params (if (json-object-p params) params (json-object))))
      (case keyword
        (:initialize
         (agent--initialize agent params))
        (:authenticate
         (or (agent-authenticate agent
                                 (acp-field params "methodId" :type ':string :required-p t)
                                 params)
             (json-object)))
        (:session-new
         (let ((arguments (agent--session-setup-arguments params)))
           (multiple-value-bind (session-id extras)
               (apply #'agent-new-session agent arguments)
             (unless (stringp session-id)
               (error 'acp-protocol-error
                      :message "AGENT-NEW-SESSION must return a session id string."))
             (agent--register-session agent session-id (getf arguments :cwd))
             (agent--extras extras "sessionId" session-id))))
        (:session-load
         (agent--require-capability agent "loadSession" method)
         (let ((arguments (agent--session-setup-arguments params))
               (session-id (acp-field params "sessionId" :type ':string :required-p t)))
           (let ((extras (apply #'agent-load-session agent :session-id session-id arguments)))
             (agent--register-session agent session-id (getf arguments :cwd))
             (agent--extras extras))))
        (:session-resume
         (agent--require-capability agent "sessionCapabilities.resume" method)
         (let ((arguments (agent--session-setup-arguments params :mcp-servers-required-p nil))
               (session-id (acp-field params "sessionId" :type ':string :required-p t)))
           (let ((extras (apply #'agent-resume-session agent :session-id session-id arguments)))
             (agent--register-session agent session-id (getf arguments :cwd))
             (agent--extras extras))))
        (:session-close
         (agent--require-capability agent "sessionCapabilities.close" method)
         (let* ((session-id (acp-field params "sessionId" :type ':string :required-p t))
                (session (agent--known-session agent session-id)))
           (with-lock-held ((acp-agent-lock agent))
             (setf (acp-agent-session-cancel-requested-p session) t))
           (agent-cancel agent session-id)
           (agent-close-session agent session-id params)
           (agent--forget-session agent session-id)
           (json-object)))
        (:session-prompt
         (let* ((session-id (acp-field params "sessionId" :type ':string :required-p t))
                (session (agent--known-session agent session-id))
                (prompt (acp-validate-content-blocks
                         (acp-field params "prompt" :type ':array :required-p t))))
           (agent--run-prompt agent session prompt params)))
        (:session-set-mode
         (let ((session-id (acp-field params "sessionId" :type ':string :required-p t))
               (mode-id (acp-field params "modeId" :type ':string :required-p t)))
           (agent--known-session agent session-id)
           (agent-set-mode agent session-id mode-id params)
           (json-object)))
        (:session-set-config-option
         (let ((session-id (acp-field params "sessionId" :type ':string :required-p t))
               (config-id (acp-field params "configId" :type ':string :required-p t)))
           (agent--known-session agent session-id)
           (let ((options (agent-set-config-option agent session-id config-id
                                                   (agent--config-option-value params)
                                                   params)))
             (json-object "configOptions" (coerce options 'vector)))))
        (:session-list
         (agent--require-capability agent "sessionCapabilities.list" method)
         (multiple-value-bind (sessions next-cursor)
             (agent-list-sessions agent
                                  :cwd (acp-field params "cwd" :type ':string)
                                  :cursor (acp-field params "cursor" :type ':string)
                                  :params params)
           (json-object "sessions" (coerce sessions 'vector)
                        "nextCursor" next-cursor)))
        (:session-delete
         (agent--require-capability agent "sessionCapabilities.delete" method)
         (let ((session-id (acp-field params "sessionId" :type ':string :required-p t)))
           (agent-delete-session agent session-id params)
           (agent--forget-session agent session-id)
           (json-object)))
        (:logout
         (agent--require-capability agent "auth.logout" method)
         (agent-logout agent params)
         (json-object))
        ((nil)
         (if (acp-extension-method-p method)
             (agent-extension-request agent method params)
             (call-next-method)))
        (t
         (call-next-method))))))

(defmethod peer-handle-notification ((agent acp-agent) connection method params)
  "Handle session/cancel and extension notifications."
  (declare (ignore connection))
  (let ((keyword (acp-method-keyword method)))
    (cond
      ((eq keyword ':session-cancel)
       (let* ((session-id (and (json-object-p params) (json-get params "sessionId")))
              (session (and (stringp session-id) (acp-agent-session agent session-id))))
         (when session
           (with-lock-held ((acp-agent-lock agent))
             (setf (acp-agent-session-cancel-requested-p session) t))
           (agent-cancel agent session-id))))
      ((and (null keyword) (acp-extension-method-p method))
       (agent-extension-notification agent method params))
      (t
       nil)))
  nil)


;;;; -- Calls to the Client --

(-> agent-client-capability-p (acp-agent string) boolean)
(defun agent-client-capability-p (agent path)
  "Return whether the client advertised the dotted capability PATH."
  (acp-capability-enabled-p (acp-agent-client-capabilities agent) path))

(-> agent--require-client-capability (acp-agent string) null)
(defun agent--require-client-capability (agent path)
  "Signal ACP-CAPABILITY-ERROR unless the client advertised PATH."
  (unless (agent-client-capability-p agent path)
    (error 'acp-capability-error
           :message (format nil "The client does not advertise ~A." path)
           :capability path))
  nil)

(-> agent-client-request (acp-agent string t &key (:timeout (or null real))) t)
(defun agent-client-request (agent method params &key (timeout nil timeout-p))
  "Send request METHOD with PARAMS to the client and return its result."
  (let ((connection (agent--connection agent)))
    (if timeout-p
        (connection-request connection method params :timeout timeout)
        (connection-request connection method params))))

(-> agent-client-notify (acp-agent string t) null)
(defun agent-client-notify (agent method params)
  "Send notification METHOD with PARAMS to the client."
  (connection-notify (agent--connection agent) method params))

(-> agent-send-update (acp-agent string hash-table &key (:meta t)) null)
(defun agent-send-update (agent session-id update &key meta)
  "Send session UPDATE, built with an ACP-UPDATE- constructor, for SESSION-ID."
  (agent-client-notify agent
                       (acp-method-name ':session-update)
                       (json-object "sessionId" session-id "update" update "_meta" meta)))

(-> agent-request-permission
    (acp-agent string hash-table list &key (:timeout (or null real)) (:meta t))
    (values keyword (or null string)))
(defun agent-request-permission (agent session-id tool-call options &key (timeout nil timeout-p) meta)
  "Ask the client for permission to run TOOL-CALL, offering permission OPTIONS.

Return the outcome keyword :SELECTED or :CANCELLED and the selected option
id. TOOL-CALL is a tool call or tool call update object."
  (let* ((params (json-object "sessionId" session-id
                              "toolCall" tool-call
                              "options" (coerce options 'vector)
                              "_meta" meta))
         (result (if timeout-p
                     (agent-client-request agent (acp-method-name ':session-request-permission)
                                           params :timeout timeout)
                     (agent-client-request agent (acp-method-name ':session-request-permission)
                                           params)))
         (outcome (json-get result "outcome")))
    (unless (json-object-p outcome)
      (error 'acp-protocol-error
             :message "The permission response lacks an outcome."
             :payload (bounded-diagnostic result)))
    (let ((kind (permission-outcome-keyword (json-get outcome "outcome"))))
      (values kind
              (and (eq kind ':selected)
                   (let ((option-id (json-get outcome "optionId")))
                     (if (stringp option-id)
                         option-id
                         (error 'acp-protocol-error
                                :message "The selected permission outcome lacks an optionId."
                                :payload (bounded-diagnostic result)))))))))

(-> agent-read-text-file
    (acp-agent string string &key (:line (or null integer)) (:limit (or null integer)))
    string)
(defun agent-read-text-file (agent session-id path &key line limit)
  "Read the text file at absolute PATH through the client, from LINE for LIMIT lines."
  (agent--require-client-capability agent "fs.readTextFile")
  (let ((result (agent-client-request agent (acp-method-name ':fs-read-text-file)
                                      (json-object "sessionId" session-id
                                                   "path" path
                                                   "line" line
                                                   "limit" limit))))
    (let ((content (json-get result "content")))
      (unless (stringp content)
        (error 'acp-protocol-error
               :message "The file read response lacks content."
               :payload (bounded-diagnostic result)))
      content)))

(-> agent-write-text-file (acp-agent string string string) null)
(defun agent-write-text-file (agent session-id path content)
  "Write CONTENT to the text file at absolute PATH through the client."
  (agent--require-client-capability agent "fs.writeTextFile")
  (agent-client-request agent (acp-method-name ':fs-write-text-file)
                        (json-object "sessionId" session-id "path" path "content" content))
  nil)

(-> agent-create-terminal
    (acp-agent string string &key (:arguments list) (:environment list)
               (:cwd (or null string)) (:output-byte-limit (or null integer)))
    string)
(defun agent-create-terminal (agent session-id command &key arguments environment cwd output-byte-limit)
  "Start COMMAND in a client terminal and return the terminal id.

ENVIRONMENT is a list of (NAME . VALUE) strings."
  (agent--require-client-capability agent "terminal")
  (let ((result (agent-client-request
                 agent (acp-method-name ':terminal-create)
                 (json-object "sessionId" session-id
                              "command" command
                              "args" (and arguments (coerce arguments 'vector))
                              "env" (and environment
                                         (map 'vector
                                              (lambda (pair)
                                                (acp-env-variable (first pair) (rest pair)))
                                              environment))
                              "cwd" cwd
                              "outputByteLimit" output-byte-limit))))
    (let ((terminal-id (json-get result "terminalId")))
      (unless (stringp terminal-id)
        (error 'acp-protocol-error
               :message "The terminal creation response lacks a terminalId."
               :payload (bounded-diagnostic result)))
      terminal-id)))

(-> agent--exit-status (t) (values boolean (or null integer) (or null string)))
(defun agent--exit-status (status)
  "Return whether STATUS reports an exit, with its exit code and signal."
  (if (json-object-p status)
      (let ((code (json-get status "exitCode"))
            (signal (json-get status "signal")))
        (values t
                (and (integerp code) code)
                (and (stringp signal) signal)))
      (values nil nil nil)))

(-> agent-terminal-output
    (acp-agent string string)
    (values string boolean boolean (or null integer) (or null string)))
(defun agent-terminal-output (agent session-id terminal-id)
  "Return TERMINAL-ID's output so far, whether it was truncated, and its exit state.

The exit state is three values: whether the command exited, its exit code,
and the terminating signal."
  (agent--require-client-capability agent "terminal")
  (let ((result (agent-client-request agent (acp-method-name ':terminal-output)
                                      (json-object "sessionId" session-id
                                                   "terminalId" terminal-id))))
    (let ((output (json-get result "output")))
      (unless (stringp output)
        (error 'acp-protocol-error
               :message "The terminal output response lacks output."
               :payload (bounded-diagnostic result)))
      (multiple-value-bind (exited-p code signal)
          (agent--exit-status (json-get result "exitStatus"))
        (values output
                (json-true-p (json-get result "truncated"))
                exited-p
                code
                signal)))))

(-> agent-wait-for-terminal-exit
    (acp-agent string string &key (:timeout (or null real)))
    (values (or null integer) (or null string)))
(defun agent-wait-for-terminal-exit (agent session-id terminal-id &key (timeout nil timeout-p))
  "Wait for TERMINAL-ID's command to exit; return its exit code and signal."
  (agent--require-client-capability agent "terminal")
  (let* ((params (json-object "sessionId" session-id "terminalId" terminal-id))
         (result (if timeout-p
                     (agent-client-request agent (acp-method-name ':terminal-wait-for-exit)
                                           params :timeout timeout)
                     (agent-client-request agent (acp-method-name ':terminal-wait-for-exit)
                                           params))))
    (multiple-value-bind (exited-p code signal)
        (agent--exit-status result)
      (declare (ignore exited-p))
      (values code signal))))

(-> agent-kill-terminal (acp-agent string string) null)
(defun agent-kill-terminal (agent session-id terminal-id)
  "Kill TERMINAL-ID's command while keeping the terminal readable."
  (agent--require-client-capability agent "terminal")
  (agent-client-request agent (acp-method-name ':terminal-kill)
                        (json-object "sessionId" session-id "terminalId" terminal-id))
  nil)

(-> agent-release-terminal (acp-agent string string) null)
(defun agent-release-terminal (agent session-id terminal-id)
  "Release TERMINAL-ID, killing its command when still running."
  (agent--require-client-capability agent "terminal")
  (agent-client-request agent (acp-method-name ':terminal-release)
                        (json-object "sessionId" session-id "terminalId" terminal-id))
  nil)

(-> agent-create-elicitation
    (acp-agent &key (:mode keyword) (:message string) (:session-id (or null string))
               (:tool-call-id (or null string)) (:request-id t) (:schema t)
               (:url (or null string)) (:elicitation-id (or null string))
               (:timeout (or null real)) (:meta t))
    (values keyword t))
(defun agent-create-elicitation
    (agent &key (mode ':form) message session-id tool-call-id request-id schema url
       elicitation-id (timeout nil timeout-p) meta)
  "Ask the user for structured input through the client.

MODE is :FORM with a SCHEMA object, or :URL with URL and ELICITATION-ID.
Scope the request with SESSION-ID, optionally TOOL-CALL-ID, or REQUEST-ID.
Return the action keyword :ACCEPT, :DECLINE, or :CANCEL and the accepted
content object, if any."
  (agent--require-client-capability agent
                                    (ecase mode
                                      (:form "elicitation.form")
                                      (:url "elicitation.url")))
  (let* ((params (json-object "mode" (ecase mode (:form "form") (:url "url"))
                              "message" message
                              "sessionId" session-id
                              "toolCallId" tool-call-id
                              "requestId" request-id
                              "requestedSchema" (and (eq mode ':form) schema)
                              "url" (and (eq mode ':url) url)
                              "elicitationId" (and (eq mode ':url) elicitation-id)
                              "_meta" meta))
         (result (if timeout-p
                     (agent-client-request agent (acp-method-name ':elicitation-create)
                                           params :timeout timeout)
                     (agent-client-request agent (acp-method-name ':elicitation-create)
                                           params))))
    (values (elicitation-action-keyword (json-get result "action") :default ':unknown-action)
            (let ((content (json-get result "content")))
              (and (json-object-p content) content)))))

(-> agent-complete-elicitation (acp-agent string) null)
(defun agent-complete-elicitation (agent elicitation-id)
  "Tell the client that URL elicitation ELICITATION-ID finished out of band."
  (agent-client-notify agent (acp-method-name ':elicitation-complete)
                       (json-object "elicitationId" elicitation-id)))
