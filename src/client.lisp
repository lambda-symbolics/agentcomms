(in-package #:agentcomms)

;;;; -- Client Role --

(defparameter *acp-client-implementation-name* "agentcomms"
  "The implementation name a client reports unless it overrides CLIENT-IMPLEMENTATION.")

(defparameter *acp-client-implementation-version* "0.1.0"
  "The implementation version a client reports unless it overrides CLIENT-IMPLEMENTATION.")

(defclass acp-client (acp-peer)
  ((connection
    :initform nil
    :accessor acp-client-connection
    :type (or null acp-connection)
    :documentation "The connection to the agent once attached.")
   (protocol-version
    :initform nil
    :accessor acp-client-protocol-version
    :type (or null integer)
    :documentation "The negotiated protocol version, or NIL before initialize.")
   (agent-capabilities
    :initform nil
    :accessor acp-client-agent-capabilities
    :type t
    :documentation "The agent capabilities object received from initialize.")
   (agent-info
    :initform nil
    :accessor acp-client-agent-info
    :type t
    :documentation "The agent implementation descriptor, when provided.")
   (auth-methods
    :initform nil
    :accessor acp-client-auth-methods
    :type list
    :documentation "The authentication methods the agent advertised.")
   (advertised-capabilities
    :initform nil
    :accessor acp-client-advertised-capabilities
    :type t
    :documentation "The capabilities this client sent in initialize.")
   (lock
    :initform (make-lock "agentcomms client")
    :reader acp-client-lock
    :type t
    :documentation "The lock guarding negotiation state."))
  (:documentation
   "The client side of an ACP connection, typically an editor driving an agent.

Subclass it and specialize the CLIENT- generic functions that serve the
agent's requests; call the CLIENT- functions below to drive the agent."))

(-> acp-client-connect
    (acp-client acp-channel &key (:name string) (:log-function (or null function))
                (:request-timeout (or null real)))
    acp-connection)
(defun acp-client-connect (client channel &key (name "agentcomms client") log-function
                                                (request-timeout *acp-default-request-timeout*))
  "Attach CLIENT to CHANNEL and return the running connection."
  (let ((connection (make-acp-connection :channel channel
                                         :peer client
                                         :name name
                                         :log-function log-function
                                         :request-timeout request-timeout)))
    (setf (acp-client-connection client) connection)
    connection))

(-> client--connection (acp-client) acp-connection)
(defun client--connection (client)
  "Return CLIENT's connection or signal that it is not attached."
  (or (acp-client-connection client)
      (error 'acp-state-error :message "The client is not attached to a connection.")))


;;;; -- Generic Functions for Client Implementations --

(defgeneric client-implementation (client)
  (:documentation "Return the implementation descriptor sent as clientInfo."))

(defgeneric client-capabilities (client)
  (:documentation
   "Return the client capabilities object sent in initialize.

Build it with ACP-CLIENT-CAPABILITIES. Agent requests for file system,
terminal, and elicitation methods are only dispatched when advertised."))

(defgeneric client-session-update (client session-id update params)
  (:documentation
   "Observe session UPDATE for SESSION-ID; ACP-UPDATE-KIND names its kind.

PARAMS is the whole notification object, including any _meta."))

(defgeneric client-request-permission (client session-id tool-call options params)
  (:documentation
   "Decide whether TOOL-CALL may run, choosing among permission OPTIONS.

Return :SELECTED and the chosen option id, or :CANCELLED when the prompt
turn was cancelled."))

(defgeneric client-read-text-file (client session-id path &key line limit params)
  (:documentation "Return the text of PATH, from 1-based LINE for at most LIMIT lines."))

(defgeneric client-write-text-file (client session-id path content params)
  (:documentation "Write CONTENT to PATH, creating the file when needed."))

(defgeneric client-create-terminal
    (client session-id command &key arguments environment cwd output-byte-limit params)
  (:documentation
   "Start COMMAND with ARGUMENTS in a terminal and return its id without waiting.

ENVIRONMENT is a list of (NAME . VALUE) strings."))

(defgeneric client-terminal-output (client session-id terminal-id params)
  (:documentation
   "Return TERMINAL-ID's output so far and its exit state.

Return five values: the output, whether it was truncated, whether the
command exited, its exit code, and its terminating signal."))

(defgeneric client-wait-for-terminal-exit (client session-id terminal-id params)
  (:documentation "Block until TERMINAL-ID exits; return its exit code and signal."))

(defgeneric client-kill-terminal (client session-id terminal-id params)
  (:documentation "Kill TERMINAL-ID's command while keeping its output readable."))

(defgeneric client-release-terminal (client session-id terminal-id params)
  (:documentation "Release TERMINAL-ID, killing its command when still running."))

(defgeneric client-create-elicitation (client params)
  (:documentation
   "Collect user input for the elicitation request PARAMS.

Return the action keyword :ACCEPT, :DECLINE, or :CANCEL and, for an
accepted form, the content object."))

(defgeneric client-elicitation-completed (client elicitation-id params)
  (:documentation "Observe that URL elicitation ELICITATION-ID completed out of band."))

(defgeneric client-extension-request (client method params)
  (:documentation "Answer the extension request METHOD, whose name begins with an underscore."))

(defgeneric client-extension-notification (client method params)
  (:documentation "React to the extension notification METHOD."))

(defmethod client-implementation ((client acp-client))
  "Report the library's own name and version."
  (acp-implementation *acp-client-implementation-name* *acp-client-implementation-version*))

(defmethod client-capabilities ((client acp-client))
  "Advertise no optional capabilities."
  (acp-client-capabilities))

(defmethod client-session-update ((client acp-client) session-id update params)
  "Ignore updates by default."
  (declare (ignore session-id update params))
  nil)

(defmethod client-request-permission ((client acp-client) session-id tool-call options params)
  "Signal that permission handling is unimplemented."
  (declare (ignore session-id tool-call options params))
  (error 'acp-method-error
         :code (acp-error-code ':internal-error)
         :message "This client does not implement session/request_permission."))

(-> client--unadvertised (string) nil)
(defun client--unadvertised (method)
  "Signal Method Not Found for an optional METHOD this client never advertised."
  (error 'acp-method-error
         :code (acp-error-code ':method-not-found)
         :message (format nil "This client does not provide ~A." method)))

(defmethod client-read-text-file ((client acp-client) session-id path &key line limit params)
  "Signal that file reading is unavailable."
  (declare (ignore session-id path line limit params))
  (client--unadvertised "fs/read_text_file"))

(defmethod client-write-text-file ((client acp-client) session-id path content params)
  "Signal that file writing is unavailable."
  (declare (ignore session-id path content params))
  (client--unadvertised "fs/write_text_file"))

(defmethod client-create-terminal ((client acp-client) session-id command
                                   &key arguments environment cwd output-byte-limit params)
  "Signal that terminals are unavailable."
  (declare (ignore session-id command arguments environment cwd output-byte-limit params))
  (client--unadvertised "terminal/create"))

(defmethod client-terminal-output ((client acp-client) session-id terminal-id params)
  "Signal that terminals are unavailable."
  (declare (ignore session-id terminal-id params))
  (client--unadvertised "terminal/output"))

(defmethod client-wait-for-terminal-exit ((client acp-client) session-id terminal-id params)
  "Signal that terminals are unavailable."
  (declare (ignore session-id terminal-id params))
  (client--unadvertised "terminal/wait_for_exit"))

(defmethod client-kill-terminal ((client acp-client) session-id terminal-id params)
  "Signal that terminals are unavailable."
  (declare (ignore session-id terminal-id params))
  (client--unadvertised "terminal/kill"))

(defmethod client-release-terminal ((client acp-client) session-id terminal-id params)
  "Signal that terminals are unavailable."
  (declare (ignore session-id terminal-id params))
  (client--unadvertised "terminal/release"))

(defmethod client-create-elicitation ((client acp-client) params)
  "Signal that elicitation is unavailable."
  (declare (ignore params))
  (client--unadvertised "elicitation/create"))

(defmethod client-elicitation-completed ((client acp-client) elicitation-id params)
  "Ignore completions by default."
  (declare (ignore elicitation-id params))
  nil)

(defmethod client-extension-request ((client acp-client) method params)
  "Answer unknown extension methods with Method Not Found."
  (declare (ignore params))
  (error 'acp-method-error
         :code (acp-error-code ':method-not-found)
         :message (format nil "Method not found: ~A" method)))

(defmethod client-extension-notification ((client acp-client) method params)
  "Ignore unknown extension notifications."
  (declare (ignore method params))
  nil)


;;;; -- Serving the Agent's Requests --

(-> client--require-advertised (acp-client string string) null)
(defun client--require-advertised (client path method)
  "Signal Method Not Found when this client never advertised capability PATH."
  (unless (acp-capability-enabled-p (acp-client-advertised-capabilities client) path)
    (client--unadvertised method))
  nil)

(-> client--exit-status-object ((or null integer) (or null string)) hash-table)
(defun client--exit-status-object (exit-code signal)
  "Return a terminal exit status object with null for absent fields."
  (json-object "exitCode" (or exit-code (json-null-value))
               "signal" (or signal (json-null-value))))

(defmethod peer-handle-request ((client acp-client) connection method params)
  "Dispatch an agent request to the client's generic functions."
  (declare (ignore connection))
  (unless (or (json-object-p params) (null params) (json-null-p params))
    (acp-invalid-params "The params must be an object."))
  (let ((params (if (json-object-p params) params (json-object)))
        (keyword (acp-method-keyword method)))
    (flet ((session-id ()
             (acp-field params "sessionId" :type ':string :required-p t))
           (terminal-id ()
             (acp-field params "terminalId" :type ':string :required-p t)))
      (case keyword
        (:session-request-permission
         (let ((tool-call (acp-field params "toolCall" :type ':object :required-p t))
               (options (acp-field params "options" :type ':array :required-p t)))
           (acp-field tool-call "toolCallId" :type ':string :required-p t)
           (dolist (option options)
             (acp-field option "optionId" :type ':string :required-p t)
             (acp-field option "name" :type ':string :required-p t))
           (multiple-value-bind (outcome option-id)
               (client-request-permission client (session-id) tool-call options params)
             (json-object "outcome"
                          (ecase outcome
                            (:selected
                             (unless (stringp option-id)
                               (error 'acp-protocol-error
                                      :message "A selected permission outcome needs an option id."))
                             (json-object "outcome" "selected" "optionId" option-id))
                            (:cancelled
                             (json-object "outcome" "cancelled")))))))
        (:fs-read-text-file
         (client--require-advertised client "fs.readTextFile" method)
         (json-object "content"
                      (client-read-text-file client (session-id)
                                             (acp-field params "path" :type ':string :required-p t)
                                             :line (acp-field params "line" :type ':integer)
                                             :limit (acp-field params "limit" :type ':integer)
                                             :params params)))
        (:fs-write-text-file
         (client--require-advertised client "fs.writeTextFile" method)
         (client-write-text-file client (session-id)
                                 (acp-field params "path" :type ':string :required-p t)
                                 (acp-field params "content" :type ':string :required-p t)
                                 params)
         (json-object))
        (:terminal-create
         (client--require-advertised client "terminal" method)
         (let ((arguments (acp-field params "args" :type ':array)))
           (dolist (argument arguments)
             (unless (stringp argument)
               (acp-invalid-params "Each terminal argument must be a string.")))
           (json-object "terminalId"
                        (client-create-terminal
                         client (session-id)
                         (acp-field params "command" :type ':string :required-p t)
                         :arguments arguments
                         :environment (acp-name-value-pairs (json-get params "env"))
                         :cwd (acp-field params "cwd" :type ':string)
                         :output-byte-limit (acp-field params "outputByteLimit" :type ':integer)
                         :params params))))
        (:terminal-output
         (client--require-advertised client "terminal" method)
         (multiple-value-bind (output truncated-p exited-p exit-code signal)
             (client-terminal-output client (session-id) (terminal-id) params)
           (json-object "output" output
                        "truncated" (acp-boolean truncated-p)
                        "exitStatus" (and exited-p (client--exit-status-object exit-code signal)))))
        (:terminal-wait-for-exit
         (client--require-advertised client "terminal" method)
         (multiple-value-bind (exit-code signal)
             (client-wait-for-terminal-exit client (session-id) (terminal-id) params)
           (client--exit-status-object exit-code signal)))
        (:terminal-kill
         (client--require-advertised client "terminal" method)
         (client-kill-terminal client (session-id) (terminal-id) params)
         (json-object))
        (:terminal-release
         (client--require-advertised client "terminal" method)
         (client-release-terminal client (session-id) (terminal-id) params)
         (json-object))
        (:elicitation-create
         (let ((mode (acp-field params "mode" :type ':string :required-p t)))
           (client--require-advertised client
                                       (cond
                                         ((string= mode "form") "elicitation.form")
                                         ((string= mode "url") "elicitation.url")
                                         (t (acp-invalid-params "~A is not an elicitation mode." mode)))
                                       method)
           (acp-field params "message" :type ':string :required-p t)
           (multiple-value-bind (action content)
               (client-create-elicitation client params)
             (json-object "action" (elicitation-action-string action)
                          "content" (and (eq action ':accept) content)))))
        ((nil)
         (if (acp-extension-method-p method)
             (client-extension-request client method params)
             (call-next-method)))
        (t
         (call-next-method))))))

(defmethod peer-handle-notification ((client acp-client) connection method params)
  "Deliver session updates, elicitation completions, and extension notifications."
  (declare (ignore connection))
  (let ((keyword (acp-method-keyword method)))
    (cond
      ((eq keyword ':session-update)
       (let ((session-id (and (json-object-p params) (json-get params "sessionId")))
             (update (and (json-object-p params) (json-get params "update"))))
         (when (and (stringp session-id) (json-object-p update))
           (client-session-update client session-id update params))))
      ((eq keyword ':elicitation-complete)
       (let ((elicitation-id (and (json-object-p params) (json-get params "elicitationId"))))
         (when (stringp elicitation-id)
           (client-elicitation-completed client elicitation-id params))))
      ((and (null keyword) (acp-extension-method-p method))
       (client-extension-notification client method params))
      (t
       nil)))
  nil)


;;;; -- Driving the Agent --

(-> client-agent-capability-p (acp-client string) boolean)
(defun client-agent-capability-p (client path)
  "Return whether the agent advertised the dotted capability PATH."
  (acp-capability-enabled-p (acp-client-agent-capabilities client) path))

(-> client--require-agent-capability (acp-client string) null)
(defun client--require-agent-capability (client path)
  "Signal ACP-CAPABILITY-ERROR unless the agent advertised PATH."
  (unless (client-agent-capability-p client path)
    (error 'acp-capability-error
           :message (format nil "The agent does not advertise ~A." path)
           :capability path))
  nil)

(-> client--require-initialized (acp-client) null)
(defun client--require-initialized (client)
  "Signal ACP-STATE-ERROR before initialize has completed."
  (unless (acp-client-protocol-version client)
    (error 'acp-state-error :message "Call CLIENT-INITIALIZE before other agent methods."))
  nil)

(-> client-agent-request (acp-client string t &key (:timeout (or null real))) t)
(defun client-agent-request (client method params &key (timeout nil timeout-p))
  "Send request METHOD with PARAMS to the agent and return its result."
  (let ((connection (client--connection client)))
    (if timeout-p
        (connection-request connection method params :timeout timeout)
        (connection-request connection method params))))

(-> client-agent-notify (acp-client string t) null)
(defun client-agent-notify (client method params)
  "Send notification METHOD with PARAMS to the agent."
  (connection-notify (client--connection client) method params))

(-> client-initialize
    (acp-client &key (:protocol-version integer) (:timeout (or null real)) (:meta t))
    hash-table)
(defun client-initialize (client &key (protocol-version *acp-protocol-version*)
                                      (timeout nil timeout-p) meta)
  "Negotiate the protocol with the agent and record its capabilities.

Return the initialize result. Signal ACP-UNSUPPORTED-VERSION when the agent
selects a version this library cannot speak."
  (let* ((capabilities (client-capabilities client))
         (params (json-object "protocolVersion" protocol-version
                              "clientCapabilities" capabilities
                              "clientInfo" (client-implementation client)
                              "_meta" meta))
         (result (if timeout-p
                     (client-agent-request client (acp-method-name ':initialize) params
                                           :timeout timeout)
                     (client-agent-request client (acp-method-name ':initialize) params)))
         (version (json-get result "protocolVersion")))
    (unless (and (integerp version) (member version *acp-supported-protocol-versions*))
      (error 'acp-unsupported-version
             :message (format nil "The agent selected protocol version ~A, which is unsupported."
                              (bounded-diagnostic version))
             :version version))
    (with-lock-held ((acp-client-lock client))
      (setf (acp-client-protocol-version client) version
            (acp-client-advertised-capabilities client) capabilities
            (acp-client-agent-capabilities client) (let ((object (json-get result "agentCapabilities")))
                                                     (if (json-object-p object) object (json-object)))
            (acp-client-agent-info client) (let ((info (json-get result "agentInfo")))
                                             (and (json-object-p info) info))
            (acp-client-auth-methods client) (json-sequence->list
                                              (let ((methods (json-get result "authMethods")))
                                                (and (vectorp methods) (not (stringp methods)) methods)))))
    result))

(-> client-authenticate (acp-client string &key (:meta t)) hash-table)
(defun client-authenticate (client method-id &key meta)
  "Run the agent-driven authentication METHOD-ID and return the result."
  (client--require-initialized client)
  (let ((method (find method-id (acp-client-auth-methods client)
                      :key (lambda (entry) (json-get entry "id"))
                      :test #'equal)))
    (when (and method (equal (json-get method "type") "terminal"))
      (error 'acp-state-error
             :message (format nil "Authentication method ~A runs in a terminal, not through authenticate."
                              method-id))))
  (client-agent-request client (acp-method-name ':authenticate)
                        (json-object "methodId" method-id "_meta" meta)))

(-> client--session-setup-params
    (string &key (:session-id (or null string)) (:mcp-servers list)
            (:additional-directories list) (:meta t))
    hash-table)
(defun client--session-setup-params (cwd &key session-id mcp-servers additional-directories meta)
  "Return the shared parameters of the session setup methods."
  (json-object "sessionId" session-id
               "cwd" cwd
               "mcpServers" (coerce mcp-servers 'vector)
               "additionalDirectories" (and additional-directories
                                            (coerce additional-directories 'vector))
               "_meta" meta))

(-> client-new-session
    (acp-client string &key (:mcp-servers list) (:additional-directories list) (:meta t))
    (values string hash-table))
(defun client-new-session (client cwd &key mcp-servers additional-directories meta)
  "Create a session rooted at absolute CWD; return its id and the whole result."
  (client--require-initialized client)
  (when additional-directories
    (client--require-agent-capability client "sessionCapabilities.additionalDirectories"))
  (let* ((result (client-agent-request client (acp-method-name ':session-new)
                                       (client--session-setup-params
                                        cwd :mcp-servers mcp-servers
                                            :additional-directories additional-directories
                                            :meta meta)))
         (session-id (json-get result "sessionId")))
    (unless (stringp session-id)
      (error 'acp-protocol-error
             :message "The session/new response lacks a sessionId."
             :payload (bounded-diagnostic result)))
    (values session-id result)))

(-> client-load-session
    (acp-client string string &key (:mcp-servers list) (:additional-directories list) (:meta t))
    hash-table)
(defun client-load-session (client session-id cwd &key mcp-servers additional-directories meta)
  "Load SESSION-ID, receiving its history as session updates, and return the result."
  (client--require-initialized client)
  (client--require-agent-capability client "loadSession")
  (client-agent-request client (acp-method-name ':session-load)
                        (client--session-setup-params
                         cwd :session-id session-id :mcp-servers mcp-servers
                             :additional-directories additional-directories :meta meta)))

(-> client-resume-session
    (acp-client string string &key (:mcp-servers list) (:additional-directories list) (:meta t))
    hash-table)
(defun client-resume-session (client session-id cwd &key mcp-servers additional-directories meta)
  "Resume SESSION-ID without history replay and return the result."
  (client--require-initialized client)
  (client--require-agent-capability client "sessionCapabilities.resume")
  (client-agent-request client (acp-method-name ':session-resume)
                        (client--session-setup-params
                         cwd :session-id session-id :mcp-servers mcp-servers
                             :additional-directories additional-directories :meta meta)))

(-> client-close-session (acp-client string &key (:meta t)) hash-table)
(defun client-close-session (client session-id &key meta)
  "Close the active SESSION-ID."
  (client--require-initialized client)
  (client--require-agent-capability client "sessionCapabilities.close")
  (client-agent-request client (acp-method-name ':session-close)
                        (json-object "sessionId" session-id "_meta" meta)))

(-> client-prompt
    (acp-client string list &key (:timeout (or null real)) (:meta t))
    (values keyword hash-table))
(defun client-prompt (client session-id prompt &key (timeout nil timeout-p) meta)
  "Send the content blocks PROMPT to SESSION-ID and wait for the turn to end.

Return the stop reason keyword, :UNKNOWN-STOP-REASON for an unrecognized
value, and the whole result."
  (client--require-initialized client)
  (let* ((params (json-object "sessionId" session-id
                              "prompt" (coerce prompt 'vector)
                              "_meta" meta))
         (result (if timeout-p
                     (client-agent-request client (acp-method-name ':session-prompt) params
                                           :timeout timeout)
                     (client-agent-request client (acp-method-name ':session-prompt) params))))
    (values (stop-reason-keyword (json-get result "stopReason") :default ':unknown-stop-reason)
            result)))

(-> client-cancel (acp-client string &key (:meta t)) null)
(defun client-cancel (client session-id &key meta)
  "Ask the agent to cancel SESSION-ID's prompt turn."
  (client-agent-notify client (acp-method-name ':session-cancel)
                       (json-object "sessionId" session-id "_meta" meta)))

(-> client-set-mode (acp-client string string &key (:meta t)) hash-table)
(defun client-set-mode (client session-id mode-id &key meta)
  "Switch SESSION-ID to MODE-ID."
  (client--require-initialized client)
  (client-agent-request client (acp-method-name ':session-set-mode)
                        (json-object "sessionId" session-id "modeId" mode-id "_meta" meta)))

(-> client-set-config-option (acp-client string string t &key (:meta t)) list)
(defun client-set-config-option (client session-id config-id value &key meta)
  "Set CONFIG-ID of SESSION-ID to VALUE, a string or a boolean; return all options."
  (client--require-initialized client)
  (let ((result (client-agent-request
                 client (acp-method-name ':session-set-config-option)
                 (if (stringp value)
                     (json-object "sessionId" session-id "configId" config-id
                                  "value" value "_meta" meta)
                     (json-object "sessionId" session-id "configId" config-id
                                  "type" "boolean" "value" (acp-boolean value) "_meta" meta)))))
    (json-sequence->list (json-get result "configOptions"))))

(-> client-list-sessions
    (acp-client &key (:cwd (or null string)) (:cursor (or null string)) (:meta t))
    (values list (or null string)))
(defun client-list-sessions (client &key cwd cursor meta)
  "List the agent's sessions, optionally filtered by CWD; return them and the next cursor."
  (client--require-initialized client)
  (client--require-agent-capability client "sessionCapabilities.list")
  (let ((result (client-agent-request client (acp-method-name ':session-list)
                                      (json-object "cwd" cwd "cursor" cursor "_meta" meta))))
    (values (json-sequence->list (json-get result "sessions"))
            (let ((next (json-get result "nextCursor")))
              (and (stringp next) next)))))

(-> client-delete-session (acp-client string &key (:meta t)) hash-table)
(defun client-delete-session (client session-id &key meta)
  "Delete SESSION-ID from the agent's session list."
  (client--require-initialized client)
  (client--require-agent-capability client "sessionCapabilities.delete")
  (client-agent-request client (acp-method-name ':session-delete)
                        (json-object "sessionId" session-id "_meta" meta)))

(-> client-logout (acp-client &key (:meta t)) hash-table)
(defun client-logout (client &key meta)
  "End the agent's authenticated state."
  (client--require-initialized client)
  (client--require-agent-capability client "auth.logout")
  (client-agent-request client (acp-method-name ':logout) (json-object "_meta" meta)))
