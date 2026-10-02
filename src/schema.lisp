(in-package #:agentcomms)

;;;; -- Protocol Versions and Methods --

(defparameter *acp-protocol-version* 1
  "The ACP major protocol version this library speaks.")

(defparameter *acp-supported-protocol-versions* '(1)
  "Every protocol version this library can negotiate, newest first.")

(defparameter *acp-schema-reference*
  "https://github.com/agentclientprotocol/agent-client-protocol schema/v1 at 59172bafdf4bb283e9f2b5baf8aa7724c9f2c65b"
  "The upstream schema revision this library's types follow.")

(defparameter *acp-agent-methods*
  '((:initialize . "initialize")
    (:authenticate . "authenticate")
    (:session-new . "session/new")
    (:session-load . "session/load")
    (:session-resume . "session/resume")
    (:session-close . "session/close")
    (:session-set-mode . "session/set_mode")
    (:session-set-config-option . "session/set_config_option")
    (:session-prompt . "session/prompt")
    (:session-cancel . "session/cancel")
    (:session-list . "session/list")
    (:session-delete . "session/delete")
    (:logout . "logout"))
  "The methods an agent serves, as (KEYWORD . WIRE-NAME).")

(defparameter *acp-client-methods*
  '((:session-request-permission . "session/request_permission")
    (:session-update . "session/update")
    (:fs-read-text-file . "fs/read_text_file")
    (:fs-write-text-file . "fs/write_text_file")
    (:terminal-create . "terminal/create")
    (:terminal-output . "terminal/output")
    (:terminal-release . "terminal/release")
    (:terminal-wait-for-exit . "terminal/wait_for_exit")
    (:terminal-kill . "terminal/kill")
    (:elicitation-create . "elicitation/create")
    (:elicitation-complete . "elicitation/complete"))
  "The methods a client serves, as (KEYWORD . WIRE-NAME).")

(-> acp-method-name (keyword) string)
(defun acp-method-name (method)
  "Return the wire name of the agent or client METHOD keyword."
  (or (rest (assoc method *acp-agent-methods*))
      (rest (assoc method *acp-client-methods*))
      (error 'acp-protocol-error
             :message (format nil "~S is not an ACP method." method))))

(-> acp-method-keyword (string) (or null keyword))
(defun acp-method-keyword (name)
  "Return the keyword for wire method NAME, or NIL when it is not an ACP method."
  (or (first (rassoc name *acp-agent-methods* :test #'string=))
      (first (rassoc name *acp-client-methods* :test #'string=))))

(-> acp-extension-method-p (string) boolean)
(defun acp-extension-method-p (name)
  "Return true when NAME is an extension method, which begins with an underscore."
  (and (plusp (length name)) (char= (char name 0) #\_)))


;;;; -- Error Codes --

(defparameter *acp-error-codes*
  '((:parse-error . -32700)
    (:invalid-request . -32600)
    (:method-not-found . -32601)
    (:invalid-params . -32602)
    (:internal-error . -32603)
    (:request-cancelled . -32800)
    (:authentication-required . -32000)
    (:resource-not-found . -32002))
  "The JSON-RPC and ACP error codes, as (KEYWORD . CODE).")

(-> acp-error-code (keyword) integer)
(defun acp-error-code (name)
  "Return the numeric JSON-RPC error code named NAME."
  (or (rest (assoc name *acp-error-codes*))
      (error 'acp-protocol-error
             :message (format nil "~S is not a known ACP error code." name))))

(-> acp-invalid-params (string &rest t) null)
(defun acp-invalid-params (control &rest arguments)
  "Signal an Invalid Params error describing the problem with CONTROL and ARGUMENTS."
  (error 'acp-method-error
         :code (acp-error-code ':invalid-params)
         :message (apply #'format nil control arguments)))

(-> acp-authentication-required (&optional string) null)
(defun acp-authentication-required (&optional (message "Authentication is required."))
  "Signal the Authentication Required error with MESSAGE."
  (error 'acp-method-error
         :code (acp-error-code ':authentication-required)
         :message message))

(-> acp-resource-not-found (string) null)
(defun acp-resource-not-found (message)
  "Signal the Resource Not Found error with MESSAGE."
  (error 'acp-method-error
         :code (acp-error-code ':resource-not-found)
         :message message))


;;;; -- Enumerations --

(defmacro define-acp-enumeration (name documentation &rest pairs)
  "Define the enumeration NAME mapping keywords to wire strings.

PAIRS are (KEYWORD \"wire_value\") entries. The macro defines the variable
*NAME-TABLE*, the function NAME-STRING converting a keyword to its wire
string, and NAME-KEYWORD converting a wire string back, returning DEFAULT
for unknown strings. Both signal ACP-PROTOCOL-ERROR for values outside the
enumeration unless a default is given."
  (let ((table (intern (format nil "*~A-TABLE*" name)))
        (to-string (intern (format nil "~A-STRING" name)))
        (to-keyword (intern (format nil "~A-KEYWORD" name))))
    `(progn
       (defparameter ,table ',(mapcar (lambda (pair) (cons (first pair) (second pair))) pairs)
         ,documentation)
       (-> ,to-string (keyword) string)
       (defun ,to-string (keyword)
         ,(format nil "Return the wire string of the ~(~A~) KEYWORD." name)
         (or (rest (assoc keyword ,table))
             (error 'acp-protocol-error
                    :message (format nil "~S is not a ~(~A~)." keyword ',name))))
       (-> ,to-keyword (t &key (:default t)) t)
       (defun ,to-keyword (string &key (default ':unknown))
         ,(format nil "Return the ~(~A~) keyword for wire STRING, or DEFAULT when unknown.

A DEFAULT of :UNKNOWN signals ACP-PROTOCOL-ERROR instead."
                  name)
         (let ((entry (and (stringp string)
                           (rassoc string ,table :test #'string=))))
           (cond
             (entry
              (first entry))
             ((eq default ':unknown)
              (error 'acp-protocol-error
                     :message (format nil "~A is not a ~(~A~)."
                                      (bounded-diagnostic string) ',name)
                     :payload string))
             (t
              default)))))))

(define-acp-enumeration stop-reason
  "Why an agent ended a prompt turn."
  (:end-turn "end_turn")
  (:max-tokens "max_tokens")
  (:max-turn-requests "max_turn_requests")
  (:refusal "refusal")
  (:cancelled "cancelled"))

(define-acp-enumeration tool-kind
  "The category of a tool call, chosen for icons and display."
  (:read "read")
  (:edit "edit")
  (:delete "delete")
  (:move "move")
  (:search "search")
  (:execute "execute")
  (:think "think")
  (:fetch "fetch")
  (:switch-mode "switch_mode")
  (:other "other"))

(define-acp-enumeration tool-call-status
  "The execution status of a tool call."
  (:pending "pending")
  (:in-progress "in_progress")
  (:completed "completed")
  (:failed "failed"))

(define-acp-enumeration plan-entry-priority
  "The relative importance of a plan entry."
  (:high "high")
  (:medium "medium")
  (:low "low"))

(define-acp-enumeration plan-entry-status
  "The execution status of a plan entry."
  (:pending "pending")
  (:in-progress "in_progress")
  (:completed "completed"))

(define-acp-enumeration permission-option-kind
  "The hint telling a client how to present a permission option."
  (:allow-once "allow_once")
  (:allow-always "allow_always")
  (:reject-once "reject_once")
  (:reject-always "reject_always"))

(define-acp-enumeration permission-outcome
  "The outcome of a permission request."
  (:selected "selected")
  (:cancelled "cancelled"))

(define-acp-enumeration content-role
  "The sender or recipient of content in annotations."
  (:assistant "assistant")
  (:user "user"))

(define-acp-enumeration content-type
  "The discriminator of a content block."
  (:text "text")
  (:image "image")
  (:audio "audio")
  (:resource-link "resource_link")
  (:resource "resource"))

(define-acp-enumeration tool-call-content-type
  "The discriminator of content produced by a tool call."
  (:content "content")
  (:diff "diff")
  (:terminal "terminal"))

(define-acp-enumeration session-update-kind
  "The discriminator of a session/update notification."
  (:user-message-chunk "user_message_chunk")
  (:agent-message-chunk "agent_message_chunk")
  (:agent-thought-chunk "agent_thought_chunk")
  (:tool-call "tool_call")
  (:tool-call-update "tool_call_update")
  (:plan "plan")
  (:available-commands-update "available_commands_update")
  (:current-mode-update "current_mode_update")
  (:config-option-update "config_option_update")
  (:session-info-update "session_info_update")
  (:usage-update "usage_update"))

(define-acp-enumeration config-option-category
  "The semantic category of a session configuration option."
  (:mode "mode")
  (:model "model")
  (:model-config "model_config")
  (:thought-level "thought_level"))

(define-acp-enumeration mcp-transport
  "The transport of an MCP server configuration."
  (:stdio "stdio")
  (:http "http")
  (:sse "sse"))

(define-acp-enumeration auth-method-type
  "How a client performs an advertised authentication method."
  (:agent "agent")
  (:terminal "terminal"))

(define-acp-enumeration elicitation-action
  "The user's answer to an elicitation."
  (:accept "accept")
  (:decline "decline")
  (:cancel "cancel"))


;;;; -- Validation of Received Objects --

(-> acp-field (t string &key (:type keyword) (:required-p boolean) (:default t)) t)
(defun acp-field (object key &key (type ':any) required-p default)
  "Return field KEY of received JSON OBJECT after checking its TYPE.

TYPE is one of :STRING, :INTEGER, :NUMBER, :BOOLEAN, :OBJECT, :ARRAY, or
:ANY. Null and absence are equivalent and yield DEFAULT, or an Invalid
Params error when REQUIRED-P. Booleans return T or NIL, arrays return
lists, and other values are returned as decoded."
  (unless (json-object-p object)
    (acp-invalid-params "A parameter object is required."))
  (let ((value (json-get object key ':null)))
    (when (json-null-p value)
      (when required-p
        (acp-invalid-params "The field ~A is required." key))
      (return-from acp-field default))
    (flet ((reject ()
             (acp-invalid-params "The field ~A must be ~(~A~)." key type)))
      (ecase type
        (:any
         value)
        (:string
         (if (stringp value) value (reject)))
        (:integer
         (if (integerp value) value (reject)))
        (:number
         (if (realp value) value (reject)))
        (:boolean
         (if (json-boolean-p value) (json-true-p value) (reject)))
        (:object
         (if (json-object-p value) value (reject)))
        (:array
         (if (and (vectorp value) (not (stringp value)))
             (coerce value 'list)
             (reject)))))))

(-> acp-capability-enabled-p (t string) boolean)
(defun acp-capability-enabled-p (capabilities path)
  "Return whether dotted PATH is advertised in received CAPABILITIES.

A capability counts as enabled when its value is JSON true or a non-null
object. Missing intermediate objects, null, and false all mean unsupported."
  (let ((value capabilities))
    (dolist (key (acp--split-path path))
      (unless (json-object-p value)
        (return-from acp-capability-enabled-p nil))
      (setf value (json-get value key ':null)))
    (and (or (json-true-p value) (json-object-p value)) t)))

(-> acp--split-path (string) list)
(defun acp--split-path (path)
  "Split dotted PATH into its keys."
  (loop with start = 0
        for position = (position #\. path :start start)
        collect (subseq path start position)
        while position
        do (setf start (1+ position))))


;;;; -- Implementations and Capabilities --

(-> acp-implementation (string string &key (:title (or null string)) (:meta t)) hash-table)
(defun acp-implementation (name version &key title meta)
  "Return an implementation descriptor naming a client or agent."
  (json-object "name" name "version" version "title" title "_meta" meta))

(-> acp-marker (boolean) t)
(defun acp-marker (enabled-p)
  "Return an empty object advertising support when ENABLED-P, else NIL."
  (and enabled-p (json-object)))

(-> acp-boolean (boolean) t)
(defun acp-boolean (value)
  "Return the JSON boolean for Lisp boolean VALUE."
  (if value (json-true-value) (json-false-value)))

(-> acp-agent-capabilities
    (&key (:load-session boolean) (:image boolean) (:audio boolean)
          (:embedded-context boolean) (:mcp-http boolean) (:mcp-sse boolean)
          (:list boolean) (:delete boolean) (:additional-directories boolean)
          (:resume boolean) (:close boolean) (:logout boolean) (:meta t))
    hash-table)
(defun acp-agent-capabilities
    (&key load-session image audio embedded-context mcp-http mcp-sse
       list delete additional-directories resume close logout meta)
  "Return the agent capabilities object advertised in the initialize response."
  (json-object
   "loadSession" (acp-boolean load-session)
   "promptCapabilities" (json-object "image" (acp-boolean image)
                                     "audio" (acp-boolean audio)
                                     "embeddedContext" (acp-boolean embedded-context))
   "mcpCapabilities" (json-object "http" (acp-boolean mcp-http)
                                  "sse" (acp-boolean mcp-sse))
   "sessionCapabilities" (json-object "list" (acp-marker list)
                                      "delete" (acp-marker delete)
                                      "additionalDirectories" (acp-marker additional-directories)
                                      "resume" (acp-marker resume)
                                      "close" (acp-marker close))
   "auth" (json-object "logout" (acp-marker logout))
   "_meta" meta))

(-> acp-client-capabilities
    (&key (:read-text-file boolean) (:write-text-file boolean) (:terminal boolean)
          (:terminal-auth boolean) (:elicitation-form boolean) (:elicitation-url boolean)
          (:boolean-config-options boolean) (:meta t))
    hash-table)
(defun acp-client-capabilities
    (&key read-text-file write-text-file terminal terminal-auth
       elicitation-form elicitation-url boolean-config-options meta)
  "Return the client capabilities object sent in the initialize request."
  (json-object
   "fs" (json-object "readTextFile" (acp-boolean read-text-file)
                     "writeTextFile" (acp-boolean write-text-file))
   "terminal" (acp-boolean terminal)
   "auth" (json-object "terminal" (acp-boolean terminal-auth))
   "elicitation" (and (or elicitation-form elicitation-url)
                      (json-object "form" (acp-marker elicitation-form)
                                   "url" (acp-marker elicitation-url)))
   "session" (and boolean-config-options
                  (json-object "configOptions" (json-object "boolean" (json-object))))
   "_meta" meta))

(-> acp-auth-method
    (string string &key (:description (or null string)) (:meta t))
    hash-table)
(defun acp-auth-method (id name &key description meta)
  "Return an authentication method the agent performs itself through authenticate."
  (json-object "id" id "name" name "description" description "_meta" meta))

(-> acp-terminal-auth-method
    (string string &key (:description (or null string)) (:arguments list)
            (:environment list) (:meta t))
    hash-table)
(defun acp-terminal-auth-method (id name &key description arguments environment meta)
  "Return a terminal authentication method run by the client.

ARGUMENTS extend the agent's launch arguments and ENVIRONMENT is a list of
(NAME . VALUE) strings overriding its environment."
  (json-object "id" id
               "name" name
               "type" "terminal"
               "description" description
               "args" (coerce arguments 'vector)
               "env" (let ((table (make-hash-table :test #'equal)))
                       (loop for (variable . value) in environment
                             do (setf (gethash variable table) value))
                       table)
               "_meta" meta))


;;;; -- Content Blocks --

(-> acp-annotations
    (&key (:audience list) (:priority (or null real)) (:last-modified (or null string)))
    hash-table)
(defun acp-annotations (&key audience priority last-modified)
  "Return content annotations; AUDIENCE lists content role keywords."
  (json-object "audience" (and audience (map 'vector #'content-role-string audience))
               "priority" priority
               "lastModified" last-modified))

(-> acp-text-content (string &key (:annotations t) (:meta t)) hash-table)
(defun acp-text-content (text &key annotations meta)
  "Return a text content block."
  (json-object "type" "text" "text" text "annotations" annotations "_meta" meta))

(-> acp-image-content
    (string string &key (:uri (or null string)) (:annotations t) (:meta t))
    hash-table)
(defun acp-image-content (data mime-type &key uri annotations meta)
  "Return an image content block carrying base64 DATA of MIME-TYPE."
  (json-object "type" "image" "data" data "mimeType" mime-type "uri" uri
               "annotations" annotations "_meta" meta))

(-> acp-audio-content (string string &key (:annotations t) (:meta t)) hash-table)
(defun acp-audio-content (data mime-type &key annotations meta)
  "Return an audio content block carrying base64 DATA of MIME-TYPE."
  (json-object "type" "audio" "data" data "mimeType" mime-type
               "annotations" annotations "_meta" meta))

(-> acp-resource-link
    (string string &key (:mime-type (or null string)) (:title (or null string))
            (:description (or null string)) (:size (or null integer))
            (:annotations t) (:meta t))
    hash-table)
(defun acp-resource-link (uri name &key mime-type title description size annotations meta)
  "Return a resource link content block."
  (json-object "type" "resource_link" "uri" uri "name" name "mimeType" mime-type
               "title" title "description" description "size" size
               "annotations" annotations "_meta" meta))

(-> acp-embedded-resource
    (string &key (:text (or null string)) (:blob (or null string))
            (:mime-type (or null string)) (:annotations t) (:meta t))
    hash-table)
(defun acp-embedded-resource (uri &key text blob mime-type annotations meta)
  "Return an embedded resource block with TEXT or base64 BLOB contents."
  (unless (or text blob)
    (error 'acp-protocol-error
           :message "An embedded resource needs text or blob contents."))
  (json-object "type" "resource"
               "resource" (json-object "uri" uri "text" text "blob" blob "mimeType" mime-type)
               "annotations" annotations
               "_meta" meta))

(-> acp-content-type (t) keyword)
(defun acp-content-type (block)
  "Return the content type keyword of received content BLOCK, or :UNKNOWN."
  (content-type-keyword (json-get block "type") :default ':unknown-type))

(-> acp-content-text (t) (or null string))
(defun acp-content-text (block)
  "Return the text of a text block or embedded text resource, else NIL."
  (let ((type (json-get block "type")))
    (cond
      ((equal type "text")
       (let ((text (json-get block "text")))
         (and (stringp text) text)))
      ((equal type "resource")
       (let ((text (json-get (json-get block "resource") "text")))
         (and (stringp text) text)))
      (t
       nil))))

(-> acp-validate-content-blocks (list) list)
(defun acp-validate-content-blocks (blocks)
  "Return BLOCKS after checking that each is a well-formed content block."
  (dolist (block blocks blocks)
    (unless (json-object-p block)
      (acp-invalid-params "Each prompt entry must be a content block object."))
    (let ((type (acp-field block "type" :type ':string :required-p t)))
      (cond
        ((string= type "text")
         (acp-field block "text" :type ':string :required-p t))
        ((or (string= type "image") (string= type "audio"))
         (acp-field block "data" :type ':string :required-p t)
         (acp-field block "mimeType" :type ':string :required-p t))
        ((string= type "resource_link")
         (acp-field block "uri" :type ':string :required-p t)
         (acp-field block "name" :type ':string :required-p t))
        ((string= type "resource")
         (let ((resource (acp-field block "resource" :type ':object :required-p t)))
           (acp-field resource "uri" :type ':string :required-p t)
           (unless (or (stringp (json-get resource "text"))
                       (stringp (json-get resource "blob")))
             (acp-invalid-params "An embedded resource needs text or blob contents."))))
        (t
         (acp-invalid-params "~A is not a content block type." type))))))


;;;; -- Tool Calls --

(-> acp-tool-call-content (t) hash-table)
(defun acp-tool-call-content (block)
  "Return tool call content wrapping content BLOCK."
  (json-object "type" "content" "content" block))

(-> acp-diff-content (string string &key (:old-text (or null string)) (:meta t)) hash-table)
(defun acp-diff-content (path new-text &key old-text meta)
  "Return tool call content showing the change of PATH to NEW-TEXT."
  (json-object "type" "diff" "path" path "newText" new-text "oldText" old-text "_meta" meta))

(-> acp-terminal-content (string &key (:meta t)) hash-table)
(defun acp-terminal-content (terminal-id &key meta)
  "Return tool call content embedding the live terminal TERMINAL-ID."
  (json-object "type" "terminal" "terminalId" terminal-id "_meta" meta))

(-> acp-tool-call-location (string &key (:line (or null integer)) (:meta t)) hash-table)
(defun acp-tool-call-location (path &key line meta)
  "Return a file location touched by a tool call."
  (json-object "path" path "line" line "_meta" meta))

(-> acp--tool-call-fields
    (string &key (:title (or null string)) (:name (or null string)) (:kind (or null keyword))
            (:status (or null keyword)) (:content t) (:locations t)
            (:raw-input t) (:raw-output t) (:meta t))
    hash-table)
(defun acp--tool-call-fields
    (tool-call-id &key title name kind status content locations raw-input raw-output meta)
  "Return the shared fields of a tool call and a tool call update."
  (json-object "toolCallId" tool-call-id
               "title" title
               "name" name
               "kind" (and kind (tool-kind-string kind))
               "status" (and status (tool-call-status-string status))
               "content" (and content (coerce content 'vector))
               "locations" (and locations (coerce locations 'vector))
               "rawInput" raw-input
               "rawOutput" raw-output
               "_meta" meta))

(-> acp-tool-call
    (string string &key (:name (or null string)) (:kind (or null keyword))
            (:status (or null keyword)) (:content list) (:locations list)
            (:raw-input t) (:raw-output t) (:meta t))
    hash-table)
(defun acp-tool-call
    (tool-call-id title &key name kind status content locations raw-input raw-output meta)
  "Return a new tool call report; KIND and STATUS are enumeration keywords."
  (acp--tool-call-fields tool-call-id
                         :title title :name name :kind kind :status status
                         :content content :locations locations
                         :raw-input raw-input :raw-output raw-output :meta meta))

(-> acp-tool-call-update
    (string &key (:title (or null string)) (:name (or null string)) (:kind (or null keyword))
            (:status (or null keyword)) (:content list) (:locations list)
            (:raw-input t) (:raw-output t) (:meta t))
    hash-table)
(defun acp-tool-call-update
    (tool-call-id &key title name kind status content locations raw-input raw-output meta)
  "Return an update to an existing tool call carrying only the given fields."
  (acp--tool-call-fields tool-call-id
                         :title title :name name :kind kind :status status
                         :content content :locations locations
                         :raw-input raw-input :raw-output raw-output :meta meta))


;;;; -- Plans, Commands, Modes, and Configuration --

(-> acp-plan-entry (string &key (:priority keyword) (:status keyword) (:meta t)) hash-table)
(defun acp-plan-entry (content &key (priority ':medium) (status ':pending) meta)
  "Return one plan entry."
  (json-object "content" content
               "priority" (plan-entry-priority-string priority)
               "status" (plan-entry-status-string status)
               "_meta" meta))

(-> acp-available-command
    (string string &key (:hint (or null string)) (:meta t))
    hash-table)
(defun acp-available-command (name description &key hint meta)
  "Return a slash command advertisement, with an input HINT when it takes text."
  (json-object "name" name "description" description
               "input" (and hint (json-object "hint" hint))
               "_meta" meta))

(-> acp-session-mode (string string &key (:description (or null string)) (:meta t)) hash-table)
(defun acp-session-mode (id name &key description meta)
  "Return a session mode descriptor."
  (json-object "id" id "name" name "description" description "_meta" meta))

(-> acp-session-mode-state (string list &key (:meta t)) hash-table)
(defun acp-session-mode-state (current-mode-id modes &key meta)
  "Return the mode state with CURRENT-MODE-ID selected among MODES."
  (json-object "currentModeId" current-mode-id
               "availableModes" (coerce modes 'vector)
               "_meta" meta))

(-> acp-config-select-option
    (string string &key (:description (or null string)) (:meta t))
    hash-table)
(defun acp-config-select-option (value name &key description meta)
  "Return one selectable value of a select configuration option."
  (json-object "value" value "name" name "description" description "_meta" meta))

(-> acp-config-select-group (string string list &key (:meta t)) hash-table)
(defun acp-config-select-group (group name options &key meta)
  "Return a named GROUP of select OPTIONS."
  (json-object "group" group "name" name "options" (coerce options 'vector) "_meta" meta))

(-> acp-config-option-select
    (string string string list &key (:description (or null string))
            (:category (or null keyword string)) (:meta t))
    hash-table)
(defun acp-config-option-select (id name current-value options &key description category meta)
  "Return a select configuration option whose CURRENT-VALUE is among OPTIONS.

CATEGORY is an enumeration keyword or a custom string beginning with an
underscore."
  (json-object "id" id "name" name "type" "select"
               "currentValue" current-value
               "options" (coerce options 'vector)
               "description" description
               "category" (acp--category-string category)
               "_meta" meta))

(-> acp-config-option-boolean
    (string string boolean &key (:description (or null string))
            (:category (or null keyword string)) (:meta t))
    hash-table)
(defun acp-config-option-boolean (id name current-value &key description category meta)
  "Return a boolean configuration option; the client must have advertised support."
  (json-object "id" id "name" name "type" "boolean"
               "currentValue" (acp-boolean current-value)
               "description" description
               "category" (acp--category-string category)
               "_meta" meta))

(-> acp--category-string ((or null keyword string)) (or null string))
(defun acp--category-string (category)
  "Return CATEGORY as its wire string, accepting custom underscore strings."
  (etypecase category
    (null nil)
    (keyword (config-option-category-string category))
    (string category)))

(-> acp-permission-option (string string keyword &key (:meta t)) hash-table)
(defun acp-permission-option (option-id name kind &key meta)
  "Return a permission option of enumeration KIND."
  (json-object "optionId" option-id "name" name
               "kind" (permission-option-kind-string kind)
               "_meta" meta))


;;;; -- MCP Server Configurations --

(-> acp-env-variable (string string &key (:meta t)) hash-table)
(defun acp-env-variable (name value &key meta)
  "Return an environment variable entry."
  (json-object "name" name "value" value "_meta" meta))

(-> acp-http-header (string string &key (:meta t)) hash-table)
(defun acp-http-header (name value &key meta)
  "Return an HTTP header entry."
  (json-object "name" name "value" value "_meta" meta))

(-> acp-mcp-server-stdio
    (string string &key (:arguments list) (:environment list) (:meta t))
    hash-table)
(defun acp-mcp-server-stdio (name command &key arguments environment meta)
  "Return a stdio MCP server configuration; ENVIRONMENT lists env variable entries."
  (json-object "name" name "command" command
               "args" (coerce arguments 'vector)
               "env" (coerce environment 'vector)
               "_meta" meta))

(-> acp-mcp-server-http (string string &key (:headers list) (:meta t)) hash-table)
(defun acp-mcp-server-http (name url &key headers meta)
  "Return an HTTP MCP server configuration."
  (json-object "type" "http" "name" name "url" url
               "headers" (coerce headers 'vector) "_meta" meta))

(-> acp-mcp-server-sse (string string &key (:headers list) (:meta t)) hash-table)
(defun acp-mcp-server-sse (name url &key headers meta)
  "Return an SSE MCP server configuration, a transport MCP has deprecated."
  (json-object "type" "sse" "name" name "url" url
               "headers" (coerce headers 'vector) "_meta" meta))

(-> acp-mcp-server-transport (t) keyword)
(defun acp-mcp-server-transport (server)
  "Return the transport keyword of received MCP SERVER configuration."
  (let ((type (json-get server "type")))
    (cond
      ((null type)
       ':stdio)
      ((equal type "stdio")
       ':stdio)
      (t
       (mcp-transport-keyword type :default ':unknown-transport)))))

(-> acp-validate-mcp-servers (list) list)
(defun acp-validate-mcp-servers (servers)
  "Return SERVERS after checking each MCP server configuration."
  (dolist (server servers servers)
    (unless (json-object-p server)
      (acp-invalid-params "Each MCP server must be an object."))
    (acp-field server "name" :type ':string :required-p t)
    (ecase (acp-mcp-server-transport server)
      (:stdio
       (acp-field server "command" :type ':string :required-p t)
       (acp-field server "args" :type ':array :required-p t))
      ((:http :sse)
       (acp-field server "url" :type ':string :required-p t))
      (:unknown-transport
       (acp-invalid-params "~A is not an MCP transport." (json-get server "type"))))))

(-> acp-name-value-pairs (t) list)
(defun acp-name-value-pairs (entries)
  "Return received env variable or header ENTRIES as (NAME . VALUE) pairs."
  (loop for entry in (json-sequence->list entries)
        when (and (json-object-p entry)
                  (stringp (json-get entry "name"))
                  (stringp (json-get entry "value")))
          collect (cons (json-get entry "name") (json-get entry "value"))))


;;;; -- Session Updates --

(-> acp--content-chunk (keyword t &key (:message-id (or null string)) (:meta t)) hash-table)
(defun acp--content-chunk (kind content &key message-id meta)
  "Return a message chunk update of KIND carrying content block CONTENT."
  (json-object "sessionUpdate" (session-update-kind-string kind)
               "content" content
               "messageId" message-id
               "_meta" meta))

(-> acp-update-user-message (t &key (:message-id (or null string)) (:meta t)) hash-table)
(defun acp-update-user-message (content &key message-id meta)
  "Return a user message chunk update, used when replaying history."
  (acp--content-chunk ':user-message-chunk content :message-id message-id :meta meta))

(-> acp-update-agent-message (t &key (:message-id (or null string)) (:meta t)) hash-table)
(defun acp-update-agent-message (content &key message-id meta)
  "Return an agent message chunk update."
  (acp--content-chunk ':agent-message-chunk content :message-id message-id :meta meta))

(-> acp-update-agent-thought (t &key (:message-id (or null string)) (:meta t)) hash-table)
(defun acp-update-agent-thought (content &key message-id meta)
  "Return an agent thought chunk update."
  (acp--content-chunk ':agent-thought-chunk content :message-id message-id :meta meta))

(-> acp--tagged-update (keyword hash-table) hash-table)
(defun acp--tagged-update (kind object)
  "Return OBJECT with the sessionUpdate discriminator for KIND added."
  (setf (gethash "sessionUpdate" object) (session-update-kind-string kind))
  object)

(-> acp-update-tool-call (hash-table) hash-table)
(defun acp-update-tool-call (tool-call)
  "Return a session update announcing TOOL-CALL, built with ACP-TOOL-CALL."
  (acp--tagged-update ':tool-call tool-call))

(-> acp-update-tool-call-progress (hash-table) hash-table)
(defun acp-update-tool-call-progress (tool-call-update)
  "Return a session update carrying TOOL-CALL-UPDATE, built with ACP-TOOL-CALL-UPDATE."
  (acp--tagged-update ':tool-call-update tool-call-update))

(-> acp-update-plan (list &key (:meta t)) hash-table)
(defun acp-update-plan (entries &key meta)
  "Return a plan update replacing the whole plan with ENTRIES."
  (json-object "sessionUpdate" (session-update-kind-string ':plan)
               "entries" (coerce entries 'vector)
               "_meta" meta))

(-> acp-update-available-commands (list &key (:meta t)) hash-table)
(defun acp-update-available-commands (commands &key meta)
  "Return an update advertising the slash COMMANDS."
  (json-object "sessionUpdate" (session-update-kind-string ':available-commands-update)
               "availableCommands" (coerce commands 'vector)
               "_meta" meta))

(-> acp-update-current-mode (string &key (:meta t)) hash-table)
(defun acp-update-current-mode (mode-id &key meta)
  "Return an update announcing the agent switched to MODE-ID."
  (json-object "sessionUpdate" (session-update-kind-string ':current-mode-update)
               "currentModeId" mode-id
               "_meta" meta))

(-> acp-update-config-options (list &key (:meta t)) hash-table)
(defun acp-update-config-options (options &key meta)
  "Return an update carrying the complete configuration OPTIONS state."
  (json-object "sessionUpdate" (session-update-kind-string ':config-option-update)
               "configOptions" (coerce options 'vector)
               "_meta" meta))

(-> acp-update-session-info
    (&key (:title t) (:updated-at t) (:meta t))
    hash-table)
(defun acp-update-session-info (&key title updated-at meta)
  "Return a session metadata update; pass :NULL to clear a field."
  (json-object "sessionUpdate" (session-update-kind-string ':session-info-update)
               "title" title
               "updatedAt" updated-at
               "_meta" meta))

(-> acp-update-usage
    (integer integer &key (:cost-amount (or null real)) (:cost-currency (or null string)) (:meta t))
    hash-table)
(defun acp-update-usage (used size &key cost-amount cost-currency meta)
  "Return a context usage update of USED tokens out of SIZE, with optional cost."
  (json-object "sessionUpdate" (session-update-kind-string ':usage-update)
               "used" used
               "size" size
               "cost" (and cost-amount cost-currency
                           (json-object "amount" cost-amount "currency" cost-currency))
               "_meta" meta))

(-> acp-update-kind (t) keyword)
(defun acp-update-kind (update)
  "Return the kind keyword of received session UPDATE, or :UNKNOWN-UPDATE."
  (session-update-kind-keyword (json-get update "sessionUpdate")
                               :default ':unknown-update))

(-> acp-session-info
    (string string &key (:title (or null string)) (:updated-at (or null string))
            (:additional-directories list) (:meta t))
    hash-table)
(defun acp-session-info (session-id cwd &key title updated-at additional-directories meta)
  "Return one entry of a session/list response."
  (json-object "sessionId" session-id "cwd" cwd "title" title "updatedAt" updated-at
               "additionalDirectories" (and additional-directories
                                            (coerce additional-directories 'vector))
               "_meta" meta))
