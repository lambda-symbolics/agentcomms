(defpackage #:agentcomms
  (:nicknames #:acp)
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:current-thread
                #:destroy-thread
                #:join-thread
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:thread-alive-p
                #:with-lock-held)
  (:import-from #:serapeum
                #:->)
  (:export
   ;; Conditions
   #:acp-error
   #:acp-error-message
   #:acp-protocol-error
   #:acp-protocol-error-payload
   #:acp-message-too-large
   #:acp-message-too-large-limit
   #:acp-method-error
   #:acp-method-error-code
   #:acp-method-error-data
   #:acp-remote-error
   #:acp-remote-error-method
   #:acp-request-cancelled
   #:acp-timeout
   #:acp-timeout-seconds
   #:acp-connection-closed
   #:acp-capability-error
   #:acp-capability-error-capability
   #:acp-unsupported-version
   #:acp-unsupported-version-version
   #:acp-state-error
   ;; JSON values
   #:*json-maximum-depth*
   #:*json-maximum-nodes*
   #:*acp-maximum-message-characters*
   #:json-object
   #:json-get
   #:json-true-value
   #:json-false-value
   #:json-null-value
   #:json-true-p
   #:json-boolean-p
   #:json-null-p
   #:json-object-p
   #:json-sequence->list
   #:json-encode
   #:json-decode))
