(in-package #:agentcomms)

;;;; -- Conditions --

(define-condition acp-error (error)
  ((message
    :initarg :message
    :reader acp-error-message
    :type string
    :documentation "The human-readable description of the failure."))
  (:report (lambda (condition stream)
             (write-string (acp-error-message condition) stream)))
  (:documentation "The root of every agentcomms condition."))

(define-condition acp-protocol-error (acp-error)
  ((payload
    :initarg :payload
    :initform nil
    :reader acp-protocol-error-payload
    :type t
    :documentation "The offending value, bounded for diagnostics."))
  (:documentation "The peer sent a message that violates JSON-RPC or ACP."))

(define-condition acp-message-too-large (acp-protocol-error)
  ((limit
    :initarg :limit
    :reader acp-message-too-large-limit
    :type integer
    :documentation "The character limit that the message exceeded."))
  (:documentation "A message exceeded the configured size bound."))

(define-condition acp-method-error (acp-error)
  ((code
    :initarg :code
    :reader acp-method-error-code
    :type integer
    :documentation "The JSON-RPC error code.")
   (data
    :initarg :data
    :initform nil
    :reader acp-method-error-data
    :type t
    :documentation "Optional structured error data."))
  (:report (lambda (condition stream)
             (format stream "~A (JSON-RPC error ~D)"
                     (acp-error-message condition)
                     (acp-method-error-code condition))))
  (:documentation
   "A JSON-RPC error with a code.

Handlers signal it to answer a request with that exact error. The same
condition reports an error response received from the peer, in which
case it is an ACP-REMOTE-ERROR."))

(define-condition acp-remote-error (acp-method-error)
  ((method
    :initarg :method
    :reader acp-remote-error-method
    :type string
    :documentation "The method whose request the peer rejected."))
  (:report (lambda (condition stream)
             (format stream "~A answered ~A with JSON-RPC error ~D."
                     (acp-remote-error-method condition)
                     (acp-error-message condition)
                     (acp-method-error-code condition))))
  (:documentation "The peer answered a request with a JSON-RPC error."))

(define-condition acp-request-cancelled (acp-method-error)
  ()
  (:default-initargs :code -32800 :message "The request was cancelled.")
  (:documentation
   "The request in progress was cancelled.

A handler signals it to answer with the Request Cancelled error, and
ACP-CHECK-CANCELLED signals it once the peer cancels the request."))

(define-condition acp-timeout (acp-error)
  ((seconds
    :initarg :seconds
    :reader acp-timeout-seconds
    :type real
    :documentation "The deadline that elapsed."))
  (:documentation "A request received no response within its deadline."))

(define-condition acp-connection-closed (acp-error)
  ()
  (:default-initargs :message "The ACP connection is closed.")
  (:documentation "The connection closed before the operation completed."))

(define-condition acp-capability-error (acp-error)
  ((capability
    :initarg :capability
    :reader acp-capability-error-capability
    :type string
    :documentation "The dotted capability path the peer did not advertise."))
  (:documentation "The peer never advertised the capability the call needs."))

(define-condition acp-unsupported-version (acp-error)
  ((version
    :initarg :version
    :reader acp-unsupported-version-version
    :type t
    :documentation "The protocol version the peer selected."))
  (:documentation "Version negotiation ended on a version this side cannot speak."))

(define-condition acp-state-error (acp-error)
  ()
  (:documentation "An operation ran outside the lifecycle state that allows it."))
