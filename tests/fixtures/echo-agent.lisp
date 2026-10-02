;;;; A minimal ACP agent in plain JSON, used to test the subprocess channel.
;;;; Run with: sbcl --script echo-agent.lisp /path/to/.qlot/setup.lisp

(require '#:asdf)

(let ((setup (find-if (lambda (argument)
                        (and (> (length argument) 10)
                             (string= "setup.lisp" argument :start2 (- (length argument) 10))))
                      sb-ext:*posix-argv*)))
  (unless setup
    (error "The echo agent needs the path of a Qlot setup.lisp as an argument."))
  (load setup))

(asdf:load-system '#:yason)

(defvar *input* (sb-sys:make-fd-stream 0 :input t :external-format :utf-8 :buffering :full)
  "Standard input as UTF-8.")

(defvar *output* (sb-sys:make-fd-stream 1 :output t :external-format :utf-8 :buffering :full)
  "Standard output as UTF-8.")

(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when absent."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (if present-p value default)))

(defun json-object (&rest pairs)
  "Return an equal hash table populated by alternating PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun write-message (message)
  "Write MESSAGE as one JSON line."
  (yason:encode message *output*)
  (terpri *output*)
  (finish-output *output*))

(defun respond (request result)
  "Answer REQUEST with RESULT."
  (write-message (json-object "jsonrpc" "2.0" "id" (json-get request "id") "result" result)))

(defun respond-error (request code message)
  "Answer REQUEST with an error."
  (write-message (json-object "jsonrpc" "2.0" "id" (json-get request "id")
                              "error" (json-object "code" code "message" message))))

(defun prompt-text (params)
  "Return the concatenated text of the prompt blocks in PARAMS."
  (with-output-to-string (stream)
    (loop for block across (json-get params "prompt" (vector))
          when (equal (json-get block "type") "text")
            do (write-string (json-get block "text" "") stream))))

(defun handle (message)
  "Serve one decoded MESSAGE."
  (let ((method (json-get message "method"))
        (params (json-get message "params" (json-object))))
    (cond
      ((null method)
       nil)
      ((not (nth-value 1 (gethash "id" message)))
       nil)
      ((equal method "initialize")
       (respond message (json-object "protocolVersion" 1
                                     "agentCapabilities" (json-object)
                                     "agentInfo" (json-object "name" "echo-agent" "version" "1")
                                     "authMethods" (vector))))
      ((equal method "session/new")
       (respond message (json-object "sessionId" "echo-1")))
      ((equal method "session/prompt")
       (write-message (json-object "jsonrpc" "2.0"
                                   "method" "session/update"
                                   "params" (json-object
                                             "sessionId" (json-get params "sessionId")
                                             "update" (json-object
                                                       "sessionUpdate" "agent_message_chunk"
                                                       "content" (json-object "type" "text"
                                                                              "text" (prompt-text params))))))
       (respond message (json-object "stopReason" "end_turn")))
      (t
       (respond-error message -32601 (format nil "Method not found: ~A" method))))))

(format *error-output* "echo-agent ready~%")
(finish-output *error-output*)

(loop for line = (read-line *input* nil nil)
      while line
      unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
        do (handle (yason:parse line :json-arrays-as-vectors t)))
