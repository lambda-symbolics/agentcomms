(in-package #:agentcomms)

;;;; -- Standard I/O Tests --

(-> test-fixture-pathname (string) pathname)
(defun test-fixture-pathname (name)
  "Return the pathname of test fixture NAME."
  (merge-pathnames (format nil "tests/fixtures/~A" name)
                   (asdf:system-source-directory '#:agentcomms)))

(-> test-qlot-setup-pathname () pathname)
(defun test-qlot-setup-pathname ()
  "Return the Qlot setup file the fixture loads its dependencies from."
  (merge-pathnames ".qlot/setup.lisp" (asdf:system-source-directory '#:agentcomms)))

(-> test-launch-echo-agent () acp-process-channel)
(defun test-launch-echo-agent ()
  "Start the echo agent fixture in a fresh SBCL."
  (acp-launch-agent (namestring sb-ext:*runtime-pathname*)
                    :arguments (list "--noinform"
                                     "--disable-debugger"
                                     "--script"
                                     (namestring (test-fixture-pathname "echo-agent.lisp"))
                                     (namestring (test-qlot-setup-pathname)))))

(define-test stdio-subprocess-agent-round-trip
  (let ((channel (test-launch-echo-agent))
        (client (make-instance 'test-client)))
    (acp-client-connect client channel :name "subprocess client" :request-timeout 60)
    (unwind-protect
         (progn
           (client-initialize client)
           (test-equal "echo-agent" (json-get (acp-client-agent-info client) "name"))
           (test-equal "echo-1" (client-new-session client "/w"))
           (test-equal ':end-turn
                       (client-prompt client "echo-1" (list (acp-text-content "ping ∑ ünïcödé"))))
           (test-equal '("ping ∑ ünïcödé") (test-client-texts client))
           (test-equal -32601
                       (acp-method-error-code
                        (test-signals acp-remote-error
                          (client-set-mode client "echo-1" "code"))))
           (test-assert (test-wait-until
                         (lambda ()
                           (search "echo-agent ready" (acp-process-channel-stderr-text channel))))
                        "standard error is captured")
           (test-assert (acp-process-channel-alive-p channel)))
      (connection-close (acp-client-connection client)))
    (test-assert (not (acp-process-channel-alive-p channel)) "the agent exits when its input closes")
    (test-equal 0 (acp-process-channel-exit-code channel))
    (test-assert (not (channel-open-p channel)))))

(define-test stdio-launch-failures
  (test-signals acp-error (acp-launch-agent "/nonexistent/agentcomms-agent"))
  (let ((channel (acp-standard-io-channel)))
    (test-assert (channel-open-p channel))
    (test-equal *acp-maximum-message-characters* (channel-maximum-message-characters channel))))
