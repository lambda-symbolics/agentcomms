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


#+sbcl
(define-test stdio-native-standard-handles
  (uiop:with-temporary-file (:stream input :direction ':io :element-type 'character
                                    :external-format ':utf-8)
    (uiop:with-temporary-file (:stream output :direction ':io :element-type 'character
                                       :external-format ':utf-8)
      (write-line "native input ∑" input)
      (finish-output input)
      (file-position input 0)
      (let ((sb-sys:*stdin* input)
            (sb-sys:*stdout* output)
            (*standard-output* (make-string-output-stream)))
        (let ((channel (acp-standard-io-channel)))
          (test-equal "native input ∑" (channel-read-message channel))
          (channel-write-message channel "native output ∑")
          (file-position output 0)
          (test-equal "native output ∑" (read-line output)))))))

(define-test stream-channel-concurrent-close-waits-for-cleanup
  (let* ((lock (make-lock "channel close test"))
         (condition (make-condition-variable))
         (release-condition (make-condition-variable))
         (entered-p nil)
         (released-p nil)
         (completed-p nil)
         (observed-p nil)
         (second-started-p nil)
         (calls 0)
         (channel nil)
         (first-thread nil)
         (second-thread nil))
    (setf channel
          (make-acp-stream-channel
           :input (make-string-input-stream "")
           :output (make-string-output-stream)
           :close-function
           (lambda ()
             (incf calls)
             (channel-close channel)
             (with-lock-held (lock)
               (setf entered-p t)
               (condition-notify condition)
               (loop until released-p do (condition-wait release-condition lock))
               (setf completed-p t)))))
    (unwind-protect
         (progn
           (setf first-thread (make-thread (lambda () (channel-close channel))))
           (with-lock-held (lock)
             (loop until entered-p do (condition-wait condition lock)))
           (setf second-thread
                 (make-thread
                  (lambda ()
                    (with-lock-held (lock)
                      (setf second-started-p t)
                      (condition-notify condition))
                    (channel-close channel)
                    (with-lock-held (lock)
                      (setf observed-p completed-p)))))
           (with-lock-held (lock)
             (loop until second-started-p do (condition-wait condition lock)))
           (sleep 0.02))
      (with-lock-held (lock)
        (setf released-p t)
        (condition-notify release-condition))
      (when first-thread (join-thread first-thread))
      (when second-thread (join-thread second-thread)))
    (test-assert observed-p "every close caller waits for cleanup completion")
    (test-equal 1 calls)
    (test-assert (not (channel-open-p channel)))))
