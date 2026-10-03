(in-package #:agentcomms)

;;;; -- Standard I/O Channels --

(defparameter *acp-process-exit-seconds* 3
  "Seconds to wait for an agent process to exit after its input closes.")

(defparameter *acp-process-kill-seconds* 2
  "Seconds to wait after a termination signal before killing an agent process.")

(defparameter *acp-process-stderr-limit* 65536
  "Characters of agent standard error retained for diagnostics.")

(-> acp-standard-io-channel (&key (:maximum-message-characters integer)) acp-stream-channel)
(defun acp-standard-io-channel
    (&key (maximum-message-characters *acp-maximum-message-characters*))
  "Return a channel over this process's standard input and output as UTF-8.

An agent launched by an editor serves its connection over this channel.
Nothing else may write to standard output while it is in use."
  (make-acp-stream-channel
   :input #+sbcl (sb-sys:make-fd-stream (sb-sys:fd-stream-fd sb-sys:*stdin*) :input t
                                          :external-format ':utf-8
                                          :buffering ':full
                                          :element-type 'character)
          #+ccl (ccl::make-fd-stream 0 :direction ':input :encoding ':utf-8 :sharing ':lock)
          #-(or sbcl ccl) *standard-input*
   :output #+sbcl (sb-sys:make-fd-stream (sb-sys:fd-stream-fd sb-sys:*stdout*) :output t
                                           :external-format ':utf-8
                                           :buffering ':full
                                           :element-type 'character)
           #+ccl (ccl::make-fd-stream 1 :direction ':output :encoding ':utf-8 :sharing ':lock)
           #-(or sbcl ccl) *standard-output*
   :maximum-message-characters maximum-message-characters))

(-> acp-standard-error-log (string) null)
(defun acp-standard-error-log (text)
  "Write diagnostic TEXT as one line on standard error; a connection log function."
  (format *error-output* "~A~%" text)
  (finish-output *error-output*)
  nil)

(-> acp-serve-standard-io (acp-agent &key (:log-function (or null function))) (or null string))
(defun acp-serve-standard-io (agent &key (log-function #'acp-standard-error-log))
  "Serve AGENT over standard I/O until the client disconnects; return the close reason."
  (acp-agent-serve agent (acp-standard-io-channel) :log-function log-function))


;;;; -- Agent Subprocesses --

(defclass acp-process-channel (acp-stream-channel)
  ((process
    :initarg :process
    :reader acp-process-channel-process
    :type t
    :documentation "The UIOP process information of the agent subprocess.")
   (command
    :initarg :command
    :reader acp-process-channel-command
    :type list
    :documentation "The command line that started the agent.")
   (stderr-thread
    :initform nil
    :accessor acp-process-channel-stderr-thread
    :type t
    :documentation "The thread draining the agent's standard error.")
   (stderr-text
    :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
    :reader acp-process-channel--stderr-buffer
    :type string
    :documentation "The retained tail of the agent's standard error.")
   (stderr-limit
    :initarg :stderr-limit
    :initform *acp-process-stderr-limit*
    :reader acp-process-channel-stderr-limit
    :type integer
    :documentation "The most standard error characters retained.")
   (stderr-lock
    :initform (make-lock "agentcomms process stderr")
    :reader acp-process-channel-stderr-lock
    :type t
    :documentation "The lock guarding the standard error buffer.")
   (exit-code
    :initform nil
    :accessor acp-process-channel-exit-code
    :type (or null integer)
    :documentation "The exit code once the process has been reaped."))
  (:documentation
   "A channel to an agent running as a subprocess of this client.

Standard error is drained to a bounded buffer for diagnostics. Closing the
channel ends the agent's input, waits for it to exit, and terminates it
when it does not."))

(-> acp-process-channel-stderr-text (acp-process-channel) string)
(defun acp-process-channel-stderr-text (channel)
  "Return the retained standard error output of the agent."
  (with-lock-held ((acp-process-channel-stderr-lock channel))
    (copy-seq (acp-process-channel--stderr-buffer channel))))

(-> acp-process-channel-alive-p (acp-process-channel) boolean)
(defun acp-process-channel-alive-p (channel)
  "Return whether the agent process is still running."
  (and (uiop:process-alive-p (acp-process-channel-process channel)) t))

(-> process-channel--append-stderr (acp-process-channel string) null)
(defun process-channel--append-stderr (channel text)
  "Append TEXT to the standard error buffer, dropping the oldest excess."
  (with-lock-held ((acp-process-channel-stderr-lock channel))
    (let ((buffer (acp-process-channel--stderr-buffer channel))
          (limit (acp-process-channel-stderr-limit channel)))
      (loop for character across text
            do (vector-push-extend character buffer))
      (when (> (length buffer) limit)
        (let ((excess (- (length buffer) limit)))
          (replace buffer buffer :start2 excess)
          (setf (fill-pointer buffer) limit)))))
  nil)

(-> process-channel--drain-stderr (acp-process-channel stream) null)
(defun process-channel--drain-stderr (channel stream)
  "Copy STREAM into the standard error buffer until it ends."
  (handler-case
      (loop for line = (read-line stream nil nil)
            while line
            do (process-channel--append-stderr channel (format nil "~A~%" line)))
    (error ()
      nil))
  nil)

(-> process-channel--wait-exit (acp-process-channel real) boolean)
(defun process-channel--wait-exit (channel seconds)
  "Poll for up to SECONDS seconds; return whether the process has exited."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (and (acp-process-channel-alive-p channel)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.02))
    (not (acp-process-channel-alive-p channel))))

(-> process-channel--shut-down (acp-process-channel) null)
(defun process-channel--shut-down (channel)
  "Wait for the agent to exit after its input closed, escalating to termination."
  (let ((process (acp-process-channel-process channel)))
    (unless (process-channel--wait-exit channel *acp-process-exit-seconds*)
      (ignore-errors (uiop:terminate-process process))
      (unless (process-channel--wait-exit channel *acp-process-kill-seconds*)
        (ignore-errors (uiop:terminate-process process :urgent t))
        (process-channel--wait-exit channel *acp-process-kill-seconds*)))
    (setf (acp-process-channel-exit-code channel)
          (handler-case
              (uiop:wait-process process)
            (error ()
              nil)))
    (let ((thread (acp-process-channel-stderr-thread channel)))
      (when (and thread (thread-alive-p thread))
        (let ((deadline (+ (get-internal-real-time) internal-time-units-per-second)))
          (loop while (and (thread-alive-p thread) (< (get-internal-real-time) deadline))
                do (sleep 0.01)))
        (when (thread-alive-p thread)
          (ignore-errors (destroy-thread thread)))))
    (ignore-errors (uiop:close-streams process)))
  nil)

(-> acp-launch-agent
    (string &key (:arguments list) (:directory t) (:environment list)
            (:stderr-limit integer) (:maximum-message-characters integer))
    acp-process-channel)
(defun acp-launch-agent (command &key arguments directory environment
                                      (stderr-limit *acp-process-stderr-limit*)
                                      (maximum-message-characters *acp-maximum-message-characters*))
  "Start agent COMMAND with ARGUMENTS as a subprocess and return its channel.

DIRECTORY is the working directory and ENVIRONMENT, when given, the
complete list of \"NAME=VALUE\" strings for the process. Connect a client
with ACP-CLIENT-CONNECT and close the connection to stop the agent."
  (let ((process (handler-case
                     (apply #'uiop:launch-program
                            (cons command arguments)
                            :input ':stream
                            :output ':stream
                            :error-output ':stream
                            :external-format ':utf-8
                            :ignore-error-status t
                            (append (when directory
                                      (list :directory directory))
                                    (when environment
                                      (list :environment environment))))
                   (error (cause)
                     (error 'acp-error
                            :message (format nil "Could not start the agent ~S: ~A"
                                             command cause))))))
    (let ((channel (make-instance 'acp-process-channel
                                  :process process
                                  :command (cons command arguments)
                                  :input (uiop:process-info-output process)
                                  :output (uiop:process-info-input process)
                                  :stderr-limit stderr-limit
                                  :maximum-message-characters maximum-message-characters)))
      (setf (slot-value channel 'close-function)
            (lambda ()
              (process-channel--shut-down channel)))
      (setf (acp-process-channel-stderr-thread channel)
            (make-thread (lambda ()
                           (process-channel--drain-stderr
                            channel (uiop:process-info-error-output process)))
                         :name "agentcomms agent stderr"))
      channel)))
