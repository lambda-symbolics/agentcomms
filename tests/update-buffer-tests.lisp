(in-package #:agentcomms)

;;;; -- Buffered Update Contracts --

(define-test update-buffer-order-and-threshold
  (let* ((sent nil)
         (buffer (make-acp-update-buffer (lambda (update) (push update sent))
                                         :thought-batch-size 4))
         (first (acp-update-agent-thought (acp-text-content "ab"))))
    (update-buffer-send buffer first)
    (test-assert (null sent))
    (update-buffer-send buffer (acp-update-agent-thought (acp-text-content "cd")))
    (test-equal "abcd" (acp-content-text (json-get (first sent) "content")))
    (test-equal "ab" (acp-content-text (json-get first "content")))
    (update-buffer-send buffer (acp-update-agent-thought (acp-text-content "ef")))
    (update-buffer-send buffer (acp-update-agent-message (acp-text-content "answer")))
    (test-equal '("abcd" "ef" "answer")
                (mapcar (lambda (update) (acp-content-text (json-get update "content")))
                        (reverse sent)))
    (update-buffer-flush buffer)
    (test-equal 3 (length sent))))

(define-test update-buffer-preserves-content-and-metadata
  (let* ((sent nil)
         (buffer (make-acp-update-buffer (lambda (update) (push update sent))))
         (one (acp-update-agent-thought (acp-text-content "a" :meta (json-object "tag" "A"))))
         (two (acp-update-agent-thought (acp-text-content "b" :meta (json-object "tag" "B"))))
         (image (acp-update-agent-thought (json-object "type" "image" "data" "AA=="
                                                      "mimeType" "image/png"))))
    (setf (gethash "extension" one) (json-object "enabled" (json-false)))
    (update-buffer-send buffer one)
    (setf (gethash "tag" (gethash "_meta" (gethash "content" one))) "changed")
    (update-buffer-send buffer two)
    (update-buffer-send buffer image)
    (test-equal 3 (length sent))
    (test-assert (eq image (first sent)))
    (let ((copy (third sent)))
      (test-equal "A" (json-get (json-get (json-get copy "content") "_meta") "tag"))
      (test-assert (json-false-p (gethash "enabled" (gethash "extension" copy)))))))

(define-test update-buffer-large-fragments-and-isolation
  (let* ((left nil) (right nil)
         (first (make-acp-update-buffer (lambda (update) (push update left)) :thought-batch-size 4))
         (second (make-acp-update-buffer (lambda (update) (push update right)) :thought-batch-size 4)))
    (update-buffer-send first (acp-update-agent-thought (acp-text-content "a")))
    (update-buffer-send second (acp-update-agent-thought (acp-text-content "b")))
    (update-buffer-send first (acp-update-agent-thought (acp-text-content "large")))
    (test-equal '("a" "large")
                (mapcar (lambda (update) (acp-content-text (json-get update "content")))
                        (reverse left)))
    (test-assert (null right))
    (update-buffer-flush second)
    (test-equal "b" (acp-content-text (json-get (first right) "content")))
    (update-buffer-send first (acp-update-agent-thought (acp-text-content "next")))
    (test-equal "next" (acp-content-text (json-get (first left) "content")))))

(define-test update-buffer-retries-failed-delivery
  (let* ((fail-p t) (sent nil)
         (buffer (make-acp-update-buffer
                  (lambda (update)
                    (when fail-p (error 'acp-state-error :message "Test sender failure."))
                    (push update sent)) :thought-batch-size 4)))
    (update-buffer-send buffer (acp-update-agent-thought (acp-text-content "ab")))
    (test-signals acp-state-error
      (update-buffer-send buffer (acp-update-agent-thought (acp-text-content "cd"))))
    (test-signals acp-state-error
      (update-buffer-send buffer (acp-update-agent-thought (acp-text-content "more"))))
    (setf fail-p nil)
    (update-buffer-flush buffer)
    (test-equal 1 (length sent))
    (test-equal "abcd" (acp-content-text (json-get (first sent) "content")))))

(define-test update-buffer-concurrent-fragments
  (let* ((sent nil)
         (buffer (make-acp-update-buffer (lambda (update) (push update sent)) :thought-batch-size 16))
         (threads (loop for character in '(#\a #\b)
                        collect (let ((text (string character)))
                                  (make-thread
                                   (lambda ()
                                     (dotimes (index 100)
                                       (update-buffer-send buffer
                                                           (acp-update-agent-thought
                                                            (acp-text-content text))))))))))
    (mapc #'join-thread threads)
    (update-buffer-flush buffer)
    (let ((text (apply #'concatenate 'string
                       (mapcar (lambda (update) (acp-content-text (json-get update "content"))) sent))))
      (test-equal 100 (count #\a text))
      (test-equal 100 (count #\b text)))))


(define-test update-buffer-coalesces-matching-metadata
  (let* ((sent nil)
         (buffer (make-acp-update-buffer (lambda (update) (push update sent))))
         (one (acp-update-agent-thought (acp-text-content "a" :meta (json-object "tag" "A"))))
         (two nil))
    (setf (gethash "messageId" one) "thought-1"
          (gethash "_meta" one) (json-object "turn" 1)
          (gethash "extension" one) (json-object "enabled" (json-false))
          two (json-decode (json-encode one))
          (gethash "text" (gethash "content" two)) "b")
    (update-buffer-send buffer one)
    (update-buffer-send buffer two)
    (update-buffer-flush buffer)
    (test-equal 1 (length sent))
    (let ((update (first sent)))
      (test-equal "ab" (acp-content-text (json-get update "content")))
      (test-equal "thought-1" (json-get update "messageId"))
      (test-equal 1 (json-get (json-get update "_meta") "turn"))
      (test-equal "A" (json-get (json-get (json-get update "content") "_meta") "tag"))
      (test-assert (json-false-p (gethash "enabled" (gethash "extension" update)))))
    (test-equal "a" (acp-content-text (json-get one "content")))
    (test-equal "b" (acp-content-text (json-get two "content")))))

(define-test update-buffer-splits-at-the-complete-wire-bound
  (let* ((text (make-string 40 :initial-element #\"))
         (update (acp-update-agent-thought (acp-text-content text)))
         (meta (json-object "tag" "notification"))
         (message (connection--notification-message
                   (acp-method-name ':session-update) (agent--update-parameters "session" update meta)))
         (limit (length (json-encode message)))
         (agent (make-instance 'acp-agent)))
    (multiple-value-bind (server peer) (make-acp-channel-pair :maximum-message-characters limit)
      (unwind-protect
           (progn
             (acp-agent-connect agent server)
             (let ((buffer (make-agent-update-buffer agent "session" :meta meta)))
               (update-buffer-send buffer update)
               (update-buffer-send buffer update)
               (update-buffer-flush buffer))
             (dotimes (index 2)
               (let* ((line (channel-read-message peer))
                      (params (json-get (json-decode line) "params")))
                 (test-assert (<= (length line) limit))
                 (test-equal text (acp-content-text (json-get (json-get params "update") "content")))
                 (test-equal "notification" (json-get (json-get params "_meta") "tag")))))
        (connection-close (acp-agent-connection agent))
        (channel-close peer)))))
