(in-package #:agentcomms)

;;;; -- Schema Tests --

(define-test schema-enumerations-round-trip
  (dolist (case '((:end-turn "end_turn" stop-reason-string stop-reason-keyword)
                  (:switch-mode "switch_mode" tool-kind-string tool-kind-keyword)
                  (:in-progress "in_progress" tool-call-status-string tool-call-status-keyword)
                  (:allow-always "allow_always" permission-option-kind-string
                   permission-option-kind-keyword)
                  (:tool-call-update "tool_call_update" session-update-kind-string
                   session-update-kind-keyword)
                  (:resource-link "resource_link" content-type-string content-type-keyword)
                  (:model-config "model_config" config-option-category-string
                   config-option-category-keyword)))
    (destructuring-bind (keyword string to-string to-keyword) case
      (test-equal string (funcall to-string keyword))
      (test-equal keyword (funcall to-keyword string))))
  (test-signals acp-protocol-error (stop-reason-string ':sideways))
  (test-signals acp-protocol-error (tool-kind-keyword "teleport"))
  (test-equal ':other (tool-kind-keyword "teleport" :default ':other))
  (test-equal ':other (tool-kind-keyword 42 :default ':other))
  (test-equal "session/request_permission" (acp-method-name ':session-request-permission))
  (test-equal ':session-prompt (acp-method-keyword "session/prompt"))
  (test-equal nil (acp-method-keyword "_zed.dev/custom"))
  (test-assert (acp-extension-method-p "_zed.dev/custom"))
  (test-assert (not (acp-extension-method-p "session/new")))
  (test-equal -32800 (acp-error-code ':request-cancelled)))

(define-test schema-field-validation
  (let ((object (json-decode "{\"s\": \"text\", \"i\": 3, \"n\": 2.5, \"b\": false, \"o\": {\"k\": 1}, \"a\": [1, 2], \"z\": null}")))
    (test-equal "text" (acp-field object "s" :type ':string :required-p t))
    (test-equal 3 (acp-field object "i" :type ':integer))
    (test-equal 2.5 (acp-field object "n" :type ':number) :test #'=)
    (test-equal nil (acp-field object "b" :type ':boolean :required-p t))
    (test-equal 1 (json-get (acp-field object "o" :type ':object) "k"))
    (test-equal '(1 2) (acp-field object "a" :type ':array))
    (test-equal ':fallback (acp-field object "z" :type ':string :default ':fallback))
    (test-equal ':fallback (acp-field object "missing" :type ':string :default ':fallback))
    (dolist (failure '(("missing" :string t) ("z" :integer t) ("s" :integer nil)
                       ("i" :string nil) ("b" :object nil) ("o" :array nil) ("a" :boolean nil)))
      (destructuring-bind (key type required-p) failure
        (test-equal -32602
                    (acp-method-error-code
                     (test-signals acp-method-error
                       (acp-field object key :type type :required-p required-p))))))
    (test-signals acp-method-error (acp-field "not an object" "s"))))

(define-test schema-capability-queries
  (let ((client (acp-client-capabilities :read-text-file t :terminal t :elicitation-url t))
        (agent (acp-agent-capabilities :load-session t :image t :list t :logout t)))
    (test-assert (acp-capability-enabled-p client "fs.readTextFile"))
    (test-assert (not (acp-capability-enabled-p client "fs.writeTextFile")))
    (test-assert (acp-capability-enabled-p client "terminal"))
    (test-assert (not (acp-capability-enabled-p client "auth.terminal")))
    (test-assert (acp-capability-enabled-p client "elicitation.url"))
    (test-assert (not (acp-capability-enabled-p client "elicitation.form")))
    (test-assert (not (acp-capability-enabled-p client "session.configOptions.boolean")))
    (test-assert (acp-capability-enabled-p
                  (acp-client-capabilities :boolean-config-options t)
                  "session.configOptions.boolean"))
    (test-assert (acp-capability-enabled-p agent "loadSession"))
    (test-assert (acp-capability-enabled-p agent "promptCapabilities.image"))
    (test-assert (not (acp-capability-enabled-p agent "promptCapabilities.audio")))
    (test-assert (acp-capability-enabled-p agent "sessionCapabilities.list"))
    (test-assert (not (acp-capability-enabled-p agent "sessionCapabilities.delete")))
    (test-assert (acp-capability-enabled-p agent "auth.logout"))
    (test-assert (not (acp-capability-enabled-p nil "anything")))
    (test-assert (not (acp-capability-enabled-p (json-decode "{\"fs\": null}") "fs.readTextFile")))
    (let ((decoded (json-decode (json-encode agent))))
      (test-assert (acp-capability-enabled-p decoded "sessionCapabilities.list"))
      (test-assert (not (acp-capability-enabled-p decoded "sessionCapabilities.close"))))))

(define-test schema-content-blocks
  (let ((blocks (list (acp-text-content "hello")
                      (acp-image-content "AAAA" "image/png" :uri "file:///x.png")
                      (acp-audio-content "BBBB" "audio/wav")
                      (acp-resource-link "file:///doc.pdf" "doc.pdf" :size 10)
                      (acp-embedded-resource "file:///a.lisp" :text "(+ 1 2)" :mime-type "text/x-lisp"))))
    (test-equal '(:text :image :audio :resource-link :resource) (mapcar #'acp-content-type blocks))
    (test-equal "hello" (acp-content-text (first blocks)))
    (test-equal "(+ 1 2)" (acp-content-text (fifth blocks)))
    (test-equal nil (acp-content-text (second blocks)))
    (test-equal blocks (acp-validate-content-blocks blocks))
    (test-equal blocks (acp-validate-content-blocks
                        (json-sequence->list (json-decode (json-encode blocks))))
                :test (lambda (expected actual)
                        (= (length expected) (length actual))))
    (test-equal ':unknown-type (acp-content-type (json-object "type" "video")))
    (test-signals acp-protocol-error (acp-embedded-resource "file:///empty"))
    (dolist (bad (list "not an object"
                       (json-object "type" "video")
                       (json-object "type" "text")
                       (json-object "type" "image" "data" "AAAA")
                       (json-object "type" "resource_link" "uri" "file:///x")
                       (json-object "type" "resource" "resource" (json-object "uri" "file:///x"))))
      (test-equal -32602
                  (acp-method-error-code
                   (test-signals acp-method-error (acp-validate-content-blocks (list bad))))))))

(define-test schema-tool-calls-and-updates
  (let* ((call (acp-tool-call "call-1" "Reading config"
                              :name "read_file" :kind ':read :status ':pending
                              :locations (list (acp-tool-call-location "/p/config.json" :line 3))
                              :raw-input (json-object "path" "/p/config.json")))
         (update (acp-tool-call-update "call-1" :status ':completed
                                       :content (list (acp-tool-call-content (acp-text-content "done"))
                                                      (acp-diff-content "/p/a" "new" :old-text "old")
                                                      (acp-terminal-content "term-1"))))
         (announce (acp-update-tool-call call))
         (progress (acp-update-tool-call-progress update)))
    (test-equal "read" (json-get call "kind"))
    (test-equal "pending" (json-get call "status"))
    (test-equal 3 (json-get (elt (json-get call "locations") 0) "line"))
    (test-equal ':tool-call (acp-update-kind announce))
    (test-equal ':tool-call-update (acp-update-kind progress))
    (test-equal "completed" (json-get progress "status"))
    (test-assert (not (nth-value 1 (gethash "title" progress)))
                 "an update carries only the fields that changed")
    (test-equal '("content" "diff" "terminal")
                (map 'list (lambda (entry) (json-get entry "type")) (json-get progress "content")))
    (test-equal ':unknown-update (acp-update-kind (json-object "sessionUpdate" "later_feature")))
    (test-equal ':agent-message-chunk
                (acp-update-kind (acp-update-agent-message (acp-text-content "hi") :message-id "m1")))
    (test-equal ':agent-thought-chunk (acp-update-kind (acp-update-agent-thought (acp-text-content "hm"))))
    (test-equal ':user-message-chunk (acp-update-kind (acp-update-user-message (acp-text-content "q"))))
    (let ((plan (acp-update-plan (list (acp-plan-entry "step one" :priority ':high)
                                       (acp-plan-entry "step two" :status ':completed)))))
      (test-equal ':plan (acp-update-kind plan))
      (test-equal '("high" "medium") (map 'list (lambda (entry) (json-get entry "priority"))
                                          (json-get plan "entries"))))
    (test-equal "USD" (json-get (json-get (acp-update-usage 10 100 :cost-amount 0.5 :cost-currency "USD") "cost")
                                "currency"))
    (test-assert (not (nth-value 1 (gethash "cost" (acp-update-usage 10 100)))))
    (test-equal "null" (json-encode (json-get (acp-update-session-info :title ':null) "title")))
    (test-equal "hint" (json-get (json-get (acp-available-command "web" "Search" :hint "hint") "input") "hint"))
    (test-equal "code" (json-get (acp-update-current-mode "code") "currentModeId"))
    (test-equal "mode" (json-get (acp-config-option-select "mode" "Mode" "ask"
                                                           (list (acp-config-select-option "ask" "Ask"))
                                                           :category ':mode)
                                 "category"))
    (test-equal "_custom" (json-get (acp-config-option-boolean "b" "B" t :category "_custom") "category"))
    (test-equal t (json-get (acp-config-option-boolean "b" "B" t) "currentValue"))))

(define-test schema-mcp-servers
  (let ((servers (list (acp-mcp-server-stdio "fs" "/bin/server" :arguments '("--stdio")
                                             :environment (list (acp-env-variable "KEY" "v")))
                       (acp-mcp-server-http "api" "https://example.test/mcp"
                                            :headers (list (acp-http-header "Authorization" "Bearer x")))
                       (acp-mcp-server-sse "events" "https://example.test/sse"))))
    (test-equal '(:stdio :http :sse) (mapcar #'acp-mcp-server-transport servers))
    (test-equal servers (acp-validate-mcp-servers servers))
    (test-equal '(("KEY" . "v")) (acp-name-value-pairs (json-get (first servers) "env")))
    (test-equal '(("Authorization" . "Bearer x"))
                (acp-name-value-pairs (json-get (second servers) "headers")))
    (test-equal nil (acp-name-value-pairs nil))
    (test-equal ':unknown-transport (acp-mcp-server-transport (json-object "type" "carrier-pigeon")))
    (dolist (bad (list (json-object "name" "x")
                       (json-object "name" "x" "command" "/bin/x")
                       (json-object "type" "http" "name" "x")
                       (json-object "type" "carrier-pigeon" "name" "x")
                       "nope"))
      (test-signals acp-method-error (acp-validate-mcp-servers (list bad)))))
  (let ((method (acp-terminal-auth-method "login" "Log in" :arguments '("--login")
                                          :environment '(("ACP_LOGIN" . "1")))))
    (test-equal "terminal" (json-get method "type"))
    (test-equal "1" (json-get (json-get method "env") "ACP_LOGIN"))
    (test-equal '("--login") (json-sequence->list (json-get method "args")))))
