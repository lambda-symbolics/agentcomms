(asdf:defsystem #:agentcomms
  :description "The Agent Client Protocol for Common Lisp agents and clients."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:serapeum
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "json")
                             (:file "channel")
                             (:file "connection")
                             (:file "schema")
                             (:file "agent")
                             (:file "client")
                             (:file "stdio"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:agentcomms/tests))))

(asdf:defsystem #:agentcomms/tests
  :description "Tests for agentcomms."
  :depends-on (#:agentcomms)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "json-tests")
                             (:file "connection-tests")
                             (:file "schema-tests")
                             (:file "agent-tests")
                             (:file "client-tests")
                             (:file "stdio-tests")
                             (:file "lifecycle-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:agentcomms '#:run-tests)))
