(in-package #:agentcomms)

;;;; -- Test Registry --

(defvar *test-cases* nil
  "The registered test names and functions, newest first.")

(defmacro define-test (name &body body)
  "Define and register one test named NAME."
  `(progn
     (setf *test-cases* (remove ',name *test-cases* :key #'first))
     (push (list ',name (lambda () ,@body)) *test-cases*)
     ',name))

(defmacro test-assert (form &optional description)
  "Signal a test failure unless FORM returns true."
  `(unless ,form
     (error "Assertion failed: ~A~@[ (~A)~]" ',form ,description)))

(defmacro test-equal (expected form &key (test '#'equal))
  "Signal a test failure unless FORM equals EXPECTED under TEST."
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value   (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (unless (funcall ,test ,expected-value ,actual-value)
         (error "Expected ~S, got ~S from ~S." ,expected-value ,actual-value ',form))
       ,actual-value)))

(defmacro test-signals (condition-type &body body)
  "Evaluate BODY and return the signaled CONDITION-TYPE condition."
  (let ((condition (gensym "CONDITION")))
    `(handler-case
         (progn
           ,@body
           (error "Expected condition ~S, but none was signaled." ',condition-type))
       (,condition-type (,condition)
         ,condition))))
