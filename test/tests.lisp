(in-package :chancery.test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(symb 'test- name)
    (let ((*package* ,*package*))
      ,@body)))


(defun run-tests ()
  (1am:run))


(defun example-1 ()
  :example-1)

(defun example-2 ()
  :example-2)

(defun one ()
  1)


(defparameter *choice* 0)

(defun test-random (n)
  (declare (ignore n))
  *choice*)


;;;; Tests --------------------------------------------------------------------
(define-test rule-literals
  (is (eql (funcall (create-rule '(:a))) :a))
  (is (eql (funcall (create-rule '(1))) 1))
  (is (string= (funcall (create-rule '("foo"))) "foo"))
  (is (eql (funcall (create-rule '(nil))) nil)))

(define-test rule-symbols
  (is (eql (funcall (create-rule '(example-1))) :example-1))
  (is (eql (funcall (create-rule '(example-2))) :example-2)))

(define-test rule-lists
  (is (equal (funcall (create-rule '((:a 1 example-1))))
             '(:a 1 :example-1))))

(define-test rule-choices
  (let ((*random* #'test-random))
    (let ((*choice* 0))
      (is (eql (funcall (create-rule '(:a :b :c))) :a)))
    (let ((*choice* 1))
      (is (eql (funcall (create-rule '(:a :b :c))) :b)))
    (let ((*choice* 2))
      (is (eql (funcall (create-rule '(:a :b :c))) :c)))))

(define-test string-basics
  (is (string= (funcall (create-string '("hello"))) "hello"))
  (is (string= (funcall (create-string '(("hello")))) "hello"))
  (is (string= (funcall (create-string '(("hello" "world")))) "hello world"))
  (is (string= (funcall (create-string '(("hello" :. "world")))) "helloworld"))
  (is (string= (funcall (create-string '(("hello" 1)))) "hello 1"))
  (is (string= (funcall (create-string '(("hello" one)))) "hello 1")))


(define-rule sample-rule
  :foo)

(define-string sample-string
  ("Hello" sample-rule))

(define-test definition-macros
  (is (eql (sample-rule) :foo))
  (is (string= (sample-string) "Hello FOO")))

;; (run-tests)
