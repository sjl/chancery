(in-package :chancery)

;;;; Utils ---------------------------------------------------------------------
(deftype non-keyword-symbol ()
  '(and symbol (not keyword)))


(defmacro -<> (expr &rest forms)
  "Thread the given forms, with `<>` as a placeholder."
  ;; I am going to lose my fucking mind if I have to program lisp without
  ;; a threading macro, but I don't want to add another dep to this library, so
  ;; here we are.
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))


(defmacro assert-nonempty (place message)
  `(assert (not (emptyp ,place)) (,place) ,message))

(defmacro gimme (n &body body)
  `(loop :repeat ,n :collect (progn ,@body)))


(defun emptyp (string)
  (zerop (length string)))

(defun cat (&rest strings)
  "Concatenate `strings` into a string."
  (apply #'concatenate 'string strings))

(defun ch (string index)
  "Return the character of `string` at `index`.  Allows negative indices."
  (if (emptyp string)
    nil
    (aref string (if (minusp index)
                   (+ (length string) index)
                   index))))

(defun chop (string n)
  "Chop `n` characters off the end of `string`"
  (subseq string 0 (max 0 (- (length string) n))))

(defun vowelp (character)
  (ensure-boolean (member character '(#\a #\e #\i #\o #\u
                                      #\A #\E #\I #\O #\U))))


(defun prefix-sums (sequence &aux (sum 0))
  (map 'list (lambda (n) (incf sum n)) sequence))

(defun separate-with-spaces (list)
  (-<> list
    (split-sequence :. <>)
    (mapcar (rcurry #'riffle #\Space) <>)
    (apply #'append <>)))

(defun join-string (&rest parts)
  (apply #'cat (mapcar #'princ-to-string parts)))


;;;; RNG ----------------------------------------------------------------------
(defparameter *random* #'random)

(defun chancery-random (n)
  (funcall *random* n))


;;;; Weightlists --------------------------------------------------------------
(defstruct (weightlist (:constructor %make-weightlist))
  weights sums items total)

(defun make-weightlist (items weights)
  "Make a weightlist of the given items and weights.

  Weights can be any `real` numbers.  Weights of zero are fine, as long as at
  least one of the weights is nonzero (otherwise there's nothing to choose).

  "
  (%make-weightlist
    :items items
    :weights weights
    :sums (prefix-sums weights)
    :total (apply #'+ 0.0 weights)))


(defmethod print-object ((object weightlist) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (mapcar #'list
                   (weightlist-weights object)
                   (weightlist-items object))
           stream)))

(defmethod make-load-form ((object weightlist) &optional environment)
  (make-load-form-saving-slots object
                               :slot-names '(weights sums items total)
                               :environment environment))


(defun weightlist-random (weightlist)
  "Return a random item from the weightlist, taking the weights into account."
  (loop :with n = (chancery-random (weightlist-total weightlist))
        :for item :in (weightlist-items weightlist)
        :for weight :in (weightlist-sums weightlist)
        :when (< n weight) :do (return item)))


;;;; Core ---------------------------------------------------------------------
(defun special-form-p (form)
  (ensure-boolean
    (and (consp form)
         (member (first form) '(quote eval)))))

(deftype special-form ()
  '(satisfies special-form-p))


(defun compile-list (contents)
  `(list ,@(mapcar #'compile-expression contents)))

(defun compile-symbol (symbol)
  `(,symbol))

(defun compile-special-form (expression)
  (destructuring-bind (symbol argument) expression
    (ecase symbol
      (quote `(quote ,argument))
      (eval argument))))

(defun compile-expression (expression)
  (typecase expression
    (null expression)
    (non-keyword-symbol (compile-symbol expression))
    (special-form (compile-special-form expression))
    (string expression)
    (list (compile-list expression))
    ; todo: vectors?
    (t expression)))


(defun build-weightlist-weighted (size weights)
  (make-weightlist (range 0 size) weights))

(defun build-weightlist-zipf (size exponent)
  (loop
    :with denominator = (loop :for n :from 1.0 :to size
                              :sum (/ (expt n exponent)))
    :repeat size
    :for rank :from 1
    :for item :from 0
    :for weight = (/ (/ (expt rank exponent))
                     denominator)
    :collect item :into items
    :collect weight :into weights
    :finally (return (make-weightlist items weights))))


(defun compile-selector-uniform (expressions)
  (values `(chancery-random ,(length expressions))
          expressions))

(defun compile-selector-weighted (expressions)
  (values `(weightlist-random
             ,(build-weightlist-weighted (length expressions)
                                         (mapcar #'first expressions)))
          (mapcar #'second expressions)))

(defun compile-selector-zipf (expressions &key (exponent 1.0))
  (values `(weightlist-random
             ,(build-weightlist-zipf (length expressions) exponent))
          expressions))

(defun compile-selector (distribution-and-options expressions)
  (destructuring-bind (distribution &rest distribution-options)
      (ensure-list distribution-and-options)
    (apply (ecase distribution
             (:uniform #'compile-selector-uniform)
             (:weighted #'compile-selector-weighted)
             (:zipf #'compile-selector-zipf))
           expressions
           distribution-options)))


(defun compile-rule-body (expression-compiler expressions distribution)
  (if (= 1 (length expressions))
    (funcall expression-compiler (first expressions))
    (multiple-value-bind (selector expressions)
        (compile-selector distribution expressions)
      `(case ,selector
         ,@(loop
            :for i :from 0
            :for expression :in expressions
            :collect `(,i ,(funcall expression-compiler expression)))))))


(defun compile-define-rule (expression-compiler name-and-options expressions)
  (destructuring-bind (name &key
                            documentation
                            (distribution :uniform)
                            (arguments '()))
      (ensure-list name-and-options)
    `(defun ,name ,arguments
       ,@(ensure-list documentation)
       ,(compile-rule-body expression-compiler expressions distribution))))

(defun compile-create-rule (expression-compiler options expressions)
  (destructuring-bind (&key documentation
                            (distribution :uniform)
                            (arguments '()))
      options
    (compile nil
             `(lambda ,arguments
                ,@(ensure-list documentation)
                ,(compile-rule-body expression-compiler
                                    expressions
                                    distribution)))))


(defmacro define-rule (name-and-options &rest expressions)
  (compile-define-rule #'compile-expression name-and-options expressions))

(defun create-rule (expressions &rest options)
  (compile-create-rule #'compile-expression options expressions))


(defmacro generate (expression)
  "Generate a single Chancery expression."
  (compile-expression expression))


;;;; Strings ------------------------------------------------------------------
(defun compile-string-combination (list)
  `(join-string ,@(-<> list
                    (separate-with-spaces <>)
                    (mapcar #'compile-string-expression <>))))

(defun compile-string-modifiers (vector)
  ; #("foo" a b c) => (c (b (a "foo")))
  (reduce (flip #'list) vector
          :start 1
          :initial-value (compile-expression (aref vector 0))))

(defun compile-string-expression (expression)
  (typecase expression
    (string expression)
    (null "")
    (non-keyword-symbol (compile-symbol expression))
    (special-form (compile-special-form expression))
    (vector (compile-string-modifiers expression))
    (cons (compile-string-combination expression))
    (t expression)))


(defmacro define-string (name-and-options &rest expressions)
  (compile-define-rule #'compile-string-expression name-and-options expressions))

(defun create-string (expressions &rest options)
  (compile-create-rule #'compile-string-expression options expressions))


(defmacro generate-string (expression)
  "Generate a single Chancery string expression."
  (compile-string-expression expression))


;;;; Modifiers ----------------------------------------------------------------
(defun cap (string)
  "Capitalize the first character of `string`."
  (assert-nonempty string "Cannot capitalize an empty string.")
  (string-capitalize string :end 1))

(defun cap-all (string)
  "Capitalize each word of `string`."
  (assert-nonempty string "Cannot capitalize-all an empty string.")
  (string-capitalize string))

(defun a (string)
  "Add an indefinite article (a or an) to the front of `string`."
  (assert-nonempty string "Cannot add an article to an empty string.")
  (cat (if (vowelp (ch string 0))
         "an "
         "a ")
       string))

(defun s (string)
  "Pluralize `string`."
  ;; todo: fix for caps
  ;; todo: make this suck less in general, see http://blog.writeathome.com/index.php/2011/12/how-to-make-nouns-plural/
  (assert-nonempty string "Cannot pluralize an empty string.")
  (case (char-downcase (ch string -1))
    (#\y (if (vowelp (ch string -2))
           (cat string "s")
           (cat (chop string 1) "ies")))
    ((#\x #\s #\z) (cat string "es"))
    (#\h (cat string
              (case (ch string -2)
                ((#\c #\s) "es")
                (t "s"))))
    (t (cat string "s"))))

(defun pos (string)
  "Make `string` posessive by adding an apostrophe (and possibly an s)."
  (assert-nonempty string "Cannot make an empty string posessive.")
  (cat string
       (if (eql #\s (ch string -1))
         "'"
         "'s")))


;;;; Scratch ------------------------------------------------------------------
