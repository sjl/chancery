(in-package :chancery)

;;;; Utils ---------------------------------------------------------------------
(deftype non-keyword-symbol ()
  '(and symbol (not keyword)))


(defmacro -<> (&rest forms)
  ;; I am going to lose my fucking mind if I have to program lisp without
  ;; a threading macro, but I don't want to add another dep to this library, so
  ;; here we are.
  (if (null forms)
    '<>
    `(let ((<> ,(first forms)))
       (-<> ,@(rest forms)))))

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


(defmethod print-object ((wl weightlist) s)
  (print-unreadable-object (wl s :type t)
    (prin1 (mapcar #'list
                   (weightlist-weights wl)
                   (weightlist-items wl))
           s)))

(defmethod make-load-form ((wl weightlist) &optional environment)
  (make-load-form-saving-slots wl
                               :slot-names '(weights sums items total)
                               :environment environment))


(defun weightlist-random (weightlist)
  "Return a random item from the weightlist, taking the weights into account."
  (loop :with n = (random (weightlist-total weightlist))
        :for item :in (weightlist-items weightlist)
        :for weight :in (weightlist-sums weightlist)
        :when (< n weight) :do (return item)))


;;;; Data ---------------------------------------------------------------------
(defun data-special-form-p (form)
  (ensure-boolean (and (consp form)
                       (member (first form) '(quote eval)))))

(deftype data-special-form ()
  '(satisfies data-special-form-p))


(defun compile-sequence (seq)
  (let ((contents (map 'list #'compile-expression seq)))
    (etypecase seq
      (list `(list ,@contents)))))

(defun compile-symbol (symbol)
  `(,symbol))

(defun compile-data-special-form (expr)
  (destructuring-bind (symbol argument) expr
    (ecase symbol
      (quote `(quote ,argument))
      (eval argument))))

(defun compile-expression (expr)
  (typecase expr
    (null expr)
    (non-keyword-symbol (compile-symbol expr))
    (data-special-form (compile-data-special-form expr))
    (string expr)
    (sequence (compile-sequence expr))
    (t expr)))


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
  (values `(random ,(length expressions))
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
  (destructuring-bind (distribution &rest options)
      (ensure-list distribution-and-options)
    (apply (ecase distribution
             (:uniform #'compile-selector-uniform)
             (:weighted #'compile-selector-weighted)
             (:zipf #'compile-selector-zipf))
           expressions
           options)))


(defun compile-define-rule (expression-compiler name-and-options expressions)
  (destructuring-bind (name &key
                            documentation
                            (distribution :uniform)
                            (arguments '()))
      (ensure-list name-and-options)
    `(defun ,name ,arguments
       ,@(ensure-list documentation)
       ,(if (= 1 (length expressions))
         (funcall expression-compiler (first expressions))
         (multiple-value-bind (selector expressions)
             (compile-selector distribution expressions)
           `(case ,selector
              ,@(loop
                 :for i :from 0
                 :for expression :in expressions
                 :collect `(,i ,(funcall expression-compiler expression)))))))))


(defmacro define-rule (name-and-options &rest expressions)
  (compile-define-rule #'compile-expression name-and-options expressions))

(defmacro gen (expression)
  "Generate a single Chancery expression."
  (compile-expression expression))


;;;; Strings ------------------------------------------------------------------
(defun string-special-form-p (form)
  (ensure-boolean (and (consp form)
                       (member (first form) '(quote eval list vector)))))

(deftype string-special-form ()
  '(satisfies string-special-form-p))


(defun compile-string-combination (list)
  (let ((contents (-<> list
                    (separate-with-spaces <>)
                    (mapcar #'compile-string-expression <>))))
    `(join-string ,@contents)))

(defun compile-string-modifiers (vector)
  (labels ((recur (modifiers)
             (if (null modifiers)
               (compile-expression (aref vector 0))
               `(,(first modifiers)
                 ,(recur (rest modifiers))))))
    (recur (nreverse (coerce (subseq vector 1) 'list)))))

(defun compile-string-sequence (sequence-type seq)
  (let ((contents (map 'list #'compile-string-expression seq)))
    (ecase sequence-type
      (list `(list ,@contents)))))

(defun compile-string-special-form (expr)
  (destructuring-bind (symbol . body) expr
    (ecase symbol
      (quote `(quote ,(first body)))
      (list (compile-string-sequence 'list body))
      (vector (compile-string-sequence 'vector body))
      (eval (first body)))))


(defun compile-string-expression (expression)
  (typecase expression
    (string expression)
    (null "")
    (non-keyword-symbol (compile-symbol expression))
    (string-special-form (compile-string-special-form expression))
    (vector (compile-string-modifiers expression))
    (cons (compile-string-combination expression))
    (t expression)))


(defmacro define-string (name-and-options &rest expressions)
  "Define a Chancery string rule for the symbol `name`.

  Each expression in `expressions` can be any valid Chancery expression.  When
  the rule is invoked one will be chosen at random and evaluated.

  Examples:

    (define-rule name \"Alice\" \"Bob\" \"Carol\")
    (define-rule place \"forest\" \"mountain\")
    (define-rule emotion \"happy\" \"sad\")

    (define-rule sentence
      (name \"was\" emotion :. \".\")
      (name \"went to the\" place :. \".\"))

  "
  (compile-define-rule 'compile-string-expression name-and-options expressions))

(defmacro gen-string (expression)
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
  (assert-nonempty string "Cannot pluralize an empty string.")
  (case (ch string -1)
    ((#\y #\Y) (if (vowelp (ch string -2))
                 (cat string "s")
                 (cat (chop string 1) "ies")))
    ((#\x #\X) (cat (chop string 1) "en"))
    ((#\z #\h #\Z #\H) (cat (chop string 1) "es"))
    (t (cat string "s"))))

(defun pos (string)
  "Make `string` posessive by adding an apostrophe (and possibly an s)."
  (assert-nonempty string "Cannot make an empty string posessive.")
  (cat string
       (if (eql #\s (ch string -1))
         "'"
         "'s")))


;;;; Scratch ------------------------------------------------------------------
