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


(defun prefix-sums (sequence &aux (sum 0))
  (map 'list (lambda (n) (incf sum n)) sequence))


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
  (ensure-boolean (member character '(#\a #\e #\i #\o #\u))))

(defun separate-with-spaces (list)
  (-<> list
    (split-sequence :. <>)
    (mapcar (rcurry #'riffle " ") <>)
    (apply #'append <>)))


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


(defun build-weightlist-uniform (values)
  (make-weightlist values (loop :for nil :in values :collect 1)))

(defun build-weightlist-weighted (values)
  (make-weightlist (mapcar #'second values)
                   (mapcar #'first values)))

(defun build-weightlist-zipf (values &key (exponent 1.0))
  (loop
    :with size = (length values)
    :with denominator = (loop :for n :from 1.0 :to size
                              :sum (/ (expt n exponent)))
    :repeat size
    :for rank :from 1
    :for weight = (/ (/ (expt rank exponent))
                     denominator)
    :collect weight :into weights
    :finally (return (make-weightlist values weights))))

(defun build-weightlist (distribution-and-options values)
  (destructuring-bind (distribution &rest options)
      (ensure-list distribution-and-options)
    (apply (ecase distribution
             (:uniform #'build-weightlist-uniform)
             (:weighted #'build-weightlist-weighted)
             (:zipf #'build-weightlist-zipf))
           values options)))


;;;; Data ---------------------------------------------------------------------
(defun data-special-form-p (form)
  (ensure-boolean (and (consp form)
                       (member (first form) '(quote eval)))))

(deftype data-special-form ()
  '(satisfies data-special-form-p))


(defun evaluate-sequence (seq)
  (map (type-of seq) #'evaluate-expression seq))

(defun evaluate-symbol (symbol)
  (if (fboundp symbol)
    (funcall symbol)
    (symbol-value symbol)))

(defun evaluate-lisp (expr)
  (eval expr))

(defun evaluate-data-special-form (expr)
  (destructuring-bind (symbol argument) expr
    (ecase symbol
      (quote argument)
      (eval (evaluate-lisp argument)))))


(defun evaluate-expression (expr)
  (typecase expr
    (non-keyword-symbol (evaluate-symbol expr))
    (data-special-form (evaluate-data-special-form expr))
    (string expr)
    (sequence (evaluate-sequence expr))
    (t expr)))


(defun build-define-rule (evaluator name-and-options expressions)
  (destructuring-bind (name &key (distribution :uniform))
      (ensure-list name-and-options)
    `(defun ,name ()
       (,evaluator
         (weightlist-random ,(build-weightlist distribution expressions))))))


(defmacro define-rule (name-and-options &rest expressions)
  (build-define-rule 'evaluate-expression name-and-options expressions))

(defmacro gen (expression)
  "Generate a single Chancery expression."
  `(evaluate-expression ',expression))


;;;; Strings ------------------------------------------------------------------
(defun string-special-form-p (form)
  (ensure-boolean (and (consp form)
                       (member (first form) '(quote eval list vector)))))

(deftype string-special-form ()
  '(satisfies string-special-form-p))


(defun evaluate-string-combination (list)
  (-<> list
    (separate-with-spaces <>)
    (mapcar #'evaluate-string-expression <>)
    (apply #'cat (mapcar #'princ-to-string <>))))

(defun evaluate-string-modifiers (vector)
  (reduce (flip #'funcall) vector
          :start 1
          :initial-value
          (princ-to-string (evaluate-string-expression (aref vector 0)))))

(defun evaluate-string-sequence (sequence-type seq)
  (map sequence-type #'evaluate-string-expression seq))

(defun evaluate-string-special-form (expr)
  (destructuring-bind (symbol . body) expr
    (ecase symbol
      (quote (first body))
      (list (evaluate-string-sequence 'list body))
      (vector (evaluate-string-sequence 'vector body))
      (eval (evaluate-lisp (first body))))))


(defun evaluate-string-expression (expr)
  (typecase expr
    (string expr)
    (null "")
    (non-keyword-symbol (evaluate-symbol expr))
    (string-special-form (evaluate-string-special-form expr))
    (vector (evaluate-string-modifiers expr))
    (cons (evaluate-string-combination expr))
    (t expr)))


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
  (build-define-rule 'evaluate-string-expression name-and-options expressions))

(defmacro gen-string (expression)
  "Generate a single Chancery string expression."
  `(evaluate-string-expression ',expression))


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
  (assert-nonempty string "Cannot pluralize an empty string.")
  (case (ch string -1)
    (#\y (if (vowelp (ch string -2))
           (cat string "s")
           (cat (chop string 1) "ies")))
    (#\x (cat (chop string 1) "en"))
    ((#\z #\h) (cat (chop string 1) "es"))
    (t (cat string "s"))))

(defun pos (string)
  "Make `string` posessive by adding an apostrophe (and possibly an s)."
  (assert-nonempty string "Cannot make an empty string posessive.")
  (cat string
       (if (eql #\s (ch string -1))
         "'"
         "'s")))


;;;; Scratch ------------------------------------------------------------------
