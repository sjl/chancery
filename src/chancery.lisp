(in-package :chancery)

;;;; Utils ---------------------------------------------------------------------
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

(defun random-elt (seq)
  (elt seq (random (length seq))))


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


(defun separate-with-spaces (list)
  (-<> list
    (split-sequence :. <>)
    (mapcar (rcurry #'riffle " ") <>)
    (apply #'append <>)))


(deftype non-keyword-symbol ()
  '(and symbol (not keyword)))


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


(defmacro define-rule (name &rest expressions)
  `(defun ,name ()
     (evaluate-expression
       (random-elt ,(coerce expressions 'vector)))))

(defmacro generate (expression)
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


(defmacro define-string (name &rest expressions)
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
  `(defun ,name ()
     (evaluate-string-expression
       (random-elt ,(coerce expressions 'vector)))))

(defmacro generate-string (expression)
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
