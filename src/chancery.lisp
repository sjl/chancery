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


;;;; Guts ---------------------------------------------------------------------
(defparameter *bindings* nil)


(defun create-binding (binding)
  (destructuring-bind (target expr) binding
    (let ((value (evaluate-expression expr)))
      (etypecase target
        (non-keyword-symbol (list target value))
        (cons (loop :for symbol :in target
                    :for val :in value
                    :append (list symbol val)))))))

(defun evaluate-bind (bindings expr)
  (let* ((new-bindings (mapcan (rcurry #'create-binding) bindings))
         (*bindings* (cons new-bindings *bindings*)))
    (evaluate-expression expr)))

(defun evaluate-bind* (bindings expr)
  (destructuring-bind (binding . remaining-bindings) bindings
    (let ((*bindings* (cons (create-binding binding)
                            *bindings*)))
      (if remaining-bindings
        (evaluate-bind* remaining-bindings expr)
        (evaluate-expression expr)))))

(defun lookup-binding (symbol)
  (loop :for frame :in *bindings*
        :for value = (getf frame symbol 'not-found)
        :do (unless (eq value 'not-found)
              (return-from lookup-binding (values value t))))
  (values nil nil))


(defun evaluate-combination (list)
  (-<> list
    (separate-with-spaces <>)
    (mapcar #'evaluate-expression <>)
    (apply #'cat (mapcar #'princ-to-string <>))))

(defun evaluate-modifiers (vector)
  (reduce (flip #'funcall) vector
          :start 1
          :initial-value (evaluate-expression (aref vector 0))))

(defun evaluate-symbol (symbol)
  (multiple-value-bind (value found) (lookup-binding symbol)
    (if found
      value
      (if (fboundp symbol)
        (funcall symbol)
        (symbol-value symbol)))))

(defun evaluate-lisp (expr)
  (eval expr))


(defun evaluate-list (list)
  (mapcar #'evaluate-expression list))

(defun evaluate-expression (expr)
  (typecase expr
    ((or string keyword null) expr)
    (symbol (evaluate-symbol expr))
    (vector (evaluate-modifiers expr))
    (cons (case (first expr)
            (quote (second expr))
            (bind (evaluate-bind (second expr) (cddr expr)))
            (bind* (evaluate-bind* (second expr) (cddr expr)))
            (eval (evaluate-lisp (second expr)))
            (list (evaluate-list (rest expr)))
            (t (evaluate-combination expr))))
    (t expr)))


(defmacro define-rule (name &rest expressions)
  "Define a Chancery rule for the symbol `name`.

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
     (evaluate-expression
       (random-elt ,(coerce expressions 'vector)))))


;;;; Modifiers ----------------------------------------------------------------
(defun cap (string)
  "Capitalize the first character of `string`."
  (assert-nonempty string "Cannot capitalize an empty string.")
  (string-capitalize string :end 1))

(defun cap-all (string)
  "Capitalize each word of `string`."
  (assert-nonempty string "Cannot capitalize-all an empty string.")
  (string-capitalize string))

(defun q (string)
  "Wrap `string` in quotation marks."
  (cat "\"" string "\""))

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
