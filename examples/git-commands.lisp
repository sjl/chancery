(in-package :chancery)
(named-readtables:in-readtable :chancery)

(define-rule noun
  "binary blob"
  "packfile"
  "refspec"
  "blob"
  "branch"
  "changeset"
  "commit"
  "conflicted merge"
  "current HEAD"
  "file"
  "head"
  "merge"
  "remote"
  "object"
  "patch"
  "ref"
  "repository"
  "symlink"
  "tag"
  "tip"
  )


(define-rule git-location%
  "repository"
  "index"
  "working tree"
  "content-addressable filesystem"
  "object store"
  "reflog"
  "current directory"
  "current repository"
  "current branch"
  "checked-out branch"
  "upstream repository"
  "DAG")

(define-rule git-folder%
  ""
  "refs"
  "logs"
  "objects"
  "hooks"
  "HEAD"
  "COMMIT_EDITMSG")

(define-rule git-folder
  (".git/" :. git-folder%))

(define-rule git-location
  ("the" git-location%)
  git-folder)

(define-rule external-location
  "Hacker News"
  "Stack Overflow"
  "Twitter"
  "Reddit"
  "Github"
  "Gitlab"
  "Github's status page"
  "/dev/random"
  "/dev/urandom"
  "your .gitconfig"
  "the git man pages"
  "the git source code"
  "your home directory")

(define-rule location
  git-location
  external-location)


(define-rule action
  (list "bisect" "bisecting")
  (list "clone" "cloning")
  (list "commit" "committing")
  (list "delete" "deleting")
  (list "display" "displaying")
  (list "fast-forward" "fast-forwarding")
  (list "fetch" "fetching")
  (list "merge" "merging")
  (list "move" "moving")
  (list "print" "printing")
  (list "prune" "pruning")
  (list "pull" "pulling")
  (list "push" "pushing")
  (list "record" "recording")
  (list "revert" "reverting")
  (list "remove" "removing")
  (list "rename" "renaming")
  (list "reset" "resetting")
  (list "resolve" "resolving")
  (list "show" "showing")
  (list "sign" "signing")
  (list "simplify" "simplifying")
  (list "update" "updating")
  (list "verify" "verifying")
  )

(defun action-verb ()
  (first (action)))


(define-rule refresh
  "update"
  "reset")

(define-rule refreshing
  "updating"
  "resetting")


(define-rule extremum
  "newest"
  "oldest"
  "largest"
  "smallest"
  "sparsest"
  "first"
  "last"
  "worst"
  "simplest"
  "best")

(define-rule adjective
  "merged"
  "unmerged"
  "symbolic"
  "uncommitted"
  "signed"
  "unsigned"
  "big-endian"
  "little-endian"
  "childless"
  "binary"
  )


(define-rule age
  "newest"
  "oldest"
  "first"
  "last")

(define-rule look-for
  "search"
  "grep"
  "bisect"
  "filter")

(define-rule temporal-adverb
  "before"
  "after"
  "without")


(defun letter ()
  (random-elt "abcdefghijklmnopqrstuvwxyz"))

(defun shellify (str)
  (string-downcase (substitute #\- #\space str)))

(define-rule short-option%
  ("-" :. letter)
  ("-" :. letter [noun shellify string-upcase]))

(defparameter *noun* nil)

(define-rule long-option%
  (eval (let ((*noun* (generate [noun shellify])))
          (generate ("--" :. *noun* :. "=<" :. *noun* :. ">"))))
  ("--" :. action-verb)
  ("--" :. extremum)
  ("--only-" :. adjective)
  ("--only-" :. [noun shellify s])
  ("--" :. action-verb :. "=<" :. [noun shellify] :. ">"))

(define-rule short-option
  short-option%
  ("[" :. short-option% :. "]"))

(define-rule long-option
  long-option%
  ("[" :. long-option% :. "]"))

(define-rule short-options
  short-option
  (short-option short-option))

(define-rule options
  long-option
  short-options
  (short-options long-option)
  (long-option short-options))


(defparameter *command* nil)
(defparameter *commanding* nil)

(define-rule description
  (look-for location "for the" age noun "and" *command* "it")
  ("read" (eval (+ 2 (random 2000))) "bytes from" location "and" *command* "them")
  (*command* "the" extremum noun "in" git-location)
  (*command* [noun a] temporal-adverb refreshing git-location)
  (*command* "and push all" adjective [noun s] "to" location)
  (*command* "all" adjective [noun s] "in" git-location)
  (*command* "the" extremum "and merge it into" git-location)
  (*command* "some" [noun s] "from a remote")
  (*command* "two or more" [noun s] "and save them to" location)
  ("move or" *command* [noun a] "in" git-location)
  ("rebase" [noun a] "onto" location "after" *commanding* "it")
  (*command* "and" refresh git-location)
  ("list," *command* :. ", or delete" [noun s]))

(defun entry ()
  (destructuring-bind (*command* *commanding*) (action)
    (generate
      ("git" *command* options #\newline :. [description cap]))))


(dotimes (_ 20) (princ (entry)) (terpri) (terpri))
