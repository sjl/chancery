(in-package :chancery)
(named-readtables:in-readtable :chancery)

(define-string noun
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
  "tip")


(define-string git-location%
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

(define-string git-folder%
  ""
  "refs"
  "logs"
  "objects"
  "hooks"
  "HEAD"
  "COMMIT_EDITMSG")

(define-string git-folder
  (".git/" :. git-folder%))

(define-string git-location
  ("the" git-location%)
  git-folder)

(define-string external-location
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
  "the blockchain"
  "your home directory")

(define-string location
  git-location
  external-location)


(define-rule action
  ("bisect" "bisecting")
  ("clone" "cloning")
  ("commit" "committing")
  ("delete" "deleting")
  ("display" "displaying")
  ("fast-forward" "fast-forwarding")
  ("fetch" "fetching")
  ("merge" "merging")
  ("move" "moving")
  ("print" "printing")
  ("prune" "pruning")
  ("pull" "pulling")
  ("push" "pushing")
  ("record" "recording")
  ("revert" "reverting")
  ("remove" "removing")
  ("rename" "renaming")
  ("reset" "resetting")
  ("resolve" "resolving")
  ("show" "showing")
  ("sign" "signing")
  ("simplify" "simplifying")
  ("update" "updating")
  ("verify" "verifying"))

(defun action-verb ()
  (first (action)))


(define-string refresh
  "update"
  "reset")

(define-string refreshing
  "updating"
  "resetting")


(define-string extremum
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

(define-string adjective
  "merged"
  "unmerged"
  "symbolic"
  "uncommitted"
  "signed"
  "unsigned"
  "big-endian"
  "little-endian"
  "childless"
  "binary")


(define-string age
  "newest"
  "oldest"
  "first"
  "last")

(define-string look-for
  "search"
  "grep"
  "bisect"
  "filter")

(define-string temporal-adverb
  "before"
  "after"
  "without")


(defun letter ()
  (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        (random 52)))

(defun shellify (str)
  (string-downcase (substitute #\- #\space str)))

(define-string short-option%
  ("-" :. letter)
  ("-" :. letter [noun shellify string-upcase]))

(define-string long-option%
  !(let ((noun $[noun shellify]))
     $("--" :. !noun :. "=<" :. !noun :. ">"))
  ("--" :. action-verb)
  ("--" :. extremum)
  ("--only-" :. adjective)
  ("--only-" :. [noun shellify s])
  ("--" :. action-verb :. "=<" :. [noun shellify] :. ">"))

(define-string short-option
  short-option%
  ("[" :. short-option% :. "]"))

(define-string long-option
  long-option%
  ("[" :. long-option% :. "]"))

(define-string short-options
  short-option
  (short-option short-option))

(define-string options
  long-option
  short-options
  (short-options long-option)
  (long-option short-options))


(defparameter *command* nil)
(defparameter *commanding* nil)

(define-string (description :arguments (command commanding))
  (look-for location "for the" age noun "and" !command "it")
  ("read" !(+ 2 (random 2000)) "bytes from" location "and" !command "them")
  (!command "the" extremum noun "in" git-location)
  (!command [noun a] temporal-adverb refreshing git-location)
  (!command "and push all" adjective [noun s] "to" location)
  (!command "all" adjective [noun s] "in" git-location)
  (!command "the" extremum "and merge it into" git-location)
  (!command "some" [noun s] "from a remote")
  (!command "two or more" [noun s] "and save them to" location)
  ("move or" !command [noun a] "in" git-location)
  ("rebase" [noun a] "onto" location "after" !commanding "it")
  (!command "and" refresh git-location)
  ("list," !command :. ", or delete" [noun s]))

(defun entry ()
  (destructuring-bind (command commanding) (action)
    $("git" !command options #\newline :.
      [!(description command commanding) cap])))


