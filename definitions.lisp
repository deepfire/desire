(in-package :cling)

(setf *perspectives* (make-hash-table :test 'equal)
      *perspective* (make-instance 'gateway-perspective :name 'root))

(defdistributor feelingofgreen.ru
  (:url-schemas (git (repo) "git.feelingofgreen.ru" (downstring (name repo))))
  (:modules
   (local
    pergamum semi-precious cling clung)))

(defdistributor common-lisp.net
  (:url-schemas (rsync (repo) "common-lisp.net" "project" (downstring (repo-umbrella repo)))
                (http (repo) "common-lisp.net" "project" (downstring (repo-umbrella repo)) "darcs" (downstring (name repo))))
  (:modules
   (darcs
    alexandria iterate)))

(define-module-dependencies
  (pergamum alexandria iterate)
  (cling pergamum semi-precious)
  (clung cling))

(minimise-dependencies)
