;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :desire)


(defun add-distributor (type hostname port path &key gate-p)
  "Make a distributor residing at HOSTNAME, with a remote of TYPE,
accesible at PORT and PATH. 
When GATE-P is true, the remote will be set as distributor's gate
remote, in which case TYPE must be subtype of GIT."
  (lret ((d (make-instance 'distributor :name hostname)))
    (let ((r (make-instance type :distributor d :distributor-port port :path path)))
      (push r (distributor-remotes d))
      (when gate-p
        (unless (typep r *gate-vcs-type*)
          (error "~@<Requested to make ~S a gate remote of ~A, but it is not a remote of gate type, i.e. ~A.~:@>"
                 r hostname *gate-vcs-type*))
        (setf (gate d) r)))))

(defun find-distributor-fuzzy (distname &aux (downcase-distname (downstring distname)))
  "Find a distributor which looks like a match for DISTNAME.
For example, domain name OGRE.SVN.SOURCEFORGE.NET refers to a distributor
called SVN.SOURCEFORGE.NET, as the remote specification takes over
domain name generation."
  (or (distributor distname :if-does-not-exist :continue)
      (do-distributors (d)
        (when (search (down-case-name d) downcase-distname)
          (return d)))))

(defun string-detect-splitsubst (string sub replacement &key allowed-separators)
  "Detect whether STRING contains SUB, in which case the string is
split, on SUB boundaries, with the latter replaced with REPLACEMENT,
and every part except the first wrapped in remote /-suppressing
syntax."
  (if-let ((posn (when-let ((posn (search sub string)))
                   (let ((postposn (+ posn (length sub))))
                     (if (and (> (length string) postposn) allowed-separators)
                         (when (member (aref string postposn) allowed-separators :test #'char=)
                           posn)
                         posn)))))
    (let ((pre-sub (when (plusp posn)
                     (subseq string 0 posn)))
          (post-sub (when (< (+ posn (length sub)) (length string))
                      (subseq string (+ posn (length sub))))))
      (values (append (if pre-sub
                          (list pre-sub :no/ replacement)
                          (list replacement))
                      (when post-sub
                        (list :no/ post-sub)))
              t)) ;; signal that we did deconstruction
    (list string)))

(defun compute-remote-path-variants (raw-path modname)
  "Given a RAW-PATH pathname component list and MODNAME, produce a list
of pathname component list variants, with MODNAME occurences substituted."
  (lret ((variants (list nil)))
    (iter (for path-elt in raw-path)
          (for (values pathname-component deconsp) = (string-detect-splitsubst path-elt modname '*module* :allowed-separators '(#\.)))
          (if deconsp
              (setf variants
                    (append (iter (for av in (copy-list variants))
                                  (collect (append av (subst '*umbrella* '*module* pathname-component))))
                            (iter (for av in (copy-list variants))
                                  (collect (append av (list path-elt))))
                            (iter (for v in variants)
                                  (collect (append v pathname-component)))))
              (setf variants
                    (iter (for v in variants)
                          (collect (append v pathname-component))))))))

(defun guess-module-name (distributor-name pathname-component-list)
  "Interpret DISTRIBUTOR-NAME and PATHNAME-COMPONENT-LIST as components
of a module URL and try to deduce name of the module."
  (declare (ignore distributor-name))
  (let* ((last (lastcar pathname-component-list))
         (name (if-let ((tail (search ".git" last)))
                 (subseq last 0 tail)
                 last)))
    (make-keyword (string-upcase name))))



(defun match-module-url-components-against-remote-set (raw-distributor-name port pcl raw-module-name remotes)
  (multiple-value-bind (dist domain-name-takeover) (string-detect-splitsubst raw-distributor-name raw-module-name '*module*)
    (let* ((variants (compute-remote-path-variants pcl raw-module-name))
           (remote (iter (for remote-path-variant in variants)
                         (let ((remote-path-variant (append (when domain-name-takeover dist) remote-path-variant)))
                           (when-let ((remote (dolist (r remotes)
                                                (when (and (equalp remote-path-variant (remote-path r))
                                                           (eql port (remote-distributor-port r)))
                                                  (return r)))))
                             (return remote))))))
      (values remote
              domain-name-takeover
              ;; choose the last path variant, which doesn't refer to *UMBRELLA*, by construction
              (unless remote (append (when domain-name-takeover dist) (lastcar variants)))))))

(defun module-ensure-distributor-match-remote (remote-type distributor-name port pathname-component-list &optional module-name)
  "Given a REMOTE-TYPE, DISTRIBUTOR-NAME, PORT, PATH and an optional
MODULE-NAME, try to first find matching distributor, creating it, if it
doesn't exist and then, match the remote, yielding them as multiple values.

Additionally, when no matching remote is present, the best module path
is determined.
Further, it is determined if distributor's hostname depends on module
name.
These additional values are returned as multiple values."
  (let ((module-name (or module-name (guess-module-name distributor-name pathname-component-list))))
    (multiple-value-bind (distributor created-distributor-p) (or (find-distributor-fuzzy distributor-name)
                                                                 (values (make-instance 'distributor :name (read-from-string distributor-name)) t))
      (multiple-value-call #'values
        distributor created-distributor-p
        (match-module-url-components-against-remote-set
         distributor-name port pathname-component-list (downstring module-name)
         (remove-if-not (curry #'remote-types-compatible-p remote-type) (distributor-remotes distributor)))))))

(defun make-remote (type domain-name-takeover distributor port path &key name)
  (flet ((query-remote-name (default-name)
           (format *query-io* "No matching remote found, has to create a new one, but the default name (~A) is occupied. Enter the new remote name, or NIL to abort: " default-name)
           (finish-output *query-io*)
           (or (prog1 (read *query-io*)
                 (terpri *query-io*))
               (return-from make-remote nil))))
    (make-instance type
                   :distributor distributor :domain-name-takeover domain-name-takeover :distributor-port port 
                   :name (or (prog1 name
                               (when name
                                 (format t ";; Choosing provided remote name ~S~%" name)))
                             (multiple-value-bind (default-name conflicted-default-name) (choose-default-remote-name distributor (vcs-type type) (transport type))
                               (if default-name
                                   (progn (format t ";; Choosing default remote name ~S~%" default-name)
                                          default-name)
                                   (query-remote-name conflicted-default-name))))
                   :module-names nil
                   :path path)))

(defun add-module-remote (url &optional module-name &key remote-name gate-p)
  "Assume MODULE-NAME is the last path element of URL, unless specified."
  (multiple-value-bind (remote-type cred distributor-name port raw-path) (parse-remote-namestring url :gate-p gate-p)
    (let* ((module-name (or module-name (guess-module-name distributor-name raw-path)))
           (downmodname (downstring module-name)))
      (when (module module-name :if-does-not-exist :continue)
        (error "~@<Module ~A already exists.~:@>" module-name))
      (multiple-value-bind (distributor created-distributor-p) (or (lret ((d (find-distributor-fuzzy distributor-name)))
                                                                     (when d
                                                                       (format t ";; Found ~A, looking for a corresponding remote..~%" (name d))))
                                                                   (format t ";; Didn't find a corresponding distributor, created ~A and a corresponding remote~%;; ..~%" distributor-name)
                                                                   (values (make-instance 'distributor :name distributor-name) t))
        (let* ((downdistname (downstring distributor-name))
               (variants (compute-remote-path-variants raw-path downmodname)))
          (multiple-value-bind (dist remote-takeover) (string-detect-splitsubst downdistname downmodname '*module*)
            (multiple-value-bind (remote created-remote-p)
                (or (iter (for remote-path-variant in variants)
                          (let ((remote-path-variant (append (when remote-takeover dist) remote-path-variant)))
                            (when-let ((remote (do-distributor-remotes (r distributor)
                                                 (when (equalp remote-path-variant (remote-path r))
                                                   (return r)))))
                              (format t ";; Found remote with a matching path, name's ~S~%" (name remote))
                              (return remote))))
                    (values (flet ((query-remote-name ()
                                     (format *query-io* "No matching remote found, has to create a new one, but the default name is occupied. Enter the new remote name, or NIL to abort: ")
                                     (finish-output *query-io*)
                                     (or (read *query-io*)
                                         (return-from add-module-remote nil))))
                              (format t ";; Couldn't find a remote with a matching path, making a new one.~%")
                              (make-instance remote-type
                                             :distributor distributor :domain-name-takeover remote-takeover :distributor-port port 
                                             :name (or (prog1 remote-name
                                                         (when remote-name
                                                           (format t ";; Choosing provided remote name ~S~%" remote-name)))
                                                       (lret ((default-name (choose-default-remote-name distributor (vcs-type remote-type))))
                                                         (when default-name
                                                           (format t ";; Choosing default remote name ~S~%" default-name)))
                                                       (query-remote-name))
                                             :module-names nil
                                             ;; choose the last path variant, which doesn't refer to *UMBRELLA*, by construction
                                             :path (append (when remote-takeover dist) (lastcar variants))))
                            t))
              (unless (remote-types-compatible-p remote-type (type-of remote))
                (error "~@<Found remote ~S, but its type ~S doesn't match type ~S.~:@>"
                       (name remote) (type-of remote) remote-type))
              (format t "deduced: ~S @ ~S :: ~S:~D / ~S => ~%~S~%"
                      module-name remote-type (name distributor) port raw-path remote)
              (values module-name distributor remote))))))))

(defparameter *auto-lust* nil)

(defmacro with-tracked-desirable-additions ((form-value added-dists added-rems added-mods added-syss added-apps) tracked-form &body body)
  (with-gensyms (orig-distributors orig-remotes orig-modules orig-systems orig-apps)
    `(multiple-value-bind (,form-value ,added-dists ,added-rems ,added-mods ,added-syss ,added-apps)
         (let ((,orig-distributors (hash-table-values *distributors*))
               (*distributors* (copy-hash-table *distributors*))
               (,orig-remotes (hash-table-values *remotes*))
               (*remotes* (copy-hash-table *remotes*))
               (,orig-modules (hash-table-values *modules*))
               (*modules* (copy-hash-table *modules*))
               (,orig-systems (hash-table-values *systems*))
               (*systems* (copy-hash-table *systems*))
               (,orig-apps (hash-table-values *apps*))
               (*apps* (copy-hash-table *apps*)))
           (values ,tracked-form
                   (set-difference (hash-table-values *distributors*) ,orig-distributors)
                   (set-difference (hash-table-values *remotes*) ,orig-remotes)
                   (set-difference (hash-table-values *modules*) ,orig-modules)
                   (set-difference (hash-table-values *systems*) ,orig-systems)
                   (set-difference (hash-table-values *apps*) ,orig-apps)))
       ,@body)))

(defun inject-desirables (d r m s a)
  (format t "~:[~;injecting distributors:~]~:*~{ ~A~}~:*~:[~;~%~]" (mapcar #'name d))
  (dolist (d d) (setf (distributor (name d)) d))
  (format t "~:[~;injecting modules:~]~:*~{ ~A~}~:*~:[~;~%~]" (mapcar #'name m))
  (dolist (m m) (setf (module      (name m)) m))
  (format t "~:[~;injecting remotes:~]~:*~{ ~A~}~:*~:[~;~%~]" (mapcar #'name r))
  (dolist (r r) (setf (remote      (name r)) r))
  (format t "~:[~;injecting systems:~]~:*~{ ~A~}~:*~:[~;~%~]" (mapcar #'name s))
  (dolist (s s) (setf (system      (name s)) s))
  (format t "~:[~;injecting apps:~]~:*~{ ~A~}~:*~:[~;~%~]" (mapcar #'name a))
  (dolist (a a) (setf (app         (name a)) a)))

(defun add-module (url &optional module-name &key systemlessp remote-name path-whitelist path-blacklist (system-type *default-system-type*) (lust *auto-lust*))
  (with-tracked-desirable-additions (deduced-module-name added-d added-r added-m added-s added-a)
      (multiple-value-bind (module-name distributor remote) (add-module-remote url module-name :remote-name remote-name)
        (declare (ignore distributor))
        (let ((module (make-instance 'module :name module-name :umbrella module-name :path-whitelist path-whitelist :path-blacklist path-blacklist)))
          (remote-link-module remote module)
          (when lust
            (let ((*fetch-errors-serious* t))
              (lust (name module))))))
    (inject-desirables added-d added-r added-m added-s added-a)))

(defun add-module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (url &key name remote-name path-whitelist path-blacklist (lust *auto-lust*)) (ensure-cons (read stream nil nil t))
    (add-module url name :lust lust :remote-name remote-name :path-whitelist path-whitelist :path-blacklist path-blacklist)))

(defun install-add-module-reader (&optional (char #\@))
  (set-dispatch-macro-character #\# char #'add-module-reader *readtable*))

(defun recognise-clbuild-remote (remote-nickname)
  (when-let ((remote-name (case remote-nickname
                            (get_svn_clnet 'common-lisp.net-svn-rsync)
                            (get_cvs_clnet 'common-lisp.net-cvs-rsync)
                            (get_ediware 'common-lisp.net-luis-ediware)
                            (get_lichteblau_com 'www.lichteblau.com)
                            (get_b9_com 'b9.com)
                            (get_clbuild_mirror 'common-lisp.net-clbuild-mirror)
                            (get_xach_com 'git.xach.com))))
    (remote remote-name)))

(defun steal-clbuild-projects-file (filename)
  (let ((*package* #.*package*)
        (*read-eval* nil)
        (new-remotes (make-hash-table)))
    (with-container new-remotes (new-remotes :type list :iterator do-new-remotes :iterator-bind-key t :if-does-not-exist :continue :if-exists :continue)
      (with-open-file (s filename)
        (iter (for raw-string = (read-line s nil nil))
              (while raw-string)
              (for string = (subseq raw-string 0 (position #\# raw-string)))
              (when (zerop (length string))
                (next-iteration))
              (for (values module-name offset0) = (read-from-string string nil nil :start 0))
              (unless module-name
                (next-iteration))
              (for module = (module module-name :if-does-not-exist :continue))
              (for (values remote-name offset1) = (read-from-string string nil nil :start offset0))
              (for known-remote = (recognise-clbuild-remote remote-name))
              (unless remote-name
                (error "~@<Malformed directive: no remote specifier for module ~A~:@>" module-name))
              (let* ((spacepos (position #\Space string :start offset1))
                     (url (subseq string offset1 spacepos))
                     (posturl (let ((*readtable* (copy-readtable)))
                                (setf (readtable-case *readtable*) :preserve)
                                (when spacepos (when-let ((posturl (read-from-string string nil nil :start spacepos)))
                                                 (princ-to-string posturl))))))
                (multiple-value-bind (remote cred)
                    (or known-remote
                        (multiple-value-bind (type cred hostname port path) (parse-remote-namestring url
                                                                                                     :slashless (eq remote-name 'get_cvs_full)
                                                                                                     :type-hint (case remote-name
                                                                                                                  (get_git 'git)
                                                                                                                  (get_darcs 'darcs)
                                                                                                                  (get_svn 'svn)))
                          ;; (format t "===( ~S ~S ~S, m ~S~%" type hostname path module-name)
                          (multiple-value-bind (distributor created-distributor-p remote domain-name-takeover deduced-path)
                              (module-ensure-distributor-match-remote type hostname port path module-name)
                            (unless (and module (or known-remote remote))
                              (syncformat t ";; =======================================~%"))
                            (when created-distributor-p
                              (format t ";; didn't find distributor at ~A, creating it~%" hostname))
                            ;; (format t "===( ~S => ~S ~S~:[~;(taken over)~]~:[~;~:*~D~]/~S ~S=~%" string type hostname domain-name-takeover port path posturl)
                            (values (or remote
                                        (progn
                                          (format t "~@<;; ~@;didn't find a remote for ~S (path ~S) on ~A, creating it~:@>~%" url (or deduced-path path) (name distributor))
                                          (when-lret ((new-remote (make-remote type domain-name-takeover distributor port (or deduced-path path))))
                                            (push (list new-remote url module-name) (new-remotes distributor))))
                                        (format t "~@<;; ~@;required remote for ~A on ~A not created, skipping module~:@>~%" path (name distributor))
                                        (next-iteration))
                                    cred))))
                  (unless (find module-name (location-module-names remote) :test #'string=)
                    (let ((module (or (module module-name :if-does-not-exist :continue)
                                      (format t "~@<;; ~@;didn't find module ~A, creating it~:@>~%" module-name)
                                      (make-instance 'module :name module-name :umbrella module-name))))
                      (remote-link-module remote module)
                      (format t "~@<;; ~@;linking module ~A to remote ~A on ~A~:@>~%" module-name (name remote) (name (remote-distributor remote)))
                      (when cred
                        (push (list module-name (cred-name cred)) (remote-module-credentials remote)))
                      (when (typep remote 'cvs)
                        (let* ((default-cvs-module-name (downstring module-name))
                               (cvs-module-name (or posturl default-cvs-module-name)))
                          (unless (string= cvs-module-name default-cvs-module-name)
                            (push (list module-name cvs-module-name) (wrinkles remote)))))))))))
      (do-new-remotes (distributor new-remotes)
        (flet ((module-url-reformulable (name url current remotes)
                 (multiple-value-bind (type cred hostname port path) (parse-remote-namestring url :slashless (typep current 'cvs-native-remote) :type-hint (vcs-type current))
                   (declare (ignore cred)) ; XXX: is it the right thing to do?
                   (match-module-url-components-against-remote-set
                    hostname port path (downstring name)
                    (remove-if-not (curry #'remote-types-compatible-p type) remotes)))))
          (iter (for (new-remote module-url module-name) in new-remotes)
                (when-let* ((module-name (when (null (rest (location-module-names new-remote))) ;; we're after single-module remotes
                                           (first (location-module-names new-remote))))
                            (better-remote (module-url-reformulable module-name module-url new-remote
                                                                    (remove new-remote (mapcar #'car new-remotes)))))
                  (syncformat t ";; =======================================~%")
                  (format t "~@<;; ~@;remote ~A is excessive, its module (~A) is better reformulated in ~A/~S~:@>~%"
                          (name new-remote) module-name (name better-remote) (remote-path better-remote))
                  (remote-link-module better-remote (module module-name)
                                      :module-module (when (typep new-remote 'cvs)
                                                       (cvs-remote-module-module new-remote module-name)))
                  (remove-remote new-remote :keep-modules t))))))))