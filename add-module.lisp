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


(defvar *auto-lust* nil
  "Whether to automatically LUST the modules during ADD-MODULE.")
(defvar *verbose-remote-matching* nil
  "Whether to comment on the progress of matching provided URLs vs. candidate remotes.")

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
          (definition-error "~@<Requested to make ~S a gate remote of ~A, but it is not a remote of gate type, i.e. ~A.~:@>"
              r hostname *gate-vcs-type*))
        (setf (gate d) r)))))

(defun subdomain-p (tentative-superdomain domain)
  "Determine whether DOMAIN is a subdomain of TENTATIVE-SUPERDOMAIN, and, in case it is,
return the subdomain part of DOMAIN before the dot.  Otherwise return NIL."
  (let ((posn (search tentative-superdomain domain :start2 1)))
    (when (and posn (char= #\. (schar domain (1- posn)))
               (= (+ posn (length tentative-superdomain)) (length domain)))
      (subseq domain 0 (1- posn)))))

(defun find-distributor-fuzzy (distname &aux
                               (downcase-distname (downstring distname)))
  "Find a distributor which looks like a match for DISTNAME.
For example, domain name OGRE.SVN.SOURCEFORGE.NET refers to a distributor
called SVN.SOURCEFORGE.NET, as the remote specification takes over
domain name generation.
The secondary value returned is a boolean specifying whether
the match was exact.
The ternary value returned designates the "
  (if-let ((distributor (distributor distname :if-does-not-exist :continue)))
    (values distributor t)
    (do-distributors (d)
      (when-let ((subdomain (subdomain-p (down-case-name d) downcase-distname)))
        (return (values d nil subdomain))))))

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

(defun compute-remote-path-variants (raw-path dirp modname)
  "Given a RAW-PATH pathname component list, a boolean specifier DIRP denoting
whether RAW-PATH designates a directory and MODNAME, produce a list of
pathname component list variants, with MODNAME occurences substituted."
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
                          (collect (append v pathname-component))))))
    (unless dirp
      (setf variants (mapcar (rcurry #'append '(:no/)) variants)))))

(defun guess-module-name (distributor-name pathname-component-list)
  "Interpret DISTRIBUTOR-NAME and PATHNAME-COMPONENT-LIST as components
of a module URL and try to deduce name of the module."
  (declare (ignore distributor-name))
  (let* ((last (lastcar pathname-component-list))
         (name (cond ((when-let ((tail (search ".git" last)))
                        (subseq last 0 tail)))
                     ((member last '("svn" "cvsroot") :test #'equal)
                      (first (last pathname-component-list 2)))
                     (t
                      last))))
    (make-keyword (string-upcase name))))



(defun match-module-url-components-against-remote-set (raw-distributor-name subdomain port pathname-component-list dirp raw-module-name remotes)
  (multiple-value-bind (dist domain-name-takeover) (if subdomain
                                                       (string-detect-splitsubst raw-distributor-name subdomain '*umbrella*)
                                                       (string-detect-splitsubst raw-distributor-name raw-module-name '*module*))
    (let* ((variants (compute-remote-path-variants pathname-component-list dirp raw-module-name))
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

(defun module-ensure-distributor-match-remote (remote-type distributor-name port pathname-component-list dirp &optional module-name)
  "Given a REMOTE-TYPE, DISTRIBUTOR-NAME, PORT, PATH, DIRP and an optional
MODULE-NAME, try to first find a matching distributor, and, when found,
a matching remote within it.
When no distributor successfully matched, a new one is created.
When no remote was matched, or no distributor existed, a corresponding remote
is not created, but rather a path looking like a best match for the parameters
provided is computed.
The values returned are: 
  - the distributor, either matched or created;
  - a boolean designating whether the distributor was created;
  - the matched remote or NIL;
  - a boolean designating whether the domain name is a function of the module;
  - a best-looking remote path specifier, when no remote was matched."
  (let ((module-name (or module-name (guess-module-name distributor-name pathname-component-list))))
    (multiple-value-bind (existing-distributor distributor-matched-exactly subdomain) (find-distributor-fuzzy distributor-name)
      (let ((distributor (or existing-distributor (make-instance 'distributor :name (read-from-string distributor-name)))))
        (when *verbose-remote-matching*
          (format t "~@<;;; ~@;~@<F~;~:[ailed to find a match~*~;ound a~:[ fuzzy~;n exact~] match~] for provided distributor name ~:@(~A~)~:[~;, as subdomain ~:*~:@(~A~) of ~A~]~:@>~:@>~%"
                  existing-distributor distributor-matched-exactly distributor-name subdomain (when subdomain
                                                                                                (name existing-distributor))))
        (multiple-value-bind (remote domain-name-takeover-by-remote-match deduced-path)
            (match-module-url-components-against-remote-set distributor-name subdomain port pathname-component-list dirp (downstring module-name)
                                                            (remove-if-not (curry #'remote-types-compatible-p remote-type)
                                                                           (distributor-remotes distributor)))
          (let* ((domain-name-takeover-by-fuzzy-distributor (and existing-distributor (not distributor-matched-exactly)))
                 (domain-name-takeover (or domain-name-takeover-by-fuzzy-distributor domain-name-takeover-by-remote-match))
                 (fallback-path (and (not remote)
                                     (if domain-name-takeover-by-fuzzy-distributor
                                         deduced-path
                                         deduced-path))))
            (values distributor (not existing-distributor)
                    remote domain-name-takeover fallback-path)))))))

(defun make-remote (type domain-name-takeover distributor port path &key name)
  (flet ((query-remote-name (default-name)
           (format *query-io* "No matching remote found, has to create a new one, but the default name (~A) is occupied.~%~
                               Enter the new remote name, or NIL to abort: " default-name)
           (finish-output *query-io*)
           (or (prog1 (read *query-io*)
                 (terpri *query-io*))
               (return-from make-remote nil))))
    (make-instance type
                   :distributor distributor :domain-name-takeover domain-name-takeover :distributor-port port 
                   :name (or (prog1 name
                               (when name
                                 (format t ";; Choosing provided remote name ~S~%" name)))
                             (multiple-value-bind (default-name conflicted-default-name) (choose-default-remote-name
                                                                                          distributor (vcs-type type) (transport type))
                               (if default-name
                                   (progn (format t ";; Choosing default remote name ~S~%" default-name)
                                          default-name)
                                   (query-remote-name conflicted-default-name))))
                   :module-names nil
                   :path path)))

(defun ensure-url-remote (url &optional (module-name nil module-name-provided-p) &key remote-name gate-p vcs-type-hint)
   "Given an URL and an optionally specified MODULE-NAME, try to ensure
existence of a remote corresponding to the URL.
This is done by first finding a matching distributor, or creating it, if it
doesn't exist, then, finding a matching remote, or creating it as well.

The values returned are:
   - the remote, matching or created, or NIL if the creation attempt failed,
   - the credentials extracted from the URL, if any,
   - the module name, which was either deduced, or specified, and
   - a boolean specifying, when a remote is returned, whether it was
     created anew."
  (when *verbose-remote-matching*
    (format t "~@<;;; ~@;Tr~@<ying to internalise URL ~S~:[~; for module ~:*~:@(~A~)~]~:[~;, with a provided hint of remote's type being ~:*~A~].~:@>~:@>~%"
            url module-name vcs-type-hint))
  (multiple-value-bind (type cred hostname port path dirp) (parse-remote-namestring url :slashless (search ":pserver" url) :type-hint vcs-type-hint :gate-p gate-p)
    (let ((module-name (or module-name (guess-module-name hostname path))))
      (when *verbose-remote-matching*
        (format t "~@<;;; ~@;De~@<duced remote type ~A~:[~;, credentials ~:*~A~], hostname ~:@(~A~)~:[~;, port ~:*~D~], path ~S with~:[out~;~] a trailing slash. ~
                             Module name ~A was ~:[guessed~;provided~].~:@>~:@>~%"
                type cred hostname port path dirp module-name module-name-provided-p))
      (multiple-value-bind (dist created-dist-p remote domain-name-takeover deduced-path) (module-ensure-distributor-match-remote
                                                                                           type hostname port path dirp module-name)
        (when created-dist-p
          (when *verbose-remote-matching*
            (format t "~@<;;; ~@;Didn't find distributor at ~A, creating it.~:@>~%" hostname)))
        (let ((new-remote (unless remote
                            (when *verbose-remote-matching*
                              (format t "~@<;;; ~@;Di~@<dn't find a remote for ~S (path ~S) on ~A, creating it.~:@>~:@>~%"
                                      url (or deduced-path path) (name dist)))
                            (make-remote type domain-name-takeover dist port (or deduced-path path) :name remote-name))))
          (values (or remote new-remote)
                  cred
                  module-name
                  (not (null new-remote))))))))

(defun ensure-remote-module (remote module-name &key credentials path-whitelist path-blacklist)
  (lret ((module (or (module module-name :if-does-not-exist :continue)
                     (make-instance 'module :name module-name :umbrella module-name :path-whitelist path-whitelist :path-blacklist path-blacklist))))
    (unless (find module-name (location-module-names remote))
      (remote-link-module remote module))
    (when credentials
      (push (list module-name (cred-name credentials)) (remote-module-credentials remote)))))

(defun add-module (url &optional module-name &key remote-name path-whitelist path-blacklist (if-touch-fails :error) vcs-type (lust *auto-lust*))
  (multiple-value-bind (remote cred module-name created-remote-p) (ensure-url-remote url module-name :remote-name remote-name :vcs-type-hint vcs-type)
    (if remote
        (multiple-value-bind (successp output) (touch-remote-module remote module-name)
          (unless successp
            (when (and created-remote-p (not (member if-touch-fails '(:warn :continue))))
              (remove-remote remote))
            (ecase if-touch-fails
              (:error (definition-error "~@<Failed to reach module ~A via remote deduced from URL ~S:~%~S.~:@>" module-name url output))
              (:abort (return-from add-module nil))
              (:warn (format t "~@<;; ~@;Failed to reach module ~A via remote deduced from URL ~S:~%~S.~:@>" module-name url output))
              (:continue)))
          (lret ((module (ensure-remote-module remote module-name :credentials cred :path-whitelist path-whitelist :path-blacklist path-blacklist)))
            (when lust
              (let ((*fetch-errors-serious* t))
                (lust (name module))))))
        (format t "~@<;; ~@;Re~@<mote for ~A was not created~:@>~:@>~%" url))))

(defun add-module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (url &key name remote-name vcs-type path-whitelist path-blacklist (if-touch-fails :error) (lust *auto-lust*)) (ensure-cons (read stream nil nil t))
    (add-module url name :if-touch-fails if-touch-fails :lust lust :remote-name remote-name :vcs-type vcs-type :path-whitelist path-whitelist :path-blacklist path-blacklist)))

(defun install-add-module-reader (&optional (char #\@))
  (set-dispatch-macro-character #\# char 'add-module-reader *readtable*))

(defun add-module-local (name &optional (mode :publish) (locality (gate *self*)) &key path-whitelist path-blacklist)
  (let ((name (canonicalise-module-name name)))
    (unless (typep locality 'gate)
      (locality-error locality "~@<Asked to add module ~A to a non-gate ~A.~:@>" name locality))
    (when (location-defines-module-p locality name)
      (module-error name "~@<Module ~A is already provided by ~A.~:@>" name (locality-pathname locality)))
    (let ((repo-dir (module-pathname name locality)))
      (unless (git-repository-present-p repo-dir t)
        (module-error name "~@<Module ~A doesn't appear to have a repository with objects in ~S.~:@>" name repo-dir)))
    (let* ((present (module name :if-does-not-exist :continue))
           (m (or present (make-instance 'module :name name :umbrella name :path-whitelist path-whitelist :path-blacklist path-blacklist))))
      (ecase mode
        (:publish (location-link-module locality m))
        (:convert (push name (gate-converted-module-names locality)))
        (:unpublished (push name (gate-unpublished-module-names locality)))
        (:hidden (push name (gate-hidden-module-names locality))))
      (notice-module-repository m locality)
      (desire (list name) :skip-present t :seal nil))))