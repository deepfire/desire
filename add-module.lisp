;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008 by
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


(defun find-distributor-fuzzy (distname &aux (downcase-distname (downstring distname)))
  "Find a distributor which looks like a match for DISTNAME.
For example, domain name OGRE.SVN.SOURCEFORGE.NET refers to a distributor
called SVN.SOURCEFORGE.NET, as the remote specification takes over
domain name generation."
  (or (distributor distname :if-does-not-exist :continue)
      (do-distributors (d)
        (when (search (down-case-name d) downcase-distname)
          (return d)))))

(defun string-detect-splitsubst (string sub replacement &aux (posn (search sub string)))
  "Detect whether STRING contains SUB, in which case the string is
split, on SUB boundaries, with the latter replaced with REPLACEMENT,
and every part except the first wrapped in remote /-suppressing
syntax."
  (if posn
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
          (for (values pathname-component deconsp) = (string-detect-splitsubst path-elt modname '*module*))
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

(defun add-module (url &optional module-name &key systemless (system-type *default-system-type*))
  "Assume MODULE-NAME is the last path element of URL, unless specified."
  (multiple-value-bind (remote-type distributor-name port raw-path) (parse-remote-namestring url)
    (let* ((module-name (or module-name (make-keyword (string-upcase (lastcar raw-path)))))
           (downmodname (downstring module-name)))
      (when (module module-name :if-does-not-exist :continue)
        (error "~@<Module ~A already exists.~:@>" module-name))
      (let* ((distributor (or (find-distributor-fuzzy distributor-name)
                              (format t ";; Didn't find a corresponding distributor, created ~A and a corresponding remote~%;; ..~%" distributor-name)
                              (make-instance 'distributor :name distributor-name)))
             (downdistname (downstring distributor-name)))
        (format t ";; Found distributor ~A, looking for a corresponding remote..~%" (name distributor))
        (multiple-value-bind (dist remote-takeover) (string-detect-splitsubst downdistname downmodname '*module*)
          (let ((variants (compute-remote-path-variants raw-path downmodname)))
            (let ((remote (or (iter (for remote-path-variant in variants)
                                    (let ((remote-path-variant (append (when remote-takeover dist) remote-path-variant)))
                                      (when-let ((remote (do-distributor-remotes (r distributor)
                                                           (when (equalp remote-path-variant (remote-path r))
                                                             (return r)))))
                                        (return remote))))
                              (make-instance 'git-remote
                                             :distributor distributor :domain-name-takeover remote-takeover :distributor-port port 
                                             :name (progn
                                                     (format *query-io* "Tried remote variants:~%")
                                                     (iter (for remote-path-variant in variants)
                                                           (format *query-io* "~S~%"(append (when remote-takeover dist) remote-path-variant))
                                                           (let ((remote-path-variant (append (when remote-takeover dist) remote-path-variant)))
                                                             (when-let ((remote (do-distributor-remotes (r distributor)
                                                                                  (when (equalp remote-path-variant (remote-path r))
                                                                                    (return r)))))
                                                               (return remote))))
                                                     (format *query-io* "Against following remotes of ~S:~%" (name distributor))
                                                     (do-distributor-remotes (r distributor)
                                                       (format *query-io* "~S~%" (remote-path r)))
                                                     (format *query-io* "No matching remote found, creating a new one. Enter the new remote name, or NIL to abort: ")
                                                     (finish-output *query-io*)
                                                     (or (read *query-io*)
                                                         (return-from add-module nil)))
                                             ;; choose the last path variant, which doesn't refer to *UMBRELLA*, by construction
                                             :path (lastcar variants)))))
              (unless (eq remote-type (type-of remote))
                (error "~@<Found remote ~S, but its type ~S doesn't match type ~S derived from specified type ~S.~:@>"
                       (name remote) (type-of remote) remote-type remote-type))
              (lret ((module (make-instance 'module :name module-name :umbrella module-name)))
                (appendf (location-modules remote) (list module-name))
                (unless systemless
                  (make-instance system-type :name module-name :module module))
                (format t "deduced: ~S @ ~S :: ~S:~D / ~S => ~%~S~%"
                        module-name remote-type (name distributor) port raw-path remote)))))))))