;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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


(define-executable gzip)
(define-executable gpg)

(defun export-signed-module-tarball (module gpg-homedir &optional gpg-agent-info ref target-locality locality &aux
                                     (gpg-agent-info (or gpg-agent-info (sb-posix:getenv "GPG_AGENT_INFO")))
                                     (ref (or ref '("master")))
                                     (module (coerce-to-module module))
                                     (target-locality (or target-locality (local-tarball *self*)))
                                     (locality (or locality (gate *self*))))
  (maybe-within-directory (module-pathname module locality)
    (let ((output-filename (subfile (locality-pathname target-locality)
                                    `(,(concatenate 'string (down-case-name module) "_latest.tar"))
                                    :type "gz")))
      (with-output-to-file (*output* output-filename)
        (pipe (git "archive" `("--prefix=" ,(down-case-name module) "_latest/") (cook-refval ref))
              (gzip)))
      (let ((*environment* `(,(format nil "GPG_AGENT_INFO=~A" gpg-agent-info))))
        (with-avoided-executable-output ()
          (gpg "--homedir" gpg-homedir "--use-agent" "--detach-sign" "--armor" "--yes" (namestring output-filename)))))))