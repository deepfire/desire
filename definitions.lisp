;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
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

(in-package :cling)


(reinit-definitions)

(defdistributor feelingofgreen.ru
  (:url-schemas (git (mod) "git.feelingofgreen.ru" (downstring (name mod))))
  (:modules
   (git
    pergamum semi-precious cling clung)))

(defdistributor common-lisp.net
  (:url-schemas (rsync (mod) "common-lisp.net" "project" (downstring (module-umbrella mod)))
                (http (mod) "common-lisp.net" "project" (downstring (module-umbrella mod)) "darcs" (downstring (name mod))))
  (:modules
   (darcs
    alexandria iterate)))

(defdistributor sanityinc.com
  (:url-schemas (git (mod) "git.sanityinc.com" (format nil "~A" (downstring (name mod)))))
  (:modules
   (git (darcs-to-git (darcs-to-git :systems nil)))))

(define-module-dependencies
  (pergamum alexandria iterate)
  (semi-precious alexandria pergamum)
  (cling pergamum semi-precious)
  (clung cling))

(minimise-dependencies *leaves*)
