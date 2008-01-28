(in-package :cling)

(defdistributor feelingofgreen.ru
  (:modules
   (local-module
    bin pergamum semi-precious lispdb cling bintype elf cl-io-mod captured-stream doc dwarf
    ieee-latex clxs sb-x86-portio
    mcdust-one mclinux mdb newlib pestilence x86-linux x86-win32)))

(defdistributor common-lisp.net
  (:url-schemas
   (rsync (module) "common-lisp.net" "project" (umbrella-name module) "cvsroot")
   (http (module) "common-lisp.net" "project" (umbrella-name module) "darcs" (named-namestring module)))
  (:modules
   (cvs-module
    flexichain mcclim zip (cxml cxml closure-common) closure gsharp climacs (slime (slime :asdf-name swank)) beirc eclipse
    (cl-jpeg cljl)
    (cl-plus-ssl cl+ssl trivial-gray-streams))
   (darcs-module
    alexandria bordeaux-threads climplayer plexippus-xpath iterate
    (closer closer-mop lw-compat)
    (clbuild cl-webdav skippy salza trivial-sockets split-sequence rfc2388 psgraph parse-number net-telent-date))
   (svn-module
    (usocket/trunk usocket) (trunk cl-irc) graphic-forms)))

(defdistributor christophe
  (:url-schemas
   (http (module) "common-lisp.net" "~crhodes" (named-namestring module)))
  (:modules
   (darcs-module
    clx)))

(defdistributor luis
  (:url-schemas
   (http (module) "common-lisp.net" "~loliveira" "darcs" (umbrella-name module)))
  (:modules
   (darcs-module
    trivial-features babel (cffi+lotsastuff cffi))))

(defdistributor ediware@luis
  (:url-schemas
   (http (module) "common-lisp.net" "~loliveira" "ediware" (named-namestring module)))
  (:modules
   (darcs-module
    cl-ppcre flexi-streams cl-fad hunchentoot chunga url-rewrite cl-who drakma)))

(defdistributor repo.or.cz
  (:url-schemas
   (git (module) "repo.or.cz" (format nil "~A.git" (named-namestring module))))
  (:modules
   (git-module
    darcs2git closure-html)))

(defdistributor lichteblau.com
  (:url-schemas
   (http (module) "www.lichteblau.com" "git" (format nil "~A.git" (named-namestring module))))
  (:modules
   (git-http-module
    cxml-rng cxml-stp clim-alerts)))

(defdistributor lichteblau@clnet
  (:url-schemas
   (http (module) "common-lisp.net" "~dlichteblau" "inofficial" (named-namestring module)))
  (:modules
   (darcs-module
    mel-base)))

(defdistributor sullivan
  (:url-schemas
   (http (module) "rvw.doc.gold.ac.uk" "sullivan" "darcs" (named-namestring module)))
  (:modules
   (darcs-module
    midi spatial-trees)))

(defdistributor b9.com
  (:url-schemas
   (git (module) (or ("b9.com" (format nil "~A.git" (named-namestring module)))
                     ;; ("lycia.ath.cx" (named-namestring module)) ;; works like a tarpit
                     ))) ;; Stelian says it's a daily mirror
  (:modules
   (git-module
    puri md5 cl-base64 clsql rt)))

(defdistributor fractalconcept.com
  (:url-schemas
   (http (module) "www.fractalconcept.com:8000" "public" "open-source" (named-namestring module)))
  (:modules
   (svn-module
    cl-pdf cl-typesetting)))

(defdistributor www.pps.jussieu.fr
  (:url-schemas
   (http (module) "www.pps.jussieu.fr" "~jch" "software" "repos" (named-namestring module)))
  (:modules
   (darcs-module
    cl-yacc)))

(defdistributor tuxee.net
  (:url-schemas
   (git (module) "git.tuxee.net" (named-namestring module)))
  (:modules
   (git-module
    cl-vectors)))

(defdistributor xach.com
  (:url-schemas
   (git (module) "git.xach.com" (format nil "~A.git" (named-namestring module))))
  (:modules
   (git-module
    salza2 zpng vecto zpb-ttf)))

(defdistributor boinkor.net
  (:url-schemas
   (git (module) "sbcl.boinkor.net" (named-namestring module)))
  (:modules
   (git-module
    sbcl)))

;; TODO: acl-compat

(define-module-dependencies
  (babel rt trivial-features alexandria)
  (beirc trivial-features alexandria babel cffi spatial-trees flexichain slime mcclim usocket trivial-gray-streams flexi-streams cl-irc split-sequence cl-ppcre cl-fad)
  (cffi rt trivial-features alexandria babel)
  (chunga trivial-gray-streams flexi-streams)
  (cl+ssl trivial-features alexandria babel cffi trivial-gray-streams flexi-streams)
  (cl-irc split-sequence usocket trivial-gray-streams flexi-streams)
  (cl-pdf iterate)
  (cl-typesetting iterate cl-pdf)
  (cl-vectors zpb-ttf)
  (cl-webdav chunga cl-base64 cl-ppcre trivial-features alexandria babel cffi flexi-streams cl+ssl md5 rfc2388 url-rewrite hunchentoot closure-common puri trivial-gray-streams cxml cl-fad)
  (clim-alerts mel-base closure-html bordeaux-threads trivial-sockets salza2 zip skippy closure closure-common cxml parse-number cl-ppcre cl-yacc plexippus-xpath cxml-stp puri cl-base64 chunga split-sequence usocket trivial-gray-streams flexi-streams cl+ssl drakma net-telent-date clx trivial-features alexandria babel cffi spatial-trees flexichain slime mcclim)
  (climacs trivial-features alexandria babel cffi spatial-trees slime mcclim flexichain)
  (climplayer trivial-features alexandria babel cffi spatial-trees flexichain slime mcclim cl-ppcre split-sequence)
  (closer-mop lw-compat)
  (closure trivial-features alexandria babel cffi mcclim spatial-trees flexichain slime clx puri cxml closure-common closure-html bordeaux-threads trivial-sockets salza2 zip trivial-gray-streams flexi-streams skippy)
  (closure-common trivial-gray-streams)
  (closure-html closure-common trivial-gray-streams flexi-streams)
  (clsql md5 rt trivial-features alexandria babel cffi)
  (cxml closure-common puri trivial-gray-streams)
  (cxml-rng closure-common puri trivial-gray-streams cxml cl-ppcre cl-yacc parse-number cl-base64)
  (cxml-stp alexandria closure-common puri trivial-gray-streams cxml parse-number cl-ppcre cl-yacc plexippus-xpath)
  (drakma puri cl-base64 chunga split-sequence usocket trivial-features alexandria babel cffi trivial-gray-streams flexi-streams cl+ssl)
  (flexi-streams trivial-gray-streams)
  (graphic-forms trivial-features alexandria babel cffi lw-compat closer-mop)
  (gsharp trivial-features alexandria babel cffi spatial-trees slime mcclim flexichain midi closure-common puri trivial-gray-streams cxml)
  (hunchentoot cl-who chunga cl-base64 cl-ppcre trivial-features alexandria babel cffi trivial-gray-streams flexi-streams cl+ssl md5 rfc2388 url-rewrite)
  (mcclim trivial-features alexandria babel cffi spatial-trees flexichain slime clx)
  (mel-base trivial-features alexandria babel cffi trivial-gray-streams flexi-streams cl+ssl)
  (plexippus-xpath closure-common puri trivial-gray-streams cxml parse-number cl-ppcre cl-yacc)
  (trivial-features rt babel cffi alexandria)
  (usocket split-sequence)
  (vecto cl-vectors salza2 zpng zpb-ttf)
  (zip salza2 trivial-gray-streams flexi-streams)
  (zpng salza2))

(minimise-dependencies)