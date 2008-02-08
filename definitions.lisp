(in-package :cling)

(defdistributor feelingofgreen.ru
  (:url-schemas
   (git (repo) "git.feelingofgreen.ru" (downstring (name repo))))
  (:modules
   (local
    pergamum semi-precious lispdb cling bintype elf cl-io-mod captured-stream dwarf
    ieee-latex clxs sb-x86-portio)))

(defdistributor freedesktop.org
  (:url-schemas
   (git (repo) "anongit.freedesktop.org" "git" "xorg" "lib" (downstring (name repo))))
  (:modules
   (git libpciaccess)))

(defdistributor common-lisp.net
  (:url-schemas
   (rsync (repo) "common-lisp.net" "project" (downstring (repo-umbrella repo)) "cvsroot")
   (http (repo) "common-lisp.net" "project" (downstring (repo-umbrella repo)) "darcs" (downstring (name repo))))
  (:modules
   (cvs
    flexichain mcclim zip (cxml cxml closure-common) closure gsharp climacs (slime (slime :asdf-name swank)) beirc eclipse (linedit (linedit :cvs-module src))
    (cl-jpeg cljl)
    (cl-plus-ssl cl+ssl trivial-gray-streams))
   (darcs
    alexandria bordeaux-threads climplayer plexippus-xpath iterate
    (closer closer-mop lw-compat))
   (svn (usocket/trunk usocket) (trunk cl-irc) graphic-forms)))

(defdistributor clnet-clbuild
  (:url-schemas (http (repo) "common-lisp.net" "project" "clbuild" (downstring (name repo))))
  (:modules (darcs (clbuild (clbuild :asdf-name nil)))))

(defdistributor clnet-clbuild-mirror
  (:url-schemas (http (repo) "common-lisp.net" "project" "clbuild" "mirror" (downstring (name repo))))
  (:modules (darcs
             (clbuild cl-webdav skippy salza trivial-sockets split-sequence rfc2388 psgraph parse-number net-telent-date ltk ironclad chipz))))

(defdistributor christophe
  (:url-schemas
   (http (repo) "common-lisp.net" "~crhodes" (downstring (name repo))))
  (:modules
   (darcs clx)))

(defdistributor luis
  (:url-schemas
   (http (repo) "common-lisp.net" "~loliveira" "darcs" (downstring (repo-umbrella repo))))
  (:modules
   (darcs trivial-features babel (cffi+lotsastuff cffi))))

(defdistributor ediware@luis
  (:url-schemas
   (http (repo) "common-lisp.net" "~loliveira" "ediware" (downstring (name repo))))
  (:modules
   (darcs
    cl-ppcre flexi-streams cl-fad hunchentoot chunga url-rewrite cl-who drakma)))

(defdistributor repo.or.cz
  (:url-schemas
   (git (repo) "repo.or.cz" (format nil "~A.git" (downstring (name repo)))))
  (:modules
   (git darcs2git closure-html)))

(defdistributor lichteblau.com
  (:url-schemas
   (http (repo) "www.lichteblau.com" "git" (format nil "~A.git" (downstring (name repo)))))
  (:modules
   (git-http cxml-rng cxml-stp clim-alerts)))

(defdistributor lichteblau@clnet
  (:url-schemas
   (http (repo) "common-lisp.net" "~dlichteblau" "inofficial" (downstring (name repo))))
  (:modules
   (darcs mel-base)))

(defdistributor sullivan
  (:url-schemas
   (http (repo) "rvw.doc.gold.ac.uk" "sullivan" "darcs" (downstring (name repo))))
  (:modules
   (darcs midi spatial-trees)))

(defdistributor b9.com
  (:url-schemas
   (git (repo) "b9.com" (format nil "~A.git" (downstring (name repo)))))
  (:modules
   (git puri md5 cl-base64 clsql rt)))

(defdistributor fractalconcept.com
  (:url-schemas
   (svn (repo) "www.fractalconcept.com:8000" "public" "open-source" (downstring (name repo))))
  (:modules
   (svn cl-pdf cl-typesetting)))

(defdistributor www.pps.jussieu.fr
  (:url-schemas
   (http (repo) "www.pps.jussieu.fr" "~jch" "software" "repos" (downstring (name repo))))
  (:modules
   (darcs cl-yacc)))

(defdistributor tuxee.net
  (:url-schemas
   (git (repo) "git.tuxee.net" (downstring (name repo))))
  (:modules
   (git cl-vectors)))

(defdistributor xach.com
  (:url-schemas
   (git (repo) "git.xach.com" (format nil "~A.git" (downstring (name repo)))))
  (:modules
   (git salza2 zpng vecto zpb-ttf)))

(defdistributor boinkor.net
  (:url-schemas
   (git (repo) "sbcl.boinkor.net" (downstring (name repo))))
  (:modules
   (git (sbcl (sbcl :asdf-name nil)))))

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
  (mcclim trivial-features alexandria babel cffi spatial-trees flexichain slime clx graphic-forms)
  (mel-base trivial-features alexandria babel cffi trivial-gray-streams flexi-streams cl+ssl)
  (plexippus-xpath closure-common puri trivial-gray-streams cxml parse-number cl-ppcre cl-yacc)
  (trivial-features rt babel cffi alexandria)
  (usocket split-sequence)
  (vecto cl-vectors salza2 zpng zpb-ttf)
  (zip salza2 trivial-gray-streams flexi-streams)
  (zpng salza2))

(defdistributor feelingofgreen.ru
  (:modules
   (local
    bin doc giveio mcdbio winnt-lptaccess mcdust-one mclinux mdb newlib pestilence x86-linux x86-win32)))

(define-module-dependencies
  (pergamum alexandria iterate)
  (bintype pergamum)
  (elf bintype)
  (dwarf bintype)
  (cl-io-mod bintype)
  (lispdb pergamum semi-precious sb-x86-portio linedit libpciaccess bordeaux-threads slime elf cffi)
  (cling pergamum semi-precious))

(minimise-dependencies)

(defapplication 'climacs 'climacs 'climacs 'climacs-rv)