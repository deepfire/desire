#!/bin/bash
argv0_url="http://www.feelingofgreen.ru/shared/src/desire/climb.sh"            #
################################################################################
fail() {
    echo "ERROR: $*"
    exit 1
}
handle_self_update() {
    if test ! "${climb_updated_p}"
    then
        if wget "${argv0_url}" -O "$0" > ${void} 2>&1
        then
            export climb_updated_p="t"
            bash $0 "$@"
            exit $?
        else
            fail "failed to self-update using ${argv0_url}"
        fi
    else
        echo "NOTE: insiduous self-update successful, continuing."
    fi
}
###
### Don't change the amount of characters above this comment.
###

version="11.1.0"

argv0="$(basename $0)"

default_root="${HOME}/.desire"
default_lisp="sbcl"
default_wishmaster="git.feelingofgreen.ru"
default_http_wishmaster="git.feelingofgreen.ru/shared/src"
default_desire_branch="master"

void="/dev/null"

print_version_and_die() {
    cat <<EOF
climb.sh ${version}
Copyright (C) 2009-2011 Samium Gromoff.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
EOF
    exit 0
}

print_help_and_die() {
    cat <<EOF
Usage:  ${argv0} [OPTION]... [STORAGE-ROOT]
Bootstrap, update or perform other actions on a desire installation in
either STORAGE-ROOT or the default location in '${default_root}'
(possibly set by ${defaults}).

  -u           Self-update and continue processing other options, using
                the updated version.
  -r ROOT      Use bootstrap ROOT location.
  -l LISP      Use the LISP binary, instead of the 'sbcl' default.
  -n HOSTNAME  Use HOSTNAME as a bootstrap node.  Defaults to the contents of
                ${wishmaster_cfg}, whenever it's present.
                HOSTNAME must refer to a node participating in desire protocol.
  -b URL       Use URL as a bootstrap URL ('file' scheme is allowed).
  -t BRANCH    Check out BRANCH of metastore on the bootstrap node other than
                the default.  The default is the same as the used branch
                of desire.
  -m MODULE(s) Retrieve a space-separated list of MODULEs, once ready.
  -s SYSTEM    Install or update the module relevant to SYSTEM, then load it.
  -a APP       Load system containing APP, as per -s, then launch it.
  -x EXPR      Evaluate an expression, in the end of it all.
  -k PACKAGE   Set current PACKAGE, before evaluation of EXPR.
  -p PHASE(s)  Apply buildslave PHASEs to MODULEs specified through -m.
  -d           Enable debug optimisation of Lisp code.
  -g           Disable debugger, causing desire dump stack and abort on errors,
                instead of entering the debugger.
  -e           Enable explanations about external program invocations.
  -v           Crank up verbosity.
  -V           Print version.
  -h           Display this help message.

As step zero, when the -u switch is provided, climb.sh is updated
using wget from the canonical location at ${argv0_url}, and then
normal processing is continued, using the updated version.

When LISP is provided, it specifies the name of the lisp
implementation binary to call.

During the first step, a storage root location is either created or
validated.  The storage root must be a writable directory.

When STORAGE-ROOT is not specified, the value of the ROOT variable
within the ${defaults} file is looked up for an absolute pathname
referring to a valid storage location.  If this condition is met, that
directory is accepted as STORAGE-ROOT, otherwise an error is
signalled.

When STORAGE-ROOT is specified, it must be either an absolute pathname
referring to a valid storage location, or it must denote a
non-occupied filesystem location, with a writable parent directory.

During the second step, desire and its dependencies are either
retrieved, or updated, in the case when they are already present in
STORAGE-ROOT.

Next, a specific branch of desire is checked out, configurable with
the -b option and defaulting to "${default_desire_branch}".

Further, the -n and -t options alter, correspondingly, the hostname of
the desire node used for bootstrap, and a branch of that node's
metastore to use.  These options default to ${default_wishmaster} and
the name of the branch of desire, accordingly.

During the next step a lisp is started and desire initialisation is
attempted, with the above determined values of hostname and metastore
branch.

Once the initialisation is complete, MODULE, SYSTEM and APP provide
optional convenience shortcuts for module installation, system loading
and application launching.  Any of these can be omitted, as the
required information is easily deduced.  Note that the more granular
objects determine the objects of lower granularity.

After all these steps, EXPR is executed, if it was provided, with
PACKAGE optionally set as current.

Report ${argv0} bugs to _deepfire on feelingofgreen <dot> ru
Desire launchpad team: <https://launchpad.net/~desire>
Docs: <http://www.feelingofgreen.ru/shared/src/desire/doc/overview.html>
EOF
    exit $1
}

while getopts :ul:r:n:b:t:m:s:a:x:k:p:dgevVh opt
do
    case $opt in
        u)  handle_self_update "$@";;
        l)  ALT_LISP="${OPTARG}";;
        r)  ROOT="${OPTARG}";;
        b)  ALT_WISHMASTER='local'
            ALT_BOOTSTRAP_URL="${OPTARG%/}";;
        # b)  DESIRE_BRANCH="${OPTARG}";;
        n)  ALT_WISHMASTER="${OPTARG}"
            BOOTSTRAP_URL="git://${OPTARG%/}";;
        t)  METASTORE_BRANCH="${OPTARG}";;
        m)  MODULES="${OPTARG}";;
        s)  SYSTEM="${OPTARG}";;
        a)  APP="${OPTARG}";;
        x)  EXPR="${OPTARG}";;
        k)  PACKAGE="${OPTARG}";;
        p)  PHASES="${OPTARG}";;
        d)  DEBUG="3";;
        g)  DISABLE_DEBUGGER="t";;
        e)  EXPLAIN="t";;
        v)  VERBOSE="t";;
        V)  print_version_and_die;;
        h) 
            print_help_and_die 0;;
        :)
            echo -e "\nERROR: required option '-${OPTARG}' lacks an argument\n"
            print_help_and_die 1;;
        ?)
            echo -e "\nERROR: invalid option '-${OPTARG}' provided\n"
            print_help_and_die 1;;
    esac
done
if test ${OPTIND} > 0
then
    shift $((OPTIND - 1))
fi
test "$*" && \
    fail "unknown arguments: $*"

###
### Storable parameter processing
###
# contains:
# default_root
# default_lisp
# default_wishmaster
defaults="${HOME}/.desire-defaults"
if test -f "${defaults}"
then
    . "${defaults}"
fi

###
### Argument defaulting and reporting
###
ROOT="${ROOT:-${default_root}}"
test "${VERBOSE}" && echo "NOTE: using '${ROOT}' as storage root"

LISP=${ALT_LISP:-"${default_lisp}"}
test "${VERBOSE}" && echo "NOTE: will use '${LISP}' as the lisp implementation executable"

test "${VERBOSE}" -a "${ALT_WISHMASTER}" && echo "NOTE: choosing an alternate bootstrap wishmaster: '${ALT_WISHMASTER}'"
WISHMASTER="${ALT_WISHMASTER}"
WISHMASTER=${WISHMASTER:=${default_wishmaster}}

###
if test ! -f "${defaults}"
then
    echo "NOTE: recording default options in '${defaults}'"
    ( cat <<EOF
default_root='${ROOT}'
default_lisp='${LISP}'
default_wishmaster='${WISHMASTER}'
EOF
        ) > "${defaults}"
fi

###
test "${VERBOSE}" -a "${ALT_BOOTSTRAP_URL}" && echo "NOTE: choosing an alternate bootstrap URL: '${ALT_BOOTSTRAP_URL}'"
BOOTSTRAP_URL=${ALT_BOOTSTRAP_URL:-git://${WISHMASTER}}

test "${VERBOSE}" -a "${DESIRE_BRANCH}" && echo "NOTE: choosing an alternate branch of desire: '${DESIRE_BRANCH}'"
DESIRE_BRANCH=${DESIRE_BRANCH:-${default_desire_branch}}
test "${VERBOSE}" -a "${METASTORE_BRANCH}" && echo "NOTE: choosing a specific metastore branch: '${METASTORE_BRANCH}'"
METASTORE_BRANCH=${METASTORE_BRANCH:-${DESIRE_BRANCH}}

test "${VERBOSE}" -a "${MODULES}" && echo "NOTE: will install or update ${MODULES}, along with dependencies"
MODULES=${MODULES:-nil}
test "${VERBOSE}" -a "${SYSTEM}" && echo "NOTE: will load ${SYSTEM}, after installing or updating relevant module"
SYSTEM=${SYSTEM:-nil}
test "${VERBOSE}" -a "${APP}"    && echo "NOTE: will launch ${APP}, after updating/loading relevant module/system"
APP=${APP:-nil}

test "${VERBOSE}" -a "${EXPR}" && echo "NOTE: will execute ${EXPR}, in the end of it all"
EXPR=${EXPR:-nil}
test "${VERBOSE}" -a "${PACKAGE}" && echo "NOTE: will set ${PACKAGE} as current package, before executing the above expression"
PACKAGE=${PACKAGE:-nil}
test "${VERBOSE}" -a "${PHASES}" && echo "NOTE: will execute buildbot ${PHASES} on ${MODULES}, in the end of it all"
PHASES=${PHASES:-nil}

test "${VERBOSE}" -a "${DEBUG}" && echo "NOTE: optimising for debug"
DEBUG=${DEBUG:-1}
test "${VERBOSE}" -a "${DISABLE_DEBUGGER}" && echo "NOTE: disabling debugger"

test "${VERBOSE}" -a "${EXPLAIN}" && echo "NOTE: turning on execution explanation feature of desire"
EXPLAIN=${EXPLAIN:-nil}

###
### Accepted user arguments, on to some validation.
###
case ${LISP} in
    ccl|ccl64|lx86cl|lx86cl64 )
        impl=ccl;;
    sbcl )
        impl=sbcl;;
    ecl )
        impl=ecl;;
    clisp )
        impl=clisp;;
    gcl )   echo "ERROR: GCL is not supported.";;
    * )     echo "ERROR: unknown name of a lisp implementation binary: ${LISP}" ;;
esac
case ${impl} in
    sbcl )
        EVAL="--eval"
        DISABLE_DEBUGGER="${DISABLE_DEBUGGER:+--disable-debugger}"
        QUIET="--noinform"
        SUPPRESS_INITS="--no-sysinit --no-userinit"
        ;;
    ccl )
        EVAL="--eval"
        DISABLE_DEBUGGER=""
        QUIET="--quiet"
        SUPPRESS_INITS="--no-init"
        ;;
    ecl )
        EVAL="-eval"
        DISABLE_DEBUGGER=""
        QUIET="-q"
        SUPPRESS_INITS="-norc"
        ;;
    clisp )
        EVAL="-x"
        if test ! -z "$DISABLE_DEBUGGER"
        then
            DISABLE_DEBUGGER="-on-error exit"
        else
            DISABLE_DEBUGGER="-on-error debug"
        fi
        QUIET="--silent"
        SUPPRESS_INITS="-norc"
        ;;
esac

#######################################################
###                                                   #
### Done processing user arguments, on to the action. #
###                                                   #
#######################################################
desire_deps="alexandria asdf cl-fad executor pergamum informatimago iterate"

valid_directory_pathname_p() {
    test "${1##/}" != "${1}"
    return $?
}

ensure_root() {
    local path="$1"
    if test -d "${path}"
    then
        test -d "${path}" || fail "${path} does not refer to a directory"
        test -w "${path}" || fail "${path} is not writable"
    else
        mkdir "${path}" || \
            fail "failed to create storage directory at '${path}'"
    fi
}

git_set_head_index_tree() {
    local path="$1"
    local ref="$2"
    ( cd "${path}" && git reset --hard "${ref}" )
    return $?
}

degraded_to_http=
clone_module() {
    local root="$1"
    local module="$2"
    if test -z "${degraded_to_http}"
    then
        if ! git clone -o "${WISHMASTER}" ${BOOTSTRAP_URL}/${desire_dep} "${root}/${module}" >${void} 2>&1
        then
            degraded_to_http="T"
            echo "NOTE: failed to go through a native protocol, degrading to a dumb HTTP transport"
            clone_module "${root}" "${module}"
        fi
    else
        git clone -o "${WISHMASTER}" http://${default_http_wishmaster}/${desire_dep}/.git/ "${root}/${module}" >${void} || \
            fail "failed to retrieve ${module}"
    fi
}

update_module() {
    # Updating is non-critical.
    local root="$1"
    local module="$2"
    (cd "${root}/${module}" && git fetch "${WISHMASTER}" >${void} 2>&1 && git reset --hard "remotes/${WISHMASTER}/master" >${void}) || \
        echo "failed to update ${module}"
}

clone_or_update_dependencies() {
    local root="$1"
    for desire_dep in ${desire_deps} desire
    do
        test "${VERBOSE}" && echo -n "      $desire_dep: "
        # Handle stashed modules.
        if test -d "${root}/${desire_dep}_"
        then
            mv "${root}/${desire_dep}_" "${root}/${desire_dep}"
        fi
        if test -d "${root}/${desire_dep}"
        then
            update_module "${root}" "${desire_dep}"
        else
            clone_module "${root}" "${desire_dep}"
        fi
        test "${VERBOSE}" && echo "ok"
    done
}

###
### The start of it all
###
valid_directory_pathname_p "${ROOT}" || \
    fail "${ROOT} is not an absolute pathname"
ROOT="${ROOT%%/}"

ensure_root "${ROOT}"
clone_or_update_dependencies "${ROOT}"

test "${VERBOSE}" && echo "NOTE: checking out '${DESIRE_BRANCH}' branch of desire..."
git_set_head_index_tree "${ROOT}/desire" "${WISHMASTER}/${DESIRE_BRANCH}" || \
    fail "failed to check out  '${DESIRE_BRANCH}' of desire"

test "${VERBOSE}" && echo "NOTE: cranking up verbosity"
VERBOSE="${VERBOSE:-nil}"

#######################################################
###                                                   #
### Shrug off chains of POSIX...                      #
###                                                   #
#######################################################
if test "x${VERBOSE}" == "xt"; then echo "NOTE: all done going into lisp..."; fi
CONGRATULATING_MESSAGE="\"


   Congratulations! You have reached a point where you can wish for any package
  desire knows about. Just type (lust 'desiree) and it will happen.
  You can link desire's pool of packages into ASDF by ensuring that
  #p\\\"${ROOT}/.asdf-registry/\\\" is in your ASDF:*CENTRAL-REGISTRY*

  To see what's possible, issue:
    (apropos-desr 'clim)
  or
    (list-modules)

  Have fun!

\""
export SBCL_BUILDING_CONTRIB=t
${LISP} ${QUIET} ${SUPPRESS_INITS} ${DISABLE_DEBUGGER} \
	${EVAL} "
(progn
    #+clisp (delete-package :asdf-extensions)
    #+clisp (delete-package :asdf)
    #+ecl
    (require :cmp))" \
	${EVAL} "
(progn
  (defmacro unstyled (verbosep () &body body)
    \`(flet ((body () ,@body))
        (if ,verbosep 
            (body)
            (handler-bind ((style-warning (function muffle-warning)))
              (body)))))
  (export (list (quote unstyled)))
  (defpackage #:org.feelingofgreen.temp.climb
    (:use :common-lisp))
  (in-package #:org.feelingofgreen.temp.climb))" \
	${EVAL} "
(progn
  ;; disable compiler verbosity
  (let ((verbose (and ${DEBUG} ${VERBOSE})))
    (setf (values *compile-verbose* *compile-print* *load-verbose*) (values verbose verbose verbose)
          #+ecl #+ecl
          c::*compiler-break-enable* t))
  (declaim (optimize (debug ${DEBUG}))
           #+sbcl
           (sb-ext:muffle-conditions sb-ext:code-deletion-note sb-ext:compiler-note style-warning))
  #+(or sbcl ccl)
  (cl-user:unstyled ${VERBOSE} ()
    (load (compile-file \"${ROOT}/asdf/asdf.lisp\"))))" \
	${EVAL} "
(progn
  (defparameter *asdf-root* (pathname-directory (parse-namestring \"${ROOT}/\")))
  (defun basic-root-modules-search (system)
    (let* ((name (asdf::coerce-name system))
           (file (make-pathname :directory (append *asdf-root* (list name)) :name name :type \"asd\" :case :local)))
      (when (and file (probe-file file))
        file)))
  (let ((asdf:*system-definition-search-functions* (cons (quote basic-root-modules-search)
                                                         asdf:*system-definition-search-functions*)))
    (handler-case (cl-user:unstyled ${VERBOSE} ()
                    (asdf:operate (quote asdf:load-op) :desire :verbose ${VERBOSE}))
      (error (c)
        (format t \"~%Got condition:~%~A~%\" c)
        (invoke-debugger c))))
  (in-package :desr))" \
	${EVAL} "
(block nil
  ;; configure desire verbosity
  (setf *execute-explanatory* ${EXPLAIN} *execute-verbosely* ${VERBOSE} *verbose-repository-maintenance* ${VERBOSE})
  (handler-case (init \"${ROOT}/\" ${ALT_WISHMASTER:+:wishmaster-name \"${WISHMASTER}\"}
                                   ${ALT_BOOTSTRAP_URL:+:bootstrap-url \"${BOOTSTRAP_URL}/\"}
                                   ${http_proxy:+:http-proxy \"${http_proxy}\"}
                                   :wishmaster-branch :${METASTORE_BRANCH}
                                   :verbose ${VERBOSE})
    (desire-error (c)
      (format t \"~&;;;~%;;; During bootstrap, caught error:~%~A~%\" c)
      (funcall (find-symbol \"QUIT\" #+sbcl :sb-ext #+clisp :ext #+ecl :si #+ccl :ccl))))
  (format t ${CONGRATULATING_MESSAGE})
  (let* ((app (app (quote ${APP}) :if-does-not-exist :continue))
         (system  (if app
                      (app-system app)
                      (system (quote ${SYSTEM}) :if-does-not-exist :continue)))
         (module-spec (quote ${MODULES}))
         (phases (ensure-list (quote ${PHASES})))
         (modules (if system
                      (list (list (name (system-module system))))
                      (remove nil (mapcar (lambda (x) (name (module x :if-does-not-exist :continue))) (ensure-list module-spec)))))
         (desire (or module-spec (quote ${SYSTEM}) (quote ${APP}))))
    (when (and desire (not modules))
      (error \"~@<~S was/were desired, but no such entity (application, system or module) is known.~:@>\"
             desire))
    (when modules
      (if phases
          (buildslave modules phases ${VERBOSE})
          (desire modules :verbose ${VERBOSE} :skip-present t)))
    (when system
      (loadsys system :verbose ${VERBOSE}))
    (when app
      (run app))))" \
	${EVAL} "
(when ${PACKAGE}
  (in-package ${PACKAGE}))" \
	${EVAL} "
(when (quote ${EXPR})
  ${EXPR})"
