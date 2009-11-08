#!/bin/bash
argv0_url="http://www.feelingofgreen.ru/shared/git/desire/climb.sh"            #
################################################################################
handle_self_update() {
    if test ! "${climb_updated_p}"
    then
        if wget "${argv0_url}" -O "$0" > /dev/null 2>&1
        then
            export climb_updated_p="t"
            bash $0 "$@"
            exit $?
        else
            echo "ERROR: failed to self-update using ${argv0_url}"
            exit 1
        fi
    else
        echo "NOTE: insiduous self-update successful, continuing."
    fi
}
###
### Don't change the amount of characters above this comment.
###

version="9.11.1"

argv0="$(basename $0)"

default_bootstrap_node="git.feelingofgreen.ru"
default_desire_branch="master"

print_version_and_die() {
    cat <<EOF
climb.sh ${version}
Copyright (C) 2009 Samium Gromoff.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
EOF
    exit 0
}

print_help_and_die() {
    cat <<EOF
Usage:  ${argv0} [OPTION]... [STORAGE-ROOT]
Bootstrap, update or perform other actions on a desire installation
in either STORAGE-ROOT, or a location specified in ~/.climb-root

  -u          Self-update.
  -n HOSTNAME Use HOSTNAME as a bootstrap node.
                HOSTNAME must refer to a node participating in desire protocol.
  -b BRANCH   Check out BRANCH of desire other than 'master'.
  -t BRANCH   Check out BRANCH of metastore on the bootstrap node other than
                the default.  The default is the same as the used branch
                of desire.
  -m MODULE   Retrieve MODULE, once ready.
  -s SYSTEM   Install or update the module relevant to SYSTEM, then load it.
  -a APP      Load system containing APP, as per -s, then launch it.
  -x EXPR     Evaluate an expression, in the end of it all.
  -d          Enable debug optimisation of Lisp code.
  -n          Disable debugger, causing desire dump stack and abort on errors,
                instead of entering the debugger.
  -e          Enable explanations about external program invocations.
  -v          Crank up verbosity.
  -V          Print version.
  -h          Display this help message.

During first step storage root location is determined, as follows:

When the -u switch is provided, $0 is updated using wget from the
canonical location at ${argv0_url},
and then execution is continued, using the updated version.

When STORAGE-ROOT is not specified, look up ~/.climb-root and see if it
refers to a writable directory containing a writable 'git' subdirectory.
If this condition is met, that directory is accepted as STORAGE-ROOT,
otherwise an error is signalled.

When STORAGE-ROOT is specified, it must refer to a non-occupied filesystem
location, with a present, writable parent directory.

During the second step, desire and its dependencies are either retrieved,
or updated, when they are already present in STORAGE-ROOT.

Next, when -b is specified, a branch of desire other than the default one
(${default_desire_branch}) is checked out.

Further, the -n and -t options specify, correspondingly, the hostname
of the desire node used for bootstrap, and a branch of that node's metastore
to use.  These options default to ${default_bootstrap_node} and
the branch of desire, accordingly.

Then a lisp is started and desire initialisation is attempted, with
determined hostname and metastore branch.

Once the initialisation is complete, MODULE, SYSTEM and APP provide
optional convenience shortcuts for module installation, system loading
and application launching.  Any of these can be omitted, as the required
information is easily deduced.  Note that the more granular objects
determine the objects of lower granularity.

After all these steps, EXPR is executed, if it was provided.

Report ${argv0} bugs to _deepfire on feelingofgreen <dot> ru
Desire launchpad team: <https://launchpad.net/~desire>
Docs: <http://www.feelingofgreen.ru/shared/git/desire/doc/overview.html>
EOF
    exit $1
}

while getopts :un:b:t:m:s:a:x:dnevVh opt
do
    case $opt in
        u)  handle_self_update "$@";;
        n)  BOOTSTRAP_NODE="$OPTARG";;
        b)  DESIRE_BRANCH="$OPTARG";;
        t)  METASTORE_BRANCH="$OPTARG";;
        m)  MODULE="$OPTARG";;
        s)  SYSTEM="$OPTARG";;
        a)  APP="$OPTARG";;
        x)  EXPR="$OPTARG";;
        d)  DEBUG="3";;
        n)  DISABLE_DEBUGGER="--disable-debugger";;
        e)  EXPLAIN="t";;
        v)  VERBOSE="t";;
        V)  print_version_and_die;;
        h) 
            print_help_and_die 0;;
        :)
            echo -e "\nERROR: required option '-$OPTARG' lacks an argument\n"
            print_help_and_die 1;;
        ?)
            echo -e "\nERROR: invalid option '-$OPTARG' provided\n"
            print_help_and_die 1;;
    esac
done
shift $((OPTIND - 1))

ROOT="$1"
shift 1
test "$@" && echo "ERROR: unknown arguments: $@" && exit 1

test "$VERBOSE" -a "$BOOTSTRAP_NODE" && echo "NOTE: choosing an alternate bootstrap node: '$BOOTSTRAP_NODE'"
BOOTSTRAP_NODE=${BOOTSTRAP_NODE:-${default_bootstrap_node}}
test "$VERBOSE" -a "$DESIRE_BRANCH" && echo "NOTE: choosing an alternate branch of desire: '$DESIRE_BRANCH'"
DESIRE_BRANCH=${DESIRE_BRANCH:-${default_desire_branch}}
test "$VERBOSE" -a "$METASTORE_BRANCH" && echo "NOTE: choosing a specific metastore branch: '$METASTORE_BRANCH'"
METASTORE_BRANCH=${METASTORE_BRANCH:-$DESIRE_BRANCH}

test "$VERBOSE" -a "$MODULE" && echo "NOTE: will install or update $MODULE, along with dependencies"
MODULE=${MODULE:-nil}
test "$VERBOSE" -a "$SYSTEM" && echo "NOTE: will load $SYSTEM, after installing or updating relevant module"
SYSTEM=${SYSTEM:-nil}
test "$VERBOSE" -a "$APP"    && echo "NOTE: will launch $APP, after updating/loading relevant module/system"
APP=${APP:-nil}

test "$VERBOSE" -a "$EXPR" && echo "NOTE: will execute $EXPR, in the end of it all"
EXPR=${EXPR:-nil}

test "$VERBOSE" -a "$DEBUG" && echo echo "NOTE: optimising for debug"
DEBUG=${DEBUG:-1}
test "$VERBOSE" -a "$DISABLE_DEBUGGER" && echo "NOTE: disabling debugger"

test "$VERBOSE" -a "$EXPLAIN" && echo "NOTE: turning on execution explanation feature of desire"
EXPLAIN=${EXPLAIN:-nil}

#######################################################
###                                                   #
### Done processing user arguments, on to the action. #
###                                                   #
#######################################################
desire_deps="alexandria asdf cl-fad executor pergamum iterate"

###
### See if there is anything we can remember...
###
root="$(cat ~/.climb-root 2>/dev/null)"
if test ! "$ROOT" -a -d "$root" -a -w "$root" -a "${root:0:1}" == "/" && \
   test -d "$root/git" -a -w "$root/git"
then
    ROOT="$root"
    test "$VERBOSE" && echo "NOTE: found traces of previous bootstrap in $ROOT, updating and reusing that:"
    for desire_dep in $desire_deps desire
    do
        test "$VERBOSE" && echo -n "      $desire_dep: "
        (cd "$ROOT/git/$desire_dep" && git fetch origin >/dev/null 2>&1 && git reset --hard remotes/origin/master >/dev/null) || \
            { echo "ERROR: failed to update $desire_dep"; exit 1; }
        test "$VERBOSE" && echo "ok"
    done
else
    test "$ROOT" || \
        { echo "ERROR: ~/.climb-root did not refer to a writable directory, nor was STORAGE-ROOT specified, cannot continue"; exit 1; }

    test "${ROOT:0:1}" == "/" || \
        { echo "ERROR: \$ROOT is not an absolute path"; exit 1; }

    test "$ROOT" -a -d "$(dirname $ROOT)" -a -w "$(dirname $ROOT)" -a ! -x "$ROOT" || \
        { echo "ERROR: the first argument must be an non-occupied pathname with a writable parent directory"; exit 1; }

    test "$VERBOSE" && echo "NOTE: validated \"$ROOT\" as new storage location, updating ~/.climb-root"
    echo -n "$ROOT" > ~/.climb-root

    mkdir "$ROOT" "$ROOT/git" "$ROOT/darcs" "$ROOT/cvs" "$ROOT/svn" || \
        { echo "ERROR: unable to initialise the storage location at \"$ROOT\", exiting"; exit 1; }

    test "$VERBOSE" && echo "NOTE: initialised storage location ok. Retrieving and loading desire and its dependencies:"

    for desire_dep in $desire_deps desire
    do
        test "$VERBOSE" && echo -n "      $desire_dep: "
        git clone git://$BOOTSTRAP_NODE/$desire_dep "$ROOT/git/$desire_dep" >/dev/null || \
            { echo "ERROR: failed to retrieve $desire_dep"; exit 1; }
        test "$VERBOSE" && echo "ok"
    done
fi

test "$VERBOSE" && echo "NOTE: checking out '$DESIRE_BRANCH' branch of desire..."
( cd $ROOT/git/desire && git reset --hard origin/$DESIRE_BRANCH ) || \
    { echo "ERROR: failed to check out branch '$DESIRE_BRANCH' of desire"; exit 1; }

test "$VERBOSE" && echo "NOTE: cranking up verbosity"
VERBOSE=${VERBOSE:-nil}

#######################################################
###                                                   #
### Shrug off chains of POSIX...                      #
###                                                   #
#######################################################
test "$VERBOSE" == "t" && echo "NOTE: all done going into lisp..."
CONGRATULATING_MESSAGE="\"


   Congratulations! You have reached a point where you can wish for any package
  desire knows about. Just type (lust 'desiree) and it will happen.
  You can link desire's pool of packages into ASDF by ensuring that
  #p\\\"$ROOT/git/.asdf-registry/\\\" is in your ASDF:*CENTRAL-REGISTRY*

  To see what's possible, issue:
    (apropos-desr 'clim)
  or
    (list-modules)

  Have fun!

\""
export SBCL_BUILDING_CONTRIB=t
sbcl --noinform $DISABLE_DEBUGGER \
     --eval "
(progn
  ;; disable compiler verbosity
  (setf (values *compile-verbose* *compile-print* *load-verbose*) (values nil nil nil))
  (declaim (optimize (debug ${DEBUG}))
           #+sbcl
           (sb-ext:muffle-conditions sb-ext:code-deletion-note sb-ext:compiler-note style-warning))
  (load (compile-file \"${ROOT}/git/asdf/asdf.lisp\")))" \
     --eval "
(progn
  (defparameter *asdf-root* (pathname-directory (parse-namestring \"${ROOT}/git/\")))
  (defun basic-root-modules-search (system)
    (let* ((name (asdf::coerce-name system))
           (file (make-pathname :directory (append *asdf-root* (list name)) :name name :type \"asd\" :case :local)))
      (when (and file (probe-file file))
        file)))
  (push (quote basic-root-modules-search) asdf:*system-definition-search-functions*)
  (asdf:operate (quote asdf:load-op) :desire :verbose nil)
  (in-package :desr))" \
     --eval "
(progn
  ;; configure desire verbosity
  (setf executor:*execute-explanatory* ${EXPLAIN} executor:*execute-verbosely* ${VERBOSE})
  (init \"${ROOT}/\" :wishmaster-branch :${METASTORE_BRANCH})
  (format t ${CONGRATULATING_MESSAGE})
  (let* ((app (app (quote ${APP}) :if-does-not-exist :continue))
         (system  (if app
                      (app-system app)
                      (system (quote ${SYSTEM}) :if-does-not-exist :continue)))
         (module (if system
                     (system-module system)
                     (module (quote ${MODULE}) :if-does-not-exist :continue)))
         (desire (or (quote ${MODULE}) (quote ${SYSTEM}) (quote ${APP}))))
    (when (and desire (not module))
      (error \"~@<~S was desired, but no such entity (application, system or module) is known.~:@>\"
             desire))
    (when module
      (lust (name module)))
    (when system
      (require (down-case-name system)))
    (when app
      (run app))
    (when (quote ${EXPR})
      ${EXPR})))"