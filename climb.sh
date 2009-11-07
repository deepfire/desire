#!/bin/bash
version="0.1"

if test ! "$1" || test "$1" = "--help"
then
    cat <<EOF
climb.sh, version $version - a bash script for bootstrapping desire on unix systems
Usage:  climb.sh ABSOLUTE-ROOT-PATH [APP-OR-SYS-OR-MOD-NAME [BOOTSTRAP-HOST]]

Bootstrap desire, the Common Lisp software knowledge and distribution system from
BOOTSTRAP-HOST (which defaults to git.feelingofgreen.ru), optionally installing
the entity with APP-OR-SYS-OR-MOD-NAME.
EOF
    exit
fi

ROOT="$1"
DESIRE="${2:-nil}"
BOOTSTRAP_NODE=${3:-git.feelingofgreen.ru}

test "$2" && echo "NOTE: upon successful bootstrap will install and load $DESIRE, along with dependencies, and maybe launch it"

test "$EXPLAIN" && echo "NOTE: turning on execution explanation feature of desire"
EXPLAIN=${EXPLAIN:+t}
EXPLAIN=${EXPLAIN:-nil}

test "$VERBOSE" && echo "NOTE: turning on verbose external execution"
VERBOSE=${VERBOSE:+t}
VERBOSE=${VERBOSE:-nil}

test "$BRANCH" && echo "NOTE: choosing an alternate branch of desire: '$BRANCH'"
BRANCH=${BRANCH:-master}

test "$METABRANCH" && echo "NOTE: choosing a specific metastore branch: '$METABRANCH'"
METABRANCH=${METABRANCH:-$BRANCH}

test "$DISABLE_DEBUGGER" && echo "NOTE: disabling debugger"
DISABLE_DEBUGGER=${DISABLE_DEBUGGER:+--disable-debugger}

test "$DEBUG" && echo echo "NOTE: optimising for debug"
DEBUG=${DEBUG:+3}
DEBUG=${DEBUG:-1}

#######################################################
###                                                   #
### Done processing user arguments, on to the action. #
###                                                   #
#######################################################
desire_deps="alexandria asdf cl-fad executor pergamum iterate"

###
### See if there is anything we can remember...
###
root="$(cat ~/.climb-root)"
if test -d "$root" -a -w "$root" -a "${root:0:1}" == "/"
then
    re=t
    ROOT="$root"
    echo "NOTE: found traces of previous bootstrap in $ROOT, updating and reusing that:"
    for desire_dep in $desire_deps desire
    do
        echo -n "      $desire_dep: "
        ( cd "$ROOT/$desire_dep" && git fetch origin >/dev/null 2>&1 && git reset --hard remotes/origin/master >/dev/null || \
            echo "FATAL: failed to update $desire_dep" && exit 1 )
        echo "ok"
    done
else
    if test ! "${ROOT:0:1}" = "/"
    then
        echo "\$ROOT is not an absolute path"
        exit 1
    fi
    test "$ROOT" -a -d "$(dirname $ROOT)" -a -w "$(dirname $ROOT)" -a ! -x "$ROOT" || \
        echo "ERROR: the first argument must be an non-occupied pathname with a writable parent directory" && exit 1
    echo "NOTE: validated \"$ROOT\" as storage location"
    echo -n "$ROOT" > ~/.climb-root

    mkdir "$ROOT" "$ROOT/git" "$ROOT/darcs" "$ROOT/cvs" "$ROOT/svn" || \
        echo "FATAL: unable to initialise the storage location at \"$ROOT\", exiting" && exit 1

    echo "NOTE: initialised storage location ok. Retrieving and loading desire and its dependencies:"

    for desire_dep in $desire_deps desire
    do
        echo -n "      $desire_dep: "
        git clone git://$BOOTSTRAP_NODE/$desire_dep "$ROOT/$desire_dep" >/dev/null || \
            echo "FATAL: failed to retrieve $desire_dep" && exit 1
        echo "ok"
    done
fi

echo "NOTE: checking out '$BRANCH' branch of desire..."
( cd $ROOT/desire;  git reset --hard origin/$BRANCH; )

#######################################################
###                                                   #
### Shrug off POSIX chains...                         #
###                                                   #
#######################################################
echo "NOTE: all done going into lisp..."
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
  (declaim (optimize (debug $DEBUG))
           #+sbcl
           (sb-ext:muffle-conditions sb-ext:code-deletion-note sb-ext:compiler-note style-warning))
  (load (compile-file \"$ROOT/asdf/asdf.lisp\")))" \
     --eval "
(progn
  (defparameter *root* (pathname-directory (parse-namestring \"$ROOT/\")))
  (defun basic-root-modules-search (system)
    (let* ((name (asdf::coerce-name system))
           (file (make-pathname :directory (append *root* (list name)) :name name :type \"asd\" :case :local)))
      (when (and file (probe-file file))
        file)))
  (push 'basic-root-modules-search asdf:*system-definition-search-functions*)
  (asdf:operate 'asdf:load-op 'desire :verbose nil)
  (in-package :desr))" \
     --eval "
(progn
  ;; configure desire verbosity
  (setf executor:*execute-explanatory* $EXPLAIN executor:*execute-verbosely* $VERBOSE)
  (init \"$ROOT/\" :wishmaster-branch :$METABRANCH)
  (format t $CONGRATULATING_MESSAGE)
  (let* ((app (app '$DESIRE :if-does-not-exist :continue))
         (system  (if app
                      (app-system app)
                      (system '$DESIRE :if-does-not-exist :continue)))
         (module (if system
                     (system-module system)
                     (module '$DESIRE :if-does-not-exist :continue))))
    (when (and '$DESIRE (not module))
      (error \"~@<A desire ~S was provided, but no such entity (application, system or module) is known to desire.~:@>\"
             '$DESIRE))
    (when module
      (lust (name module)))
    (when system
      (require (down-case-name system)))
    (when app
      (run app))))"