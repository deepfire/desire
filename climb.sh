#!/bin/bash
version="0.1"

ROOT="$1"
DESIRE="${2:-nil}"
DESIRE_HOME=${3:-git.feelingofgreen.ru}

if ! test "$DESIRE" = "nil"
then
    echo "NOTE: upon successful bootstrap will install, load and maybe launch $DESIRE, along with dependencies"
fi

if test -z "$EXPLAIN"
then
    EXPLAIN=nil
else
    EXPLAIN=t
    echo "NOTE: turning on execution explanation feature of desire"
fi

if test -z "$VERBOSE"
then
    VERBOSE=nil
else
    VERBOSE=t
    echo "NOTE: turning on verbose external execution"
fi

if test -z "$BRANCH"
then
    BRANCH=master
else
    echo "NOTE: choosing an alternate branch of desire: '$BRANCH'"
fi

if test -z "$METABRANCH"
then
    METABRANCH=$BRANCH
else
    echo "NOTE: choosing a metastore branch different from desire branch: '$METABRANCH'"
fi

if test ! -z "$DISABLE_DEBUGGER"
then
    DISABLE_DEBUGGER="--disable-debugger"
    echo "NOTE: disabling debugger"
fi

if test -z "$DEBUG"
then
    DEBUG=0
else
    DEBUG=3
    echo "NOTE: optimising for debug"
fi

desire_home=git://$DESIRE_HOME

if test -z "$RANDOM"
then
    echo "\$RANDOM is empty, nothing good will come out ot that -- please use GNU Bash"
    exit 1
fi

if test -z "$1" || test "$1" = "--help"
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

desire_deps="alexandria asdf cl-fad executor pergamum iterate"
if test -f ~/.climb-seed
then
    seed="$(cat ~/.climb-seed)"
    root="$(cat ~/.climb-root)"
    if test -d "/tmp/$USER-desire-temp-$seed" && test -d "$root" && test "${root:0:1}" = "/"
    then
        re=t
        ROOT="$root"
        echo "NOTE: found leftovers from previous bootstrap attempt in /tmp/$USER-desire-temp-$seed and $root, reusing them..." 
    else
        seed="$RANDOM"
    fi
else
    seed="$RANDOM"
fi
echo -n "$seed" > ~/.climb-seed
temp_asdf_suffix="$USER-desire-temp-$seed"
temp_asdf_root="/tmp/$temp_asdf_suffix"


if test -z "$re"
then
    if test ! "${ROOT:0:1}" = "/"
    then
        echo "\$ROOT is not an absolute path"
        exit 1
    fi
    test -z "$ROOT" && echo "ERROR: the first parameter must designate the desired location of the desire root" && exit 1
    test ! -d "$(dirname $ROOT)" && echo "ERROR: the parent directory of the first parameter must exist" && exit 1
    test -x "$ROOT" && echo "ERROR: the first parameter must specify a pathname not associated with any existing object" && exit 1
fi

echo "NOTE: will use \"$ROOT\" as root directory for mirror subroots"
echo "NOTE: will use \"$temp_asdf_root\" as bootstrap package directory"

if test -z "$re"
then
    mkdir $temp_asdf_root || ( echo "FATAL: unable to create the bootstrap package directory at \"$temp_asdf_root\", exiting" && exit 1 )
    mkdir $ROOT || ( echo "FATAL: unable to create the root directory at \"$ROOT\", exiting" && exit 1 )
    mkdir $ROOT/git $ROOT/darcs $ROOT/hg $ROOT/cvs $ROOT/svn

    echo -n "$ROOT" > ~/.climb-root
    echo "NOTE: created required directories ok. Retrieving and loading desire and its dependencies:"

    for desire_dep in $desire_deps desire
    do
        echo -n "      $desire_dep: "
        git clone $desire_home/$desire_dep "$temp_asdf_root/$desire_dep" >/dev/null || ( echo "FATAL: failed to retrieve $desire_dep" && exit 1 )
        echo "ok"
    done
    echo "NOTE: checking out branch $BRANCH of desire..."
    ( cd $temp_asdf_root/desire; git reset --hard origin/$BRANCH; )
else
    echo "NOTE: found required directories ok. Updating desire and dependencies:"
    for desire_dep in $desire_deps desire
    do
        echo -n "      $desire_dep: "
        ( cd "$temp_asdf_root/$desire_dep" && git fetch origin >/dev/null 2>&1 && git reset --hard remotes/origin/master >/dev/null || ( echo "FATAL: failed to retrieve $desire_dep" && exit 1 ))
        echo "ok"
    done
    echo "NOTE: checking out '$BRANCH' branch of desire..."
    ( cd $temp_asdf_root/desire;  git reset --hard origin/$BRANCH; )
fi

echo "NOTE: all done going into lisp..."

export SBCL_BUILDING_CONTRIB=t

sbcl --noinform $DISABLE_DEBUGGER \
     --eval "
(progn
  (setf (values *compile-verbose* *compile-print* *load-verbose*) (values nil nil nil))
  (declaim (optimize (debug $DEBUG))
           #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note sb-ext:compiler-note style-warning))
  (load (compile-file \"/tmp/$temp_asdf_suffix/asdf/asdf.lisp\")))" \
     --eval "
(progn
  (defparameter *temp-root* '(:absolute \"tmp\" \"$temp_asdf_suffix/\"))
  (defun temp-modules-search (system)
   (let* ((name (asdf::coerce-name system))
          (file (make-pathname :directory (append *temp-root* (list name)) :name name :type \"asd\" :case :local)))
     (when (and file (probe-file file))
         file)))
  (push 'temp-modules-search asdf:*system-definition-search-functions*)
  (asdf:operate 'asdf:load-op 'desire :verbose nil))" \
     --eval "(in-package :desr)" \
     --eval "
(progn
  (setf executor:*execute-explanatory* $EXPLAIN executor:*execute-verbosely* $VERBOSE)
  (init \"$ROOT/\" :wishmaster-branch :$METABRANCH)
  (format t \"~&~%~%~
   Congratulations! You have reached a point where you can wish for any package~%~
  desire knows about. Just type (lust 'desiree) and it will happen.~%~
  You can link desire's pool of packages into ASDF by ensuring that~%~
  #p\\\"$ROOT/git/.asdf-registry/\\\" is in your ASDF:*CENTRAL-REGISTRY*~%~%~
  To see what's possible, issue:~%~
    (apropos-desr 'clim)~%~
  or~%~
    (list-modules)~%~%~
  Have fun!~%~%\")
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