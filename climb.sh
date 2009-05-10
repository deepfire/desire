ROOT="$1"
APP="$2"
DESIRE_HOME=${3:-git.feelingofgreen.ru}

desire_home=git://$DESIRE_HOME

desire_deps="alexandria cl-fad pergamum iterate semi-precious"
temp_asdf_suffix="$USER-desire-temp-$RANDOM"
temp_asdf_root="/tmp/$temp_asdf_suffix"

test -z "$ROOT" && echo "ERROR: the first parameter must designate the desired location of the desire root." && exit 1
test ! -d "$(dirname $ROOT)" && echo "ERROR: the parent directory of the first parameter must exist." && exit 1
test -x "$ROOT" && echo "ERROR: the first parameter must specify a pathname not associated with any existing object." && exit 1

echo "NOTE: will use \"$ROOT\" as root directory for mirror subroots."
echo "NOTE: will use \"$temp_asdf_root\" as bootstrap package directory."

mkdir $temp_asdf_root || ( echo "FATAL: unable to create the bootstrap package directory at \"$temp_asdf_root\", exiting." && exit 1 )
mkdir $ROOT || ( echo "FATAL: unable to create the root directory at \"$ROOT\", exiting." && exit 1 )
mkdir $ROOT/git $ROOT/darcs $ROOT/hg $ROOT/cvs $ROOT/svn

echo "NOTE: created required directories ok. Retrieving and loading desire and its dependencies..."

for desire_dep in $desire_deps desire
do
    git clone $desire_home/$desire_dep "$temp_asdf_root/$desire_dep" >/dev/null || ( echo "FATAL: failed to retrieve $desire_dep." && exit 1 )
done

sbcl --noinform \
     --eval "(require :asdf)" \
     --eval "(setf (values *compile-verbose* *compile-print* *load-verbose*) (values nil nil nil))" \
     --eval "(defparameter *temp-root* '(:absolute \"tmp\" \"$temp_asdf_suffix/\"))" \
     --eval "(defun temp-modules-search (system)
               (let* ((name (asdf::coerce-name system))
	              (file (make-pathname :directory (append *temp-root* (list name)) :name name :type \"asd\" :case :local)))
                 (when (and file (probe-file file))
                   file)))" \
     --eval "(push 'temp-modules-search asdf:*system-definition-search-functions*)" \
     --eval "(asdf:operate 'asdf:load-op 'desire :verbose nil)" \
     --eval "(desire:init \"$ROOT/\")" \
     --eval "(desire:format t \"~&~%~%   Congratulations! You have reached a point where you can wish for any package~%  desire knows about. Just type (desire* 'wish) and desire will make it happen.\")"
