(in-package :cling)

(defmethod fetch ((to (eql :please)) (from (eql :fail)))
  (false))
