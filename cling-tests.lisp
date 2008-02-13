(in-package :cling)

(defmethod pull ((to (eql :please)) (from (eql :fail)))
  (false))
