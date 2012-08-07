(in-package :cl-todoist)


(defun make-indexed-hash-table (list index-fn &rest rest &key &allow-other-keys)
  "Make a hash table from a list of objects, where the objects are the
  values and key for a particular object obj is (index-fn obj)"
  (let ((ht (apply #'make-hash-table rest)))
    (iterate
      (for obj in list)
      (setf (gethash (funcall index-fn obj) ht) obj)
      (finally (return ht)))))
