;;;; package.lisp

(defpackage #:cl-todoist
  (:use #:cl #:iterate #:alexandria #:json :cl-anonfun)
  (:import-from :metatilities defclass*))

