;;;; cl-todoist.lisp

(in-package #:cl-todoist)

(defun key-to-lisp (json-key)
  (string-upcase (substitute #\- #\_ json-key)))

(defun decode-json-from-string* (json-string)
  (let ((json:*json-symbols-package* (find-package :cl-todoist))
        (json:*json-identifier-name-to-lisp* #'key-to-lisp))
    (json:decode-json-from-string json-string)))

(define-constant +methods-base-map+
    (alist-hash-table '(("sync-get" . "TodoistSync/v2")) :test 'equal))

(define-constant +methods-map+
    (alist-hash-table '(("sync-get" . "get")) :test 'equal))

(defun request (method parameters &optional json-only)
  "Perform a standard TODOIST API call, returning the parsed JSON result."
  (let ((drakma:*text-content-types* '(("text") ("application" "json")))
        (response
          (drakma:http-request (format nil "http://todoist.com/~A/~A"
                                       (gethash method +methods-base-map+ "API")
                                       (gethash method +methods-map+ method))
                               :parameters parameters)))
    (if json-only
        response
        (handler-case (decode-json-from-string* response)
          (t (x) response)))))

(defclass* full-sync ()
  (user
   projects
   label-list
   incomplete-items)
  :automatic-initargs
  :automatic-accessors)

(defmethod initialize-instance :after ((instance full-sync) &key projects)
  "Create a list "
  (setf (incomplete-items instance)
        (make-indexed-hash-table (mappend #'items projects)
                                 #'id)))

(defun get-all (user)
  "Get the full sync of the entire user's set"
  (let ((response (request "sync-get" `(("api_token" . ,(api-token user))))))
    (let* ((projects-data  (assoc-value response 'projects )))
      (make-instance 'full-sync
                     :projects (mapcar #'make-project-from-json-data
                                       projects-data)
                     :label-list (mapcar #'(lambda (x) (assoc-value x 'name))
                                         (assoc-value response 'labels))
                     :user user))))

(defun get-item-ids-by-tag (user tag)
  "Get all incomplete item ids with a given tag."
  (let ((response 
         (request "query" `(("token" . ,(api-token user))
                            ("queries" . ,(format nil "[\"@~A\"]" tag))))))
    (mapcar (fn (assoc-value % 'item-id)) (cdaar response))))

(defun get-item-by-id ())
