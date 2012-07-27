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
                                       (gethash method +methods-map+ "method"))
                               :parameters parameters)))
    (if json-only
        response
        (decode-json-from-string* response))))

(defun get-all (user)
  (request "sync-get" `(("token" . ,(api-token user)))) t)
