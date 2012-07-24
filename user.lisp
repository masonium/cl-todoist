(in-package :cl-todoist)

(defclass* user ()
  ((api-token :initform nil)
   full-name
   mobile-number
   timezone
   id
   email)
  :automatic-initargs
  :automatic-accessors)

(defmethod print-object ((user user) str)
  (print-unreadable-object (user str :type t :identity nil)
    (format str "~A (~A) " (id user) (email user))))

(defun login (email password)
  (let ((response (request "login" `(("email" . ,email)
                                     ("password" . ,password)))))
    (if (and (stringp response) (string= response "LOGIN_ERROR"))
        (error "Could not login. Incorrect credentials.")
        (json:make-object response 'user))))
