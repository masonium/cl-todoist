(in-package :cl-todoist)

(defclass* project ()
  (user-id
   name
   color
   item-order
   cache-count
   indent
   id)
  (:automatic-accessors))

(defmethod print-object ((project project) str)
  (print-unreadable-object (project str :type t :identity nil)
    (format str "~A" (name project))))

(defun get-projects (user)
  (let ((response (request "getProjects" (list (cons "token" (api-token user))))))
    (mapcar (fn (json:make-object % 'project)) response)))

(defclass* item ()
  (id
   project-id
   user-id
   due-date
   in-history
   content
   checked)
  (:automatic-accessors))

(defmethod print-object ((item item) str)
  (print-unreadable-object (item str :type t :identity nil)
    (format str "(~A) ~A" (id item)
            (let ((desc (content item)))
              (if (<= (length desc) 30)
                  desc
                  (format nil "~A..." (subseq desc 0 27)))))))

(defun get-uncompleted-items (user project)
  (let ((response (request "getUncompletedItems"
                           (list (cons "token" (api-token user))
                                 (cons "project_id" (format nil "~A" (id project)))))))
    (mapcar (fn (json:make-object % 'item)) response))))

(defun get-completed-items (user project &key interval label)
  (let* ((method (if (and (null interval) (null label) project)
                     "getCompletedItems"
                     "getAllCompletedItems"))
         (project-option
            (when project
              (let ((project-id
                      (etypecase project
                        (string project)
                        (integer (format nil "~A" project))
                        (project (format nil "~A" (id project))))))
                (list (cons "project_id" project-id)))))
         (interval-option (when interval (list (cons "interval" interval))))
         (label-option (when label (list (cons "label" label))))
         (response (request method
                            (append (list (cons "token" (api-token user)))
                                    project-option interval-option label-option))))
    (mapcar (fn (json:make-object % 'item))
            (if (string= method "getCompletedItems")
                response
                (cdr (assoc 'items response))))))
