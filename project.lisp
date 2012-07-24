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
