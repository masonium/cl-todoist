;;;; cl-todoist.asd
(asdf:defsystem #:cl-todoist
  :serial t
  :depends-on ("alexandria" "iterate" "drakma"
               "cl-json" "anaphora" "cl-anonfun" "metatilities")
  :components ((:file "package")
               (:file "project")
               (:file "cl-todoist")))

