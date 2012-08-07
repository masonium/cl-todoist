;;;; cl-todoist.asd
(asdf:defsystem #:cl-todoist
  :serial t
  :depends-on ("alexandria" "iterate" "drakma"
               "cl-json" "anaphora" "cl-anonfun" "metatilities"
               "local-time")
  :components ((:file "package")
               (:file "user")
               (:file "util")
               (:file "project")
               (:file "cl-todoist")))

