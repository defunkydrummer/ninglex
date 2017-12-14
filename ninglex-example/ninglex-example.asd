(asdf:defsystem ninglex-example
  :version "0.1"
  :author "Flavio Egoavil"
  :license "MIT"
  :depends-on (:ninglex
               :jonathan  ;; used in example
               :uiop  ;; used in example
               :spinneret ;; used in example
               )
  :components ((:module "example"  ; example/  dir
                :components
                ((:file "util")
                 (:file "example" :depends-on ("util")))))

  
  :description "Ninglex micro-web framework example"
  :long-description "")
