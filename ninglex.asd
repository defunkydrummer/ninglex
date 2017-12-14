(asdf:defsystem ninglex
  :version "0.1"
  :author "Flavio Egoavil"
  :license "MIT"
  :depends-on (:ningle
               :lack 
               :clack 
               )
  :components ((:module "src"  ; src/  dir
                :components
                ((:file "ninglex")
                )))

  
  :description "Ninglex micro-web framework"
  :long-description
  #.(with-open-file (stream
                     (merge-pathnames
                             #p"README.markdown"
                            (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))

;; ;; package with globals for app
;; (defpackage #:ninglex-config (:export #:*base-directory*))

;; ;; base directory 
;; (defparameter ninglex-config:*base-directory* 
;;   (make-pathname :name nil :type nil :defaults *load-truename*))
