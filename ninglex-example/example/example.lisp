(in-package :cl-user)
(defpackage ninglex.example
  (:use :cl :ninglex :ninglex.util)
  (:import-from :lack.builder
   :builder)
  (:import-from :spinneret
   :*html*
                :with-html))
(in-package :ninglex.example)

;; example 1. I define my own 
;; route handler function, which will take two params: "age" and "name"
(defun my-fun (params)
  (with-request-params params ((n "name") (a "age"))
    (string-response
     (format nil "Hello, ~a of ~a years old!" n a))))

;; and then, I bind this function to a route 
(set-route "/hello" #'my-fun)

;; try: http://localhost:5000/hello?name=XYZ&age=99

;; example 1.1: shorthand for the same
(with-route ("/hello2" my-params)
  (with-request-params my-params ((n "name") (a "age"))
      (string-response
       (format nil "Hello, ~a of ~a years old!" n a))))

;; try: http://localhost:5000/hello2?name=XYZ&age=99

;; example 1.2: The same but done the old Ningle way, if you like:
(setf (ningle:route ninglex:*app* "/oldeway" :method :GET)
      #'(lambda (params)          
          (format nil "Hello, ~A" (cdr (assoc "name" params :test 'equal )))
          ))

;; example 2. route that outputs Json.
;; FYI: "jojo" is synonymous for the Jonathan package
(defun json-test (params)
  (declare (ignore params))
  (string-response
   (jojo:to-json '(:|name| "Common Lisp" :born 1984 :impls (SBCL CLISP)))))

(set-route "/jsontest" #'json-test)

;; try: http://localhost:5000/jsontest

;; example 3. HTML output example with spinneret
(defun html-hello (params)
  (declare (ignore params))
  (html-response  ;this just sets the content-type accordingly
   (with-output-to-string (*html*)
     (with-html
       (:doctype)
       (:html
        (:head
         (:title "title"))
        (:body (:h1 "Hello Common Lisp!")
               (:img :src "static/logo-compact.png")))))))

(set-route "/html-hello" #'html-hello :method :GET)

;; try: http://localhost:5000/html-hello

;; The above example uses a static file dir thus needs the following:
;; Set static root directory for serving the static files
(defparameter *static-root*
  (merge-pathnames #P"static/"
                   (uiop:pathname-directory-pathname
                    (or *load-pathname*
                        *compile-file-pathname*))))

(defun start-example ()
  "Start the server"
  (start :static-root *static-root*))

