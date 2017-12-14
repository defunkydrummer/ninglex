# Ninglex

A really tiny, ready-to-go micro web framework for simple, quick and dirty stuff, based on Ningle. 
It is ready to go, learning curve almost zero. 

## What is it good for?

* When you are coding a bit of Lisp and you need to output some HTML locally on your browser, or JSON, or etc, in the minimal amount of time

* When you are a beginner and want your Lisp to serve HTML pages or you want to write a minimal Lisp backend.

## Why?
Not as small as Ningle and not as big as Caveman2. 

Ninglex is only about defining your routes and your route handlers. Starting and stopping the server. The rest is left to your control.

Underlying Ninglex is Eitaro Fukamachi's Clack & Lack, which allows different servers, so your app can be hosted usingHunchentoot Wookie, etc. What this means is that there's something that you want to do with Ninglex and you don't know how to do, you can do it by glancing at Clack and Lack's documentation. 

## Usage

See example directory and load system "ninglex-example". Don't have time for that? This is most of *example.lisp*, assuming you have loaded libraries "jonathan" and "spinneret", otherwise example 2 and 3 below will not work:

```common-lisp
;; load lib
(ql:quickload :ninglex)
(in-package :ninglex)


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


;; so we start the server!!

(start-example )


```

The above example showed 90% of what you need about Ninglex. 
Want to define a handler for POST requests? use :POST instead of :GET on set-route.

## Acknowledgements

Ninglex is Ningle eXtended, that is, it is based on Eitaro Fukamachi's [ningle](https://github.com/fukamachi/ningle).

## License

Licensed under the MIT license.
