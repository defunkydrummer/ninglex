# Ninglex

A really tiny, ready-to-go micro web framework for simple, quick and dirty stuff, based on [Ningle](https://github.com/fukamachi/ningle). 
It is ready to go, learning curve almost zero. 

## What is it good for?

* When you are coding a bit of Lisp and you need to output some HTML locally on your browser, or JSON, or etc, in the minimal amount of time

* When you are a beginner and want your Lisp to serve HTML pages or you want to write a minimal Lisp backend.

## Why?
Not as small as Ningle and not as big as Caveman2.  
Ningle was too minimal, so Ninglex adds just a few functions and macros on top of Ningle, then gets you ready to go!

Ninglex is only about defining your routes and your route handlers. Starting and stopping the server. The rest is left to your control.

Underlying Ninglex is Eitaro Fukamachi's Clack & Lack, which allows different servers, so your app can be hosted using Hunchentoot Wookie, etc. What this means is that there's something that you want to do with Ninglex and you don't know how to do, you can do it by glancing at Clack and Lack's documentation. 

## Usage

See example directory and load system "ninglex-example". Don't have time for that? This is most of *example.lisp*, assuming you have loaded libraries "jonathan" and "spinneret", otherwise example 2 and 3 below will not work:

*For newbies: Make sure you load the package.  For this example we'll be inside the package "Ninglex". If we are on other package we'll have to prefix all function calls with "ninglex:"*

```common-lisp
;; load lib
(ql:quickload :ninglex)
(in-package :ninglex)
```

Example 1: Define your own route handler function, which will take two params: "age" and "name".

This will make your server answer GET requests on http://localhost:5000 

The supplied parameters "name" and "age" will be available as "n" and "a" values. 

```common-lisp
(defun my-fun (params)
  (with-request-params params ((n "name") (a "age"))
    (string-response
     (format nil "Hello, ~a of ~a years old!" n a))))

;; and then, I bind this function to a route 
(set-route "/hello" #'my-fun)
```

Now we need to start the server
```common-lisp
(start ) 
```

Try: http://localhost:5000/hello?name=XYZ&age=99

(stop) stops the server.

We can do the same, but in less lines, without having to do a "defun".

```common-lisp
(with-route ("/hello2" my-params)
  (with-request-params my-params ((n "name") (a "age"))
      (string-response
       (format nil "Hello, ~a of ~a years old!" n a))))

```

Want to capture parametrized URLs? This example is useful:
*(here we are using spinneret to output html)*

```common-lisp
(with-route ("/person/:name" params)
  (with-request-params params ((n :name))
    (ninglex:html-response
     (with-output-to-string (*html*)
       (with-html
         (:p :class "title is-1" n)

;; etc
```


Want to output JSON? make sure you load the Jonathan library (*Newbies: do (ql:quickload "jonathan")*) 

```common-lisp
;; FYI: "jojo" is synonymous for the Jonathan package
(with-route ("/jsontest" params) 
  (declare (ignore params))
  (json-response ;like string-response but sets correct http content-type
   (jojo:to-json '(:|name| "Common Lisp" :born 1984 :impls (SBCL CLISP)))))
```
Try: http://localhost:5000/jsontest

Want to output HTML? Ok, let's use the "spinneret" library by Ruricolist (of course you can use other HTML library):

```common-lisp
(with-route ("/html-hello" params)
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
```
The above example uses a static file dir thus needs the following:

```common-lisp
;; Set static root directory for serving the static files
(defparameter *static-root*
  (merge-pathnames #P"static/"
                   (uiop:pathname-directory-pathname
                    (or *load-pathname*
                        *compile-file-pathname*))))

(defun start-example ()
  "Start the server"
  (start :static-root *static-root*))
```

We can (stop ) the server and start again:
```common-lisp
(start-example )
```

The above example showed 90% of what you need about Ninglex. 
Want to define a handler for POST requests? use :POST instead of :GET on set-route or with-route

More info available by taking a look at [Ningle](https://github.com/fukamachi/ningle).


## Acknowledgements

Ninglex is Ningle eXtended, that is, it is based on Eitaro Fukamachi's [ningle](https://github.com/fukamachi/ningle).
As well as based of course in Eitaro's Clack and Lack. 
Thanks Eitaro!

## License

Licensed under the MIT license.
