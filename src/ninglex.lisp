(in-package :cl-user)
(defpackage ninglex
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:export
   :*app*
   :*catch-errors*
   :*show-errors*
   :*http-status-codes*
   :*handler*
   :with-request-params
   :set-route
   :with-route
   :string-response
   :html-response
   :json-response
   :get-param-value
   :start
   :stop))

(in-package :ninglex)

;; ninglex:*app*
;; 'global' reference to the instance of your application
(defvar *app* (make-instance 'ningle:<app>)
  "Object representing your Ningle/Ninglex application.")

;; Left there so you can use them someday...
(defparameter *http-status-codes*
  '(:ok 200
    :not-found 404
    :server-error 500)
  "Useful HTTP status codes")

(defun get-param-value (param-list name)
  (declare (type (or symbol string) name)
           (type list param-list))
  "Obtain the value of a request parameter called 'name' (string)"
  (cdr (assoc name param-list :test 'equal)))

(defmacro with-request-params (params-variable param-list &body body)
  (declare (type list param-list) (type symbol params-variable))
  "Binds symbols to values corresponding to the request's param values,
then executes the body.

Params-variable is the variable that has the params.
Param-list should be list of (symbol param-name-as-string).
"
  `(symbol-macrolet
       ,(mapcar (lambda (entry)
                  (let ((var-name 
                          (first entry)) ; name of var to create (symbol)
                        (param-name   ; name of param (string)
                          (second entry)))
                    (list var-name ; define symbol
                          ;; which will expand to:
                          `(get-param-value ,params-variable ,param-name)
                          )))
         param-list)
     ,@body))

;; Important Note: Clack expects that the return value from
;; the route function is:
;; (STATUS-CODE list body)
;; where body must be a vector of character, OR otherwise
;; it can be a list of strings. 
(defun string-response (text-string &key
                                      (status-code 200)
                                      (content-type "text/plain"))
  "Creates standard text (string) response for use in route handlers."
  `(,status-code
    (:content-type ,content-type)
    (,text-string)))  

;; HTML response
(defmacro html-response (html-string &rest args)
  "Creates standard HTML response from string, for use in route handlers."
  `(string-response ,html-string :content-type "text/html" ,@args))
                  
;; JSON response
(defmacro json-response (html-string &rest args)
  "Standard response for JSON"
  `(string-response ,html-string :content-type "application/json" ,@args))
                   

(defun set-route
    (route function &key (method :GET))
  "Assign function to route"
  (setf (ningle:route *app* route :method method)
        function))

(defparameter *catch-errors* t
  "When t, handle top level errors inside `with-route', but responding with a 500 error.")
  
(defparameter *show-errors* nil
  "When t and `*catch-errors*' is t, send the condition in the response body.")

(defmacro with-route ((route-string params-var &key (method :GET)) &body body)
  "When calling the route, execute the body, binding the params to params-var"
  `(set-route
    ,route-string
    (lambda (,params-var)
      (declare (ignorable ,params-var))
      (block top-level-handler
          (handler-bind
              ((serious-condition
                 (lambda (condition)
                   (when *catch-errors*
                     (format *error-output* "~A~%" condition)
                     (return-from top-level-handler
                       (string-response
                        (when *show-errors*
                          (format nil "~A" condition))
                        :status-code 500))))))
            ,@body)))
    :method ,method))
  

;; Handler for start/stop.
(defvar *handler* nil
  "Handler for starting/stopping the server")

;; Quick and dirty function to start server
(defun start ( &key (server :hunchentoot)
                    (port 5000)
                    (address "127.0.0.1")
                    (debug t)
                    (silent nil)
                    (use-thread t)
                    ;; note: needs trailing / to correctly capture file URLs
                    (static-path "/static/")
                    ;; note: needs trailing / to work, as well
                    (static-root #P"/static/"))
  "Start the server."
  (when *handler*
    "Server already started!")
  (unless *handler*
    (format t  "Ninglex: Starting server in port ~d... ~%" port)
    (setf *handler*
          (clack:clackup
           ;; use lack builder to enable middlewares
           (lack.builder:builder
            ;; session lack middleware
            :session
            ;; "static" lack middleware
            (:static :path static-path
                     :root static-root)
            *app*)
           ;; Clackup options
           :server server
           :address address
           :port port
           :debug debug
           :silent silent
           :use-thread use-thread
           ))))

;; Stop server
(defun stop ()
  "Stop the server"
  (when *handler*
    (format t "Ninglex: Stopping server... ~%")
    (clack:stop *handler*)
    (setf *handler* nil) ;Clear the handler (TODO: is this ok? )
    ))

