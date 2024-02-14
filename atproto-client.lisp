#!/usr/bin/env sbcl --script

(load "~/quicklisp/setup.lisp")
(load "settings.lisp")

(let ((*standard-output* (open "/dev/null" :direction :output :if-exists :append)))
    (ql:quickload 'drakma)
    (ql:quickload 'cl-json))

(defparameter *access-jwk* "")
(defparameter *endpoint* (settings:get-endpoint (string "login")))
(defparameter *request-json* (settings:get-json (string "identifier")))

(defvar headers `(("Authorization" . ,(format nil "Bearer ~A" *access-jwk*))))

(defun call-api ()
  (drakma:http-request
    *endpoint*
    :method :post
    :content-type "application/json"
;    :additional-headers headers
    :content (read-file-into-string *request-json*)))

;(defun loading-config-file (file)
;  (let ((config (read-file-into-string file)))
;        (cl-json:decode-json-from-string config)))

(defun read-file-into-string (file)
  (with-open-file (stream file)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defvar response
  (cl-json:decode-json-from-string 
    (map 'string #'code-char (call-api))))

(with-open-file (stream "did.json"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream "~A" (map 'string #'code-char (call-api))))
