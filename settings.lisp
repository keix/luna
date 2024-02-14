(defpackage :settings (:use :cl))
(in-package :settings)

(defvar endpoints
  '((:login . "https://bsky.social/xrpc/com.atproto.server.createSession")))

; --------------------------------------------------------------------------

(defun get-endpoint (command)
  (cdr (assoc (intern (string-upcase command) :keyword) endpoints)))

(defun get-json (file-name) 
  (format nil "~A.json" file-name))

(export 'get-endpoint :settings)
(export 'get-json :settings)
