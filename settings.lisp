(defpackage :settings (:use :cl))
(in-package :settings)

(defvar endpoints
  '((:login         . "https://bsky.social/xrpc/com.atproto.server.createSession")
    (:get-profile   . "https://bsky.social/xrpc/app.bsky.actor.getProfile")
    (:create-record . "https://bsky.social/xrpc/com.atproto.repo.createRecord")))

; --------------------------------------------------------------------------

(defun get-endpoint (command)
  (cdr (assoc (intern (string-upcase command) :keyword) endpoints)))

(defun get-json (file-name) 
  (format nil "~A.json" file-name))

(export 'get-endpoint :settings)
(export 'get-json :settings)
