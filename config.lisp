; Configuration management for Luna Bluesky client
; --------------------------------------------------------------------------------------

(defpackage :luna.config
  (:use :cl)
  (:export #:*quicklisp-path*
           #:*null-device*
           #:*lexicon-dir*
           #:*did-json-path*
           #:*identifier-json-path*
           #:*service-url*
           #:*default-post-text*
           #:*post-collection*
           #:load-config))

(in-package :luna.config)

; Default configuration values
(defparameter *quicklisp-path* 
  (or (uiop:getenv "LUNA_QUICKLISP_PATH")
      "~/quicklisp/setup.lisp"))

(defparameter *null-device*
  (if (member :win32 *features*)
      "NUL"
      "/dev/null"))

(defparameter *lexicon-dir*
  (or (uiop:getenv "LUNA_LEXICON_DIR")
      "atproto/lexicons/"))

(defparameter *did-json-path*
  (or (uiop:getenv "LUNA_DID_PATH")
      "atproto/client/did.json"))

(defparameter *identifier-json-path*
  (or (uiop:getenv "LUNA_IDENTIFIER_PATH")
      "atproto/client/identifier.json"))

(defparameter *service-url*
  (or (uiop:getenv "LUNA_SERVICE_URL")
      "https://bsky.social"))

(defparameter *default-post-text*
  (or (uiop:getenv "LUNA_DEFAULT_POST_TEXT")
      "Hello from Bluesky!"))

(defparameter *post-collection*
  "app.bsky.feed.post")

; Configuration file support
(defun config-file-path ()
  (merge-pathnames ".luna/config.lisp" (user-homedir-pathname)))

(defun load-config ()
  "Load user configuration if it exists"
  (let ((config-file (config-file-path)))
    (when (probe-file config-file)
      (load config-file))))

; Load user config on package load
(load-config)