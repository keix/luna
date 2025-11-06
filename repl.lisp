; Luna REPL - Interactive Bluesky Client
; --------------------------------------------------------------------------------------

(load "~/quicklisp/setup.lisp")
(load "config.lisp")
(load "settings.lisp")

(let
  ((*standard-output*
     (open luna.config:*null-device* :direction :output :if-exists :append)))
  (ql:quickload 'drakma)
  (ql:quickload 'cl-json)
  (ql:quickload 'local-time)
  ; cl-readline is optional, fallback to basic input
  (handler-case
      (ql:quickload 'cl-readline)
    (error () nil)))

; Load the main client functions without executing main logic
(defvar *luna-repl-mode* t)
(load "bluesky-client.lisp")
(load "json-pretty.lisp")

(defpackage :luna.repl
  (:use :cl)
  (:export #:start-repl))

(in-package :luna.repl)

(defparameter *repl-running* t)
(defparameter *current-session* nil)
(defparameter *prompt* "luna> ")

(defstruct luna-session
  did
  handle
  access-jwt
  refresh-jwt
  endpoint-url)

(defun print-banner ()
  (format t "~%")
  (format t "╔═══════════════════════════════════════════════════════════════╗~%")
  (format t "║                     Luna Bluesky Client                       ║~%")
  (format t "╚═══════════════════════════════════════════════════════════════╝~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  post [text]    - Post a message~%")
  (format t "  timeline       - Show your timeline~%")
  (format t "  profile        - Show your profile~%")
  (format t "  follows        - List who you follow~%")
  (format t "  followers      - List your followers~%")
  (format t "  login          - Login/refresh session~%")
  (format t "  help           - Show this help~%")
  (format t "  exit/quit      - Exit Luna~%")
  (format t "~%"))

(defun ensure-session ()
  "Ensure we have a valid session"
  (if (and *current-session* (probe-file cl-user::did-json))
      t
      (progn
        (format t "Not logged in. Please use 'login' first.~%")
        nil)))

(defun update-session ()
  "Update session from did.json"
  (when (probe-file cl-user::did-json)
    (let ((session-data (cl-user::loading-config-file cl-user::did-json)))
      (setf *current-session*
            (make-luna-session
             :did (cdr (assoc :DID session-data :test #'equal))
             :handle (cdr (assoc :HANDLE session-data :test #'equal))
             :access-jwt (cdr (assoc :ACCESS-JWT session-data :test #'equal))
             :refresh-jwt (cdr (assoc :REFRESH-JWT session-data :test #'equal))
             :endpoint-url (cl-user::get-endpoint "create-record"))))))

(defun multiline-input ()
  "Get multiline input for post text"
  (format t "Enter your post (empty line to finish):~%")
  (let ((lines '()))
    (loop
      (let ((line (read-line)))
        (if (string= line "")
            (return)
            (push line lines))))
    (format nil "~{~A~^~%~}" (reverse lines))))

(defun handle-post-command (args)
  "Handle post command with optional text"
  (unless (ensure-session) (return-from handle-post-command))
  (let* ((text (if (and args (not (string= (first args) "")))
                   (first args)
                   (multiline-input)))
         (endpoint-url (cl-user::get-endpoint "create-record")))
    (when (and text (not (string= text "")))
      (format t "Posting: ~A~%" text)
      (cl-user::create-record endpoint-url text)
      (format t "~%Posted successfully!~%"))))

(defun handle-timeline-command ()
  "Show timeline"
  (unless (ensure-session) (return-from handle-timeline-command))
  (let ((endpoint-url (cl-user::get-endpoint "get-timeline")))
    (let* ((response (with-output-to-string (*standard-output*)
                       (cl-user::get-timeline endpoint-url)))
           (formatted (luna.json:format-timeline response)))
      (format t "~A~%" formatted))))

(defun handle-profile-command ()
  "Show profile"
  (unless (ensure-session) (return-from handle-profile-command))
  (let ((endpoint-url (cl-user::get-endpoint "get-profile")))
    (let* ((response (with-output-to-string (*standard-output*)
                       (cl-user::get-profile endpoint-url)))
           (formatted (luna.json:format-profile response)))
      (format t "~A~%" formatted))))

(defun handle-follows-command ()
  "Show follows"
  (unless (ensure-session) (return-from handle-follows-command))
  (let ((endpoint-url (cl-user::get-endpoint "get-follows")))
    (cl-user::get-follows endpoint-url)
    (format t "~%")))

(defun handle-followers-command ()
  "Show followers"
  (unless (ensure-session) (return-from handle-followers-command))
  (let ((endpoint-url (cl-user::get-endpoint "get-followers")))
    (cl-user::get-followers endpoint-url)
    (format t "~%")))

(defun handle-login-command ()
  "Login or refresh session"
  (let ((endpoint-url (cl-user::get-endpoint "create-session")))
    (if (probe-file cl-user::identifier-json)
        (progn
          (format t "Logging in...~%")
          (cl-user::create-session endpoint-url)
          (update-session)
          (if *current-session*
              (format t "Logged in as @~A~%" (luna-session-handle *current-session*))
              (format t "Login failed. Check your credentials.~%")))
        (format t "No identifier.json found. Please create it first.~%"))))

(defun parse-command (input)
  "Parse command and arguments from input"
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) input)))
    (if (string= trimmed "")
        (values "" nil)
        (let ((space-pos (position #\Space trimmed)))
          (if space-pos
              (values (subseq trimmed 0 space-pos)
                      (list (string-trim '(#\Space #\Tab) (subseq trimmed (1+ space-pos)))))
              (values trimmed nil))))))

(defun handle-command (command args)
  "Dispatch command to appropriate handler"
  (let ((clean-command (string-trim '(#\Space #\Tab #\Newline) command)))
    (cond
      ((or (string-equal clean-command "exit")
           (string-equal clean-command "quit"))
       (setf *repl-running* nil)
       (format t "Goodbye!~%"))
    
      ((string-equal clean-command "help")
       (print-banner))
      
      ((string-equal clean-command "post")
       (handle-post-command args))
      
      ((string-equal clean-command "timeline")
       (handle-timeline-command))
      
      ((string-equal clean-command "profile")
       (handle-profile-command))
      
      ((string-equal clean-command "follows")
       (handle-follows-command))
      
      ((string-equal clean-command "followers")
       (handle-followers-command))
      
      ((string-equal clean-command "login")
       (handle-login-command))
      
      ((string= clean-command "")
       ; Empty command, do nothing
       )
      
      (t
       (format t "Unknown command: '~A'. Type 'help' for available commands.~%" clean-command)))))

(defun start-repl ()
  "Start the Luna REPL"
  (print-banner)
  
  ; Try to load existing session
  (update-session)
  (when *current-session*
    (format t "Logged in as @~A~%" (luna-session-handle *current-session*)))
  
  ; Main REPL loop
  (loop while *repl-running* do
    (handler-case
        (progn
          (format t "~A" *prompt*)
          (force-output)
          (let ((input (read-line)))
            (when input
              (multiple-value-bind (command args)
                  (parse-command input)
                (handle-command command args)))))
      (end-of-file ()
        (setf *repl-running* nil)
        (format t "~%Goodbye!~%"))
      (error (e)
        (format t "Error: ~A~%" e)))))

; Start REPL automatically when this file is loaded
(luna.repl:start-repl)

