#!/usr/bin/sbcl --script
; Luna - Main entry point
; --------------------------------------------------------------------------------------

(defun print-usage ()
  (format t "Luna - Bluesky Client~%~%")
  (format t "Usage:~%")
  (format t "  luna                     Start interactive REPL mode~%")
  (format t "  luna <command> [args]    Run a single command~%~%")
  (format t "Commands:~%")
  (format t "  create-session           Login to Bluesky~%")
  (format t "  refresh-session          Refresh authentication~%")
  (format t "  get-profile              Show your profile~%")
  (format t "  get-timeline             Show your timeline~%")
  (format t "  get-follows              List who you follow~%")
  (format t "  get-followers            List your followers~%")
  (format t "  create-record [text]     Create a post~%~%")
  (format t "Options:~%")
  (format t "  --repl                   Force REPL mode~%")
  (format t "  --help                   Show this help~%")
  (format t "  --version                Show version~%"))

(let ((args (cdr sb-ext:*posix-argv*)))
  (cond
    ; Help option
    ((or (member "--help" args :test #'string=)
         (member "-h" args :test #'string=))
     (print-usage))
    
    ; Version option
    ((or (member "--version" args :test #'string=)
         (member "-v" args :test #'string=))
     (format t "{\"version\": \"v0.0.05\"}"))
    
    ; REPL mode (explicit or no args)
    ((or (null args)
         (member "--repl" args :test #'string=))
     (load "repl.lisp"))
    
    ; Single command mode
    (t
     (load "bluesky-client.lisp"))))
