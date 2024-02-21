; This is the wapper of Bluesky API.
; https://www.docs.bsky.app/
; --------------------------------------------------------------------------------------
(load "~/quicklisp/setup.lisp")
(load "settings.lisp")

(let
  ((*standard-output*
     (open "/dev/null" :direction :output :if-exists :append)))
  (ql:quickload 'drakma)
  (ql:quickload 'cl-json)
  (ql:quickload 'local-time))


; --------------------------------------------------------------------- Basic functions.
(defun get-endpoint (command)
  (format nil "~A/xrpc/~A" service
          (cdr (assoc (intern (string-upcase command) :keyword) endpoints))))


(defun get-local-time ()
  (string
    (local-time:format-timestring
      nil (local-time:now) :format local-time:+iso-8601-format+)))


(defun get-help ()
  (format t "~A" error-message))


(defun get-version ()
  (format t "~A" version))


(defun get-lexicon (lexicon)
  (loading-config-file
    (format nil "atproto/lexicons/~A.json" (substitute #\/ #\. lexicon))))


(defun get-authorization-access-jwk ()
  `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk)))))


(defun get-authorization-refresh-jwk ()
  `(("Authorization" . ,(format nil "Bearer ~A" (get-refresh-jwk)))))


(defun loading-config-file (file)
  (let ((config (read-file-into-string file)))
        (cl-json:decode-json-from-string config)))


(defun read-file-into-string (file)
  (with-open-file (stream file)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))


(defun write-file-from-string (input output)
  (with-open-file
    (stream output
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create)
    (format stream "~A" input)))


(defun set-options (args) (format nil "~A" (car args)))


(defun invoke-command-safely (args)
  (let ((endpoint (get-endpoint (car args))))
    (if (eq endpoint '()) (get-help)
      (let ((options (set-options (cdr args))))
        (funcall (intern (string-upcase (car args))))))))


; ------------------------------------------------------------- Get the value from DID.
(defun get-access-jwk ()
  (if (probe-file did-json)
    (cdr (assoc :ACCESS-JWT (loading-config-file did-json) :test #'equal))))


(defun get-refresh-jwk ()
  (if (probe-file did-json)
    (cdr (assoc :REFRESH-JWT (loading-config-file did-json) :test #'equal))))


(defun get-did ()
  (if (probe-file did-json)
    (cdr (assoc :DID (loading-config-file did-json) :test #'equal))))


; ------------------------------------------------------------------ Call API commands.
(defun create-session ()
  (write-file-from-string
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :post
           :accept "application/json"
           :content-type "application/json"
           :content (read-file-into-string identifier-json))) did-json))


(defun refresh-session ()
  (write-file-from-string
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :post
           :accept "application/json"
           :additional-headers (get-authorization-refresh-jwk))) did-json))


(defun get-profile ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :get
           :accept "application/json"
           :additional-headers (get-authorization-access-jwk)
           :parameters `(("actor" . ,(get-did)))))))


(defun get-actor-feeds ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :get
           :additional-headers (get-authorization-access-jwk)
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun get-timeline ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :get
           :accept "application/json"
           :additional-headers (get-authorization-access-jwk)
           ;:parameters `(("algorithm" . "")
           ;              ("limit" . 100)
           ;              ("cursor" . "")))))
           :parameters nil
           ))))


(defun get-follows ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :get
           :accept "application/json"
           :additional-headers (get-authorization-access-jwk)
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun get-followers ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :get
           :accept "application/json"
           :additional-headers (get-authorization-access-jwk)
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun create-record ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           endpoint
           :method :post
           :accept "application/json"
           :content-type "application/json"
           :additional-headers (get-authorization-access-jwk)
           :content (cl-json:encode-json-to-string
                      `(("repo" . ,(get-did))
                        ("collection" . "app.bsky.feed.post")
                        ;("rkey" . "")
                        ;("validate" . nil)
                        ;("swapCommit" . "")
                        ("record" . (("$type" . "app.bsky.feed.post")
                                     ("text"  . "I guess tomorrow will be mostly rainy.")
                                     ("createdAt" . ,(get-local-time))))))))))


(if (eq (cdr *posix-argv*) '()) (get-help)
  (funcall #'invoke-command-safely (cdr *posix-argv*)))
