; This is the wapper of Bluesky API.
; https://docs.bsky.app/
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
  (let ((api-path (cdr (assoc (intern (string-upcase command) :keyword) endpoints))))
    (when api-path
      (format nil "~A/xrpc/~A" service api-path))))


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
    (when config
      (handler-case
        (cl-json:decode-json-from-string config)
        (error (e)
          (format *error-output* "Error parsing JSON from ~A: ~A~%" file e)
          nil)))))


(defun read-file-into-string (file)
  (handler-case
    (with-open-file (stream file)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream) contents))
    (error (e)
      (format *error-output* "Error reading file ~A: ~A~%" file e)
      nil)))


(defun write-file-from-string (input output)
  (with-open-file
    (stream output
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create)
    (format stream "~A" input)))


(defun set-options (args) 
  (format nil "~A" (car args)))


(defun invoke-command-safely (args)
  (let* ((command (car args))
         (endpoint-url (get-endpoint command)))
    (if (null endpoint-url)
        (get-help)
        (progn
          (setf endpoint endpoint-url)
          (funcall (intern (string-upcase command)))))))


; ------------------------------------------------------------- Get the value from DID.
; Generic function for getting values from DID JSON
(defun get-did-value (key)
  (when (probe-file did-json)
    (cdr (assoc key (loading-config-file did-json) :test #'equal))))

(defun get-access-jwk ()
  (get-did-value :ACCESS-JWT))

(defun get-refresh-jwk ()
  (get-did-value :REFRESH-JWT))

(defun get-did ()
  (get-did-value :DID))

; ------------------------------------------------------------------ Call API commands.
; Generic function for GET API calls
(defun call-get-api (endpoint-url &key actor-param other-params)
  (let* ((headers (get-authorization-access-jwk))
         (parameters (append
                       (when actor-param `(("actor" . ,(get-did))))
                       other-params))
         (request-spec (build-get-request endpoint-url headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))

; Pure functions for building requests
(defun build-session-request (endpoint-url identifier-content)
  `(:endpoint ,endpoint-url
    :method :post
    :accept "application/json"
    :content-type "application/json"
    :content ,identifier-content))

(defun build-refresh-request (endpoint-url refresh-headers)
  `(:endpoint ,endpoint-url
    :method :post
    :accept "application/json"
    :additional-headers ,refresh-headers))

(defun build-get-request (endpoint-url headers &optional parameters)
  `(:endpoint ,endpoint-url
    :method :get
    :accept "application/json"
    :additional-headers ,headers
    :parameters ,parameters))

(defun build-post-request (endpoint-url headers content)
  `(:endpoint ,endpoint-url
    :method :post
    :accept "application/json"
    :content-type "application/json"
    :additional-headers ,headers
    :content ,content))

; Pure functions for processing responses
(defun process-response (response)
  (map 'string #'code-char response))

; Function to execute HTTP request (side effect isolated here)
(defun execute-request (request-spec)
  (let ((endpoint (getf request-spec :endpoint))
        (args (loop for (key value) on request-spec by #'cddr
                    unless (eq key :endpoint)
                    collect key and collect value)))
    (apply #'drakma:http-request endpoint args)))
(defun create-session ()
  (let* ((identifier-content (read-file-into-string identifier-json))
         (request-spec (build-session-request endpoint identifier-content))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (write-file-from-string processed-response did-json)))


(defun refresh-session ()
  (let* ((refresh-headers (get-authorization-refresh-jwk))
         (request-spec (build-refresh-request endpoint refresh-headers))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (write-file-from-string processed-response did-json)))


(defun get-profile ()
  (call-get-api endpoint :actor-param t))


(defun get-actor-feeds ()
  (call-get-api endpoint :actor-param t
                ;:other-params '(("limit" . 50) ("cursor" . ""))
                ))


(defun get-timeline ()
  (call-get-api endpoint))


(defun get-follows ()
  (call-get-api endpoint :actor-param t
                ;:other-params '(("limit" . 50) ("cursor" . ""))
                ))


(defun get-followers ()
  (call-get-api endpoint :actor-param t
                ;:other-params '(("limit" . 50) ("cursor" . ""))
                ))


(defun create-record ()
  (let* ((headers (get-authorization-access-jwk))
         (content (cl-json:encode-json-to-string
                    `(("repo" . ,(get-did))
                      ("collection" . "app.bsky.feed.post")
                      ;("rkey" . "")
                      ;("validate" . nil)
                      ;("swapCommit" . "")
                      ("record" . (("$type" . "app.bsky.feed.post")
                                   ("text"  . "I guess tomorrow will be mostly rainy.")
                                   ("createdAt" . ,(get-local-time)))))))
         (request-spec (build-post-request endpoint headers content))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


(if (eq (cdr *posix-argv*) '()) (get-help)
  (funcall #'invoke-command-safely (cdr *posix-argv*)))
