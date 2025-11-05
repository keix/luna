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
; Pure functions for building requests
(defun build-session-request (identifier-content)
  `(:endpoint ,endpoint
    :method :post
    :accept "application/json"
    :content-type "application/json"
    :content ,identifier-content))

(defun build-refresh-request (refresh-headers)
  `(:endpoint ,endpoint
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
         (request-spec (build-session-request identifier-content))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (write-file-from-string processed-response did-json)))


(defun refresh-session ()
  (let* ((refresh-headers (get-authorization-refresh-jwk))
         (request-spec (build-refresh-request refresh-headers))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (write-file-from-string processed-response did-json)))


(defun get-profile ()
  (let* ((headers (get-authorization-access-jwk))
         (parameters `(("actor" . ,(get-did))))
         (request-spec (build-get-request endpoint headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


(defun get-actor-feeds ()
  (let* ((headers (get-authorization-access-jwk))
         (parameters `(("actor" . ,(get-did))
                       ;("limit" . 50)
                       ;("cursor" . "")
                       ))
         (request-spec (build-get-request endpoint headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


(defun get-timeline ()
  (let* ((headers (get-authorization-access-jwk))
         (parameters nil)
         (request-spec (build-get-request endpoint headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


(defun get-follows ()
  (let* ((headers (get-authorization-access-jwk))
         (parameters `(("actor" . ,(get-did))
                       ;("limit" . 50)
                       ;("cursor" . "")
                       ))
         (request-spec (build-get-request endpoint headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


(defun get-followers ()
  (let* ((headers (get-authorization-access-jwk))
         (parameters `(("actor" . ,(get-did))
                       ;("limit" . 50)
                       ;("cursor" . "")
                       ))
         (request-spec (build-get-request endpoint headers parameters))
         (response (execute-request request-spec))
         (processed-response (process-response response)))
    (format t "~A" processed-response)))


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
