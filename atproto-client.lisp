; https://www.docs.bsky.app/

(load "~/quicklisp/setup.lisp")
(load "settings.lisp")

(let
  ((*standard-output* (open "/dev/null" :direction :output :if-exists :append)))
  (ql:quickload 'drakma)
  (ql:quickload 'cl-json)
  (ql:quickload 'local-time))


; ------------------------------------------------------------------- ISO-8601 DateTime.
(defun get-local-time ()
  (string
    (local-time:format-timestring nil (local-time:now) :format local-time:+iso-8601-format+)))


; ------------------------------------------------------------------------ I/O funtions.
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


; ------------------------------------------------------------- Get the value from DID.
(defun get-access-jwk ()
  (if (probe-file "did.json")
    (cdr (assoc :ACCESS-JWT (loading-config-file (string "did.json")) :test #'equal))))


(defun get-refresh-jwk ()
  (if (probe-file "did.json")
    (cdr (assoc :REFRESH-JWT (loading-config-file (string "did.json")) :test #'equal))))


(defun get-did ()
  (if (probe-file "did.json")
    (cdr (assoc :DID (loading-config-file (string "did.json")) :test #'equal))))


; ------------------------------------------------------------------ Call API commands.
(defun create-session ()
  (write-file-from-string
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "create-session"))
           :method :post
           :content-type "application/json"
           :content (read-file-into-string (settings:get-json (string "identifier")))))
    (string "did.json")))


(defun refresh-session ()
  (write-file-from-string
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "refresh-session"))
           :method :post
           :accept "application/json"
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-refresh-jwk))))))
    (string "did.json")))


(defun get-profile ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "get-profile"))
           :method :get
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           :parameters `(("actor" . ,(get-did)))))))


(defun get-actor-feeds ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "get-actor-feeds"))
           :method :get
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun get-timeline ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "get-timeline"))
           :method :get
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           ;:parameters `(("algorithm" . "")
           ;              ("limit" . 100)
           ;              ("cursor" . "")))))
           ))))


(defun get-follows ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "get-follows"))
           :method :get
           :accept "application/json"
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun get-followers ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "get-followers"))
           :method :get
           :accept "application/json"
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           :parameters `(("actor" . ,(get-did))
                         ;("limit" . 50)
                         ;("cursor" . "")
                         )))))


(defun post ()
  (format t "~A"
    (map 'string #'code-char
         (drakma:http-request
           (settings:get-endpoint (string "create-record"))
           :method :post
           :accept "application/json"
           :content-type "application/json"
           :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (get-access-jwk))))
           :content (cl-json:encode-json-to-string
                      `(("repo" . ,(get-did))
                        ("collection" . "app.bsky.feed.post")
                        ;("rkey" . "")
                        ;("validate" . nil)
                        ;("swapCommit" . "")
                        ("record" . (("$type" . "app.bsky.feed.post")
                                     ("text"  . "Hello, World!")
                                     ("createdAt" . ,(get-local-time))))))))))


(let ((arg (second *posix-argv*)))
  (when arg
    (let ((command (intern (string-upcase arg))))
      (funcall command))))
