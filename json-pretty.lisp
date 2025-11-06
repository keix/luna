; JSON Pretty Printer for Luna
; --------------------------------------------------------------------------------------

(defpackage :luna.json
  (:use :cl)
  (:export #:pretty-print-json #:format-profile #:format-timeline))

(in-package :luna.json)

(defun pretty-print-json (json-string &key (indent 0))
  "Pretty print JSON string with proper indentation"
  (let ((data (cl-json:decode-json-from-string json-string)))
    (format-json-object data indent)))

(defun format-json-object (obj &optional (indent 0))
  "Format a JSON object with indentation"
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (cond
      ((null obj) "null")
      ((listp obj)
       (if (and (consp (first obj)) (symbolp (car (first obj))))
           ; Association list (object)
           (format nil "{~%~{~A~^,~%~}~%~A}"
                   (mapcar (lambda (pair)
                             (format nil "~A  \"~A\": ~A"
                                     indent-str
                                     (string-downcase (symbol-name (car pair)))
                                     (format-json-object (cdr pair) (+ indent 2))))
                           obj)
                   indent-str)
           ; Regular list (array)
           (format nil "[~%~{~A~^,~%~}~%~A]"
                   (mapcar (lambda (item)
                             (format nil "~A  ~A"
                                     indent-str
                                     (format-json-object item (+ indent 2))))
                           obj)
                   indent-str)))
      ((stringp obj) (format nil "\"~A\"" obj))
      ((numberp obj) (format nil "~A" obj))
      ((eq obj t) "true")
      ((eq obj nil) "false")
      (t (format nil "\"~A\"" obj)))))

(defun format-profile (json-string)
  "Format profile data in a user-friendly way"
  (let ((data (cl-json:decode-json-from-string json-string)))
    (format nil "Profile Information:~%~
                 Handle: @~A~%~
                 Display Name: ~A~%~
                 Description: ~A~%~
                 Followers: ~A~%~
                 Following: ~A~%~
                 Posts: ~A~%~
                 Created: ~A~%"
            (cdr (assoc :handle data))
            (or (cdr (assoc :display-name data)) "")
            (or (cdr (assoc :description data)) "")
            (cdr (assoc :followers-count data))
            (cdr (assoc :follows-count data))
            (cdr (assoc :posts-count data))
            (cdr (assoc :created-at data)))))

(defun format-timeline (json-string)
  "Format timeline data in a user-friendly way"
  (let ((data (cl-json:decode-json-from-string json-string)))
    (let ((feed (cdr (assoc :feed data))))
      (format nil "Timeline (~A posts):~%~%~{~A~%~%~}"
              (length feed)
              (mapcar #'format-post feed)))))

(defun format-post (post-data)
  "Format a single post"
  (let* ((post (cdr (assoc :post post-data)))
         (author (cdr (assoc :author post)))
         (record (cdr (assoc :record post))))
    (format nil "@~A: ~A~%  (~A)"
            (cdr (assoc :handle author))
            (cdr (assoc :text record))
            (cdr (assoc :created-at record)))))