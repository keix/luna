(defpackage :settings (:use :cl))
(in-package :settings)

(defvar endpoints
  '((:create-session  . "https://bsky.social/xrpc/com.atproto.server.createSession")
    (:refresh-session . "https://bsky.social/xrpc/com.atproto.server.refreshSession")
    (:get-profile     . "https://bsky.social/xrpc/app.bsky.actor.getProfile")
    (:create-record   . "https://bsky.social/xrpc/com.atproto.repo.createRecord")
    (:get-timeline    . "https://bsky.social/xrpc/app.bsky.feed.getTimeline")
    (:get-actor-feeds . "https://bsky.social/xrpc/app.bsky.feed.getActorFeeds")
    (:get-follows     . "https://bsky.social/xrpc/app.bsky.graph.getFollows")
    (:get-followers   . "https://bsky.social/xrpc/app.bsky.graph.getFollowers")))

; --------------------------------------------------------------------------

(defun get-endpoint (command)
  (cdr (assoc (intern (string-upcase command) :keyword) endpoints)))

(defun get-json (file-name) 
  (format nil "~A.json" file-name))

(export 'get-endpoint :settings)
(export 'get-json :settings)
