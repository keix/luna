(defvar version (string "{\"version\": \"v0.0.05\"}"))
(defvar error-message (string "{\"error\": \"example\"}"))

(defvar service luna.config:*service-url*)

(defvar endpoint nil)
(defvar lexicon nil)
(defvar options nil)
(defvar content nil)
(defvar parameters nil)

(defvar endpoints
  '((:create-session  . "com.atproto.server.createSession")
    (:refresh-session . "com.atproto.server.refreshSession")
    (:create-record   . "com.atproto.repo.createRecord")
    (:get-profile     . "app.bsky.actor.getProfile")
    (:get-timeline    . "app.bsky.feed.getTimeline")
    (:get-actor-feeds . "app.bsky.feed.getActorFeeds")
    (:get-follows     . "app.bsky.graph.getFollows")
    (:get-followers   . "app.bsky.graph.getFollowers")))

(defvar did-json luna.config:*did-json-path*)
(defvar identifier-json luna.config:*identifier-json-path*)
