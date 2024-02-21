(defvar version (string "{\"version\": \"v0.0.05\"}"))
(defvar error-message (string "{\"error\": \"example\"}"))

(defvar service "https://bsky.social")

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

(defvar did-json (string "atproto/client/did.json"))
(defvar identifier-json (string "atproto/client/identifier.json"))
