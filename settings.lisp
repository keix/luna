(defvar version (string "{\"version\": \"v0.0.05\"}"))
(defvar error-message (string "{\"error\": \"example\"}"))

(defvar endpoints
  '((:create-session  . "https://bsky.social/xrpc/com.atproto.server.createSession")
    (:refresh-session . "https://bsky.social/xrpc/com.atproto.server.refreshSession")
    (:create-record   . "https://bsky.social/xrpc/com.atproto.repo.createRecord")
    (:get-profile     . "https://bsky.social/xrpc/app.bsky.actor.getProfile")
    (:get-timeline    . "https://bsky.social/xrpc/app.bsky.feed.getTimeline")
    (:get-actor-feeds . "https://bsky.social/xrpc/app.bsky.feed.getActorFeeds")
    (:get-follows     . "https://bsky.social/xrpc/app.bsky.graph.getFollows")
    (:get-followers   . "https://bsky.social/xrpc/app.bsky.graph.getFollowers")))

(defvar did-json (string "json/did.json"))
(defvar identifier-json (string "json/identifier.json"))
