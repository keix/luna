## AT Protocol client for Bluesky.

This is AT Protocol client for Bluesky written in Common Lisp.

### Get started.

- This program depends on Quicklisp.
- This program has been tested with SBCL (v2.2.5)

```
 $ git clone git@github.com:keix/bluesky.git
```

### Usage.

First, define the "identifier" and "password" in identifier.json.

```
 $ vi json/identifier.json
```

- login

```
 $ sbcl --script atproto-client.lisp
```
