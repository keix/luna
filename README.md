## AT Protocol client for Bluesky

This is [AT Protocol](https://atproto.com/) client for [Bluesky](https://bsky.app/) written in Common Lisp and it returns JSON always.

## Get started

- This program depends on Quicklisp
- This program is tested with [SBCL](https://www.sbcl.org/) (v2.2.5) on macOS Version 12.2.1

```
 $ git clone git@github.com:keix/bluesky.git
```

## Usage

### Create active session

To get started, you need to define your "identifier" (such as handle or DID) and "password" within `identifier.json` located in `json` directory.
Open the `identifier.json` with your preferred text editor. The following example uses VIM.

```
 $ vi json/identifier.json
```

Once your `identifier.json` is properly set up, you can initiate a session by executing the following command.
This command will store the response into the `json/did.json` file.

```
 $ sbcl --script atproto-client.lisp create-session
```

For more information on AT Protocol's identity, visit: [AT Protocol's Identity Guide.](https://atproto.com/guides/identity)

### Commands

Below are the available commands if you have an active session. 

Please be aware that the current implementation of these commands is part of an ongoing development process. As such, the feature set is not yet complete and may undergo significant changes to enhance functionality and user experience.

| command | description |
|---|---|
| create-session | Start a new session with the server |
| refresh-session| Renew the current session to extend its validity |
| create-record | Submit a new record to the server |
| get-profile | Retrieve the profile information of the current user |
| get-timeline | Get a timeline of records from the user and followed actors |
| get-actor-feeds | Fetch the latest records published by a specific actor |
| get-follows | List the actors that the current user is following |
| get-followers | List the actors that are following the current user |

## License

Under the terms of the [MIT License](https://opensource.org/license/MIT/). Copyright (c) 2024 Kei Sawamura (a.k.a keix)
