# Luna - Bluesky client written in Common Lisp

Luna is a [Bluesky](https://bsky.app/) client written in Common Lisp, offering both interactive REPL and command-line interfaces.

## Get started

- This program depends on Quicklisp
- This program is tested with [SBCL](https://www.sbcl.org/) (v2.5.4) on Gentoo Linux 6.12.21

```
git clone git@github.com:keix/luna.git
```

## Usage

### Interactive Mode

Experience Luna through its conversational interface:

```
./luna.lisp
```

Commands flow naturally: `profile`, `timeline`, `post`, `follows`, `followers`, `login`, `help`, `exit`.
The REPL welcomes you with formatted, human-readable responses instead of raw JSON.

### Command-line Mode

For those who prefer directness:

```
./luna.lisp get-profile
./luna.lisp create-record "Your thoughts here"
```

### Create active session

To get started, you need to define your "identifier" (such as handle or DID) and "password" within `identifier.json` located in `atproto/client` directory.
Open the `identifier.json` with your preferred text editor. The following example uses Vim.

```
vi atproto/client/identifier.json
```

Once your `identifier.json` is properly set up, you can initiate a session by executing the following command.
This command will store the response into the `atproto/client/did.json` file.

```
./luna.lisp create-session
```

For more information on AT Protocol's identity, visit: [AT Protocol's Identity Guide.](https://atproto.com/guides/identity)

### Commands

Below are the available commands if you have an active session. 

Luna speaks both interactively and directly, adapting its voice to your preferred mode of conversation.

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
