# Getting Started

Getting started with...

- [the core application](#markdown-header-switchboard-core)
- [the client protocol](#markdown-header-switchboard-client-protocol)

## Switchboard Core

This section is for people who want to develop on the core Switchboard
application.

Switchboard is written in Erlang, and primarily uses Erlang tooling. This
will familiarize you with how to fit the tooling pieces together to get
a tight development loop.

Note: through the rest of this section, `./switchboard` will refer
to the release's control script, located in, `_rel/bin/switchboard`.

### Finding and Building the Documentation

Documentation of the various core modules is provided as comments
using [edoc](http://www.erlang.org/doc/apps/edoc/chapter.html). If
you'd rather have the documentation as html, run `make docs`, and then
point your browser to `doc/index.html`.

The `switchboard` provides the public Erlang interface for the
Switchboard application, and exposes the controls for adding and
removing accounts, and subscribing to event channels via
[`uwiger/gproc`](https://github.com/uwiger/gproc).

By default, documentation is not created for private functions.
Please open an issue if this causes any problems.

### Connecting a Shell

You can attach an Erlang shell to a running Switchboard application via

    ./switchboard/remote_console

From the erlang shell, try

    switchboard:accounts().  %% -> [Accounts]

Note: `switchboard:accounts/0` returns the list of accounts which
are actively being monitored. If no accounts have been added, this
is the empty list.

### Finding and Running Tests

At the moment, all tests are written in EUnit and must be called
from a running Switchboard application. Most tests are written in
a module separate from the code which they are testing. These
modules are kept in `src/test/eunit`.

For example, `switchboard_tests:test().` run at the Erlang console
will run all of the tests in the `switchboard_tests` module.

The `TEST` and `LIVE_TEST` provide boolean controls for knocking
out tests, keeping them out of compiled production code.

TODO mechanism for running all tests -- common_test?

Tests for a module are automatically run by `reloader.erl` when a file
is reloaded (credit for `reloader.erl` goes to `mochi/mochiweb`).

## Switchboard Client Protocol

This section is for people who want to write a Switchboard client, or
learn how Swithcboard communicates with clients.

TODO describe differences between client / worker

### A Subset of JMAP

At the moment, the Switchboard client protocol is a subset of
[jmap](http://jmap.io). The first 8 chapters of the jmap spec provides
a rationale for using JMAP over HTTP/websockets, the structure
of a JMAP exchange, the data model, and data types.

TL;DR: a client connects to the server over a websocket.  If
you're running Switchboard locally the url is
`ws://127.0.0.1:8000/clients`. Data is encoded in UTF8 JSON.  Clients
issue a list of commands to the server in the form form
`[[${method <string>}, ${args <object>}, ${tag (optional)}], ...]`.
The server runs the commands in order, generating and returning
a list of one respsone per command. The responses take the same
form as the commands. The examples below should make this a
bit more clear.


- Implemented
    - `getMailboxes`
	    - No permissions
- Added
    - `connect`

### `connect`

Using plain auth:

    C: ["connect", {"host": "imap.google"
                    "port": 993,
					"auth": {
					  "type": "plain",
					  "username": "username@gmail.com",
					  "password": "drowssap"}}]
    S: ["connected", {}]

Using XOAUTH2:

    C: ["connect", {"host": "imap.google"
                    "port": 993,
					"auth": {
					  "type": "xoauth2",
					  "username": "dispatchonme@gmail.com",
					    "token": {
						  "type": "refresh"
					      "token": "1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc",
					      "url": "https://accounts.google.com/o/oauth2/token"}}}]
    S: ["connected", {}]


### `getMailboxes`

    C: ["getMailboxes"]
    S: ["mailboxes", []]

### Example Client

An example client written in javascript is located in
[`client/switchboardclient.js`](../client/switchboardclient.js). The
sourcefile contains comments mapping out common components of a
Switchboard client, and is a great reference for understanding how the
protocol works.

To try the client out, first start the Switchboard application, then
run `make clientserve` -- that requires Python 2.7.x, and just serves
the `client` folder. Point your browser to
[http://127.0.0.1:8001/switchboardclient.html](http://127.0.0.1:8001/switchboardclient.html),
and it should display a page with some getting started commands. Open
your browser's javascript console and try them out.
