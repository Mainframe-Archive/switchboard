# Getting Started

- [Core Application](#markdown-header-switchboard-core)
- [Client Protocol](#markdown-header-switchboard-client-protocol)

## Switchboard Core

This section is for people who want to develop on the core Switchboard
application.

Note: through the rest of this section, `./switchboard` will refer
to the release's control script, located in `_rel/bin/switchboard`
if you're building from source'.

### Switchboard Erlang API

The quickest way to interact with Switchboard is through the application's
Erlang API. Assuming Switchboard is running, `./switchboard remote_console`
will attach a remote console to the running application. Here's
an example session:


    %% Subscribe to messages of new emails.
	true = switchboard:subscribe(new).

    %% List the active accounts. Assuming no one's been here first, there should be none.
    [] = switchboard:accounts().

	%% Add a new account. (XOAUTH2 is also supported)
	switchboard:add({ssl, <<"imap.gmail.com">>, 993},
	                {plain, <<"youremail@gmail.com">>, <<"yourpassword">>},
					[<<"INBOX">>]).

    %% List the now active account.
    [<<"youremail@gmail.com">>] = switchboard:accounts().

    %% Send yourself an email, wait a minute, then flush the message queue to see it.
	flush().

    %% Stop the account from being monitored.
	ok = switchboard:stop(<<"youremail@gmail.com">>).

    %% Unsubscribe from new messages.
	true = switchboard:unsubscribe(new).


See `src/switchboard.erl` for more detailed documentation of the
public interfaces.

### Finding and Building the Documentation

Documentation of the core modules is provided as in-source comments
using [edoc](http://www.erlang.org/doc/apps/edoc/chapter.html)
formatting. If you'd rather have the documentation as html, run `make
docs`, and then point your browser to `doc/index.html`.

The `switchboard` module provides the public Erlang interface for the
Switchboard application, and exposes the controls for adding and
removing accounts, and subscribing to event channels via
[`uwiger/gproc`](https://github.com/uwiger/gproc).

`imap` is a partial imap client. Though not yet mature,
it's written as a single `gen_server` so that it can
easily be dropped into other projects.

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

For example, `switchboard:test().` run at the Erlang console
will run all of the tests in the `switchboard` module.
`switchboard:test_all()` will run all tests listed in the function call --
hopefully all of the eunit tests in the application.

The `TEST` and `LIVE_TEST` provide boolean controls for knocking
out tests, keeping them out of compiled production code.

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

TL;DR: a client connects to the server over a websocket.  If you're
running Switchboard locally, the url defaults to
`ws://127.0.0.1:8080/clients`. Data is encoded in UTF8 JSON. Clients
issue a list of commands to the server in the form form
`[[${method <string>}, ${args <object>}, ${tag (optional)}], ...]`.
The server runs the commands in order, generating and returning a list
of one respsone per command. The responses take the same form as the
commands. The examples below should be a bit more clean than my made
up pseudocode.


### `connect`

A high level description is provided
[here](http://jmap.io/#transport-and-authentication), but
the details have been left up to the implementer. `connect`
accepts the `host` and `port` of the IMAP server, as well
as an `auth` object that is used to login after connecting
to the server. A `connected` response indicates that
the connection and login was successful.

Using plain auth:

    C: [["connect", {"host": "imap.google"
                     "port": 993,
					 "auth": {
					   "type": "plain",
					   "username": "username@gmail.com",
					   "password": "drowssap"}}]]
    S: [["connected", {}]]

Using XOAUTH2:

    C: [["connect", {"host": "imap.google"
                     "port": 993,
					 "auth": {
					   "type": "xoauth2",
					   "username": "dispatchonme@gmail.com",
					     "token": {
						   "type": "refresh"
					       "token": "1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc",
					       "url": "https://accounts.google.com/o/oauth2/token"}}}]]
    S: [["connected", {}]]

### `idle`

The `idle` command tells the server to create IMAP connections for the
set of mailboxe names listed. It is not a part of the jmap spec.
If the server failed to create any connections, it will include a
`failed` key in the response args with a list of failed mailbox names.

Each call of "idle" replaces the list of mailboxes being monitored.


    C: [["idle", {"list": ["INBOX", "NON-EXISTENT"]}]]
	S: [["idling", {"failed": ["NON-EXISTENT"]}]]

    # The server will send unsolicited responses
	# Warning: this message format is going to change to get more jmap-y
    S: [["newMessage", {"item": {
	                      "uid": 12,
	                      "envelope": {
						    "date": "Fri, 30 May 2014 15:24:03 -0700",
							"subject": "Ahoy!",
							"from": {"address": {"name": "Errol", "email": "errol@gmail.com"}},
						    "sender": {"address": {"name": "Errol", "email":"errol@gmail.com"}},
							"replyto":{"address":{"name":"Errol","email":"errol@gmail.com"}},
							"to":{"address":{"email":"mail.dispatch.test@gmail.com"}},
							"cc": [], "bcc": [], "inreplyto": [],
							"messageid":"<etPan.53890503.7e0c57b1.113@Thomass-MacBook-Pro.local>"},
							"flags": [],
							"internaldate": "30-May-2014 22:24:05 +0000",
							"rfc822size": 3885},
					      "account": "mail.dispatch.test@gmail.com",
						  "mailbox":"INBOX"}]]


### [`getMailboxes`](http://jmap.io/#getmailboxes)

Returns a list of all mailbox objects.

    C: [["getMailboxes", {}]]
    S: [["mailboxes", {"state": 79153751884907,
	                   "list": [{"name": "INBOX",
					            "id": "INBOX-1"}]}]]

    C: [["getMailboxes", {"state": 79153751884907}]]
    S: [["mailboxes", {"state": 79153751884907}]]

TODO - ACL Permissions

### [`getMessageList`](http://jmap.io/#getmessagelist)

    C: [["getMessageList", {mailboxId: "INBOX-1"}]]
	S: [["messageList", {messageIds: ["INBOX-1u1", "INBOX-1u2", "INBOX-1u3"]}]]

TODO: All of the options, searching.

### Example Client

An example client written in javascript is located in
[`client/switchboardclient.js`](../priv/static/js/switchboardclient.js). The
sourcefile contains comments mapping out common components of a
Switchboard client, and is a great reference for understanding how the
protocol works.

To try the client out, start the Switchboard application and point
your browser [here](http://127.0.0.1:8080/jsclient),
and it should display a page with some getting started commands. Open
your browser's javascript console and try them out.
