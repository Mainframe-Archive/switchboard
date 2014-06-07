---
layout: guide
---

## Getting Started

- [Core Application](#markdown-header-switchboard-core)
- [Client Protocol](#markdown-header-switchboard-client-protocol)

### Switchboard Core

This section is for people who want to develop on the core Switchboard
application.

Note: through the rest of this section, `./switchboard` will refer
to the release's control script, located in `_rel/bin/switchboard`
if you're building from source.

### Switchboard Client Protocol

This section is for people who want to write a Switchboard client, or
learn how Swithcboard communicates with clients.

TODO describe differences between client / worker

#### A Subset of JMAP

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


#### `connect`

A high level description is provided
[here](http://jmap.io/#transport-and-authentication), but
the details have been left up to the implementer. `connect`
accepts the `host` and `port` of the IMAP server, as well
as an `auth` object that is used to login after connecting
to the server. A `connected` response indicates that
the connection and login was successful.

Using plain auth:

    C: [["connect", {"host": "imap.gmail.com",
                     "port": 993,
					 "auth": {
					   "type": "plain",
					   "username": "username@gmail.com",
					   "password": "drowssap"}}]]
    S: [["connected", {}]]

Using XOAUTH2:

    C: [["connect", {"host": "imap.gmail.com",
                     "port": 993,
					 "auth": {
					   "type": "xoauth2",
					   "username": "dispatchonme@gmail.com",
					     "token": {
						   "type": "refresh",
					       "token": "1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc",
					       "url": "https://accounts.google.com/o/oauth2/token"}}}]]
    S: [["connected", {}]]

#### `watchMailboxes`

The `watchMailboxes` command tells the server to create IMAP connections for the
set of mailboxe names listed. It is not a part of the jmap spec.
If the server failed to create any connections, it will include a
`failed` key in the response args with a list of failed mailbox names.

Each call of "watchMailboxes" replaces the list of mailboxes being monitored.


    C: [["watchMailboxes", {"list": ["INBOX", "NON-EXISTENT"]}]]
    S: [["watchingMailboxes", {"failed": ["NON-EXISTENT"]}]]

    # The server will send unsolicited responses
    S: [["newMessage", {"mailboxId": "INBOX!1", "messageId": "INBOX!1?17"}]]

#### [`getMailboxes`](http://jmap.io/#getmailboxes)

Returns a list of all mailbox objects.

    C: [["getMailboxes", {}]]
    S: [["mailboxes", {"state": 79153751884907,
                       "list": [{"name": "INBOX",
                                 "id": "INBOX-1"}]}]]

    C: [["getMailboxes", {"state": 79153751884907}]]
    S: [["mailboxes", {"state": 79153751884907}]]

TODO - ACL Permissions

#### [`getMessageList`](http://jmap.io/#getmessagelist)

    C: [["getMessageList", {"mailboxId": "INBOX!1"}]]
    S: [["messageList", {"messageIds": ["INBOX!1?1", "INBOX!1?2", "INBOX!1?3"]}]]

TODO: All of the options, searching.


#### [`getMessages`](http://jmap.io/#getmessages)

    C: [["getMessages", {"ids": ["INBOX!1?3", "INBOX!1?4"],
	                     "properties": ["subject", "to", "from", "textBody"]}]]
    S: [["messages", {"state": "TODO",
	                  "list": [
					    {
						  "textBody": "--bcaec5171e1f3f872b04fa044bc7\r\nContent-Type: text/plain; [...]",
						  "from":[{"name": "YouTube", "email": "noreply@youtube.com"}]
						  "to": [{"name": "", "email": "mail.dispatch.test@gmail.com"}],
						  "subject": "Pokemon dubstep, Harry Potter in cyberpunk and more '90s remixes on YouTube"
						}, {
						  "textBody": "--047d7b86e65445cc6c04fa33c13a\r\nContent-Type: text/plain; [...]",
						  "from": [{"name": "Google+", "email": "noreply-68620fcd@plus.google.com"}],
						  "to": [{"name": "", "email": "mail.dispatch.test@gmail.com"}],
						  "subject": "Top suggested Google+ Pages for you"
						}]}]]


#### Example Client

An example client written in javascript is located in
[`client/switchboardclient.js`](../priv/static/js/switchboardclient.js). The
sourcefile contains comments mapping out common components of a
Switchboard client, and is a great reference for understanding how the
protocol works.

To try the client out, start the Switchboard application and point
your browser [here](http://127.0.0.1:8080/jsclient),
and it should display a page with some getting started commands. Open
your browser's javascript console and try them out.
