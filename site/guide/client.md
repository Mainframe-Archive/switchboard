---
layout: guide
---

## Switchboard Clients

Switchboard is intended to be run as a standalone server, and
manipulated via clients. This section describes the client interfaces,
and should be useful for either writing a client, or low-level
interactions with the Switchboard application.

{::comment}
TODO describe differences between client / worker
{:/comment}

### A Subset of JMAP

At the moment, the Switchboard client protocol is a subset of
[JMAP](http://jmap.io). The first 8 sections of the JMAP spec provide
a rationale for using JMAP over HTTP/websockets, and describes the
structure of a JMAP exchange, the data model, and data types.

TL;DR the 8 sections: a client connects to the server over a
websocket. If you're running Switchboard locally, the URL defaults to
`ws://127.0.0.1:8080/clients`. Data is encoded as UTF8 JSON. Clients
issue a list of commands to the server in the form
`[[${method <string>}, ${args <object>}, ${tag (optional)}], ...]`.
The server runs the commands in order, generating and returning a list
of one response per command. The responses take the same form as the
commands. The command descriptions and examples below should help
clarify.

### Example Client

An example client written in JavaScript is located in
[`client/switchboardclient.js`](../priv/static/js/switchboardclient.js). The
sourcefile contains comments for common components of a Switchboard
client, and is a useful reference for understanding how the protocol
works.

To try the client out, start the Switchboard application and point
your browser [here](http://127.0.0.1:8080/jsclient). Your browser
should render a page with some getting started commands. Go ahead and
open your browser's JavaScript console and try them out.

### Commands

#### `connect`

A high level description is provided
[here](http://jmap.io/#transport-and-authentication), but the details
have been left up to the implementer. `connect` accepts the `host` and
`port` of the IMAP server, as well as an `auth` object that is used to
login after connecting to the server. A `connected` response indicates
that the connection and login was successful.

Using `PLAIN` auth (fake credentials):

{% highlight javascript %}
C: [["connect", {"host": "imap.gmail.com",
                 "port": 993,
				 "auth": {
				   "type": "plain",
				   "username": "username@gmail.com",
				   "password": "drowssap"}}]]
S: [["connected", {}]]
{% endhighlight %}

Using `XOAUTH2` auth (fake credentials):

{% highlight javascript %}
C: [["connect", {"host": "imap.gmail.com",
                 "port": 993,
				 "auth": {
				   "type": "xoauth2",
				   "username": "username@gmail.com",
				     "token": {
					   "type": "refresh",
				       "token": "1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc",
				       "url": "https://accounts.google.com/o/oauth2/token"}}}]]
S: [["connected", {}]]
{% endhighlight %}

#### `watchMailboxes`

The `watchMailboxes` command tells the server to create IMAP connections for the
set of mailbox names listed. It is not a part of the JMAP spec.
If the server failed to create any connections, it will include a
`failed` key in the response arguments with a list of failed mailbox names.

Each call of "watchMailboxes" replaces the list of mailboxes being monitored.

{% highlight javascript %}
C: [["watchMailboxes", {"list": ["INBOX", "NON-EXISTENT"]}]]
S: [["watchingMailboxes", {"failed": ["NON-EXISTENT"]}]]

// The server will send unsolicited responses
S: [["newMessage", {"mailboxId": "INBOX!1", "messageId": "INBOX!1?17"}]]
{% endhighlight %}

#### [`getMailboxes`](http://jmap.io/#getmailboxes)

Returns a list of all mailbox objects.

{% highlight javascript %}
C: [["getMailboxes", {}]]
S: [["mailboxes", {"state": 79153751884907,
                   "list": [{"name": "INBOX",
                             "id": "INBOX-1"}]}]]

C: [["getMailboxes", {"state": 79153751884907}]]
S: [["mailboxes", {"state": 79153751884907}]]
{% endhighlight %}

{::comment}
TODO - ACL Permissions
{:/comment}


#### [`getMessageList`](http://jmap.io/#getmessagelist)


{% highlight javascript %}
C: [["getMessageList", {"mailboxId": "INBOX!1"}]]
S: [["messageList", {"messageIds": ["INBOX!1?1", "INBOX!1?2", "INBOX!1?3"]}]]
{% endhighlight %}

{::comment}
TODO: All of the options, searching.
{:/comment}


#### [`getMessages`](http://jmap.io/#getmessages)

{% highlight javascript %}
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
{% endhighlight %}

Next up: [examples]({{site.baseurl}}/guide/examples).
