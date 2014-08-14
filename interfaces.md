---
layout: guide
---

## Switchboard Workers and Clients

Switchboard is intended to be run as a standalone server that connects
to external workers and clients. The worker and client interfaces are
bidirectional, allowing external processes to receive notifications of
new emails, as well as fetch more data about the email. Switchboard
handles concurrent requests to IMAP accounts.


### Workers v Clients

Switchboard workers and clients differ by their account access
permissions:

A *worker* can process emails across all email accounts Switchboard is
connected to. The worker can command Switchboard to connect to
multiple accounts. The connections are not terminated when the worker
dies, instead they must be terminated explicitly. Workers share most
commands with clients, with the caveat that the command arguments must
include an `account` key-value indicating what account the command
should be applied to.

A *client* can only connect to and interact with a single email
account via Switchboard. After executing a successful `connect`
command, the client enters a `connected` state during which it can't
execute execute another `connect` command. While in the `connected`
state, commands issued are applied to the account to which the client
connected. If the client holds the last reference to the account when
it dies, the account's IMAP connections are closed. By default, the
credentials are not saved (beware your logs).

Workers and clients can be mixed on a single Switchboard server,
but at the moment due to the IMAP connection sharing and watch
commands being in need of an upgrade, this isn't recommended.


### A Subset of JMAP

At the moment, the Switchboard client interface is a subset of
[JMAP](http://jmap.io), and the worker interface is a variation on
it. The documentation on this page is sufficient to use Switchboard,
and contains links to relevant portions of the JMAP spec. But, if you
are interested in learning more about JMAP, the first 8 sections of
the JMAP spec is very intersting, providing a rationale for using JMAP
over HTTP/websockets, and describing the structure of a JMAP exchange,
the data model, and data types.

A Switchboard worker or client connects to the server over a
websocket. If you're running Switchboard locally, the URL defaults to
`ws://192.168.50.2:8080/clients`. Data is encoded as UTF8 JSON. Clients
issue a list of commands to the server in the form
`[[${method <string>}, ${args <object>}, ${tag (optional)}], ...]`.
The server runs the commands in order, generating and returning a list
of one response per command. The responses take the same form as the
commands. The command descriptions and examples below should help
clarify.


### Worker Commands

Worker commands which are applied to an account, e.g. commands other
than `connect` and `watchAll`, require `account` to be specified in
the command's arguments.

#### connect

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
                   "username": "emma.nutt@gmail.com",
                   "password": "secret"}}]]
S: [["connected", {}]]
{% endhighlight %}

Using `XOAUTH2` auth (fake credentials):

{% highlight javascript %}
C: [["connect", {"host": "imap.gmail.com",
                 "port": 993,
                 "auth": {
                   "type": "xoauth2",
                   "username": "emma.nutt@gmail.com",
                     "token": {
                       "type": "refresh",
                       "token": "1/kif0yuTDHWu7UKtTCNtgDWTeoj_IYZM-SPmyxNiDCjc",
                       "url": "https://accounts.google.com/o/oauth2/token"}}}]]
S: [["connected", {}]]
{% endhighlight %}

#### watchAll

The `watchAll` command causes the server to send the worker notifications of
all new emails which arrive.

{% highlight javascript %}
C: [["watchAll", {}]]
S: [["watchingAll", {}]]

// The server will now send unsolicited responses as new messages arrive
S: [["newMessage", {"account": "emma.nutt@gmail.com",
                    "mailboxId": "INBOX!1",
                    "messageId": "INBOX!1?17"}]]
{% endhighlight %}

#### watchMailboxes

The `watchMailboxes` command tells the server to create IMAP
connections for the set of mailbox names listed. It is not a part of
the JMAP spec.  If the server failed to create any connections, it
will include a `failed` key in the response arguments with a list of
failed mailbox names.

Each call of "watchMailboxes" replaces the list of mailboxes being monitored.

{% highlight javascript %}
C: [["watchMailboxes", {"account": "emma.nutt@gmail.com",
                        "list": ["INBOX", "NON-EXISTENT"]}]]
S: [["watchingMailboxes", {"failed": ["NON-EXISTENT"]}]]

// The server will send unsolicited responses
S: [["newMessage", {"mailboxId": "INBOX!1", "messageId": "INBOX!1?17"}]]
{% endhighlight %}

#### [getMailboxes](http://jmap.io/#getmailboxes)

Returns a list of all mailbox objects.

{% highlight javascript %}
C: [["getMailboxes", {"account": "emma.nutt@gmail.com"}]]
S: [["mailboxes", {"state": 79153751884907,
                   "list": [{"name": "INBOX",
                             "id": "INBOX-1"}]}]]

C: [["getMailboxes", {"state": 79153751884907}]]
S: [["mailboxes", {"state": 79153751884907}]]
{% endhighlight %}

{::comment}
TODO - ACL Permissions
{:/comment}

#### [getMessageList](http://jmap.io/#getmessagelist)

{% highlight javascript %}
C: [["getMessageList", {"account": "emma.nutt@gmail.com",
                        "mailboxId": "INBOX!1"}]]
S: [["messageList", {"messageIds": ["INBOX!1?1", "INBOX!1?2", "INBOX!1?3"]}]]
{% endhighlight %}

{::comment}
TODO: All of the options, searching.
{:/comment}

#### [getMessages](http://jmap.io/#getmessages)

{% highlight javascript %}
C: [["getMessages", {"account": "emma.nutt@gmail.com",
                     "ids": ["INBOX!1?3", "INBOX!1?4"],
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


### Client Commands

Unlike worker commands, client commands do not require an `account`
argument. Instead, the client must issue a connect command for an
account, at which point all commands will be applied to that account.

#### connect

The client's connect command is identical to the Worker's connect
command. When the client issues a successful connect command, it will
not be able to connect to any more accounts, and it will only be able
to issue commands for that account.

#### watchMailboxes

This command is equivalent to the worker's `watchMailboxes`, and
tells Switchboard to provide notifications for the list of accounts.

{% highlight javascript %}
C: [["watchMailboxes", {"list": ["INBOX", "NON-EXISTENT"]}]]
S: [["watchingMailboxes", {"failed": ["NON-EXISTENT"]}]]

// The server will send unsolicited responses
S: [["newMessage", {"mailboxId": "INBOX!1", "messageId": "INBOX!1?17"}]]
{% endhighlight %}

#### [getMailboxes](http://jmap.io/#getmailboxes)

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


#### [getMessageList](http://jmap.io/#getmessagelist)

{% highlight javascript %}
C: [["getMessageList", {"mailboxId": "INBOX!1"}]]
S: [["messageList", {"messageIds": ["INBOX!1?1", "INBOX!1?2", "INBOX!1?3"]}]]
{% endhighlight %}

{::comment}
TODO: All of the options, searching.
{:/comment}


#### [getMessages](http://jmap.io/#getmessages)

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

Next up: [examples]({{site.baseurl}}/examples).
