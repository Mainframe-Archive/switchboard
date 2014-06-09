---
layout: guide
---

## FAQ

### Why is Switchboard unable to perform a PLAIN LOGIN for my Gmail account?

If you are unable to loin to a Gmail account using `PLAIN`
authorization it could be because of Google's fraud
detection. Usually, the flagged login must be from a different
geographic location, a remote server, or a proxy. The error should
provide a URL where you can confirm that you initiated the login
attempt.

{::comment}
TODO - include shell
{:/comment}

You also won't be able to login using PLAIN authorization if you are
using Google's [2-step
verification](http://www.google.com/landing/2step/) without an
[application-specific password](https://support.google.com/accounts/answer/185833?hl=en).

### How many IMAP connections does Switchboard use?

For each account, one IMAP connection is created for sending commands,
plus one IMAP connection for each mailbox to be monitored. This
separation of responsibilites across IMAP connections allows for
simpler management of the IMAP connections, though at the cost of
[using multiple IMAP connections](https://support.google.com/mail/answer/97150?hl=en).

### Why not just IMAP+IDLE?

While the Switchboard API provides some IMAP+IDLE like functionality,
it is specifically intended to take the pain out of real time email
processing across many users. IMAP is a sharp, if sometimes low-level
and difficult, tool and will remain preferable for general purpose
email tasks.

### What's a simple example of Switchboard being used?

Switchboard's first role was sending new email push notifications to
the [Spatch iOS app](http://spatch.co). Due to mobile process
backgrounding restrictions and battery abuse, it's difficult to
monitor for new emails client-side. Instead, the Spatch app securely
posts users' OAuth tokens to a server running Switchboard, which
begins to monitor the accounts for incoming emails. When a new email
arrives, Switchboard notifies a subscribed worker, which then sends
down a push notification to the app.

In this case, Switchboard handled creating the IMAP connections,
keeping tabs on new emails coming down, and giving the worker
the data that it needed to send down a nicely formatted push
notification.
