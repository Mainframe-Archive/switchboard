---
layout: guide
---

## What is Switchboard?

Switchboard is a framework for processing email. It consists of a core that
maintains a connection to an IMAP server and routes event notifications to a
set of workers. The workers interact with the IMAP server through a simple API
exposed by the Switchboard core.

### Features

<ul class="bulletPoints1">
<li>
  Totally open source &mdash; developers can extend Switchboard or use
  the API to create workers specific to their product.
</li>
<li>
  Switchboard handles the boilerplate, leaving the logic to you
  &mdash; Switchboard frees you from the plumbing of server-side email
  monitoring and fetching, allowing you to focus on your products
  and services.
</li>
<li>
  You deploy it &mdash; by running persistent processes on your own server,
  Switchboard can help you avoid mobile OS backgrounding restrictions
  and heavy battery consumption, freeing up the client and leaving
  you in control.
</li>
</ul>

### Use Cases

You could use Switchboard to help you:

<ul class="bulletPoints1">
  <li>
    Send email triggered push notifications to a mobile device. Switchboard
    makes this easy, even across many email accounts.
    <a href="http://spatch.io">Spatch</a>'s need for this is why Switchboard exists.
  </li>

  <li>
    Store email image attachments to a Dropbox folder. Switchboard can
    upload attachments incoming emails to Dropbox.
  </li>

  <li>
    Unsubscribe from unwanted mailing lists. Given the code
    to select unwanted emails and follow their unsubscribe link,
    Switchboard can remove you from unwanted mailing lists as their
    emails arrive
  </li>
</ul>

Switchboard can do all these things at the same time across a set of
email accounts, with each use case implemented as a separate worker.

### First Steps

You are at the beginning of the Switchboard User Guide. This guide can
take you from installation, to setting up a Switchboard worker or
client.

To get Switchboard up and running, take a look at the
[install]({{site.baseurl}}/install) pages.

To see an example of how Switchboard can be used, see the
[Examples]({{site.baseurl}}/examples) pages with examples and
walkthroughs on:

<ul class="bulletPoints1">
  <li>Sending email push notifications</li>
  <li>Storing email image attachments to a Dropbox folder</li>
</ul>

To see documenation of the Switchboard application's modules and
functions, see the [API Docs]({{site.baseurl}}/doc).
