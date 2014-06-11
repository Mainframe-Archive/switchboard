---
layout: guide
---

## What is Switchboard?

Switchboard provides high-level tools for managing IMAP clients across
multiple email accounts and providers, and an API allowing workers to
process emails as they arrive. It handles the boilerplate of IMAP
connection management and new email monitoring so that you can focus
on innovating around email.

### Features

<ul class="bulletPoints1">
<li>
  Totally open source &mdash; developers can extend Switchboard or use
  the API to create workers specific to their product.
</li>
<li>
  Switchboard handles the boilerplate, leaving the logic to you
  &mdash; Swtichboard frees you from the plumbing of server-side email
  monitoring and fetching, allowing you to focus on your products
  and services.
</li>
<li>
  In the cloud &mdash; having processes running in the cloud,
  Switchboard can help you avoid mobile OS backgrounding restrictions
  and heavy battery consumption, freeing up local memory and leaving
  you in control of your service.
</li>
</ul>

### Use Cases

You could use Switchboard to help you:

<ul class="bulletPoints1">
  <li>
    Send email triggered push notifications to a mobile device. Switchboard
    makes this easy across many users. Actually,
    <a href="http://spatch.io">Spatch</a>'s need for this is why Switchboard exists.
  </li>

  <li>
    Store email image attachments to a Dropbox folder. Switchboard could
    monitor your or your users' email accounts to upload new email
    attachments right to their Dropbox.
  </li>

  <li>
    Unsubscribe from unwanted mailing lists. Say you code the logic
    to select unwanted emails and follow their subscribe link,
    Switchboard can run your script across all incoming emails for a
    set of accounts, clearing them up.
  </li>
</ul>

In fact, Switchboard could do all these things at once across a set
of email accounts, with each use case implemented as a separate worker.

### First Steps

You are at the beginning of the Switchboard User Guide. This guide can
take you from installation, to setting up a client or worker.

To get Switchboard up and running, take a look at the
[install]({{site.baseurl}}/guide/install) pages.

To see an example of how Switchboard can be used, see the
[Examples]({{site.baseurl}}/guide/examples) pages with examples and
walkthroughs on:

<ul class="bulletPoints1">
  <li>Sending email push notifications</li>
  <li>Storing email image attachments to a Dropbox folder</li>
</ul>

To see documenation of the Switchboard application's modules and
functions, see the [API Docs]({{site.baseurl}}/doc).
