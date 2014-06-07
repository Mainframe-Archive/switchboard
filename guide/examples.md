---
layout: guide
---

## Examples

### Overview

Switchboard [and JMAP] are young, so there are only two client
implementations:

{::comment}
TODO - add python client
{:/comment}

- A javascript client located inkkkkk
- A ruby client

Switchboard allows to develop a wide variety of clients. The following
sourcefile contains comments mapping out common components of a
Switchboard client, and is a great reference for understanding how the
protocol works.

To try the client out, start the Switchboard application and point
your browser, and it should display a page with some getting started
commands. Open your browser's javascript console and try them out.

{::comment}
### Push Notifications
{:/comment}

### Ruby Sample App Tutorial - Email Dropbox Uploader

### Email 

Follow this example tutorial to build your first `switchboard` client in Ruby. This simple example, to be run in the command line, syncs attachments from a users' emails to his or her dropbox.

#### Setup

First off, make sure switchboard is running on your local server (see
'[Switchboard Installation](https://bitbucket.org/thusfresh/imapswitchboard/src/master/guide/installation.md
"Installation")' for details)

After your `switchboard` server is up, we are ready to start building
our Ruby client so we can communicate with our `switchboard` server.

To get started, I created the following `Gemfile`:

{% highlight ruby %}
source 'https://rubygems.org'
ruby '2.1.1' # we haven't tested other versions yet. Let us know if you have any issues

gem 'faye-websocket'
gem 'dropbox-sdk'
{% endhighlight %}

`faye-websocket` is a pretty handy gem extracted from the
'[faye project](https://github.com/faye/faye-websocket-ruby)' , which
we'll use to create our websocket client. To upload files to Dropbox
we'll use Dropbox's very own
'[ruby SDK](https://www.dropbox.com/developers/core/start/ruby)'


#### The initial client

The beginnings of our client come straight from the
'[faye-websocket documentation](https://github.com/faye/faye-websocket-ruby#using-the-websocket-client)',
it's pretty easy to set up:

`awesome-dropbox-uploader.rb`

{% highlight ruby %}
require 'faye/websocket'
require 'eventmachine'
require 'json'

EM.run {
  ws = Faye::WebSocket::Client.new('ws://127.0.0.1:8080/clients') # we will use the default switchboard url -> read below for explanation.

  ws.on :open do |event|
    p [:open]
  end

  ws.on :message do |event|
    p [:message, JSON.parse(event.data)]
  end

  ws.on :close do |event|
    p [:close, event.code, event.reason]
    ws = nil
  end
}
{% endhighlight %}


The `Faye::Websocket::Client` takes an url to listen to, so we'll use
the default local `switchboard` url. The client responds to the usual
websocket methods, which currently output notifications/data to the
console upon being called. When we receive a message from the server,
the client will parse it so we can do some funky stuff with it later.

If we execute this file `ruby your-file-name.rb`(we will call it
awesome-dropbox-uploader.rb for the rest of the tutorial) we'll see:

`[:open]`

...and nothing else. This is because without adding an account to
`switchboard`, `switchboard` doesn't know what to send our way. So
let's close our client (`Ctrl + c`, upon which you'll see a closing
message) and add an account.

#### Adding/listening to an account

The `switchboard` client protocol is currently a subset of
[JMAP](http://jmap.io/), with data encoded in `UTF8 JSON`. Clients can
issue commands in the form `[["methodName", {options}]]`, using the
`send` method. In this example we're using Switchboard's Gmail oauth
to keep things simple:

{% highlight ruby %}
ws.on :open do |event|
  p[:open]

  ws.send '[
              ["connect",
                {"host": "imap.gmail.com",
                  "port": 993,
                  "auth": {"type": "plain",
                            "username": "YOUR EMAIL",
                            "password": "YOUR PASSWORD"
                          }
                }
              ]
            ]'
end
{% endhighlight %}

This time, if we run our client (`ruby awesome-dropbox-uploader.rb`)
it'll automatically connect, and we should see the message
[:message, [["connected", {}]]].

Although we're now connected there's one last thing we've got to do
before we'll start getting emails through. We need to tell
`switchboard` which mailbox to listen to. That's easily done with the
`idle` command, which takes as argument a list of mailboxes.

{% highlight ruby %}
ws.send '[["idle", {"list": ["INBOX"]}]]'
{% endhighlight %}

The server will respond with `[:message, [["idling", {}]]]`

If we restart our client now, when we receive an email Switchboard
will automatically send through the email's message ID and its mailbox
ID:

{% highlight ruby %}
["newMessage", {"mailboxId"=>"INBOX!661382159", "messageId"=>"INBOX!661382159?9631"}]
{% endhighlight %}

By now you probably have noticed that messages sent from the server
take the following form:

{% highlight ruby %}
[["messageName", {params}]]
{% endhighlight %}

We can use this to make what's output to our console when we receive
messages a little more specific:


{% highlight ruby %}
ws.on :message do |event|
  m = JSON.parse(event.data).flatten!

  case m[0] #using the method name to decide what to do
    when "connected"
      puts 'connection made'
    when "idling"
      puts 'idling away'
    when "newMessage"
      puts 'new message'
      puts "#{m[1]}" #the message content
    else
      puts "#{m}"
  end
end
{% endhighlight %}


Hardcoding our email and password is pretty simplistic, so let's pass
them in through the command line:


{% highlight ruby %}
puts "let's add an account to switchboard"
puts 'what is your email?'
email = gets.strip
puts 'what is your password?'
password = gets.strip
{% endhighlight %}


We'll need to interpolate the values into our command to the server (this means having to escape a lot of double quotation marks):


{% highlight ruby %}
"[[\"connect\", {\"host\": \"imap.gmail.com\",
                \"port\": 993,
                \"auth\": {\"type\": \"plain\",
                            \"username\": \"#{email}\",
                            \"password\": \"#{password}\"
                          }
              }
]]"
{% endhighlight %}


Now when we run our 'awesome-dropbox-uploader' we're prompted to fill
in our Gmail username and password, then `Switchboard` automatically
connects and listens to our inbox for us. Pretty sweet!

So now our client's in pretty good shape. Apart from one pretty
crucial thing - we're not retrieving any attachments to
upload. Whoops.

In order to retrieve an attachment we need to use the `message ID` we
get upon receiving an email to make another call to Switchboard,
retrieving the specific email information we need. We can do this with
`getMessages`, which takes as arguments message ids and the email
properties we're looking for, from subject to textBody.

Retrieving the attachments directly is still a work in progress, but
we can get around that by retrieving the raw email data in order to
parse it in our client:

{% highlight ruby %}
ws.on :message do |event|
  m = JSON.parse(event.data).flatten!

  case m[0]
    .
    .
    .
    when "newMessage"
      puts 'new message'
      message_id = m[1]["messageId"]
      ws.send "[
                 [\"getMessages\", 
                  {
                    \"ids\": [\"#{message_id}\"], 
                    \"properties\": [\"raw\"]
                  }
                ]
              ]"
    else
    .
    .
    .
  end
end
{% endhighlight %}


Now we have the raw email data. We can use the handy
[mail](https://github.com/mikel/mail) `gem` to parse it and retrieve
our attachments. After adding and bundling the `gem`, we are ready to
add a new case to our message method:


{% highlight ruby %}
ws.on :message do |event|
  m = JSON.parse(event.data).flatten!

  case m[0]
    .
    .
    .
    when "messages"
      # first we need to retrieve the raw data from the server response
      raw_msg = m[1]["list"][0]["raw"]
      # then parse it with the mail gem and retrieve the attachments
      msg = Mail.new(raw_msg)
      attachments = msg.attachments
      puts "#{attachments}"
    else
    .
    .
    .
  end
end
{% endhighlight %}

Attachments are returned in an array. If an e-mail has no attachments,
Switchboard will return an empty array. If you're interested, an
attachment looks something like this:

{% highlight ruby %}
[#<Mail::Part:2159230880, Multipart: false, Headers: 	<Content-Type: image/jpeg; name="swag.jpg">, 	<Content-Transfer-Encoding: base64>, <Content-	Disposition: attachment; filename="swag.jpg">, 	<X-Attachment-Id: f_hw297rj60>>]
{% endhighlight %}

#### Dropbox authentication

Now we've got our attachments, we need to set up our dropbox
uploader. First off is authentication.

Setting up `dropbox oauth` is incredibly easy, and well documented. It
is a little awkward to use though, due to it being split into two
parts (authorising the app and then pasting in a confirmation code to
finish the process).

Here's a quick implementation taken from the Dropbox docs (you'll need
to register your app on the app console to get a key and secret first:
https://www.dropbox.com/developers/apps)



{% highlight ruby %}
APP_KEY = 'SOME KEY'
APP_SECRET = 'SOME SECRET'

flow = DropboxOAuth2FlowNoRedirect.new(APP_KEY, 	APP_SECRET)

authorize_url = flow.start()

puts '1. Go to: ' + authorize_url
puts '2. Click "Allow" (you might have to log in 	first)'
puts '3. Copy the authorization code'
print 'Enter the authorization code here: '
code = gets.strip

access_token, user_id = flow.finish(code)
{% endhighlight %}


We can implement a simple dropbox uploader using the access token we stored when we authenticated with dropbox, passing in the attachments from our emails:


{% highlight ruby %}
class DropboxUploader
  class << self
    def upload_file(attachments, token)
      attachments.each do | attachment |
        filename = attachment.filename
        begin
          file = attachment.body.decoded
          client = DropboxClient.new(token)
          client.put_file("/#{filename}", file)
        rescue => e
          puts "Unable to save data for #{filename} because #{e.message}"
        end
      end
    end
  end
end
{% endhighlight %}


Add the uploader to our `messages` method and we're good to go:


{% highlight ruby %}
ws.on :message do |event|
  m = JSON.parse(event.data).flatten!

  case m[0]
  .
  .
  .
    when "messages"
      .
      .
      .

      DropboxUploader::upload_file(attachments, access_token)
  .
  .
  .
  end
end
{% endhighlight %}


And there you have it. A quick command line application that syncs
your email attachments with your dropbox, and shows off the awesome
power of Switchboard.


#### Tidying up

You can start extracting out the code into classes to neaten things up
a bit, for example:


{% highlight ruby %}
class SwitchboardClient
  class << self
    def open(ws, email, password)
      ws.send authorisation(email, password)

      ws.send '[["idle", {"list": ["INBOX"]}]]'
    end

    def authorisation(email, password)
      "[[\"connect\", {\"host\": \"imap.gmail.com\",
                 \"port\": 993,
                 \"auth\": {\"type\": \"plain\",
                         \"username\": \"#{email}\",
                         \"password\": \"#{password}\"
                                }
                    }
      ]]"
    end

    .
    .
    .

  end
end
{% endhighlight %}


And then you can call:

{% highlight ruby %}
EM.run do
  ws = Faye::WebSocket::Client.new('ws://127.0.0.1:8080/clients')

  ws.on :open do |event|
    SwitchboardClient::open(ws, email, password)
  end

  ws.on :message do |event|
    SwitchboardClient::message(ws, event)
  end

  ws.on :close do |event|
    SwitchboardClient::close(event)
  end
end
{% endhighlight %}


Follow this example tutorial to build your first switchboard client in Ruby.
This simple example, to be run in the command line, syncs attachments from a
users' emails to his or her dropbox.
