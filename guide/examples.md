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

Switchboard allows developers to develop a wide variety of clients. The 
following sourcefile contains comments mapping out common components of a
Switchboard client, and is a great reference for understanding how the
protocol works.

To try the client out, start the Switchboard application and point
your browser at http://127.0.0.1:8080/jsclient - it should display 
a page with some getting started commands. Open your browser's 
javascript console and try them out.

{::comment}
### Push Notifications
{:/comment}

### Ruby Sample App Tutorial - Email Dropbox Uploader

### Email 

Follow this example tutorial to build your first `switchboard` client in Ruby. This simple example, to be run in the command line, syncs attachments from a user's emails to his or her Dropbox.

#### Setup

First off, make sure switchboard is running on your local server (see
[Switchboard Installation](/switchboard/guide/install/
"Installation") for details)

After the `switchboard` server is up, you're ready to start building
your Ruby client so you can communicate with it.

To get started, create the following `Gemfile`:

{% highlight ruby %}
source 'https://rubygems.org'
ruby '2.1.1' # we haven't tested other versions yet. Let us know if you have any issues

gem 'faye-websocket'
gem 'Dropbox-sdk'
{% endhighlight %}

`faye-websocket` is a pretty handy gem extracted from the
[faye project](https://github.com/faye/faye-websocket-ruby), which
provides classes for easily building WebSocket servers and clients in Ruby. 
Dropbox's '[ruby SDK](https://www.Dropbox.com/developers/core/start/ruby)' 
is well developed and documented, which will make it
simple to upload files to your Dropbox.


#### The initial client

The beginnings of the client come straight from the
[faye-websocket documentation](https://github.com/faye/faye-websocket-ruby#using-the-websocket-client),
it's pretty easy to set up:

`awesome-Dropbox-uploader.rb`

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


The `Faye::Websocket::Client` takes an url to listen to, so you can use
the default local `switchboard` url. The client responds to the usual
websocket methods, which currently output notifications/data to the
console upon being called. When the client receives a message from the server,
it'll parse it for later use.

If you execute `ruby your-file-name.rb` (we'll call it
awesome-Dropbox-uploader.rb for the rest of the tutorial) you'll see:

{% highlight ruby %}
[:open]
{% endhighlight %}

...and nothing else. This is because without adding an account to
`switchboard` it doesn't know what to send your way. So close your 
client for the moment (`Ctrl + c`, upon which you'll see a closing
message) and move on to the next section: adding an account.

#### Adding/listening to an account

The `switchboard` client protocol is currently a subset of
[JMAP](http://jmap.io/), with data encoded in `UTF8 JSON`. Clients can
issue commands in the form `[["methodName", {options}]]`, using the
`send` method. This example uses Switchboard's plain auth:

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

This time, if you run our client (`ruby awesome-Dropbox-uploader.rb`)
it'll automatically connect, and you should see the message

{% highlight ruby %}
[:message, [["connected", {}]]]
{% endhighlight %}

Although you're now connected there's one last thing you've got to do
before you'll start getting emails through. We need to tell
`switchboard` which mailbox to listen to. That's easily done with the
`watchMailboxes` command, which takes as argument a list of mailboxes.

{% highlight ruby %}
ws.send '[["watchMailboxes", {"list": ["INBOX"]}]]'
{% endhighlight %}

The server will respond with `[:message, [["watchedMailboxes", {}]]]`

If you restart your client now, when you receive an email Switchboard
will automatically send through the email's `messageId` and its `mailboxId`:

{% highlight ruby %}
["newMessage", {"mailboxId"=>"INBOX!671882951", "messageId"=>"INBOX!671882951?2743"}]
{% endhighlight %}

By now you probably have noticed that messages sent from the server
take the following form:

{% highlight ruby %}
[["messageName", {params}]]
{% endhighlight %}

You can use this to make what's output to your console when you receive
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


Hardcoding an email and password is pretty simplistic, so let's change that
to enable you to pass them in through the command line:


{% highlight ruby %}
puts "let's add an account to switchboard"
puts 'what is your email?'
email = gets.strip
puts 'what is your password?'
password = gets.strip
{% endhighlight %}


You'll need to interpolate the values into your command to the server (this means having to escape a lot of double quotation marks):


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


Now when you run your 'awesome-Dropbox-uploader' you're prompted to fill
in your Gmail username and password, then `Switchboard` automatically
connects and listens to your inbox for you. Pretty sweet!

So now your client's in pretty good shape. Apart from one pretty
crucial thing - you're not retrieving any attachments to
upload. Whoops.

In order to retrieve an attachment you need to use the `messageId` you
get upon receiving an email to make another call to Switchboard,
retrieving the specific email information you need. You can do this with
`getMessages`, which takes as arguments message ids and the email
properties you're looking for, from subject to textBody.

Retrieving the attachments directly is still a work in progress, but
we can get around that by retrieving the raw email data in order to
parse it in your client:

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


Now you have the raw email data. You can use the handy
[mail](https://github.com/mikel/mail) gem to parse it and retrieve
your attachments. After adding and bundling the gem, you're ready to
add a new case to the message method:


{% highlight ruby %}
ws.on :message do |event|
  m = JSON.parse(event.data).flatten!

  case m[0]
    .
    .
    .
    when "messages"
      # first the client needs to retrieve the raw data from the server response
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

Attachments are returned in an array. If an email has no attachments,
Switchboard will return an empty array. If you're interested, an
attachment looks something like this:

{% highlight ruby %}
[#<Mail::Part:2159230880, Multipart: false, Headers: 	<Content-Type: image/jpeg; name="swag.jpg">, 	<Content-Transfer-Encoding: base64>, <Content-	Disposition: attachment; filename="swag.jpg">, 	<X-Attachment-Id: f_hw297rj60>>]
{% endhighlight %}

#### Dropbox authentication

Now the client is retrieving your email attachments, the next step
is to set up the Dropbox uploader. First off is authentication.

Setting up `Dropbox OAuth` is incredibly easy, and well documented. It
is a little awkward to use though, due to it being split into two
parts (authorising the app and then pasting in a confirmation code to
finish the process).

Here's a quick implementation taken from the Dropbox docs (you'll need
to register your app on the app console [here](https://www.Dropbox.com/developers/apps) 
to get a key and secret first).



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


You can implement a simple Dropbox uploader using the access token stored when you authenticated with Dropbox, passing in the attachments from your emails:


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


Add the uploader to your `messages` method and you're good to go:


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
your email attachments with your Dropbox, and shows off the awesome
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
users' emails to his or her Dropbox.
