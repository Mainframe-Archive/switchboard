---
layout: guide
---

## Installation

### Impatient? On OS X? Install from binary tarball.

To quickly get Switchboard up and running on Mac OS X, you can use a prebuilt
release. If you want to do a quick install (I recommend verifying the contents
of the URL):

{% highlight bash %}
curl -sL "https://github.com/thusfresh/switchboard/releases/download/0.1.1/switchboard.tar.gz" | tar -xz && ./switchboard/bin/switchboard console
{% endhighlight %}

That will download, unpack, and start the application,
dumping you at the Erlang console. `switchboard/bin/switchboard` is the
startup/control script. You can run it without arguments to see a list
of its commands.

Because you won't be building Switchboard forom source, the
[worker/client section]({{site.baseurl}}/interfaces) will
probably be most interesting to you next. Check out the
[Switchboard Erlang API]({{site.baseurl}}/doc) or
[example]({{site.baseurl}}/buide/example) section for directions on
how to start fetching emails.


### Install from Source

Switchboard uses
[extend/erlang.mk](https://github.com/extend/erlang.mk) and
[erlware/relx](https://github.com/erlware/relx) to build and
package the Switchboard application.

These instructions assume a UNIX-like system with `make` and `git` on
your path, and an installation of Erlang/OTP R17. To install Erlang
you can use Homebrew if you're on Mac OS X, else check
[here](https://www.erlang-solutions.com/downloads/download-erlang-otp)
for the appropriate package/architecture and installation
instructions.

Windows support is coming in the form of
[vagrant](https://github.com/thusfresh/switchboard/issues/20).

This snippet will clone the project, build Switchboard, package it
into a release, and start it:

{% highlight bash %}
git clone https://github.com/thusfresh/switchboard switchboard
cd switchboard
# Make the release
make
# Start the application
./_rel/bin/switchboard start
# Check whether the application is running
./_rel/bin/switchboard ping  # -> ping

# Print more options, like stopping Switchboard
./_rel/bin/switchboard
{% endhighlight %}

Please create an issue if there were any problems.

Next up:
[worker and client interfaces]({{site.baseurl}}/interfaces).
