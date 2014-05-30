# Getting Started

Getting started with...

- [the core application](#markdown-header-switchboard-core)
- [the client protocol](#markdown-header-switchboard-client-protocol)

## Switchboard Core<a name="core"></a>

This section is for people who want to develop on the core Switchboard
application.

Switchboard is written in Erlang, and primarily uses Erlang tooling. This
will familiarize you with how to fit the tooling pieces together to get
a tight development loop.

Note: through the rest of this section, `./switchboard` will refer
to the release's control script, located in, `_rel/bin/switchboard`.

### Finding and Building the Documentation

Documentation of the various core modules is provided as comments
using [edoc](http://www.erlang.org/doc/apps/edoc/chapter.html). If
you'd rather have the documentation as html, run `make docs`, and then
point your browser to `doc/index.html`.

The `switchboard` provides the public Erlang interface for the Switchboard
application. Its documentation shows the current controls for Switchboard.

By default, documentation is not created for private functions.

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

For example, `switchboard_tests:test().` run at the Erlang console
will run all of the tests in the `switchboard_tests` module.

The `TEST` and `LIVE_TEST` provide boolean controls for knocking
out tests, keeping them out of compiled production code.

TODO make test location consistent
TODO mechanism for running all tests -- common_test?

Tests for a module are automatically run by `reloader.erl` when a file
is reloaded (credit for `reloader.erl` goes to `mochi/mochiweb`).

## Switchboard Client Protocol

This section is for people who want to write a Switchboard client, or
just understand more about how the Swithcboard Protocol works.


### A Subset of JMAP

At the moment, the Switchboard client protocol is a subset of
[jmap](http://jmap.io). Reading through some of the spec is a
great way to orient yourself.

- Implemented
    - `getMailboxes`
	    - No permissions
- Added
    - `connect`

### Example Session

### Example Client Implementation

An example client written in Python can be seen here
[https://bitbucket.org/thusfresh/spatch-python]. `spatch/maildispatch`
is the actual client, while `examples/looplistener.py` gives an
example of the client's usage.

TODO -- include better documentation around this
