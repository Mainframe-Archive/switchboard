# imapswitchboard

This application provides an API for interacting with IMAP
servers across multiple accounts.

For each account, one active IMAP connection is created,
plus one IMAP connection for each mailbox to be monitored.
This separation of responsibilites across IMAP connections
allows for vastly simpler management of the IMAP connections
at the cost of multiple IMAP connections. My experience has
been that it's worth it.

`imapswitchboard` uses a custom IMAP module, located in
`src/imap.erl`. Its type specs indicate what commands
it currently supports.

## Getting Started

`imapswitchboard` uses `extend/erlang.mk` -- just ran `make`
to build the application, and you can start an erlang shell
and load up the application with `make console`.

Currently there is no `relx.config` to build a release.

## Usage

TODO
