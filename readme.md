# Switchboard

This application provides an API for interacting with IMAP servers
across multiple accounts.

For each account, one active IMAP connection is created, plus one IMAP
connection for each mailbox to be monitored.  This separation of
responsibilites across IMAP connections allows for vastly simpler
management of the IMAP connections at the cost of multiple IMAP
connections. My experience has been that it's worth it.

`Switchboard`'s uses a custom IMAP module, located in
`src/imap.erl`. Its type specs indicate what commands it currently
supports.

## Supervision Structure

`switchboard_sup` is a `simple_one_for_one` supervisor that watches
`switchboard_account_sup` child supervisors. The
`switchboard_accounts` is the top-level supervisor for a single
account, watching `active_imap`, the active imap process,
`switchboard_idlers`, a supervisor watching the imap idling processes.

## Getting Started

`switchboard` uses `extend/erlang.mk` -- just ran `make` to build
the application, and you can start an erlang shell and load up the
application with `make console`.

Currently there is no `relx.config` to build a release.