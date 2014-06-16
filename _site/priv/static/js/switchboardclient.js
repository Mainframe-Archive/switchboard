//! switchboardclient.js
//! version : 0.1.0
//! authors : Thomas Moulia <jtmoulia@gmail.com>
//! license : BSD 3-Clause
//! http://switchboard.spatch.co

/**
 * Client connections provide a connection specification to the
 * server when connecting. The server uses the `host` and `port`
 * to open an ssl connection to the IMAP server. The `auth`
 * contains authorization credentials which are used to
 * login to the IMAP server.
 *
 * XOAUTH2 is also supported for auth.
 *
 */
var ConnSpec = {host: "imap.gmail.com",
                port: 993,
                auth: {
                    type: "plain",
                    username: "mail.dispatch.test@gmail.com",
                    password: "i>V99JuMVEs;"}};

// The Switchboard application's default client endpoint.
var URL = "ws://127.0.0.1:8080/clients";


(function() {

    /**
     * The `Sender` handles wrapping and sending commands to
     * the Switchboard JMAP interface.
     */

    function Sender (conn) {
        // Use incrementing tags to uniquely ID cmd/resp pairs.
        var tag = 0;
        // Open cmd's callbacks are inserted in cmds, keyed by the `tag`.
        var cmds = {};

        /**
         * On the websocket's `onmessage`, parse the responses.
         *
         * If the response is tagged, check for a callback stored
         * in `cmds`. If present, call the callback with the response
         * as the only argument.
         */
        conn.onmessage = function(e) {
            var responses = JSON.parse(e.data)
            for (var i = 0; i < responses.length; i++) {
                switch (responses[i].length) {
                case 2:
                    break;
                case 3:
                    var tag = responses[i][2]
                    if (tag in cmds) {
                        try {
                            cmds[tag](responses[i]);
                        }
                        catch (e) {
                            console.log(e);
                        }
                        delete cmds[tag];
                    }
                    break;
                }
            }
        }

        conn.onerror = function(error) {
            console.log(error);
        }

        conn.onclose = function(e) {
            console.log("Socket closing");
        }

        /**
         * `makeCmd` composes the call arguments into a list,
         * adding a tag if not specified.
         */
        this.makeCmd = function (method, args, callback, tagArg) {
            var tagArg = tagArg === undefined ? tag++ : tagArg;
            if (callback) {
                cmds[tagArg] = callback;
            }
            return [method, args, tagArg];
        }.bind(this)

        /**
         * `sendCmds` prepares and sends off a list of commands,
         * caching the commands if they're tagged.
         */
        this.sendCmds = function (cmds) {
            conn.send(JSON.stringify(cmds));
            return this;
        }.bind(this)
    }


    /**
     * `SwitchboardClient` is an client to the Switchboard application.
     * Switchboard operates as a sort of [JMAP](http://jmap.io) to IMAP
     * proxy.
     *
     * Communication is JSON over WebSockets.
     */
    function Client (url, connspec) {
        // Open a connection to the Switchboard server.
        var conn = new WebSocket(url);
        var sender = new Sender (conn);

        /**
         * On the websocket's `onopen`, send a connect command with the connection
         * specification.
         */
        conn.onopen = function() {
            sender.sendCmds([sender.makeCmd("connect", connspec)]);
        }.bind(this);

        /******************
         * Public Interface
         */

        this.watchMailboxes = function (mailboxes, callback) {
            sender.sendCmds([
                sender.makeCmd("watchMailboxes", {list: mailboxes}, callback)]);
            return this;
        }.bind(this);

        this.getMailboxes = function (callback) {
            sender.sendCmds([sender.makeCmd("getMailboxes", [], callback)]);
            return this;
        }.bind(this);

        this.getMessageList = function (mailboxId, callback) {
            var args = {mailboxId: mailboxId};
            sender.sendCmds([sender.makeCmd("getMessageList", args, callback)]);
            return this;
        }.bind(this);

        this.getMessages = function(ids, properties, callback) {
            var args = {ids: ids,
                        properties: properties || ["textBody"]};
            sender.sendCmds([sender.makeCmd("getMessages", args, callback)]);
            return this;
        }.bind(this);
    };


    function Worker (url, connspec) {
        // Open a connection to the Switchboard server.
        var conn = new WebSocket(url);
        var sender = new Sender (conn);

        /**
         * On the websocket's `onopen`, send a connect command with the connection
         * specification.
         */
        conn.onopen = function() {
            console.log("Worker is connected...");
        }.bind(this);

        /******************
         * Public Interface
         */

        this.connect = function (connspec, callback) {
            sender.sendCmds([sender.makeCmd("connect", connspec, callback)]);
            return this;
        }.bind(this);

        this.watchAll = function (callback) {
            sender.sendCmds([sender.makeCmd("watchAll", {}, callback)]);
            return this;
        }.bind(this);

        this.watchMailboxes = function (account, mailboxes, callback) {
            var args = {account: account, list: mailboxes};
            sender.sendCmds([sender.makeCmd("watchMailboxes", args, callback)]);
            return this;
        }.bind(this);

        this.getMailboxes = function (account, callback) {
            var args = {account: account};
            sender.sendCmds([sender.makeCmd("getMailboxes", args, callback)]);
            return this;
        }.bind(this);

        this.getMessageList = function (account, mailboxId, callback) {
            var args = {
                mailboxId: mailboxId,
                account: account};
            sender.sendCmds([sender.makeCmd("getMessageList", args, callback)]);
            return this;
        }.bind(this);

        this.getMessages = function(account, ids, properties, callback) {
            var args = {
                account: account,
                ids: ids,
                properties: properties || ["textBody"]};
            sender.sendCmds([sender.makeCmd("getMessages", args, callback)]);
            return this;
        }.bind(this);

    };


    /**
     * Exposing Switchboard
     */

    this.switchboard = {
        Client: Client,
        Worker: Worker
    }

})();
