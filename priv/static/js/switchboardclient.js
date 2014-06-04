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


/**
 * `SwitchboardClient` is an client to the Switchboard application.
 * Switchboard operates as a sort of [JMAP](http://jmap.io) to IMAP
 * proxy.
 *
 * Communication is done using JSON over WebSockets.
 */
function SwitchboardClient (url, connspec) {
    // Open a connection to the Switchboard server.
    var conn = new WebSocket(url);
    // Use incrementing tags to uniquely ID cmd/resp pairs.
    var tag = 0;
    // Open cmd's callbacks are inserted in cmds, keyed by the `tag`.
    var cmds = {};


    /******************
     * Internal Helpers
     */

    /**
     * `makeCmd` composes the call arguments into a list,
     * adding a tag if not specified.
     */
    function makeCmd (method, args, callback, tagArg) {
	var tagArg = tagArg === undefined ? tag++ : tagArg;
	if (callback) {
	    cmds[tagArg] = callback;
	}
	var cmd = [method, args, tagArg];
	return cmd;
    }


    /******************
     * Public Interface
     */

    /**
     * `sendCmds` prepares and sends off a list of commands.
     */
    this.sendCmds = function (cmds) {
	conn.send(JSON.stringify(cmds));
	return this;
    }.bind(this);


    this.idle = function (mailboxes, callback) {
	return this.sendCmds([makeCmd("idle", {list: mailboxes}, callback)]);
    }.bind(this);

    this.getMailboxes = function (callback) {
	return this.sendCmds([makeCmd("getMailboxes", [], callback)]);
    }.bind(this);

    this.getMessageList = function (mailboxId, callback) {
	var args = {mailboxId: mailboxId};
	return this.sendCmds([makeCmd("getMessageList", args, callback)]);
    }.bind(this);

    this.getMessages = function(ids, properties, callback) {
	var args = {ids: ids,
		    properties: properties || ["textBody"]};
	return this.sendCmds([makeCmd("getMessages", args, callback)]);
    }.bind(this);


    /*********************
     * Websocket Callbacks
     */

    /**
     * On the websocket's `onopen`, send a connect command with the connection
     * specification.
     */
    conn.onopen = function() {
	this.sendCmds([makeCmd("connect", connspec)])
    }.bind(this);

    conn.onerror = function(error) {
	console.log(error);
    }

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
	console.log(responses);
    }

    conn.onclose = function(e) {
	console.log("Socket closing");
    }
};
