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
    // Open a connection to the Switchboard server
    var conn = new WebSocket(url);
    var tag = 0;
    var cmds = {};

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


    /**
     * Public Interface
     */

    /**
     * `sendCmds` prepares and sends off a list of commands.
     */
    this.sendCmds = function (cmds) {
	return conn.send(JSON.stringify(cmds))
    };

    this.getMailboxes = function (callback) {
	return this.sendCmds([makeCmd("getMailboxes", [], callback)]);
    }.bind(this);

    conn.onopen = function() {
	console.log("Connecting...");
	this.sendCmds([makeCmd("connect", connspec)])
    }.bind(this);

    conn.onerror = function(error) {
	console.log(error);
    }

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
			cmds[tag](response);
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
};
