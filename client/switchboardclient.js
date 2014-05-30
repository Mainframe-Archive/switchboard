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
var URL = "wss://127.0.0.1:8080/clients";


/**
 * `SwitchboardClient` is an client to the Switchboard application.
 * Switchboard operates as a sort of [JMAP](http://jmap.io) to IMAP
 * proxy.
 *
 * Communication is done using JSON over WebSockets.
 */
function SwitchboardClient (url, connspec) {
    // Open a connection to the Switchboard server
    console.log("Working");
    var conn = new WebSocket(url);
    var tag = 0;
    var cmds = {};

    /**
     * Send a command to the server using the tagged form.
     *
     * Stores the command hashed by tag for response handling.
     */
    this.cmd = function(method, args, tagArg) {
	var tagArg = tagArg === undefined ? tag++ : tagArg;
	var cmd = [method, args, tag];
	cmds[tag] = cmd;
	tag++;
	console.log("Sending cmd: " + json.stringify(cmd));
	conn.send(json.stringify(cmd));
    }

    conn.onopen = function() {
	console.log("Connecting...");
	this.cmd("connect", connspec);
    }.bind(this);

    conn.onerror = function(error) {
	console.log(error);
    }

    conn.onmessage = function(e) {
	console.log("Received " + e.data);
    }
};
