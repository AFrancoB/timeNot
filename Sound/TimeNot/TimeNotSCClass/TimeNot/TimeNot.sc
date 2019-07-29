TimeNot {

	// will enable the interpreter to send the code as string to haskell if the key word is in the file
	*start { | key= "timeNot" |

		(
		thisProcess.interpreter.preProcessor = { |code|

			if(  Document.current.string.contains(key.asString), {

				NetAddr.new("127.0.0.1",57300).sendMsg("/evaluate",code).postln;

				code.postln;

				// test is true
				"//+//".postln;

			},{
				// test is false
				"-".postln;

				code;

			});

		};
		)

	}


	*defaultServerConfig {
		var server;
		Server.default.options.memSize= 512000*20;
        Server.default.options.maxNodes=128*1024;
		Server.default.options.numWireBufs= 512;
		server = Server.local;
		server.options.numBuffers = 1024 * 16;
		server.latency = 0.05;
		^server
	}

}







