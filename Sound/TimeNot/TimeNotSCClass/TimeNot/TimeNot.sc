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

	// creates the OSCDef to produce sound events taking into consideration
	*connect {
		(
			OSCdef(\timeNot, {|msg, time, addr, recvPort|

				(instrument: ~instrs.(msg[4].asString), timingOffset: msg[1], sust: msg[2], freq: msg[3], amp: msg[5], which: ~osc.(msg[4].asString.postln) ).play

			}, "/canon");
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







