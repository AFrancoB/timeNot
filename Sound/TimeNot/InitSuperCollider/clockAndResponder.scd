// to get the NDT of each event

~fromIntsToTime= { | secs, mSecs |
	var m, ts, clockDifs, now, time;
	m= mSecs * 0.000001;
	ts= secs + m;
	now = Date.getDate.rawSeconds;
//	clockDifs= (Date.getDate.rawSeconds - SystemClock.seconds); // diff between posix and SysClo
	time= ts - now;
	time= time + 0.2;
	time
};

~fromIntsToTime.(1564082818, 123456);

// sc responder for messages with scheduler
// goes to the connect method of the TimeNot class
(
OSCdef(\timeNot, {|msg, time, addr, recvPort|

	msg[1].postln;
	~time= ~fromIntsToTime.(msg[1],msg[2]);
	~time.postln;

	(instrument: ~instrs.(msg[4].asString), timingOffset: ~time, sust: msg[3], freq: msg[4], amp: msg[6], which: ~osc.(msg[5].asString.postln) ).play

}, "/canon");


)



