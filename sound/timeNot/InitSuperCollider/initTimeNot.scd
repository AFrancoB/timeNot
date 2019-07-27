// Init for TimeNot
(
TimeNot.defaultServerConfig;
s.boot
);


s.waitForBoot({
	var path = thisProcess.nowExecutingPath.dirname;

	// OSC def that recieves msgs from the haskell core

	TimeNot.connect;


	(
		// allocates drumKit buffers and gives them a simple synthdef
		(path++"/drumKit.scd").load;
		// create synthesis definitions for oscilattor-based synthesis
		(path++"/oscillators.scd").load;
		// creates fx, at the moment the compander is necessary. Its in bus is 99 and its out is 0
		(path++"/fx_filters.scd").load;
	);

	Task({
		1.wait;
		~comp= Synth(\compander);
		"compander activated in bus: 99 out bus: 0".postln;
		"to change compander: ~comp.set(\out, 0, \in, 99, \gate, 1)".postln;
		0.5.wait;
		"Bienvenido a TimeNot".postln;"un parásito en el IDE de SuperCollider".postln
	}).play;

})

//Synth(\osciladores, [\freq, 72, \amp, 1, \sust, 1, \which, ~osc.("sin")]);
//Synth(\bd)
