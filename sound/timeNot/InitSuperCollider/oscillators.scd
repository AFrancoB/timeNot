(
// synths with oscillators

//Synth(\osciladores, [\freq, 60, \amp, 1, \sust, 1, \which, 0]);
(
SynthDef(\osciladores, {|freq, amp, pan=0, sust, out=99, which=0|
	var sin, saw, sqr, tri, sig, env;

	sin= SinOsc.ar(freq.midicps);
	saw= Saw.ar(freq.midicps);
	sqr= Pulse.ar(freq.midicps);
	tri= LFTri.ar(freq.midicps);

	sig= Select.ar(which, [sin,saw,sqr,tri]);
	sig= sig*amp;
	sig= Pan2.ar(sig, pan);

	env= EnvGen.kr(Env.perc(0.001,sust),doneAction:2);

	Out.ar(out, sig*env)

}).add
);

// Synth(\bd)
(
~instrs = {|instrument|
	switch(instrument,
		"sin", {\osciladores},
		"saw", {\osciladores},
		"sqr", {\osciladores},
		"tri", {\osciladores},

		"bd",  {\bd},
		"oh",  {\oh},
		"oh2", {\oh2},
		"vibraslap", {\vibraslap},
		"maracas", {\maracas},
		"cabasa", {\cabasa},
		"whistle", {\whistle},
		"whistle2", {\whistle2},
		"lowbongo", {\lowbongo},
		"shortguiro", {\shortguiro},
		"hibongo", {\hibongo},
		"clp", {\clp},
		"csh", {\csh},
		"sd", {\sd},
		"hiconga", {\hiconga},
		"longguiro", {\longguiro},
		"clh", {\clh},
		"clh2", {\clh2},
		"tambourine", {\tambourine},
		"tambourine2", {\tambourine2},

				{nil})
}
);
(
~osc = {|oscillator|
	switch(oscillator,
		"sin", {0},
		"saw", {1},
		"sqr", {2},
		"tri", {3},
				{nil})

};
);
);
