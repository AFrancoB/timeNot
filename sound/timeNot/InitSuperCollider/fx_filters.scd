// effectos filtros y demás cosas

(
SynthDef(\compander, {|in=99, out=0, gate=1, pan= 0|
	var sig, env;
	sig= Pan2.ar(Compander.ar(In.ar(in,2), WhiteNoise.ar(0.99), 0.75, mul:0.9), pan);
	env= EnvGen.kr(Env.adsr(0.0001,1,0.1),gate, doneAction:0);
	Out.ar(out, sig*env)
}).add;
);
