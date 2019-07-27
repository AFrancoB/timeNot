// execute here to compile every function and synthdef

//load drum sample catalogue;
var path = thisProcess.nowExecutingPath.dirname;
var drumKit = path++"/drumKits/IAMM_Kits";
var instruments;

~drumKit = SoundFile.collectIntoBuffers(drumKit++"/Kit_1/*",s);

(
//make durmkit as synthdefs
instruments = [
    [\oh, ~drumKit[0].bufnum],
    [\oh2, ~drumKit[1].bufnum],
    [\vibraslap, ~drumKit[2].bufnum],
    [\maracas, ~drumKit[3].bufnum],
    [\cabasa, ~drumKit[4].bufnum],
    [\whistle, ~drumKit[5].bufnum],
    [\whistle2, ~drumKit[6].bufnum],
    [\lowbongo, ~drumKit[7].bufnum],
    [\shortguiro, ~drumKit[8].bufnum],
    [\bd, ~drumKit[9].bufnum],
    [\hibongo, ~drumKit[10].bufnum],
    [\clp, ~drumKit[11].query.bufnum],
    [\csh, ~drumKit[12].bufnum],
    [\sd, ~drumKit[13].bufnum],
    [\hiconga, ~drumKit[14].bufnum],
    [\longguiro, ~drumKit[15].bufnum],
    [\clh, ~drumKit[16].bufnum],
    [\clh2, ~drumKit[17].bufnum],
    [\tambourine, ~drumKit[18].bufnum],
    [\tambourine2, ~drumKit[19].bufnum],
    [\tambourine3, ~drumKit[20].bufnum],
    [\woodblock, ~drumKit[21].bufnum],
    [\pedh, ~drumKit[22].bufnum],
    [\lowagogo, ~drumKit[23].bufnum],
    [\hiagogo, ~drumKit[24].bufnum],
    [\bd2, ~drumKit[25].bufnum]
];

instruments.do({|instrument|
    var buf = instrument[1];
    SynthDef(instrument[0],
        {| out = 99, freq = 1, amp = 0.5, pan = 0, pos = 0, rel=15 |
            var sig,env;
            sig = Mix.ar(PlayBuf.ar(2,buf,
                freq,1,
                BufDur.kr(buf)*pos*44100,        doneAction:2));
            env = EnvGen.ar(Env.linen(0.0,rel,0),doneAction:2);
            sig = sig * env;
            sig = sig * amp;
            Out.ar(out,Pan2.ar(sig,pan));
    }).add;
});
);




// (instrument: \clp).play


