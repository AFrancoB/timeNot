# Time Not <!-- omit in toc -->
A computational notation for time-oriented live coding, inspired and developed within the project: `Nanc-in-a-Can/canon-generator`

- [Objectives](#Objectives)
- [Installation](#Installation-as-a-SuperCollider-Extension-and-parasite)
- [Choose an Environment for Time Not](#Choose-an-Environment-for-Time-Not)
- [Usage](#Usage)
  - [Duration and Sequence](#Duration-and-Sequence)
  - [As a rhythm generator](#As-a-rhythm-generator)
    - [Onset Patterns](#Onset-Patterns)
    - [Repeat Patterns](#Repeat-Patterns)
    - [Euclidean Patterns](#Euclidean-Patterns)
  - [As a canon generator](#As-a-canon-generator)
    - [Ratio and Transposition](#Ratio-and-Transposition)
    - [Convergence Point](#Convergence-Point)
    - [Canonic examples](#Canonic-examples)
  - [Instruments](#Instruments)
  - [Other sound parameters](#Other-sound-parameters)
    - [Pitch](#Pitch)
    - [Amp](#Amp)
- [Further Development](#Further-Development)
- [Time](#Time)


## Objectives
`Time Not` is a computational notation capable of expressing complex rhythmic ideas with a particular emphasis in poly-temporality as developed by the socialist Mexican artist Conlon Nancarrow. The main idea approached in this notation is the [tempo canon](https://github.com/nanc-in-a-can/canon-generator#temporal-canons). However, many rhythmic strategies, techniques and ideas can be notated easily including euclidean rhythms and onset patterns. This notation was developed as a way to expand the conceptual mechanisms of the [Nanc-in-a-Can/canon-generator](https://github.com/nanc-in-a-can/canon-generator) software beyond the restrains of SuperCollider's notational capabilities. The creation of this notation aimed to integrate rhythmic strategies with the aforementioned canonic ideas in such a way that nuance and specificity of temporal organisation produces a fertile ground for time-oriented sonic experimentation.

## Installation as SuperCollider Extension and parasite

- In the terminal go to the location of this repository and run the commands: `cabal configure` and `make build`.

- Run ghci in the terminal with the command `ghci` at this repository's location. Load the file Main.hs with the command `:l Main.hs`.

- Once compiled run the function: `main`.

- Open SuperCollider go to edit -> preferences and click in the Interpeter section.

- Include the folder TimeNotSCClass and reboot the interpreter.

- Load the initTimeNot.scd file until it reads: 
"Bienvenido a TimeNot
un parásito en el IDE de SuperCollider"

- In a new file evaluate `TimeNot.start("timeNot")`. Erase that and evaluate the following lines.

```
// timeNot

x

```


## Choose an Environment for Time Not
### SuperCollider Parasite
The software can be dowloaded and installed as a SuperCollider extension that uses its IDE as an editor and the SC Server as a audio engine.The notation is activated with the class `TimeNote` and its three methods `defaultServerConfig`, `connect` and `start`. A series of files with samples, buffers, synthdefs and other necessary complements are downloaded with this repositopry including the file initTimeNot. The init file will load every item necessary to produce sound with the default instruments and configurtions. Init will compile `TimeNot.defaultServerConfig` and `TimeNot.connect` which will set adequate server configurations and produce the necessary OSC communication between the core program and SuperCollider. In order to produce personalised synthesis processes for this platform the OSC messages recieved from the Haskell core are identified with the path `"/canon"` and  `"/printError"`. As the tempo is defined in UTCTime a simple algorithm was produced to synchronised it with SuperCollider's system clock. Once the init file is compiled, the notation can run with the method `start`. Start has one argument, this is a key word that if found in a given file will send the text found in the editor as a string to the Haskell core. 

```
TimeNot.start("timeNot")

```
A recomended practice is to have a timeNot file identified with a comment with the keyword in the top of it, like the following program.

```
// timeNot

xxxx samples: bd

```

### Estuary Music Live Coding Language 
[Estuary](https://github.com/dktr0/estuary) is a multi-lingual and multi-contextual live coding symbiont allocated online. Thus, it requires zero installation and can be used as an instrument or as a platform for network collaboration ensembles. TimeNot was developed in McMaster University under the supervision of David Ogborn and it is closely related to the activity in the Network Imagination Laboratory and the artistic practice of the Cybernetic Orchestra. TimeNot is a music creation language that is part of the constellation of languages that can be used for live coding in Estuary and it can be found in Estuary's [website](https://intramuros.mcmaster.ca/). Soon there will be help files and tutorials for timeNot in this environment.

## Usage
The notation assists computer musicicans to expressively write and reason complex rhythmic structure and canonic forms. Syntactically it is divided in four strata that will be explained in detail below: The first produces an overall duration or a sequence of durations for the same canonic/rhythmic structure, the second generates rhythmic structures, the third one turns such rhythmic structure into canonic form and finally the keys that produce instruments, pitch, amp and other non-temporal parameters are notated. The rhythmic aspects of the notation can be used independently from the canonic ones. Nevertheless a minimal rhythmic idea has to be written for the notation to produce sound, also rhythmic ideas are necessary in order to produce a canonic form. The default values in all except rhythmic stratum allow the user to quickly start producing sound without having to write a broad program. It is compatible with the idea of "from scratch" live coding in which economy of resources is key. 

### Duration and Sequence
The duration of the rhythmic/canonic structure is determined by a float followed by a `s` that represents seconds. However, the number could be a number of beats in a given tempo mark (bpm) by adding a `t` or indicate amount of cycles (cps) by adding a `c`. To determine a duration the float should be embedded in a special list that uses special symbols and separators. The lists that uses the `|:  :|` as a constrain and the `|` as a separator generate a loop (this function is not implemented yet). The lists between `|.  .|` and `|` as separator produce a single instance of the sequence of rhythmic or canonic structures. If the duration of the structure is not written, the default is a 2 second sound (without looping). It is easy to change the metric depth of a rhythmic structure without lossing a reference to the original tempo, the notation required is M2, M4, M8 to multiply the tempo by 2, 4 and 8, and m2, m4, m8 multiplies the tempo by 0.5, 0.25, 0.125. 

The first line of the next example produces a single structure with a duration of 4 seconds. The second changes the total duration of the canon from 4 seconds to 8, the third creates 8 repetitions of a 2 second structure and the last one produces a sequence of three structures with the corresponding durations in a loop. 

```
|. 4s .| x

|. 4sM2 .| x

|. 2s % 8 .| x

|: 2s | 1s | 3s :| x 

```

### As a rhythm generator
A fundamental aspect of this notation is its capability to create, with an expressive notation, rhythmic ideas. There are three main ways to produce rhythmic figures: through a series of onset patterns, through the repetition of a series of onset patterns or via a notation that produces euclidean algorithms. The sub-notation that allows rhythm production is recursive, thus it is possible to write nested structures.

#### Onset Patterns
The way in which patterns are notated uses the chars `x`and `o`. The former produces a sound attack and the latter produces a rest. In this way it is possible to produce rhythmic patterns freely. In the following example the first pattern produces two attacks followed by 6 rests, the second pattern produces a Cuban tresillo and the last produces a 3/4 pattern with two eight notes in the last beat.

```
xxoooooo

xooxooxo

xoxoxx
```

#### Repeat Patterns
With the symbols `!` and `#` is possible to indicate how to repeat a pattern. After the `!` the onset, repeat or euclidean pattern to be repeated should be written and after the `#` a value that represents the number of repetitions. The first example above can be simplified as follows. 

```
xx!o#6;

```

#### Euclidean Patterns
The most expressive and complete notational tool for rhythm is the Euclidean pattern notation. It can be enmeshed with the onset pattern and the repeat notation. The non-optional values to be given are n and k values explained in the paper by Godfried Toussaint on [euclidean algorithms](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf). The syntax is `k:e:n`, the following code also generates a Cuban tresillo:

```
3:e:8
```

The k and n values are chained by the operator `:e:`. This parser has other three similar operators: `:r:` which creates a rotation value and `p:` that produces recursively a pattern that can be a repeat, an onset or another euclidean.

A full example of the euclidean pattern is:

```
5:e:8:r:1 p:xx;

3:e:8 p:2:e:3:r:1

```
An example of a comprehensive use of this rhythmic notation might be:

```
|: 4s :|
xxo!3:e:8 p:xx#2xooox

```

### As a canon generator
The capability to generate temporal canons is a concept already explored in depth in the Nanc-in-a-Can Canon Generator. The model that is implemented here so far is the one denominated convergence canon. The canonic function needs to have a list of ratios and its corresponding list of transpositions and convergence point, along with the rhythmic structure that will be canonised. The absolute length of the whole canon is determined in the duration of the structure.  

A noticeable difference with Nanc-in-a-Can is the way in which the canon parameters are understood. Given the multi-contextual nature of TimeNot and the idea of an overall duration of the canon determined a priori the temporal transposition identified as `tempos` in Nanc-In-A-Can is here identified as `ra`, referencing ratios. The parameter transposition is useful when using pitched instruments, synthdefs, given the emphasis in sample-based instruments in platforms like Estuary, in which Time Not is implemented, the `tr` parameter is now optional in the case of pitched instruments and in the case of unpitched (sample-based) it is uncompatible. In this way, less typing is needed to produce a "canonic transformation" and the idea of a temporal canon becomes even more focused on temporal aspects of the musical ideas. 

#### Ratio and Transposition
Ratio key (`ra:`) is a list of time proportions separated by `:` which indicates the proportional nature of the tempos produced by ratio. A set of ratios as `3:4` would produce a relationship of three sounds in one voice every 4 in the other. Transposition (`tr`) is a list with `|` as a separator that produces a transposition value in midinotes. Transposition has a default of 0 since unpitched instruments will not need it. If a different length is provided between these two lists the smaller list will cylce until reaching the size of the biggest. 

#### Convergence Point
The convergence point is the instant in which the chronometric time and the structural time of each voice is identical. This means that the event position of the canonised rhythmic ideas converges. The default value of the CP is 0, this means that the voices will coincide in the first event. The syntax to provide a different cp is `cp: int`. Some keywords are accepted, like `last` that will provide a cp in the last attack, `lastx` which will place the cp in the last onset attack, `palindrome` will provide a cp in the event closest to the middle of the structure. If the int for the CP is greater than the number of events in a canonic voice it will cycle back.

Canon examples:

```
|: 35s :| xoox!5:e:8#5xxoxoooxo
ra: 13:17:20 tr: 0|3|7 cp: 10
synths: saw sqr tri
eu pitch: 60 67 65 68 72 75.5 60 55 56 59 60

|: 12s :| !xxoxox#4 ra: 1:0.5 cp: 5 samples: cabasa hibongo
```

### Instruments
The instrument key is a list of instrument names. The name of the instruments represent a SynthDef that can be found in the SuperCollider file XXXXXXXXXXXX. Instruments are organised as lists that will cylce through each voice of the canon. This means that the first instrument will be used in the canonic voice 1, second instrument in canonic voice 2, etc. If the number of voices is greater than the number of instruments, the list will cycle back. For the moment there is no ntoation to produce many instruments for one voice. It is necessary to indicate the type of instrument with the keys: `synths:` and `samples:`, and `dirst:` for instruments in Estuary.

### Other sound parameters
For the moment, there are three ways to organise these parameters: the key `iso` will generate an isorhythmic relationship between the rhythmic values and parameter. In other words the parameter will cycle as a sequence from left to right as many times as necessary to cover all the rhythmic values. The key `isoGrid` produces something similar to `iso` however sounds and rests are taken into consideration, this means that the parameter sequence will cycle over the sounds and rests of the rhythmic structure. Finally, the key `eu` produces a euclidean distribution of parameter values taking into consideration the rests and sounds of the rhythmic structure. For the moment there is only one pattern for the organisation of all keys, in the future each parameter will have its own organisation pattern.

#### Pitch
There are two kinds of instruments: Pitched and unpitched. The sample-based instruments normally do not have pitch, so declaring pitch will do nothing when using one of these, the unpitched instruments are sample-based and a similar key is provided to this instruments, they will respond to the parameter `rate`.

The combination of a pitched instrument and a pitch pattern will respond to the canon parameter transposition. The pitch pattern will be mapped through the trasnposition values producing the voices with the addition of the pitch and the transposition value. 

#### Amp

The key `amp` will produce a pattern of amplitude values in decibels that will be the same for every voice of the canon. Similarly to pitch, but it cannot be transposed canonically.

Parameters example:
```
5:e:8 samples: clp rate: 0.5 1 amp: 0.5 1 0.25

3:e:8 ra: 1:1.5:0.6 tr: 0|-12|12 synths: tri saw sqr pitch: 60 65 68 amp: 0.3 0.5 0.7
```

## Further Development

There are many areas in which this notation needs to become more robust. Firstly the rhythmic notation needs to produce recursive structures. In other words, the possibility to have nested rhythms as in McLean's Tidal Cycles or in the rhythmic ideas of Ferneyhough. The rhythmic notation can also recieve other characters that could allow a more flexible treatment of the instruments and the voices. For example a distinction between `x` and `X` could allow two different instruments to produce a differetniated rhythmic structure that do not depend on rests. The use of integers to identify voices could allow the user to create variations within the canonic voices. 

The keys `ra:`, `tr:` and the sound patterns should recieve functions to algorithmically produce canons and sound patterns. Integrating Haskell's powerfull functional programming capabilities into the notation.

The canonic structures developed by Conlon Nancarrow include divergence canons, acceleration canons, multicanonic structures. All of these should be integrated to the notation so it is possible to continue to explore the polytemporality proposed by the Mexican artist.

The sound parameters will have to include other especific keys like pan or out and also include the possibility of personalised synthesis definitions that SuperCollider enables. 