# EDM-DSL
EDM-DSL is a domain specific language embedded in Haskell for electronic dance music production.
To get it up and running on your machine:

1) Install [Sonic Pi](https://sonic-pi.net/)
2) Install [Sonic Pi tool](https://github.com/lpil/sonic-pi-tool) (for use with the command line)
3) Change the variable sPTPath (in the source code) to the path of your Sonic Pi tool installation
4) Start sonic Pi before using EDM-DSL
5) Run test.hs from the command line

## Background:
EDM-DSL is implemented via a parser, evaluation functions, and a backend that compiles
the evaluated code into Sonic Pi code. The first step is the parsing of a .edmdsl file. A coder
will write their song into a .edmdsl file and it may look something like the following:

```
drumbass = 10 * = X :j O :j X :j O;
drumsnare = 10 * = X :j X :j O :j X;
drumcymbal = 10 * = O :j O :j X :j O;
bassTrack = drum_bass_hard drumbass;
snareTrack = drum_snare_hard drumsnare;
cymbalTrack = drum_cymbal_closed drumcymbal;
beatSong = := bassTrack := snareTrack := cymbalTrack
```

The above song is an example of a beat consisting of a bassdrum, snare drum, and cymbals,
played for a total of 40 notes. The program parses the specified file into expressions. The
expressions are then evaluated by a set of functions that convert them into a track. The
track is then converted into Sonic Pi code via a series of backend functions.

###### For further information please read the pdf file in the repository

-- EDM-DSL is adapted from [HMusic](https://www.nime.org/proceedings/2019/nime2019_paper074.pdf) --
