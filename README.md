Description
===========

A simulation of Schelling's model of residential segregation. Background [here](http://nifty.stanford.edu/2014/mccown-schelling-model-segregation/) and [here](https://lectures.quantecon.org/jl/schelling.html). To clone:

```
git clone https://github.com/wchen30030/schelling-model
```

How to Run
==========

The main module for the program is `Simulation.hs` compiled with `ghc Simulation.hs`. The terminal-based program normally runs with a random 10x10 grid. Add the `-r` flag to set the initial configuration:
```
./Simulation -r
```
Program is in Haskell.
