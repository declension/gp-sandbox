Genetic Programming sandbox
===========================
[![Build Status](https://travis-ci.org/declension/gp-sandbox.svg?branch=master)](https://travis-ci.org/declension/gp-sandbox)

Small experiments with Genetic Programming using the [GenProg](http://hackage.haskell.org/package/genprog) library.

This is influenced by my previous [Java-based card-playing evolution experiment](https://github.com/declension/OhHellStrategyEvolution/), with a view to using the increased power and brevity of functional programming / Haskell (specifically [Higher Order Functions](https://wiki.haskell.org/HOF) and [Algebraic data types](https://wiki.haskell.org/Algebraic_data_type))

Project setup
-------------

 * The project is [built with Stack](stack.yaml) and based on an LTS Stackage.
 * Currently, a static copy of `genprog` is included in [`lib/`](lib/) for ease of building (and exploration locally, to be honest). It's on Hackage, [but not Stackage](https://github.com/jsnajder/genprog/issues/2) which would make this codebase smaller, more standard and easy to upgrade (doesn't construct a build plan currently).

### Building

```bash
stack build
```

### Running
```bash
stack exec gp-sandbox
```


### Tests
The `HSpec` unit tests are run thus:
```bash
stack test
```
