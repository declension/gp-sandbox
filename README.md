Genetic Programming sandbox
===========================

Small experiments with Genetic Programming using the [GenProg](http://hackage.haskell.org/package/genprog) library.

As this project progresses, it may take influence from my previous [Java-based card-playing evolution experiment](https://github.com/declension/OhHellStrategyEvolution/), with a view to using the increased power and brevity of functional programming / Haskell (specifically [Higher Order Functions](https://wiki.haskell.org/HOF) and [Algebraic data types](https://wiki.haskell.org/Algebraic_data_type))

Project setup
-------------

 * The project is [built with Stack](stack.yaml) and based on an LTS Stackage.
 * Currently, a static copy of `genprog` is included in [`lib/`](lib/) for ease of building (and exploration locally, to be honest). It's on Hackage, [but not Stackage](https://github.com/jsnajder/genprog/issues/2) which would make this codebase smaller, more standard and easy to upgrade.

### Building

```bash
stack build
```

### Running
```bash
stack exec gp-sandbox
```
