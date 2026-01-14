-- template for announcing a new release

Frédéric Bour and I are pleased to announce a new release of Fix.

In short, Fix is a toolkit that helps perform memoization and fixed point
computations (including data flow analyses). More generally, it offers a
number of basic algorithmic building blocks that can be useful in many
circumstances.

In this release, two new modules have been added:

  * [Fix.Minimize](http://cambium.inria.fr/~fpottier/fix/doc/fix/Fix/Minimize/)
    offers a minimization algorithm for deterministic finite automata (DFAs).
    It is based on Antti Valmari's 2012 paper, "Fast brief practical DFA
    minimization".

  * [Fix.Partition](http://cambium.inria.fr/~fpottier/fix/doc/fix/Fix/Partition/)
    offers a partition refinement data structure, which is used by the minimization
    algorithm, and could be useful in other algorithms.

There are other minor
[changes](https://gitlab.inria.fr/fpottier/fix/-/blob/master/CHANGES.md).

The library can be installed as follows:

```
  opam update
  opam install fix.20230505
```

[Documentation](http://cambium.inria.fr/~fpottier/fix/doc/fix/)
is available online.
