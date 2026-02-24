# Fuzzer effectiveness benchmark

```
$ cd tests
$ elm make src/EffectivenessMain.elm --output effectiveness-runner.js
$ node effectiveness-runner.js
```

Inspired by chapter 5 of "How to Specify It!: A Guide to Writing Properties of
Pure Functions"
(https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf).

Eight bugs injected into an implementation of a Binary Search Tree:

| Bug # | Description |
|-------|-------------|
| 1	| `insert` discards the existing tree, returning a single-node tree just containing the newly inserted value.
| 2	| `insert` fails to recognize and update an existing key, inserting a duplicate entry instead.
| 3	| `insert` fails to update an existing key, leaving the tree unchanged instead.
| 4	| `delete` fails to rebuild the tree above the key being deleted, returning only the remainder of the tree from that point on (an easy mistake for those used to imperative programming to make).
| 5	| Key comparisons reversed in `delete`; only works correctly at the root of the tree.
| 6	| `union` wrongly assumes that all the keys in the first argument precede those in the second.
| 7	| `union` wrongly assumes that if the key at the root of `t` is smaller than the key at the root of `t'` , then all the keys in t will be smaller than the key at the root of `t'`.
| 8	| `union` works correctly, except that when both trees contain the same key, the left argument does not always take priority.

(Since `delete` is used in `union`, the two delete tests cause failures in the
union tests as well -> 10 test failures instead of just 8.)

We want to measure fuzzer effectiveness for a given test suite + test runner
implementation. For example, like in the paper, we could compare various _types
of PBT tests_, or we could compare random-generation fuzzers vs coverage-guided
fuzzers (and in these, various input mutation algorithms).
