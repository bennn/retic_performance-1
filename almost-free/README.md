almost-free
===


Background
---

There's a paper in VMIL 2019 that uses configurations with 1 type annotation
 to explain configurations with N type annotations.
See figure 2.

```
  @inproceedings{grmhn-vmil-2019,
    title = {Which of My Transient Type Checks Are Not (Almost) Free?},
    author = {Gariano, Isaac Oscar and Roberts, Richard and Marr, Stefan and Homer, Michael and Noble, James},
    booktitle = {{VMIL}},
    pages = {58--66},
    year = {2019}
  }
```

They make a bar graph showing the performance of every configuration with
 exactly one type annotation.
In these graphs, only 1 or 2 bars are "interesting" ... i.e. far off from 1x
 overhead.
These interesting type annotation (bars) also appear in "interesting" groups
 of points in a set of randomly-sampled configurations.

None of this is made technically precise in the paper.

The question is: does our Transient Reticulated data ALSO have bar graphs with
 only 1 or 2 "interesting" bars?
If so, can we quantify "interesting" and make precise predictions about the
 configurations?

We have all the ground-truth data (except, probably, for the largest benchmarks)
 so lets see.


VMIL notes
---

Contribution
- bars => patterns
- 1-2 bars exceptional

Moth transient =/ Retic transient

"line 8" typecheck very surprising

typo: spectralnorm isn't in fig 2, must mean pystone

code for the "higher than normal" conclusion?



1-bar notes
---

Tried highlighting the scatterplots for each 1-type configuration.
A program with N types => N scatterplots, where a config is highlighted if it
 has the matching type "active".

For many benchmarks, highlighting the slowest 1-type highlights an interesting
 group of points.
... fsm jpeg kcfa morsecode sieve snake suffixtree synth tetris zombie http2
 Espionage call_method call_simple go meteor nqueens spectralnorm

A few benchmarks are boring.
 fannkuch - has 1 type annotation, nothing to predict
 nbody pidigits - adding types does little/nothing to change performance

The last few are interesting: chaos float futen pystone
 the max 1-type does not get a clear pattern,
 the plots are so big its hard to tell if any set succeeds / fails,
 probably need 3 bars for the pattern

So, these suggest need better method.


TODO
---

- [X] read paper
- [ ] email authors for their code + benchmarks
- [X] which 1-ann configurations do we have for our large benchmarks?
  - everything for sample_fsm aespython stats
  - need to run them
- [X] make simple bar graphs
  - [X] pepm
  - [X] icfp ... both these are interesting!
- [X] spot-check for interesting groups of points
  - compare median to quartile?
  - we have more bars beyond +-0.1 and higher maxs
  - median ofter off baseline=1 line!
- [X] map bars to points
  - add list of points to the exact plot?
  - make special grace plot helper?
- [ ] make all 1-bar pictures
  - missing slowSHA and take5, run on NSA
- [ ] what is "slow"? is it "at least as bad as the worst 1-type config"?
- [ ] script to check "slow" coverage / find a minimal partition that covers
  - should be easy ... 1) set all slow configs 2) filter by type ... repeat
- [ ] 
- [ ] how many of our functions have 3 or more args?
- [ ] what if 2 levels? more? (Huh? 2020-03-12)
- [ ] workshop target @ ICFP .... Scheme or MiniKanren? Because we're using
      them as tools.

