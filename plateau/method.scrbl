#lang gm-plateau-2017

@; TODO untyped configuration vs dynamically typed

@title[#:tag "sec:method"]{Evaluation Method}

@citet[takikawa-popl-2016] introduce a three-step method for evaluating the performance of
 a gradual typing system:
 (1) identify a suite of fully-typed programs;
 (2) measure the performance of all gradually-typed @emph{configurations} of the programs;
 (3) count the number of configurations with performance overhead no greater than a certain limit.
Takikawa @|etal| apply this method to Typed Racket, a gradual typing system
 with module-level granularity.
In other words, a fully-typed Typed Racket program with @${M} modules defines
 a space of @${2^M} configurations.

Reticulated supports gradual typing at a much finer granularity,
 making it impractical to directly apply the Takikawa method.
The following subsections therefore generalize the Takikawa method (@section-ref{sec:method:adapt})
 and describe the protocol we use to evaluate Reticulated (@section-ref{sec:protocol}).


@section[#:tag "sec:method:adapt"]{Generalizing the Takikawa Method}

A gradual typing system enriches a dynamically typed language with a notion of static typing.
The type system defines which syntactic units@note{E.g. variables, expressions, modules} of a program may be typed;
 this is the so-called @emph{granularity} of the gradual typing system.
A performance evaluation must consider the ways that a programmer may write
 type annotations, subject to practical constraints.

@definition["granularity"]{
  The @emph{granularity} of an evaluation is the syntactic unit at which
   the evaluation adds or removes type annotations.
}

For example, the evaluation in @citet[takikawa-popl-2016] is at the granularity
 of modules.
The evaluation in @citet[vss-popl-2017] is at the granularity
 of whole programs.
@Section-ref{sec:protocol} defines the @emph{function and class-fields} granularity that we use.

After defining a granularity, a performance evaluation must define a suite of
 programs to measure.
A potential complication is that such programs may depend on external libraries
 or other modules that lie outside the scope of the evaluation.
@; TODO generalize 'modules'?

@definition["experimental, control"]{
  The @emph{experimental modules} in a program define its configurations.
  The @emph{control modules} in a program are common across all configurations.
}

The experimental modules and granularity of type annotations define the
 configurations of a fully-typed program.

@definition["configurations"]{
  Let @${P \tcstep P'}
   if and only if program @${P'} can be obtained from
   @${P} by annotating one syntactic unit in an experimental module.
  Let @${\tcmulti} be the reflexive, transitive closure of the @${\tcstep} relation.@note{The @${\tcstep} relation expresses the notion of a @emph{type conversion step}@~cite[takikawa-popl-2016]. The @${\tcmulti} relation expresses the notion of @emph{term precision}@~cite[svcb-snapl-2015].}
  @; note^2: `e0 -->* e1` if and only if `e1 <= e0`
  The @emph{configurations} of a fully-typed program @${P^\tau} are all
   programs @${P} such that @${P\!\tcmulti P^\tau}.
}

A performance evaluation must measure the running time of these configurations
 relative to the same program without gradual typing.
In Typed Racket, this baseline is the performance of Racket running the
 untyped configuration.
In Reticulated, the baseline is the performance of Python running the untyped configuration.

@; TODO confusing 'program' and 'configuration', maybe need to define "baseline" or underlying-program?

@definition["performance ratio"]{
  A @emph{performance ratio} is the running time of a program
   divided by the running time of the same program in the absence of gradual typing.
}

An @emph{exhaustive} performance evaluation measures the performance of every
 configuration.
The natural way to interpret this data is to choose a notion of "good performance"
 and count the number of "good" configurations.
In this spirit, @citet[takikawa-popl-2016] ask programmers to consider the
 performance overhead they could deliver to clients of their software.

@definition[@deliverable{D}]{
  For @$|{D \in \mathbb{R}^{+}}|, a configuration is @deliverable{D} if its performance ratio is no greater than @${D}.
}

If an exhaustive performance evaluation is infeasible, one alternative is
 to select configurations via simple random sampling and measure the
 proportion of @deliverable{D} configurations in the sample.
Repeating this experiment yields a @emph{simple random approximation} of the
 true proportion of @deliverable{D} configurations.
@; TODO this is unclear

@definition[@approximation["r" "s" "95"]]{
  Given @${r} samples each containing @${s} configurations chosen uniformly at random,
   a @approximation["r" "s" "95"] is a @${95\%} confidence interval for the
   proportion of @deliverable{D} configurations in each sample.
}

The appendix contains theoretical and empirical justification for the simple
 random approximation method.


@section[#:tag "sec:protocol"]{Protocol}

@parag{Granularity}
The evaluation presented in @section-ref{sec:exhaustive} is at the granularity
 of @emph{function and class fields}.
In general, one syntactic unit in the experiment is either one function,
 one method, or the collection of all fields for one class.
In particular, the class in @figure-ref{fig:cash} has 3 syntactic units and
 therefore @${2^3} configurations.


@parag{Benchmark Creation}
To convert a Reticulated program into a benchmark, we:
 (1) build a driver module that runs the program and collects timing information;
 (2) remove any non-determinism or I/O actions;
 (3) partition the program into experimental and control modules; and
 (4) ascribe types to the experimental modules.
When ascribing types, we re-use any type annotations or comments in the original program.
@; TODO awkward


@parag{Data Collection}
For benchmarks with at most @$|{2^{21}}| configurations, we conduct an exhaustive
 evaluation.
For larger benchmarks, with @${F} functions and @${C} classes,
 we conduct a simple random approximation using
 @integer->word[NUM-SAMPLE-TRIALS] samples of @${@id[SAMPLE-RATE] * (F + C)}
 configurations.

All data in this paper was produced by jobs we sent
 to the @emph{Karst at Indiana University}@note{@url{https://kb.iu.edu/d/bezu}} high-throughput computing cluster.
Each job:
@itemlist[#:style 'ordered
@item{
  reserved all processors on one node for 24 hours;
}
@item{
  downloaded fresh copies of @|PYTHON|
  and Reticulated (commit @hyperlink["https://github.com/mvitousek/reticulated/commit/e478343ce7c0f2bc50d897b0ad38055e8fd9487d"]{@tt{e478343}}
  on the @hyperlink["https://github.com/mvitousek/reticulated"]{@tt{master}} branch);
}
@item{
  (repeatedly)
  selected a random configuration to measure,
  ran the configuration's main module @id[NUM-ITERATIONS] times,
  and recorded the result of each run.
}
]
Cluster nodes are IBM NeXtScale nx360 M4 servers with two Intel Xeon E5-2650 v2
 8-core processors, 32 GB of RAM, and 250 GB of local disk storage.
