#lang gm-dls-2017

@title[#:tag "sec:vs-tr"]{Why is Reticulated so Fast ...}

The worst slowdown we observe in Reticulated is within one order of magnitude.
By contrast, many partially typed Typed Racket programs are two orders of
 magnitude slower than their untyped counterparts@~cite[takikawa-popl-2016 greenman-jfp-2017].
While implementation technology and the peculiarities of the benchmarks
 affect performance, this order-of-magnitude gap suggests fundamental differences
 between Typed Racket and Reticulated.

We have identified three factors that contribute to the relative
 performance of Reticulated.
@;;;; contribute to the seemingly-improved
First, Reticulated's type system lacks support for unions, recursive types,
 and variable-arity functions.
@;;;; something like "... lack support for common python idioms"
Second, Reticulated's error messages rarely provide actionable feedback.
Third, Reticulated guarantees an alternative notion of type soundness.
All three factors affect not only performance, but also Reticulated programmers.
@;;;; dont need the comma, its not a whole sentence BY THE WAY i dont like this sentence --Ben


@section{Missing Types}
@(define pystone-union-fields
        @; grep for 'PtrComp = ' to find assignments
        @; It's initially `None`, and assigned away-from and back-to `None`
        @;  in `Proc1`
        '(PSRecord.PtrComp))
@(define stats-union-functions
        @; Most of these functions all have a dead-giveaway pair of lines:
        @; ```
        @;  if type(cols) not in [list,tuple]:
        @;      cols = [cols]
        @; ```
        '(abut simpleabut colex linexand recode))
@(define dyn* '(go pystone stats take5))
@; TODO add better in-file evidence

@;;;; not benchmarks, programs
@;;;; dont use "Fist" "Second" THird" just sy the names
@;;;; start the section instead with "Retic lacks supports for union, rec, ....... var-arity
@;;;;  then explain that its common python idioms, also that it affected our benchmarks
@;;;;  always, first sentece of every paragraph should explain focus, and focus should be coherent throughout section
@Integer->word[(length dyn*)] of our benchmarks must resort to dynamic typing
 because Reticulated cannot express the desired type.
First and second, the @bm{pystone} and @bm{stats} benchmarks require union types.
Third, the @bm{go} benchmark contains a recursive class type.
Fourth, one function in the @bm{take5} benchmark accepts optional arguments.@note{@url{https://github.com/mvitousek/reticulated/issues/32}}

If Reticulated cannot express such types, Python programmers will frequently
 need to rewrite their programs before they can try gradual typing.
In our own experience, we rewrote several benchmarks that used @tt{None} as
 a default value to use a well-typed sentinel value.
@;;;; dont say "in our experience"
We also attempted to rewrite a shallowly-embedded Lisp interpreter to use a
 deep-embedding, but stopped for the lack of recursive types.
@;;;; probably remove Lisp interpreter ... unless the experience makes sense in the new section 7.1
@;;;; this situation calls for evaluation ... size of rewrites ... like what Takikawa etal at ECOOP 2015 or TobinHochstadt + Matthias at ICFP 2010 (probably that paper, Logical types)

Such rewrites are both time-consuming and prone to introduce bugs.
Developers would benefit if Reticulated added support for these types.
Indeed, @|PEP-484| specifies syntax for generics, untagged union types,
 recursive types, optional arguments, and keyword arguments.

Enforcing these types at run-time, however, will impose a higher cost than
 the single-test types that Reticulated programmers must currently use.
A union type or (equi-)recursive type requires a disjunction of type tests, and
 a variable-arity procedure requires a sequence of type checks.
If, for example, every type annotation @${T} in our benchmarks was instead a
 union type with @${T} and @tt{Void}, then overall performance would be nearly
 2x worse.


@section{Uninformative Errors}
@(define vss-popl-2017-benchmarks
   '(callsimple nqueens pidigits meteor fannkuch nbody callmethod
     callmethodslots pystone float chaos go spectralnorm))
@(define vss-2x-benchmarks
   '(nqueens meteor fannkuch callmethod callmethodslots pystone float chaos go))

Errors matter@~cite[f-keynote-2002].
When systems work, everyone is happy, but when systems break, developers really
 want error messages that pinpoint the source of the fault.
@;;;; need to explain "FAULT" because there are 2 kinds
@;;;; 1. static type errors (have decent messages)
@;;;; 2. dynamic type errors, because a value and a type annotation mis-match
@;;;; for (2) its very important to have (1) the value (2) the type (3) the boundary
@;;;; retics stack trace + generic "check failed on this value" isn't enough,
@;;;; worst problem is, programmer has to find the value with no help from the error message
@;;;; flat/higher-order ... is not the focus
@;;;; matthias talked about 3 problems on the board, they are:
@;;;;  1. error message doesn't give the type annotation (can usually recover from stack trace, also a line number)
@;;;;  2. if fucntion return fails test, you just ge tthe value not the function (though can usually deduce from stack trace)
@;;;;  3. dont know where bad value came from (worse if higher order, but I don't think that's important to say)

Reticulated currently produces simple error messages that supply (1) a value
 that failed some check and (2) a stack trace.
For flat types, these clues often suffice to deduce the type
 check that halted the program.
@;;;To illustrate, consider the following small program:
@;;;
@;;;@nested[@python|{
@;;;def id(x)->Int:
@;;;  return x
@;;;id(None)
@;;;}|]@;
@;;;running this program produces the following error message:
@;;;
@;;;@nested[@exact|{{\footnotesize\begin{verbatim}
@;;;Traceback (most recent call last):
@;;;  File "/usr/local/bin/retic", line 6, in <module>
@;;;    retic.main()
@;;;  File ".../reticulated/retic/retic.py", line 155, in main
@;;;    reticulate(program, prog_args=args.args.split(), flag_sets=args)
@;;;  File ".../reticulated/retic/retic.py", line 104, in reticulate
@;;;    utils.handle_runtime_error(exit=True)
@;;;  File ".../reticulated/retic/retic.py", line 102, in reticulate
@;;;    _exec(code, __main__.__dict__)
@;;;  File ".../reticulated/retic/exec3/__init__.py", line 2, in _exec
@;;;    exec (obj, globs, locs)
@;;;  File "test.py", line 3, in <module>
@;;;    id(None)
@;;;  File "test.py", line 2, in id
@;;;    return x
@;;;  File ".../reticulated/retic/runtime.py", line 91, in check_type_int
@;;;    return val if isinstance(val, int) else (check_type_bool(val) if not flags.FLAT_PRIMITIVES else rse(val))
@;;;  File ".../reticulated/retic/runtime.py", line 100, in check_type_bool
@;;;    return val if isinstance(val, bool) else rse(val)
@;;;  File ".../reticulated/retic/runtime.py", line 88, in rse
@;;;    raise Exception(x)
@;;;Exception: None
@;;;\end{verbatim}}}|]@;
For function types and parameterized types, the relevant annotation is
 slightly harder to find, but still possible via the stack trace.
Unfortunately, such annotations are useless if they are correct.
If the fault is due to a bad value, the programmer must inspect the program to
 find where it came from.

Improving the error messages will add performance overhead.
For example, @citet[vss-popl-2017] built an extension to Reticulated that improves these
 error messages by reporting all casts that may have led to the dynamic type error.
@;;;; improving/improves is redundant
Their evaluation reports the @|t/p-ratio|s of
 @integer->word[(length vss-popl-2017-benchmarks)]
 programs from @|TPPBS|; in @integer->word[(length vss-2x-benchmarks)]
 programs, adding blame tracking to the fully-typed configuration
 at least doubled the @|t/p-ratio|.
@;;;; TOOO LONG keep it simple, like "their evaluation says the TP ratio may double"


@section{Alternative Soundness}

Sound type systems are useful because they provide guarantees.
If a static type system proves that a term has type @${\tau}, then @${\tau}
 specifies the term's behavior.
The type system can use this specification to find small logical errors throughout a program,
 the compiler can rely on this specification to generate efficient code,
 and the programmer can trust this specification as an API.
@;;;; use semicolons for the list items

Gradual typing systems cannot provide exactly the same guarantees,
 but Typed Racket's soundness is quite similar to conventional soundness.
In Typed Racket, typed code is sound in the conventional sense; for example, the
 compiler may use the types to eliminate run-time tag-checks@~cite[sthff-padl-2012].
Untyped code is quarantined.
An untyped value @${v} can only cross the boundary into typed code via
 a type @${\tau}.
Typed Racket enforces the behavioral specification implied by @${\tau}
 by compiling the type to a contract; if @${v} does not satisfy the contract,
 the programmer receives an error message containing @${v}, @${\tau}, and
 the relevant boundary@~cite[tfffgksst-snapl-2017].
@;;;; what is the point of this paragraph? why so much about typed racket?
@;;;; 1. just say can't do normal soundness because there's possibility of untyped code
@;;;; 2. one possibility is to generalize, add a 4th case. This happens to be what TR did.
@;;;; 3. another possibility is to CHANGE the old ways, this is what retic chose to do, they change 1 to 1', maybe they also have a 4' gotta think about it
@;;;; 4. (maybe also say) optional typing is just 2/3, just fewer guarantees

@; -------------------------------------------------------
@; MF: we should put a visual marker here, like a line 
@(define running 
  @exact{\par \noindent \hrulefill \par \noindent Running this program yields:})

@;;;; maybe make one of these cases a nested list
@figure["fig:magic"
        @list{A well-typed Reticulated program}]{
@python|{
    def make_strings()->List(String):
      xs = []
      for i in range(3):
        if   i == 0: xs.append(i)
        elif i == 1: xs.append(True)
        else       : xs.append(make_strings)
      return xs

    make_strings()
}|}

Reticulated takes an alternative approach.
It guarantees tag-level soundness.
As @figure-ref{fig:magic} demonstrates, a Reticulated term with type @tt{List(String)} may evaluate to a list containing any kind of data.
On one hand, this fact is harmless since tag soundness implies that any read from a variable with type @tt{List(String)} is tag-checked.
On the other hand, Reticulated does not guard values that exit typed regions.
Thus, two interesting scenarios can arise:
@itemlist[#:style 'ordered
@item{
  (the @emph{typhoid mary} scenario) Typed code can create an ill-typed value,
  pass it to untyped code, and trigger an error by violating an implicit
  assumption in the untyped code.
  The source of such ``disguised'' type errors may be difficult to track down.
  @;;;; 'may be' => 'is'
}
@item{
  (the @emph{sybil} scenario) Two typed contexts can safely reference the same value at incompatible types.
}
]@;
It remains to be seen whether these potential scenarios cause practical issues.
@;;;; ".... cause problems in practice."
Developers may embrace the flexibility of tag-soundness and use Reticulated
 in combination with unit tests.
At the moment, the only conclusion our data supports is that Reticulated's
 tag checks impose less performance overhead than Typed Racket's behavioral contracts.

