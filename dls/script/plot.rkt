#lang racket/base

;; Plotting

;; TODO ?
;; - average-case
;; - worst-case
;; - paths
;; - lattice
;; - overhead delta (what's the delta between?)

(require racket/contract)
(provide
  (contract-out
    [overhead-plot
     (-> performance-info? pict?)]
    ;; Render an overhead plot for the given benchmark

    [performance-vs-num-types-plot
     (-> performance-info? pict?)]
    ;; TODO
))

(require
  "benchmark-info.rkt"
  "performance-info.rkt"
  "sample.rkt"
  "util.rkt"
  pict
  plot/no-gui
  plot/utils
  (only-in racket/math
    exact-floor))

;; =============================================================================
;; Parameters, constants

(define TICK-SIZE 4)
(define TITLE-FACE "Liberation Serif")

(define-syntax-rule (defparam id val type)
  (begin (define id (make-parameter val))
         (provide id)))

(defparam *LEGEND-VSPACE* 10 Pict-Units)
(defparam *LEGEND-HSPACE* 20 Pict-Units)
(defparam *OVERHEAD-PLOT-WIDTH* 600 Pict-Units)
(defparam *OVERHEAD-PLOT-HEIGHT* 300 Pict-Units)
(defparam *OVERHEAD-FONT-FACE* "bold" Font-Face)
(defparam *OVERHEAD-FONT-SCALE* 0.03 Nonnegative-Real)
(defparam *OVERHEAD-LABEL?* #f boolean?)
(defparam *OVERHEAD-LINE-COLOR* 3 plot-color/c)
(defparam *OVERHEAD-LINE-STYLE* 'solid plot-pen-style/c)
(defparam *OVERHEAD-LINE-WIDTH* 2 Nonnegative-Real)
(defparam *OVERHEAD-MAX* 10 Natural)
(defparam *OVERHEAD-SAMPLES* 20 Natural)
(defparam *FONT-SIZE* 10 Natural)

;; -----------------------------------------------------------------------------

(define overhead-plot
  (case-lambda
   [(pi) (performance-info->overhead-plot pi)]
   [(si) (sample-info->overhead-plot si)]))

(define (performance-info->overhead-plot pi)
  (define body
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)])
      (plot-pict
        (list
          (tick-grid)
          (make-count-configurations-function pi))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead (vs. retic-untyped)")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*))))
  (overhead-add-legend pi body))

;; TODO add caching
(define (make-count-configurations-function pi)
  (define nc (num-configurations pi))
  (function
    (λ (i)
      (define good? (make-D-deliverable? i pi))
      (pct (count-configurations pi good?) nc))
    0 (*OVERHEAD-MAX*)
    #:color (*OVERHEAD-LINE-COLOR*)
    #:samples (*OVERHEAD-SAMPLES*)
    #:style (*OVERHEAD-LINE-STYLE*)
    #:width (*OVERHEAD-LINE-WIDTH*)))

(define (make-overhead-x-ticks)
  (define MAJOR-TICKS (list 1 2 (*OVERHEAD-MAX*)))
  (define MINOR-TICKS
    (append (for/list ([i (in-range 12 20 2)]) (/ i 10))
            (for/list ([i (in-range 4 20 2)]) i)))
  (define m-ticks
    (ticks (λ (ax-min ax-max)
             (for/list ([i (in-list MAJOR-TICKS)])
               (pre-tick i #t)))
           (ticks-format/units "x")))
  (ticks-add m-ticks MINOR-TICKS #f))

(define (make-overhead-y-ticks)
  (define NUM-TICKS 3)
  (define UNITS "%")
  (ticks (λ (ax-min ax-max)
           (for/list ([y (in-list (linear-seq ax-min ax-max NUM-TICKS #:end? #t))])
             (pre-tick (exact-floor y) #t)))
         (ticks-format/units UNITS)))

(define ((ticks-format/units units) ax-min ax-max pre-ticks)
  (for/list ([pt (in-list pre-ticks)])
    (define v (pre-tick-value pt))
    (if (= v ax-max)
      (format "~a~a" (number->string v) units)
      (number->string v))))

(define (overhead-add-legend pi pict)
  (define name (render-benchmark-name (performance-info->name pi)))
  (define tp-ratio (render-typed/python-ratio (typed/python-ratio pi)))
  (define nc (render-num-configurations (num-configurations pi)))
  (rt-superimpose
    (vl-append (*LEGEND-VSPACE*)
      (hb-append (*LEGEND-HSPACE*)
        name tp-ratio)
      pict)
    nc))

(define (title-text str [angle 0])
  (text str (cons 'bold TITLE-FACE) (*FONT-SIZE*) angle))

(define (render-benchmark-name sym)
  (title-text (symbol->string sym)))

(define (render-typed/python-ratio r)
  (parameterize ([*FONT-SIZE* (sub1 (*FONT-SIZE*))])
    (title-text (format "typed/python ratio: ~ax" (rnd r)))))

(define (render-num-configurations n)
  (title-text (format "~a configurations" (add-commas n))))

(define (sample-info->overhead-plot si)
  (raise-user-error 'sample-info->overhead "not implemented"))

(define (performance-vs-num-types-plot pi)
  (raise-user-error 'performance-vs-num-types "not implemented"))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "rp-plot"
   #:args benchmark-name*
   (cond
    [(null? benchmark-name*)
     (printf "usage: raco rp-plot <benchmark-name> ...~n")]
    [else
     (define pi*
       (if (null? (cdr benchmark-name*))
         (list (benchmark->performance-info (->benchmark-info (car benchmark-name*))))
         (for/list ([n (in-list benchmark-name*)])
           (with-handlers ([exn:fail? (λ (e) (printf "Error processing '~a', run 'raco rp-plot ~a' to debug.~n" n n))])
             (benchmark->performance-info (->benchmark-info n))))))
     (define p*
       (parameterize ([*FONT-SIZE* 14])
         (map overhead-plot pi*)))
     (save-pict "rp-plot.png" (apply vl-append 50 p*))])))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  ;; TODO what to test?
  ;; - actually works, makes plots for various inputs
  ;; - same plots for "same" inputs
  ;; - count-configs fucntion? but it's a plot/function
  ;; - make ticks? also weird output format
  ;; - add legend? ditto idk what besides comparing argb pixels

  (test-case "?"
  )

)