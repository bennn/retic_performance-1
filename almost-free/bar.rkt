#lang racket/base

;; Make bar plots
;; - x axis = configs with 1 type annotation
;; - y axis = overhead
;;
;; So a benchmark with 4 "units" leads to a graph with 4 bars,
;;  each bar shows the overhead of one mixed-typed configuration.

;; TODO
;; - [X] maybe only want the max bar? either way, easiest to have 1 color
;; - [X] read paper, fully
;; - [X] make all 1-bar figures
;; - [X] read all 1-bar figures

(require
  (only-in math/statistics mean median)
  gtp-plot/reticulated-info
  gtp-plot/typed-racket-info
  gtp-plot/performance-info
  gtp-plot/configuration-info
  gtp-plot/plot
  (except-in gtp-util save-pict)
  file/glob
  racket/runtime-path
  racket/format
  racket/file
  racket/path
  pict
  pict-abbrevs
  (only-in pict text)
  plot/no-gui)

(define-runtime-path ROOT "../")
(define-runtime-path HERE "./")
(define OLD-DATA (build-path ROOT "data" "karst"))
(define PEPM-DATA (build-path HERE "data" "gm-pepm-2018"))
(define ICFP-DATA (build-path HERE "data" "gf-icfp-2018"))

(define (move-file src dst)
  (copy-file src dst)
  (delete-file src)
  (void))

(define (pepm-bm->data pre-str)
  (define str (~a pre-str))
  (define new-data (build-path PEPM-DATA str))
  (unless (directory-exists? new-data)
    (printf "WARNING: copy data for ~a~n" str)
    (make-directory new-data)
    (copy-file*
      OLD-DATA
      new-data
      (string-append str "*tab"))
    (move-file (build-path new-data (string-append str "_python.tab"))
               (build-path new-data (string-append str "-python.tab"))))
  (make-reticulated-info new-data))

(define (pepm-bm->sample pre-str)
  (define str (~a pre-str))
  (define new-data (build-path PEPM-DATA str))
  (unless (directory-exists? new-data)
    ;;(printf "WARNING: copy data for ~a~n" str)
    (make-directory new-data)
    (copy-file*
      OLD-DATA
      new-data
      (string-append str "*tab"))
    (move-file (build-path new-data (string-append str "_python.tab"))
               (build-path new-data (string-append str "-python.tab")))
    (move-file (build-path new-data (string-append str "_retic-typed.tab"))
               (build-path new-data (string-append str "-retic-typed.tab")))
    (move-file (build-path new-data (string-append str "_retic-untyped.tab"))
               (build-path new-data (string-append str "-retic-untyped.tab")))
    (with-output-to-file (build-path new-data (string-append str "-meta.rktd"))
      (lambda ()
        (writeln
          (hasheq 'num-units
                  (case pre-str
                    ((sample_fsm) 19)
                    ((aespython) 34)
                    ((stats) 79)
                    ((Espionage) 12)
                    (else (raise-argument-error 'sample-data "unknown sample benchmark" pre-str)))))))
    (let ((sdir (build-path OLD-DATA "sample" str)))
      (when (directory-exists? sdir)
        (for ((sample (in-glob (build-path sdir "sample*"))))
          (define num (car (regexp-match #rx"[0-9]+" (file-name-from-path sample))))
          (define dst (build-path new-data (format "sample-~a.tab" num)))
          (copy-file sample dst)))))
  (make-reticulated-info new-data))

(define (glob1 p)
  (define m* (glob p))
  (cond
    [(or (null? m*) (not (null? (cdr m*))))
     (raise-arguments-error 'glob1 "expected one match, got 0 or 2+" "pattern" p "results" m*)]
    [else
     (car m*)]))

(define (tr-bm->data src)
  (make-typed-racket-info (glob1 (build-path ICFP-DATA (format "~a-*.rktd" src)))))

;; ---

(define exhaustive-bm*
  '(futen http2 call_method call_simple chaos fannkuch float go meteor
    nbody nqueens pidigits pystone spectralnorm Espionage PythonFlow
    take5 slowSHA))

(define sample-bm*
  '(sample_fsm aespython stats))

(define tr*
  '(tag_fsm tag_jpeg tag_kcfa tag_morsecode tag_sieve tag_snake tag_suffixtree
    tag_synth tag_tetris tag_zombie))

(define (cfg-id<? id0 id1)
  (cond
    [(and (typed-racket-id? id0) (typed-racket-id? id1))
     (typed-racket-id<? id0 id1)]
    [(and (reticulated-id? id0) (reticulated-id? id1))
     (reticulated-id<? id0 id1)]
    [else
     (raise-arguments-error 'cfg-id<? "mismatched ids" "id0" id0 "id1" id1)]))

(define (has-1-type? cfg)
  (= 1 (configuration-info->num-types cfg)))

(define (grid pi*)
  (define b
    (parameterize ([*GRID-NUM-COLUMNS* 1]
                   [*GRID-Y* #f]
                   [*POINT-COLOR* 2]
                   [*POINT-ALPHA* 0.3])
      (grid-plot (lambda (x)
                   (define pi+ (filter-performance-info x has-1-type?))
                   (define bad-types
                     (let* ((cfg* (for/list ((c (in-configurations pi+))) c))
                            (ci (confidence-interval (map configuration-info->mean-runtime cfg*))))
                       (for/list ((c (in-list cfg*))
                                  #:when (let ((r (configuration-info->mean-runtime c)))
                                           (or (< r (car ci))
                                               (< (cdr ci) r))))
                         (configuration-info->id c))))
                   (define (cfg->style i cfg)
                     (define pc (*POINT-COLOR*))
                     (define id (configuration-info->id cfg))
                     (define c
                       (or
                         (for/or ((bad (in-list bad-types))
                                  (i (in-naturals (+ pc 1)))
                                  #:when (cfg-id<? bad id))
                           i)
                         pc))
                     (hash 'color c 'line-color c))
                   (with-handlers ((exn:fail:filesystem? (lambda (e) (displayln (exn-message e)) (text (~a (performance-info->name x))))))
                     (parameterize ([*CONFIGURATION->STYLE* cfg->style])
                       (hb-append
                         40
                         (exact-runtime-plot x)
                         (make-grace-bars pi+)))))
                 pi*)))
  (add-rectangle-background b #:x-margin 10 #:y-margin 10))

(define (grid2 make-pi*)
  (parameterize ((*POINT-COLOR* 2) (*POINT-ALPHA* 0.3))
    (for ((make-pi (in-list make-pi*)))
      (define pi (make-pi))
      (define name (performance-info->name pi))
      (printf "now ~s~n" name)
      (define pi+ (filter-performance-info pi has-1-type?))
      (define cfg1* (for/list ((c (in-configurations pi+))) c))
      (parameterize ((*OVERHEAD-FREEZE-BODY* (< 9 (performance-info->num-units pi))))
        (save-pict
          (format "nsa/~a.png" name)
          (add-rectangle-background #:x-margin 10 #:y-margin 10
            (apply
              vl-append
              20
              (for/list ((bad-cfg (in-list cfg1*)))
                (define bad-type (configuration-info->id bad-cfg))
                (define (cfg->style i cfg)
                  (define pc (*POINT-COLOR*))
                  (define id (configuration-info->id cfg))
                  (define c
                    (if (cfg-id<? bad-type id)
                      (+ pc 1)
                      pc))
                  (hash 'color c 'line-color c))
                (with-handlers ((exn:fail:filesystem? (lambda (e) (displayln (exn-message e)) (text (~a (performance-info->name pi))))))
                  (parameterize ([*CONFIGURATION->STYLE* cfg->style])
                    (hb-append
                      40
                      (exact-runtime-plot pi)
                      (make-grace-bars pi+))))))))))))

(define (make-pepm)
  (define e* (map pepm-bm->data exhaustive-bm*))
  (define s* '() #;(map pepm-bm->sample sample-bm*))
  (grid (append e* s*)))

(define (make-icfp)
  (define r* (map tr-bm->data tr*))
  (grid r*))

;; (save-pict "ebars-icfp.png" (make-icfp))
;; (save-pict "ebars-pepm.png" (make-pepm))

  (grid2
    (append (map (lambda (data) (lambda () (tr-bm->data data))) (list (car tr*)))
            #;(map (lambda (data) (lambda () (pepm-bm->data data))) exhaustive-bm*)))

;; ---

(module+ test
  (require rackunit)

  (test-case "init"
    (delete-directory/files "data/Espionage" #:must-exist? #false)
    (check-pred reticulated-info? (pepm-bm->data 'Espionage))
    (delete-directory/files "data/Espionage")
    (check-pred reticulated-info? (pepm-bm->sample 'Espionage))
    (delete-directory/files "data/Espionage"))

)
