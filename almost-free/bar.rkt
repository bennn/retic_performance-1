#lang racket/base

;; Make bar plots
;; - x axis = configs with 1 type annotation
;; - y axis = overhead
;;
;; So a benchmark with 4 "units" leads to a graph with 4 bars,
;;  each bar shows the overhead of one mixed-typed configuration.
;;
;; TODO move plot into gtp-plot

(require
  (only-in math/statistics mean)
  gtp-plot/reticulated-info
  gtp-plot/performance-info
  gtp-plot/configuration-info
  gtp-plot/plot
  gtp-util
  file/glob
  racket/runtime-path
  racket/format
  racket/file
  racket/path
  (only-in pict text)
  plot/no-gui)

(define-runtime-path ROOT "../")
(define-runtime-path HERE "./")
(define OLD-DATA (build-path ROOT "data" "karst"))
(define NEW-DATA (build-path HERE "data"))

(define (move-file src dst)
  (copy-file src dst)
  (delete-file src)
  (void))

(define (bm->data pre-str)
  (define str (~a pre-str))
  (define new-data (build-path NEW-DATA str))
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

(define (bm->sample pre-str)
  (define str (~a pre-str))
  (define new-data (build-path NEW-DATA str))
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

;; ---

(define exhaustive-bm*
  '(futen http2 slowSHA call_method call_simple chaos fannkuch float go meteor
    nbody nqueens pidigits pystone spectralnorm Espionage PythonFlow
    take5))

(define sample-bm*
  '(sample_fsm aespython stats))

(define e* (map bm->data exhaustive-bm*))
(define s* (map bm->sample sample-bm*))
(define b
  (parameterize ([*GRID-NUM-COLUMNS* 2]
                 [*GRID-Y* #f])
    (grid-plot (lambda (x)
                 (with-handlers ((exn:fail? (lambda (e) (text (~a (performance-info->name x))))))
                   (make-grace-bars x))) (append e* s*))))
(save-pict "ebars.png" b)

;; ---

(module+ test
  (require rackunit)

  (test-case "init"
    (delete-directory/files "data/Espionage" #:must-exist? #false)
    (check-pred reticulated-info? (bm->data 'Espionage))
    (delete-directory/files "data/Espionage")
    (check-pred reticulated-info? (bm->sample 'Espionage))
    (delete-directory/files "data/Espionage"))

)
