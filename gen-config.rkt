#lang racket

(define config-file "branches.config")
(define always "--profile")
(define branches '(main using-ruler-nightlies using-ruler-baseline))
(define flags '(("" "-o generate:taylor") ("" "-o rules:numerics")))

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_-]" (string char))
         (string char)
         "_"))
   ""))

(define (gen-config)
  (call-with-output-file config-file
    #:exists 'replace
    (λ (o)
      (for ([config (in-list (apply cartesian-product branches flags))])
        (match-define (cons branch options) config)
        (define options* (string-join (filter-not (λ (s) (= (string-length s) 0)) (cons always options)) " "))
        (define fname (fix-name (format "~a-~a" branch options*)))
        (fprintf o "~a ~a ~a\n" branch fname options*)))))

(module+ main
  (command-line
    #:args ()
    (gen-config)))

