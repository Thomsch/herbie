#lang racket

(require math/flonum plot/no-gui)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../alternative.rkt" "../interface.rkt" "../syntax/read.rkt" "../core/regimes.rkt" 
         "../sandbox.rkt")

(provide make-axis-plot make-points-plot)

(struct color-theme (scatter line fit))
(define *red-theme* (color-theme "pink" "red" "darkred"))
(define *blue-theme* (color-theme "lightblue" "blue" "navy"))
(define *green-theme* (color-theme "lightgreen" "green" "darkgreen"))

(define (double-transform)
  (define repr (get-representation (if (flag-set? 'precision 'double) 'binary64 'binary32)))
  (invertible-function
   (compose (representation-repr->ordinal repr) (curryr ->flonum repr))
   (compose (representation-ordinal->repr repr) round)))

(define (double-axis)
  (make-axis-transform (double-transform)))

(define (power10-upto x)
  (define ->repr (if (flag-set? 'precision 'double) real->double-flonum real->single-flonum))
  (if (= x 0)
      '()
      (reverse
       (let loop ([power (round (/ (log x) (log 10)))])
         (define value (->repr (expt 10.0 power)))
         (if (= value 0) '() (cons value (loop (- power 1))))))))

(define (possible-ticks min max)
  ;; Either
  ;; + 0 is between min and max
  ;; + 0 is one of min and max (two cases)
  ;; + min and max are on the same side of 0 (two cases)
  (sort 
   (cond
    [(< (* min max) 0) (append (map - (power10-upto (- min))) '(0.0) (power10-upto max))]
    [(= min 0) (cons 0 (power10-upto max))]
    [(= max 0) (append (map - (power10-upto (abs min))) '(0.0))]
    [(> min 0) (set-subtract (power10-upto max) (power10-upto min))]
    [(< max 0) (map - (set-subtract (power10-upto (abs min)) (power10-upto (abs max))))])
   <))

(define (pick-spaced-indices necessary possible number)
  "Choose `number` entries from among `possible`, with every index in `necessary` chosen and even spacing between choices."

  ;; `possible` and `number` are numbers; `necessary` is a list of numbers less than `possible`
  ;; The approach here is a dynamic programming algorithm.
  ;; The algorithm minimizes the sum squared of gaps between chosen ticks.
  ;; State is a vector of ticks chosen left of point + total weight left of point.
  ;; At the Nth iter, the Ith entry in array has N ticks including I (except first few)
  (define initial
    (for/vector ([i (in-range possible)])
      (define chosen (sort (filter (curryr <= i) necessary) <))
      (define weight (apply + (for/list ([left (cons 0 chosen)] [right chosen]) (sqr (- right left)))))
      (cons (reverse chosen) weight)))

  (define final
    (for/fold ([initial initial]) ([iter (in-range number)])
      (define upper
        (if (< iter (length necessary))
            (last (take (sort necessary <) (+ 1 iter)))
            possible))
      (for/vector ([i (in-range possible)])
        (if (< i upper)
            (let*-values ([(stoppers) (filter (curryr < i) necessary)]
                          [(stopper) (if (null? stoppers) 0 (apply max stoppers))]
                          [(j* score*)
                           (for/fold ([j* #f] [score* #f]) ([j (in-range stopper i)])
                             (match-define (cons *j *score) (vector-ref initial j))
                             (define score (+ *score (if (null? *j) (sqr i) (sqr (- i (car *j))))))
                             (if (or (not j*) (< score score*)) (values (cons i *j) score) (values j* score*)))])
              (if j*
                  (cons j* score*)
                  (cons (list i) 1)))
            (vector-ref initial i)))))

  (define stopper (if (null? necessary) 0 (apply max necessary)))
  (car (argmin (λ (x) (+ (cdr x) (sqr (- (- possible 1) (caar x))))) (drop (vector->list final) stopper))))

(define (choose-ticks min max)
  (define possible (possible-ticks min max))

  (cond
   [(< (length possible) 12)
    ;; If there aren't enough possible big ticks, we fall back to the standard method
    (append
     (if (<= min 1.0 max) (list (pre-tick 1.0 #t)) '())
     (if (<= min 0.0 max) (list (pre-tick 0.0 #t)) '())
     (if (<= min -1.0 max) (list (pre-tick -1.0 #t)) '())
     ((ticks-layout (ticks-scale (linear-ticks #:number 6 #:base 10 #:divisors '(1 2 5)) (double-transform))) min max))]
   [else
    (define necessary (filter identity (map (curry index-of possible) '(1.0 0.0 -1.0))))
    (define major-indices (pick-spaced-indices necessary (length possible) 12))
    (for/list ([idx major-indices])
      (pre-tick (list-ref possible idx) #t))]))

(define (double-ticks)
  (ticks
   choose-ticks
   (λ (lft rgt pticks)
     (for/list ([ptick pticks])
       (define val (pre-tick-value ptick))
       (if (or (= val 0) (< 0.01 (abs val) 100))
           (~r (exact->inexact val) #:precision 4)
           (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e"))))))

(define (error-points errs pts #:axis [axis 0] #:color [color *blue-theme*] #:alpha [alpha 0.02])
  (define x
    (if (number? axis)
        (λ x (list-ref x axis))
        (eval-prog axis 'fl)))
  (points
    (for/list ([pt pts] [err errs])
      (vector (apply x pt) (ulps->bits err)))
    #:sym 'fullcircle #:color (color-theme-line color) #:alpha alpha #:size 4))

(define (best-alt-points point-alt-idxs var-idxs)
  (define point-idxs (remove-duplicates (map cadr point-alt-idxs)))
  (define points-list (for/list ([i point-idxs])
    (filter (λ (x) (= (cadr x) i)) point-alt-idxs)))
  (define non-empty-points-list (for/list ([point-list points-list])
                                  point-list))
  (for/list ([point-list non-empty-points-list] [color (range 2 121)])
    (points (map (λ (p) (list (list-ref (car p) (car var-idxs))
                              (list-ref (car p) (cadr var-idxs))))
                 point-list) #:color color #:sym 'fullcircle #:size 5)))

(define (herbie-ratio-point-colors test-points baseline-errors herbie-errors oracle-errors)
  (define points-with-colors (for/list ([point test-points] [base-err baseline-errors]
                                        [herbie-err herbie-errors]
                                        [oracle-err oracle-errors])
    (define span (- base-err oracle-err))
    (define herbie-percent (if (= span 0) 1 (/ (- base-err herbie-err) span)))
    (define color-num (max (round (* 240 herbie-percent)) 0))
    (list point color-num)))
  (define colors (remove-duplicates (map cadr points-with-colors)))
  (for/list ([c colors])
    (filter (λ (p) (eq? (cadr p) c)) points-with-colors)))

(define (herbie-ratio-point-renderers points-colors var-idxs)
  (for/list ([l points-colors])
    (define color-num (cadar l))
    (define point-color (list color-num color-num color-num))
    (define color-points (map (λ (l) (list (list-ref (car l) (car var-idxs))
                                           (list-ref (car l) (cadr var-idxs)))) l))
    (points color-points #:color point-color #:sym 'fullcircle #:size 5)))

(define (error-axes pts #:axis [axis 0])
  (list
   (y-tick-lines)
   (error-points (map (const 1) pts) pts #:axis axis #:alpha 0)))

(define (with-herbie-plot #:title [title #f] thunk)
  (parameterize ([plot-width 800] [plot-height 300]
                 [plot-background-alpha 0]
                 [plot-x-transform (double-axis)]
                 [plot-x-ticks (double-ticks)]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label #f]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-axis? #t]
                 [plot-y-axis? #t]
                 [plot-font-size 10]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-label title])
    (thunk)))

(define (herbie-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max (*bit-width*)))
        (lambda () (plot-pict (cons (y-axis) renderers) #:y-min 0 #:y-max (*bit-width*)))))
  (with-herbie-plot #:title title thunk))

(define (with-alt-plot #:title [title #f] thunk)
  (parameterize ([plot-width 800] [plot-height 800]
                 [plot-background-alpha 1]
                 [plot-x-transform (double-axis)]
                 [plot-x-ticks (double-ticks)]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label #f]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-transform (double-axis)]
                 [plot-y-ticks (double-ticks)]
                 [plot-y-tick-label-anchor 'left]
                 [plot-y-label #f]
                 [plot-y-far-axis? #t]
                 [plot-y-far-ticks no-ticks]
                 [plot-font-size 10]
                 [plot-y-label title])
    (thunk)))

(define (alt-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (lambda () (plot-file renderers port kind)))
  (with-alt-plot #:title title thunk))

(define (errors-by x errs pts)
  (sort (map (λ (pt err) (cons (apply x pt) err)) pts errs) < #:key car))

(define (vector-binary-search v x cmp)
  (define (search l r)
    (define mid (ceiling (/ (+ l r) 2)))
    (define c (cmp x (vector-ref v mid)))
    (cond
     [(= r l) l]
     [(= r (+ l 1))
      (if (= c 0) r l)]
     [(< c 0)
      (search l mid)]
     [(> c 0)
      (search mid r)]
     [(= c 0)
      mid]))
  (search 0 (vector-length v)))

(define (histogram-function errors-by-out #:bin-size [bin-size 32])
  (define xs (for/vector ([(x err) (in-dict errors-by-out)]) x))
  (define errs (for/vector ([(x err) (in-dict errors-by-out)]) err))

  (λ (x)
    (define idx (vector-binary-search xs x -))
    (list->vector
     (map ulps->bits
          (sort
           (cond
            [(<= (vector-length errs) bin-size) (vector->list errs)]
            [(< idx (/ bin-size 2))
             (for/list ([i (in-range 0 bin-size)]) (vector-ref errs i))]
            [(> idx (- (vector-length errs) (/ bin-size 2)))
             (for/list ([i (in-range (- (vector-length errs) bin-size) (vector-length errs))])
               (vector-ref errs i))]
            [else
             (define idx-min (round (- idx (/ bin-size 2))))
             (for/list ([i (in-range idx-min (+ idx-min bin-size))]) (vector-ref errs i))])
           <)))))

(define (error-avg errs pts #:axis [axis 0] #:vars [vars '()]
                   #:color [color *blue-theme*] #:bin-size [bin-size 128])
  (define get-coord
    (if (number? axis)
        (λ x (list-ref x axis))
        (eval-prog `(λ ,vars ,axis) 'fl)))
  (define eby (errors-by get-coord errs pts))
  (define histogram-f (histogram-function eby #:bin-size bin-size))
  (define (avg-fun x)
    (define h (histogram-f x))
    (/ (apply + (vector->list h)) (vector-length h)))
  ;; TODO: This is a weird hack in several ways, and ideally wouldn't exist
  ;; TODO: This doesn't work in single-precision
  (define-values (min max)
    (match* ((car (first eby)) (car (last eby)))
            [(x x) (values #f #f)]
            [(x y) (values (flmax (flnext -inf.0) x) (flmin (flprev +inf.0) y))]))
  (function avg-fun min max
            #:width 2 #:color (color-theme-fit color)))

(define (error-mark x-val)
  (inverse (const x-val) #:color "gray" #:width 3))

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define (regime-splitpoints altn)
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))

(define (points->doubles pts repr)
  (cond
    [(or (real? (caar pts)) (complex? (caar pts))) pts]
    [else
     (map (curry map (curryr repr->fl repr)) pts)]))

(define (make-axis-plot result out idx)
  (define var (list-ref (test-vars (test-result-test result)) idx))
  (define split-var? (equal? var (regime-var (test-success-end-alt result))))
  (define repr (get-representation (test-output-prec (test-result-test result))))
  (define pts (points->doubles (test-success-newpoints result) repr))
  (herbie-plot
   #:port out #:kind 'png
   (error-axes pts #:axis idx)
   (map error-mark (if split-var? (regime-splitpoints (test-success-end-alt result)) '()))))

(define (make-points-plot result out idx letter)
  (define-values (theme accessor)
    (match letter
      ['r (values *red-theme*   test-success-start-error)]
      ['g (values *green-theme* test-success-target-error)]
      ['b (values *blue-theme*  test-success-end-error)]))

  (define repr (get-representation (test-output-prec (test-result-test result))))
  (define pts (points->doubles (test-success-newpoints result) repr))
  (define err (accessor result))

  (herbie-plot
   #:port out #:kind 'png
   (error-points err pts #:axis idx #:color theme)
   (error-avg err pts #:axis idx #:color theme)))

(define (make-alt-plots point-alt-idxs alt-idxs title out)
  (define best-alt-point-renderers (best-alt-points point-alt-idxs alt-idxs))
  (alt-plot best-alt-point-renderers #:port out #:kind 'png #:title title))

(define (make-point-alt-idxs result)
  (define repr (get-representation (test-output-prec (test-result-test result))))
  (define all-alts (test-success-all-alts result))
  (define all-alt-bodies (map (λ (alt) (eval-prog (alt-program alt) 'fl repr)) all-alts))
  (define newpoints (test-success-newpoints result))
  (define newexacts (test-success-newexacts result))
  (oracle-error-idx all-alt-bodies newpoints newexacts repr))

(define (make-contour-plot point-colors var-idxs title out)
  (define point-renderers (herbie-ratio-point-renderers point-colors var-idxs))
  (alt-plot point-renderers #:port out #:kind 'png #:title title))

#;
(define (make-plots result rdir profile? debug?)
  (define (open-file #:type [type #f] idx fun . args)
    (call-with-output-file (build-path rdir (format "plot-~a~a.png" idx (or type ""))) #:exists 'replace
      (apply curry fun args)))

  (define vars (program-variables (alt-program (test-success-start-alt result))))
  (when (and debug? (>= (length vars) 2))
    (define point-alt-idxs (make-point-alt-idxs result))
    (define newpoints (test-success-newpoints result))
    (define baseline-errs (test-success-baseline-error result))
    (define herbie-errs (test-success-end-error result))
    (define oracle-errs (test-success-oracle-error result))
    (define point-colors (herbie-ratio-point-colors newpoints baseline-errs herbie-errs oracle-errs))
    (for* ([i (range (- (length vars) 1))] [j (range 1 (length vars))])
      (define alt-idxs (list i j))
      (define title (format "~a vs ~a" (list-ref vars j) (list-ref vars i)))
      (open-file (- (+ j (* i (- (length vars)))) 1) #:type 'best-alts
                 make-alt-plots point-alt-idxs alt-idxs title)
      (open-file (- (+ j (* i (- (length vars)))) 1) #:type 'contours
                 make-contour-plot point-colors alt-idxs title)))

  (for ([var (test-vars (test-result-test result))] [idx (in-naturals)])
    (when (> (length (remove-duplicates (map (curryr list-ref idx) (test-success-newpoints result)))) 1)
      ;; This is bad code
      (open-file idx make-axis-plot result idx)
      (open-file idx #:type 'r make-points-plot result idx 'r)
      (when (test-success-target-error result)
        (open-file idx #:type 'g make-points-plot result idx 'g))
      (open-file idx #:type 'b make-points-plot result idx 'b))))
