#lang racket

;; Arithmetic identities for rewriting programs.

(require "../common.rkt" "types.rkt" "syntax.rkt")

(provide *rules* *simplify-rules* *fp-safe-simplify-rules* (struct-out rule))

(module+ internals
  (provide define-ruleset define-ruleset* register-ruleset! *rulesets*))

;; Rulesets
(define *rulesets* (make-parameter '()))

;; Rules
(define *rules* (make-parameter '()))
(define *simplify-rules* (make-parameter '()))
(define *fp-safe-simplify-rules* (make-parameter '()))

;; Note on rules
;; fp-safe-simplify ⊂ simplify ⊂ all
;;
;; all                    requires at least one tag of an active group of rules
;; simplify               same req. as all + 'simplify' tag
;; fp-safe-simplify       same req. as simplify + 'fp-safe' tag ('fp-safe' does not imply 'simplify')
;;

(define (update-rules rules groups)
  (when (ormap (curry flag-set? 'rules) groups)             ; update all
    (*rules* (append (*rules*) rules))
    (when (set-member? groups 'simplify)                    ; update simplify
      (*simplify-rules* (append (*simplify-rules*) rules))
      (when (set-member? groups 'fp-safe)                   ; update fp-safe
        (*fp-safe-simplify-rules*
          (append (*fp-safe-simplify-rules*) rules))))))

;; Rule struct

(struct rule (name input output itypes otype)
        ;; Input and output are patterns
        ;; itypes is a mapping, variable name -> representation
        ;; otype is a representation
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (fprintf port "#<rule ~a>" (rule-name rule)))])

(define (rule-ops-supported? rule)
  (define (ops-in-expr expr)
    (cond
      [(list? expr)
       (and (impl-exists? (car expr))
            (for/and ([subexpr (cdr expr)])
              (ops-in-expr subexpr)))]
      [else true]))
  (ops-in-expr (rule-output rule)))

(register-reset
 #:priority 10 ; Must be higher than priority for pruning operators
 (λ ()
   (*rulesets*
    (for/list ([ruleset (*rulesets*)])
      (match-define (list rules groups types) ruleset)
      (list (filter rule-ops-supported? rules) groups types)))))

;;
;;  Rule loading
;;

(define-values (type-of-rule repr-of-rule)
  (let () ; `let` not `begin` since these are expanded in different phases
    (define ((type/repr-of-rule get-info name) input output ctx)
      (let loop ([input input] [output output])
        (cond [(list? input)    (if (equal? (car input) 'if)
                                    ; special case for 'if'
                                    ; return the 'type/repr-of-rule' of the ift branch
                                    (loop (caddr input) output)
                                    (get-info (car input) 'otype))]
              [(list? output)   (if (equal? (car output) 'if)
                                    ; special case for 'if'
                                    ; return the 'type/repr-of-rule' of the ift branch
                                    (loop input (caddr output))
                                    (get-info (car output) 'otype))]
              ;; fallback: if symbol, check the ctx for the type
              [(symbol? input)  (dict-ref ctx input)]
              [(symbol? output) (dict-ref ctx output)]
              [else             (error name "could not compute type of rule ~a -> ~a"
                                            input output)])))
    (values (type/repr-of-rule real-operator-info 'type-of-rule)
            (type/repr-of-rule operator-info 'repr-of-rule))))

;; Rulesets defined by reprs. These rulesets are unique
(define (register-ruleset! name groups var-ctx rules)
  (define rules*
    (for/list ([r rules])
      (match-define (list rname input output) r)
      (rule rname input output var-ctx
            (repr-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*)))
  (update-rules rules* groups))
      
(define-syntax define-ruleset
  (syntax-rules ()
   [(define-ruleset name groups [rname input output] ...)
    (define-ruleset name groups #:type () [rname input output] ...)]
   [(define-ruleset name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset! 'name 'groups `((var . ,(get-representation 'type)) ...)
                       '((rname input output) ...))]))

(define (register-ruleset*! name groups var-ctx rules)
  (define rules*
    (for/list ([ru (in-list rules)])
      (match-define (list rname input output) ru)
      (rule rname input output var-ctx
            (type-of-rule input output var-ctx))))
  (*rulesets* (cons (list rules* groups var-ctx) (*rulesets*)))
  (update-rules rules* groups))
  
(define-syntax define-ruleset*
  (syntax-rules ()
   [(define-ruleset* name groups [rname input output] ...)
    (define-ruleset* name groups #:type () [rname input output] ...)]
   [(define-ruleset* name groups #:type ([var type] ...) [rname input output] ...)
    (register-ruleset*! 'name 'groups `((var . type) ...)
                        '((rname input output) ...))]))

;;;
;;; Ruler nightlies interface
;;;

;
; Web garbage
;
(module web racket/base
  (require html net/url xml xml/path json)
  (provide get-html get-html-element get-json)

  (define (read-html-as-xexpr in)
     (define xml-el (element #f #f 'root '() (read-html-as-xml in)))
     (caddr (xml->xexpr xml-el)))

  ; Returns the contents of a JSON file hosted at `url-str`.
  (define (get-json url-str)
    (define url (string->url url-str))
    (call/input-url url get-pure-port read-json))

  ; Returns an html file hosted at `url-str` as an X-expr.
  (define (get-html url-str)
    (define url (string->url url-str))
    (call/input-url url get-pure-port read-html-as-xexpr))

  ; Takes an X-expr `xexpr` and returns all values specified by path `key`.
  (define (get-html-element key xexpr)
    (se-path*/list key xexpr))
)

(struct ruler-manifest (filename groups type op-table))

(define bool-op-table '(("&" . "and") ("|" . "or") ("^" . "!=")  ("~" . "not")))
(define rational-op-table '(("~" . "neg")))

;
; Configuration
;

(define nightly-root "http://nightly.cs.washington.edu/reports/ruler/")
(define nightly-branch "main")
(define json-path "json")

(define rules-info
  (list (ruler-manifest "bool.json" '(bools) 'bool bool-op-table)
        (ruler-manifest "rational.json" '(arithmetic) 'real rational-op-table)))

;
; Rule parsing
;

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))
   
(define (parse-ruler-rule lhs rhs table)
  (define (ruler-string-expr->expr str)
    (call-with-input-string (string-replace* str table) read))

  (define (parse-expr expr)
    (define vars (mutable-set))
    (values
      (let loop ([expr (ruler-string-expr->expr expr)])
        (match expr
          [(list '~ arg)
           (list 'neg (loop arg))]
          [(list 'sqr arg)
           (define arg* (loop arg))
           (list '* arg* arg*)]
          [(list op args ...)
           (cons op (map loop args))]
          [(? number?) expr]
          [(? symbol?)
           (define str-repr (symbol->string expr))
           (cond
             [(eq? (string-ref str-repr 0) #\?)
              (set-add! vars (substring str-repr 1))
              (string->symbol (substring str-repr 1))]
             [else
              (list expr)])]))
      (for/set ([s (in-mutable-set vars)]) s)))

  (define (expr-has-op? expr)
    (match expr
      [(list op head rest ...) #t]
      [_ #f]))

  (define-values (lhs* lhs-vars) (parse-expr lhs))
  (define-values (rhs* rhs-vars) (parse-expr rhs))
  (define rule (list lhs* rhs* (and (expr-has-op? lhs*) 'simplify)))
  (values rule (set-union lhs-vars rhs-vars)))

;
; Rule scraper
;

(require (submod "." web))

(define (load-rules-from-ruler!)
  ; Returns the path of the newest Ruler report
  (define (get-newest-report)
    (printf "Scraping reports at `~a` ...\n" nightly-root)

    (define nightly-html (get-html nightly-root))
    (define report-paths (filter (λ (p) (not (string=? p "../")))
                                (get-html-element '(a #:href) nightly-html)))
    (printf " Found ~a reports\n" (length report-paths))

    (define reports-on-branch
      (reap [sow]
        (for ([path (in-list report-paths)])
          (match (string-split path "%3A")
            [(list time host branch commit)
            (when (string=? nightly-branch branch)
              (sow (list path time commit)))]
            [_
            (printf "Invalid report: ~a" path)]))))
    (printf " ~a reports associated with branch `~a`\n" (length reports-on-branch) nightly-branch)

    (define (report>? r1 r2)
      (match-define (list p1 time1 commit1) r1)
      (match-define (list p2 time2 commit2) r2)
      (> (string->number time1) (string->number time2)))
    
    (when (empty? reports-on-branch)
      (error 'load-rules-from-ruler "No reports matching branch: ~a\n" nightly-branch))
    (define newest (first (sort reports-on-branch report>?)))
    (match-define (list report-path report-time commit) newest)
    (printf " Newest report has timestamp: ~a\n" report-time)
    (printf " Newest report has commit: ~a\n" (substring commit 0 (- (string-length commit) 1)))

    report-path)

  ; Registers a rule set
  (define (register-ruler-ruleset! name groups var-ctx rules)
    (printf "  Registering ruleset `~a` ...\n" name)
    (printf "   Groups: ~a\n" groups)
    (printf "   Vars:   ~a\n" var-ctx)
    (printf "   Rules:  ~a\n" (length rules))
    (register-ruleset*!
      name groups var-ctx
      (for/list ([rule (in-list rules)] [i (in-naturals 1)])
        (match-define (list lhs rhs _) rule)
        (define rule-name (string->symbol (format "~a-~a" name i)))
        (printf "    ~a: ~a => ~a\n" rule-name lhs rhs)
        (list rule-name lhs rhs))))

  ; Loads a rules file (JSON) and adds the rules to Herbie's database
  (define (load-ruler-file! info url)
    (printf " Loading rules at `~a` ...\n" url)
    (match-define (ruler-manifest name groups type op-table) info)
    (define json (get-json url))

    (printf "  Parsing rules ...\n")
    (define vars (mutable-set))
    (define rules
      (for/list ([rule (in-list (hash-ref json 'rules))] [counter (in-naturals 1)])
        (match-define (list lhs rhs) (string-split rule " ==> "))
        (define-values (rule* rule-vars) (parse-ruler-rule lhs rhs op-table))
        (set-union! vars rule-vars)
        rule*))

    (define-values (simplify non-simplify)
      (partition (λ (r) (eq? (third r) 'simplify)) rules))

    (define var-ctx (for/list ([v (in-set vars)]) (cons (string->symbol v) type)))
    (register-ruler-ruleset! name groups var-ctx non-simplify)
    (register-ruler-ruleset! (format "~a-simplify" name) (cons 'simplify groups) var-ctx simplify)

    (printf "  Done\n")
    (void))

  (define report-path (get-newest-report)) 
  (define json-dir (build-path nightly-root report-path json-path))
  (printf "Looking for rules at `~a` ...\n" json-dir)
  (for ([info (in-list rules-info)])
    (define json-path (build-path json-dir (ruler-manifest-filename info)))
    (load-ruler-file! info (path->string json-path)))

  (void))

; Invoked when this module is instantiated
(load-rules-from-ruler!)
