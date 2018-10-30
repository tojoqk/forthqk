#lang typed/racket
(require (for-syntax racket))
(require racket/fixnum)
(require "stack.rkt")

(struct state ([stack : stack]
               [word-table : Word-Table])
  #:mutable)
(define-type Exprs (U (Listof Expr)))
(define-type Expr (U Atom Symbol If-Expr Define-Expr))
(define-type If-Expr (List 'if Exprs Exprs))
(define-type Define-Expr (Pair ': (Pair Symbol Exprs)))
(define-type Word-Table (Immutable-HashTable Symbol Word))
(define-type Word (-> state state))

(require/typed "reader.rkt"
  [read (->* () (Input-Port) Exprs)])

(: eval (->* (Exprs) (state) state))
(define (eval es [s (current-state)])
  (if (null? es)
      s
      (let ([e (car es)]
            [es (cdr es)])
        (cond
          [(atom? e)
           (stack-push! (state-stack s) e)
           (eval es s)]
          [(symbol? e)
           (eval-word e es s)]
          [(eq? 'if (car e))
           (eval-if e es s)]
          [(eq? ': (car e))
           (eval-: e es s)]
          [else
           (error 'error "?")]))))
(provide eval)

(: eval-word (-> Symbol Exprs state state))
(define (eval-word e es s)
  (eval es
        ((hash-ref (state-word-table s) e) s)))

(: eval-if (-> If-Expr Exprs state state))
(define (eval-if e es s)
  (if (fx= 0 (stack-pop! (state-stack s)))
      (eval (caddr e) s)
      (eval (cadr e) s)))

(: eval-: (-> Define-Expr Exprs state state))
(define (eval-: e es s)
  (set-state-word-table!
   s
   ((inst hash-set Symbol Word)
    (state-word-table s)
    (car (cdr e))
    (let ([body : Exprs (cdr (cdr e))])
      (lambda ([s : state]) (eval body s)))))
  (eval es s))

(define-syntax (define/forthqk x)
  (syntax-case x ()
    [(_ (name args ...) wt n body ...)
     (let ([n (syntax->datum #'n)])
       (with-syntax ([(g ...) (generate-temporaries #'(args ...))]
                     [(r ...) (generate-temporaries
                               (for/list ([i n]) i))])
         #`(set! wt
                 (hash-set
                  wt
                  'name
                  (lambda ([s : state])
                    (let* ([st : stack (state-stack s)]
                           [g : Atom (stack-pop! st)]
                           ...)
                      (let-values
                          ([([r : Atom] ...)
                            (#,@(cons
                                 #'(lambda ([args : Atom] ...)
                                     body ...)
                                 (reverse (syntax->list #'(g ...)))))])
                        (stack-push! st r) ...
                        s)))))))]))

(define primitive-word-table
  (let ([wt : Word-Table (hash)])
    (define/forthqk (+ x y) wt 1
      (fx+ x y))
    (define/forthqk (* x y) wt 1
      (fx* x y))
    (define/forthqk (- x y) wt 1
      (fx- x y))
    (define/forthqk (= x y) wt 1
      (if (fx= x y) 1 0))
    (define/forthqk (< x y) wt 1
      (if (fx< x y) 1 0))

    (define/forthqk (swap x y) wt 2
      (values y x))

    (define/forthqk (rot x y z) wt 3
      (values y z x))

    (define/forthqk (over x y) wt 3
      (values x y x))

    (define/forthqk (dup x) wt 2
      (values x x))

    (define/forthqk (drop x) wt 0
      (values))

    (define/forthqk (cr) wt 0
      (newline)
      (values))

    (define/forthqk (|.| x) wt 0
      (display x)
      (values))

    (set! wt
          (hash-set
           wt
           'dump
           (lambda ([s : state])
             (let ([st : stack (state-stack s)])
               (displayln
                (reverse (stack->list st)))
               s))))
    wt))
(provide primitive-word-table)

(: current-state (Parameterof state))
(define current-state (make-parameter
                       (state (make-stack)
                              primitive-word-table)))
(provide current-state)
