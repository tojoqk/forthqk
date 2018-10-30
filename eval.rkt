#lang typed/racket
(require (for-syntax racket))
(require racket/fixnum)
(require "stack.rkt")

(struct state ([stack : stack]
               [word-table : Word-Table])
  #:mutable)
(define-type Exprs (U (Listof Expr)))
(define-type Expr (U Fixnum Symbol If-Expr Define-Expr))
(define-type If-Expr (List 'if Exprs Exprs))
(define-type Define-Expr (Pair ': (Pair Symbol Exprs)))
(define-type Word-Table (Immutable-HashTable Symbol Word))
(define-type Word (-> state state))

(require/typed "reader.rkt"
  [read (->* () (Input-Port) Exprs)])

(: eval (-> state Exprs state))
(define (eval s es)
  (if (null? es)
      s
      (let ([e (car es)]
            [es (cdr es)])
        (cond
          [(fixnum? e)
           (stack-push! (state-stack s) e)
           (eval s es)]
          [(symbol? e)
           (eval-word s e es)]
          [(eq? 'if (car e))
           (eval-if s e es)]
          [(eq? ': (car e))
           (eval-: s e es)]
          [else
           (error 'error "?")]))))
(provide eval)

(: eval-word (-> state Symbol Exprs state))
(define (eval-word s e es)
  (eval ((hash-ref (state-word-table s) e) s)
        es))

(: eval-if (-> state If-Expr Exprs state))
(define (eval-if s e es)
  (if (fx= 0 (stack-pop! (state-stack s)))
      (eval s (cadr e))
      (eval s (caddr e))))

(: eval-: (-> state Define-Expr Exprs state))
(define (eval-: s e es)
  (set-state-word-table!
   s
   ((inst hash-set Symbol Word)
    (state-word-table s)
    (car (cdr e))
    (let ([body : Exprs (cdr (cdr e))])
      (lambda ([s : state]) (eval s body)))))
  s)

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
                           [g : Fixnum (stack-pop! st)]
                           ...)
                      (let-values
                          ([([r : Fixnum] ...)
                            (#,@(cons
                                 #'(lambda ([args : Fixnum] ...)
                                     body ...)
                                 (reverse (syntax->list #'(g ...)))))])
                        (stack-push! st r) ...
                        s)))))))]))

(define primitive
  (let ([wt : Word-Table (hash)])
    (define/forthqk (+ x y) wt 1
      (fx+ x y))
    (define/forthqk (* x y) wt 1
      (fx* x y))
    (define/forthqk (- x y) wt 1
      (fx- x y))
    (define/forthqk (= x y) wt 1
      (if (fx= x y) 1 0))

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
