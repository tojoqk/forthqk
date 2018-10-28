#lang racket
(require (for-syntax racket))
(define-syntax (provide/forthqk x)
  (syntax-case x ()
    [(_ f n m)
     (let ([n (syntax->datum #'n)]
           [m (syntax->datum #'m)])
       (with-syntax ([(g ...) (generate-temporaries (range n))]
                     [(r ...) (generate-temporaries (range m))])
         #`(begin
             (define (func stack)
               (let*-values ([(g stack) (values (car stack) (cdr stack))]
                             ...)
                 (define-values (r ...)
                   (#,@(cons #'f (reverse (syntax->list #'(g ...))))))
                 (list* #,@(reverse (syntax->list #'(r ...))) stack)))
             (provide (rename-out [func f])))))]))

(provide/forthqk + 2 1)
(provide/forthqk * 2 1)
(provide/forthqk - 2 1)
(provide/forthqk = 2 1)

(define (swap x y) (values y x))
(provide/forthqk swap 2 2)

(define (rot x y z) (values y z x))
(provide/forthqk rot 3 3)

(define (over x y) (values x y x))
(provide/forthqk over 2 3)

(define (dup x) (values x x))
(provide/forthqk dup 1 2)

(define (drop x) (values))
(provide/forthqk drop 1 0)

(define (cr)
  (newline)
  (values))
(provide/forthqk cr 0 0)

(define (|.| x) (values)
  (display x)
  (values))
(provide/forthqk |.| 1 0)

(define (dump stack)
  (newline)
  (display (reverse stack))
  stack)
(provide dump)
