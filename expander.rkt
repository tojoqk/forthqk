#lang racket
(provide #%datum #%app #%top #%top-interaction)
(require (for-syntax racket))
(require (only-in "reader.rkt" read))

(define-syntax (forthqk-module-begin x)
  (syntax-case x ()
    [(k (((WORD BODY  ...) ...)
         (EXPR ...)))
     #`(#%module-begin
        (%define-word WORD (BODY ...))
        ...
        (%execute '() (list EXPR ...))
        (void))]))
(provide (rename-out [forthqk-module-begin #%module-begin]))

(define-syntax (%if x)
  (syntax-case x ()
    [(_ (t ... tl) (e ... el))
     #`(lambda (stack)
         (let ([expr (car stack)])
           (if expr
               #,(if (number? (syntax->datum #'tl))
                     #'(%execute (cdr stack) (list t ... tl))
                     #'(tl (%execute (cdr stack) (list t ...))))
               #,(if (number? (syntax->datum #'el))
                     #'(%execute (cdr stack) (list e ... el))
                     #'(el (%execute (cdr stack) (list e ...)))))))]))
(provide %if)

(define (%execute stack exprs)
  (for/fold ([stack stack])
            ([expr exprs])
    (if (procedure? expr)
        (expr stack)
        (cons expr stack))))
(provide %execute)

(define-syntax-rule (%define-word word (expr ...))
  (define (word stack)
    (%execute stack (list expr ...))))
(provide %define-word)

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

(define (rot x y z) (values z y x))
(provide/forthqk rot 3 3)

(define (dup x) (values x x))
(provide/forthqk dup 1 2)

(define (drop x) (values))
(provide/forthqk drop 1 0)

(define (|.| x) (values)
  (display x)
  (write-char #\space)
  (displayln 'ok)
  (values))
(provide/forthqk |.| 1 0)

