#lang racket
(provide #%datum #%app #%top #%top-interaction)
(require "word.rkt")
(provide (all-from-out "word.rkt"))

(define-syntax (forthqk-module-begin x)
  (syntax-case x ()
    [(_ (((word body  ...) ...)
         (expr ...)))
     #`(#%module-begin
        (%define-word word (body ...))
        ...
        (begin
          (%execute '() (list expr ...))
          (void)))]))
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
(provide (rename-out [%if if]))

(define (%execute stack exprs)
  (for/fold ([stack stack])
            ([expr exprs])
    (if (procedure? expr)
        (expr stack)
        (cons expr stack))))
(provide %execute)

(define-syntax (%define-word x)
  (syntax-case x ()
    [(_ word ())
     #'(define (word stack) stack)]
    [(_ word (expr ... expr-last))
     #`(define (word stack)
         #,(if (number? (syntax->datum #'expr-last))
               #'(%execute stack (list expr ... expr-last))
               #'(expr-last (%execute stack (list expr ...)))))]))
(provide %define-word)
