#lang racket
(provide #%datum #%app #%top #%top-interaction)
(require "word.rkt")
(require "stack.rkt")
(provide (all-from-out "word.rkt"))

(define-syntax (forthqk-module-begin x)
  (syntax-case x ()
    [(_ (((word body  ...) ...)
         (expr ...)))
     #`(#%module-begin
        (%define-word word (body ...))
        ...
        (begin
          (%execute (make-stack) (list expr ...))
          (void)))]))
(provide (rename-out [forthqk-module-begin #%module-begin]))

(define-syntax (%if x)
  (syntax-case x ()
    [(_ (t ... tl) (e ... el))
     #`(lambda (stack)
         (let ([expr (stack-pop! stack)])
           (if expr
               #,(cond [(number? (syntax->datum #'tl))
                        #'(%execute stack (list t ... tl))]
                       [else
                        #'(tl (%execute stack (list t ...)))])
               #,(if (number? (syntax->datum #'el))
                     #'(%execute stack (list e ... el))
                     #'(el (%execute stack (list e ...)))))))]))
(provide (rename-out [%if if]))

(define (%execute stack exprs)
  (for/fold ([stack stack])
            ([expr exprs])
    (cond [(procedure? expr)
           (expr stack)]
          [(stack-push! stack expr)
           stack])))
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
