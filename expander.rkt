#lang racket
(provide #%datum #%app #%top #%top-interaction)
(require "word.rkt")
(require "stack.rkt")
(require "eval.rkt")
(provide eval)
(provide (all-from-out "word.rkt"))

(define-syntax (forthqk-module-begin stx)
  (syntax-case stx ()
    [(_ exprs)
     #`(#%module-begin
        (begin (eval '#,(syntax->datum #'exprs))
               (void)))]))
(provide (rename-out [forthqk-module-begin #%module-begin]))
