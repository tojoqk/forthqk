#lang racket/base
(require "reader.rkt")
(require (except-in "expander.rkt" #%module-begin))
(require "stack.rkt")

(define (repl [in current-input-port])
  (parameterize ([current-input-port in]
                 [current-namespace
                  (make-base-empty-namespace)])
    (namespace-require 'forthqk/expander)
    (let loop ([stack (make-stack)])
      (flush-output)
      (define result (read))
      (cond
        [(eof-object? result) (void)]
        [else
         (let ([words (car result)]
               [exprs (cadr result)])
           (for ([word words])
             (eval `(%define-word ,(car word) ,(cdr word))))
           (loop (%execute stack (map eval exprs))))]))))
(provide repl)

(module+ main
  (repl))
