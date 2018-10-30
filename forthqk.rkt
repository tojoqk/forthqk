#lang racket/base
(require "reader.rkt")
(require "eval.rkt")

(define (repl [in (current-input-port)])
  (parameterize ([current-input-port in]
                 [current-namespace
                  (make-base-empty-namespace)])
    (namespace-require 'forthqk/expander)
    (let loop ([state (current-state)])
      (flush-output)
      (define result (read))
      (cond
        [(eof-object? result) (void)]
        [else
         (current-state (eval result))
         (loop (current-state))]))))
(provide repl)

(module+ main
  (repl))
