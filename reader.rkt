#lang racket/base
(require racket/contract)
(require (only-in racket/function identity))

(define (tokenize-line [in (current-input-port)])
  (define reversed (tokenize-line/reverse in))
  (if (eof-object? reversed)
      eof
      (reverse reversed)))
(provide/contract [tokenize-line (->* () (input-port?)
                                  (or/c
                                   (listof (or/c symbol? number?))
                                   eof-object?))])

(define (tokenize-line/reverse in)
  (define tokens (parse-token in (open-output-string) '()))
  (if (eof-object? tokens)
      eof
      (map token->value tokens)))

(define (parse-token in token-port tokens)
  (define c (tokenize-char in))
  (define (token-port->token token-port)
    (define token (get-output-string token-port))
    (if (zero? (string-length token))
        #f
        token))
  (cond
    [(eof-object? c)
     (if (and (null? tokens) (not (token-port->token token-port)))
         eof
         (error 'parse-token "unexpected eof"))]
    [(char=? c #\newline)
     (cond
       [(token-port->token token-port)
        => (λ (token) (cons token tokens))]
       [else tokens])]
    [(char-whitespace? c)
     (skip-whitespace in
                      (cond [(token-port->token token-port)
                             => (λ (token) (cons token tokens))]
                            [else tokens]))]
    [else
     (write-char c token-port)
     (parse-token in token-port tokens)]))

(define (skip-whitespace in tokens)
  (define c (peek-char in))
  (cond
    [(eof-object? c) eof]
    [(char=? c #\newline)
     (tokenize-char in)
     tokens]
    [(char-whitespace? c)
     (tokenize-char in)
     (skip-whitespace in tokens)]
    [else
     (parse-token in (open-output-string) tokens)]))

(define (token->value token)
  (cond
    [(string->number token) => identity]
    [else (string->symbol token)]))
