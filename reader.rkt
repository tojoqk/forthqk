#lang racket
(require racket/contract)
(require syntax/strip-context)
(require "tokenizer.rkt")

(define current-outer (make-parameter #f))

(define (inner?)
  (current-outer))

(define (token->value token)
  (cond
    [(string->number token) => identity]
    [(string=? token "#t") #t]
    [(string=? token "#f") #f]
    [else (string->symbol token)]))

(define (read-all in)
  (let loop ([result (read in)]
             [exprs '()])
    (cond
      [(eof-object? result) exprs]
      [else
       (loop (read in)
             (append exprs result))])))

(define (read [in (current-input-port)])
  (define tokens (tokenize in))
  (if (eof-object? tokens)
      eof
      (let-values ([(tokens exprs)
                    (read-tokens in
                                 (map token->value tokens)
                                 '())])
        (if (null? tokens)
            (reverse exprs)
            (error 'read "error")))))
(provide read)

(define (next-token in tokens)
  (cond
    [(null? tokens)
     (cond
       [(inner?)
        (let ([tokens (tokenize in)])
          (cond
            [(eof-object? tokens)
             (error 'read "unexpected eof")]
            [(null? tokens)
             (next-token in tokens)]
            [else
             (values (car tokens) (cdr tokens))]))]
       [else
        (values eof '())])]
    [else
     (values (car tokens) (cdr tokens))]))

(define (read-tokens in tokens exprs)
  (let-values ([(token tokens) (next-token in tokens)])
    (cond
      [(eof-object? token)
       (values tokens exprs)]
      [(eq? token ':)
       (read-define in tokens exprs)]
      [(eq? token '|;|)
       (if (eq? (current-outer) ':)
           (values tokens exprs)
           (error 'read "unexpected \";\""))]
      [(eq? token 'if)
       (read-if in tokens exprs)]
      [(eq? token 'else)
       (if (eq? (current-outer) 'if)
           (values tokens exprs)
           (error 'read "unexpected \"else\""))]
      [(eq? token 'then)
       (if (eq? (current-outer) 'else)
           (values tokens exprs)
           (error 'read "unexpected \"then\""))]
      [else
       (read-tokens in tokens (cons token exprs))])))

(define (read-define in tokens exprs)
  (let-values
      ([(tokens word exprs*)
        (parameterize ([current-outer ':])
          (let*-values ([(word tokens) (next-token in tokens)]
                        [(tokens exprs*)
                         (read-tokens in tokens '())])
            (values tokens word exprs*)))])
    (read-tokens in tokens
                 (cons `(: ,word ,@(reverse exprs*))
                       exprs))))

(define (read-if in tokens exprs)
  (let*-values
      ([(tokens then-exprs)
        (parameterize ([current-outer 'if])
          (read-tokens in tokens '()))]
       [(tokens else-exprs)
        (parameterize ([current-outer 'else])
          (read-tokens in tokens '()))])
    (read-tokens in tokens
                 (cons `(if ,(reverse then-exprs)
                            ,(reverse else-exprs))
                       exprs))))

(define (read-syntax src in)
  (define datum `(module forthqk-mod forthqk/expander
                   ,(read-all in)))
  (datum->syntax #f datum))
(provide read-syntax)
