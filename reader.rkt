#lang racket/base
(require racket/contract)
(require syntax/strip-context)
(require "tokenizer.rkt")

(define current-outer (make-parameter #f))

(define (inner?)
  (current-outer))

(define (read [in (current-input-port)])
  (define tokens (tokenize in))
  (if (eof-object? tokens)
      eof
      (let-values ([(tokens words stack)
                    (read-tokens in tokens '() '())])
        (if (null? tokens)
            (list (reverse words) (reverse stack))
            (error 'read "error")))))

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

(define (read-tokens in tokens words stack)
  (let-values ([(token tokens) (next-token in tokens)])
    (cond
      [(eof-object? token)
       (values tokens words stack)]
      [(eq? token ':)
       (read-define in tokens words stack)]
      [(eq? token '|;|)
       (if (eq? (current-outer) ':)
           (values tokens words stack)
           (error 'read "unexpected \";\""))]
      [(eq? token 'if)
       (read-if in tokens words stack)]
      [(eq? token 'else)
       (if (eq? (current-outer) 'if)
           (values tokens words stack)
           (error 'read "unexpected \"else\""))]
      [(eq? token 'then)
       (if (eq? (current-outer) 'else)
           (values tokens words stack)
           (error 'read "unexpected \"then\""))]
      [else
       (read-tokens in tokens words (cons token stack))])))

(define (read-define in tokens words stack)
  (let-values
      ([(tokens word words* stack*)
        (parameterize ([current-outer ':])
          (let*-values ([(word tokens) (next-token in tokens)]
                        [(tokens words* stack*)
                         (read-tokens in tokens '() '())])
            (values tokens word words* stack*)))])
    (cond
      [(not (null? words*))
       (error 'read "can't nest \":\"")]
      [else
       (read-tokens in tokens
                    (cons (cons word (reverse stack*)) words)
                    stack)])))

(define (read-if in tokens words stack)
  (let*-values
      ([(tokens then-words then-stack)
        (parameterize ([current-outer 'if])
          (read-tokens in tokens '() '()))]
       [(tokens else-words else-stack)
        (parameterize ([current-outer 'else])
          (read-tokens in tokens '() '()))])
    (cond
      [(not (and (null? then-words)
                 (null? else-words)))
       (error 'read "unexpected \":\"")]
      [else
       (read-tokens in tokens
                    words
                    (cons `(if ,(reverse then-stack)
                               ,(reverse else-stack))
                          stack))])))

(define (read-syntax src in)
  (strip-context (datum->syntax #f (read in))))
