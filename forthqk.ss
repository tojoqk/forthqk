#!r6rs
(import (except (rnrs) read))

(define-record-type (stack %make-stack stack?)
  (fields
   [mutable index]
   [immutable data]))

(define (make-stack n)
  (%make-stack 0 (make-vector n)))

(define (stack-pop! st)
  (let ([idx (stack-index st)]
        [dat (stack-data st)])
    (stack-index-set! st (- idx 1))
    (vector-ref dat (- idx 1))))

(define (stack-push! st x)
  (let ([idx (stack-index st)]
        [vec (stack-data st)])
    (vector-set! vec idx x)
    (stack-index-set! st (+ idx 1))))

(define (stack-top st)
  (vector-ref (stack-data st) (- (stack-index st) 1)))

(define (stack-empty? st)
  (zero? (stack-index st)))

(define (stack->list st)
  (let ([idx (stack-index st)]
        [data (stack-data st)])
    (let loop ([i (- idx 1)]
               [acc '()])
      (if (< i 0)
          (reverse acc)
          (loop (- i idx)
                (loop (- i 1)
                      (cons (vector-ref data i)
                            acc)))))))

(define-record-type (state make-state state?)
  (fields
   [immutable word-table]
   [immutable stack]))

(define (empty-word-table) (make-eq-hashtable))

(define-syntax define/forthqk
  (lambda (x)
    (syntax-case x ()
      [(_ (name args ...) wt n body ...)
       (let ([n (syntax->datum #'n)])
         (with-syntax ([(g ...)
                        (generate-temporaries #'(args ...))]
                       [(r ...) (generate-temporaries
                                 (let loop ([i 0])
                                   (if (= i n) '()
                                       (cons #t (loop (+ i 1))))))])
           #`(let ()
               (define (sym z)
                 (if (string? z)
                     (string->symbol z)
                     z))
               (hashtable-set!
                wt (sym 'name)
                (lambda (s)
                  (let* ([st (state-stack s)]
                         [g (stack-pop! st)]
                         ...)
                    (let-values
                        ([(r ...)
                          (#,@(cons
                               #'(lambda (args ...) body ...)
                               (reverse #'(g ...))))])
                      (stack-push! st r) ...
                      s)))))))])))

(define (primitive-word-table)
  (let ([wt (empty-word-table)])
    (define/forthqk (+ x y) wt 1
      (+ x y))
    (define/forthqk (+ x y) wt 1
      (fx+ x y))
    (define/forthqk (* x y) wt 1
      (fx* x y))
    (define/forthqk (- x y) wt 1
      (fx- x y))
    (define/forthqk (= x y) wt 1
      (if (= x y) 1 0))
    (define/forthqk (< x y) wt 1
      (if (< x y) 1 0))

    (define/forthqk (swap x y) wt 2
      (values y x))

    (define/forthqk (rot x y z) wt 3
      (values y z x))

    (define/forthqk (over x y) wt 3
      (values x y x))

    (define/forthqk (dup x) wt 2
      (values x x))

    (define/forthqk (drop x) wt 0
      (values))

    (define/forthqk (cr) wt 0
      (newline)
      (values))

    (define/forthqk ("." x) wt 0
      (display x)
      (values))

    (hashtable-set!
     wt
     'dump
     (lambda (s)
       (let ([st (state-stack s)])
         (display (reverse (stack->list st)))
         (newline)
         s)))
    wt))

(define (token->value token)
  (cond
    [(string->number token) => (lambda (x) x)]
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

(define (read in)
  (define tokens (tokenize in))
  (if (eof-object? tokens)
      (eof-object)
      (let-values ([(tokens exprs)
                    (read-tokens in
                                 (map token->value tokens)
                                 '()
                                 (make-stack 4096))])
        (if (null? tokens)
            (reverse exprs)
            (error 'read "error")))))

(define (next-token in tokens st)
  (cond
    [(null? tokens)
     (cond
       [(not (stack-empty? st))
        (let ([tokens (tokenize in)])
          (cond
            [(eof-object? tokens)
             (error 'read "unexpected eof")]
            [(null? tokens)
             (next-token in tokens st)]
            [else
             (values (car tokens) (cdr tokens))]))]
       [else
        (values (eof-object) '())])]
    [else
     (values (car tokens) (cdr tokens))]))

(define (read-tokens in tokens exprs st)
  (let-values ([(token tokens) (next-token in tokens st)])
    (cond
      [(eof-object? token)
       (values tokens exprs)]
      [(eq? token ':)
       (read-define in tokens exprs st)]
      [(eq? token (string->symbol ";"))
       (if (eq? (stack-top st) ':)
           (values tokens exprs)
           (error 'read "unexpected \";\""))]
      [(eq? token 'if)
       (read-if in tokens exprs st)]
      [(eq? token 'else)
       (if (eq? (stack-top st) 'if)
           (values tokens exprs)
           (error 'read "unexpected \"else\""))]
      [(eq? token 'then)
       (if (eq? (stack-top st) 'else)
           (values tokens exprs)
           (error 'read "unexpected \"then\""))]
      [else
       (read-tokens in tokens (cons token exprs) st)])))

(define (read-define in tokens exprs st)
  (let-values
      ([(tokens word exprs*)
        (begin
          (stack-push! st ':)
          (let*-values ([(word tokens) (next-token in tokens st)]
                        [(tokens exprs*)
                         (read-tokens in tokens '() st)])
            (stack-pop! st)
            (values tokens word exprs*)))])
    (read-tokens in tokens
                 (cons `(: ,word ,@(reverse exprs*))
                       exprs)
                 st)))

(define (read-if in tokens exprs st)
  (stack-push! st 'if)
  (let-values ([(tokens then-exprs)
                (read-tokens in tokens '() st)])
    (stack-pop! st)
    (stack-push! st 'else)
    (let-values ([(tokens else-exprs)
                  (read-tokens in tokens '() st)])
      (stack-pop! st)
      (read-tokens in tokens
                   (cons `(if ,(reverse then-exprs)
                              ,(reverse else-exprs))
                         exprs)
                   st))))

(define (tokenize in)
  (let-values ([(port get) (open-string-output-port)])
    (define tokens (parse-token in port get '()))
    (if (eof-object? tokens)
        (eof-object)
        (reverse tokens))))

(define (parse-token in port get tokens)
  (define c (read-char in))
  (define (get-token)
    (let ([str (get)])
      (if (zero? (string-length str))
          #f
          str)))
  (cond
    [(eof-object? c)
     (if (and (null? tokens) (not (get-token)))
         (eof-object)
         (error 'parse-token "unexpected eof"))]
    [(char=? c #\newline)
     (cond
       [(get-token)
        => (lambda (token) (cons token tokens))]
       [else tokens])]
    [(char-whitespace? c)
     (skip-whitespace in get
                      (cond
                        [(get-token)
                         => (lambda (token) (cons token tokens))]
                        [else tokens]))]
    [else
     (write-char c port)
     (parse-token in port get tokens)]))

(define (skip-whitespace in get tokens)
  (define c (peek-char in))
  (cond
    [(eof-object? c) (eof-object)]
    [(char=? c #\newline)
     (read-char in)
     tokens]
    [(char-whitespace? c)
     (read-char in)
     (skip-whitespace in get tokens)]
    [else
     (let-values ([(port get) (open-string-output-port)])
       (parse-token in port get tokens))]))

(define eval
  (case-lambda
    [(es s)
     (if (null? es)
         s
         (let ([e (car es)]
               [es (cdr es)])
           (cond
             [(symbol? e)
              (eval-word e es s)]
             [(not (pair? e))
              (stack-push! (state-stack s) e)
              (eval es s)]
             [(eq? 'if (car e))
              (eval-if e es s)]
             [(eq? ': (car e))
              (eval-: e es s)]
             [else
              (error 'error "?")])))]
    [(es)
     (eval es (initial-state))]))

(define (eval-word e es s)
  (eval es
        ((hashtable-ref (state-word-table s)
                        e
                        (lambda (s)
                          (error 'forthqk
                                 (string-append
                                  "undefine word ("
                                  (symbol->string e)
                                  ")"))))
         s)))

(define (eval-if e es s)
  (if (= 0 (stack-pop! (state-stack s)))
      (eval (caddr e) s)
      (eval (cadr e) s)))

(define (eval-: e es s)
  (hashtable-set!
   (state-word-table s)
   (car (cdr e))
   (let ([body (cdr (cdr e))])
     (lambda (s) (eval body s))))
  (eval es s))

(define (initial-state)
  (make-state (primitive-word-table)
              (make-stack 4096)))

(define repl
  (case-lambda
    [(in)
     (let loop ([state (initial-state)])
       (flush-output-port (current-output-port))
       (let ([result (read in)])
         (cond
          [(eof-object? result) 'done]
          [else
           (loop (eval result state))])))]
    [()
     (repl (current-input-port))]))

(repl)
