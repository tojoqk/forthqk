#lang typed/racket
(require racket/fixnum)

(define-type Atom (U Fixnum))
(provide Atom)
(define-predicate atom? Atom)
(provide atom?)

(struct stack ([index : Fixnum]
               [data : (Vectorof Atom)])
  #:mutable)
(provide stack)

(: make-stack (-> stack))
(define (make-stack)
  (stack 0 (make-vector 4096)))
(provide make-stack)

(: stack-push! (-> stack Atom Void))
(define (stack-push! s v)
  (let ([idx (stack-index s)]
        [dat (stack-data s)])
    (vector-set! dat idx v)
    (set-stack-index! s (fx+ idx 1))))
(provide stack-push!)

(: stack-pop! (-> stack Atom))
(define (stack-pop! s)
  (let ([idx (stack-index s)]
        [dat (stack-data s)])
    (begin0 (vector-ref dat (fx- idx 1))
      (set-stack-index! s (fx- idx 1)))))
(provide stack-pop!)

(: stack->list (-> stack (Listof Atom)))
(define (stack->list s)
  (let ([idx (stack-index s)]
        [dat (stack-data s)])
    (reverse
     (for/list ([i idx])
       (vector-ref dat i)))))
(provide stack->list)
