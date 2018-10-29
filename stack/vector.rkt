#lang racket
(require racket/contract)

(struct stack ([index #:mutable] data))
(define (make-stack [n 2048])
  (stack 0 (make-vector n)))
(provide/contract [make-stack (->* () (natural?) stack?)])

(define (stack-size stack)
  (stack-index stack))

(define (stack-empty? s)
  (zero? (stack-index s)))

(define (stack-full? s)
  (= (stack-index s) (vector-length (stack-data s))))

(define (stack-push! s v)
  (match-define (stack idx dat) s)
  (vector-set! dat idx v)
  (set-stack-index! s (add1 idx)))
(provide stack-push!)

(define (stack-pop! s)
  (match-define (stack idx dat) s)
  (begin0 (vector-ref dat (sub1 idx))
    (set-stack-index! s (sub1 idx))))
(provide stack-pop!)

(define/contract (stack->list s)
  (-> stack? list)
  (match-define (stack idx dat) s)
  (for/list ([i (range (sub1 idx) -1 -1)])
    (vector-ref dat i)))
(provide stack->list)
