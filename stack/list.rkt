#lang racket
(require racket/contract)

(struct stack ([data #:mutable]))
(define (make-stack)
  (stack '()))
(provide/contract [make-stack (-> stack?)])

(define (stack-empty? s)
  (null? (stack-data s)))

(define (stack-push! s v)
  (match-define (stack dat) s)
  (set-stack-data! s (cons v dat)))
(provide stack-push!)

(define (stack-pop! s)
  (match-define (stack dat) s)
  (begin0 (car dat)
    (set-stack-data! s (cdr dat))))
(provide stack-pop!)

(define (stack-top s)
  (match-define (stack dat) s)
  (car dat))
(provide stack-top)

(define/contract (stack->list s)
  (-> stack? list)
  (match-define (stack dat) s)
  dat)
(provide stack->list)
