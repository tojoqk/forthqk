#lang racket/base
(require "forthqk.rkt")
(provide (all-from-out "forthqk.rkt"))

(module reader racket/base
  (require "reader.rkt")
  (provide read-syntax))
