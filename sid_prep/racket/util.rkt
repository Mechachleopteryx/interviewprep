#lang racket/base

(require racket/stream)

(define (random-integers n)
  (stream-cons (random n) (random-integers n)))
