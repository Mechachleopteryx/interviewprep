#lang racket

(define (swap! vec i j)
  (let ([temp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j temp)))

(provide swap!)
