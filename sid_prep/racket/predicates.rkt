#lang racket/base

(define (prime? num)
  (let loop ([i 2])
    (if (<= i
            (sqrt num))
        (if (= (remainder num i)
            0)
         #f
         (loop (add1 i)))
        #t)))

(define (multiple-of? n num)
  (= (remainder num n)
     0))

(provide prime?
         multiple-of?)
