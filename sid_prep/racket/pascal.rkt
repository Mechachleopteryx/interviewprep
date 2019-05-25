#lang racket

(require "predicates.rkt")
(require "print-util.rkt")

(define (split-into-pairs lst)
  (let loop ([remaining-elements (cons 0 lst)])
    (if (null? remaining-elements)
        null
        (cons (cons (first remaining-elements)
                    (with-handlers ([exn:fail? (λ (exn) 0)])
                      (second remaining-elements)))
              (loop (rest remaining-elements))))))

(define (pascal n [renderer number->string] [wait-seconds #f])
  (let loop ([i 0] [current-row '(1)])
    (when (or (< i n)
              (= n -1))
      (let ([next-row
             (for/list ([pair (in-list (split-into-pairs current-row))])
               (+ (car pair)
                  (cdr pair)))])
        (render-list-in-display current-row
                                renderer)
        (when wait-seconds
          (sleep wait-seconds))
        (loop (add1 i) next-row)))))

(pascal -1
        (curry render-sierpinski
               #:selector (curry multiple-of? 9))
        0.1)
