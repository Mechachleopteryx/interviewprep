#lang racket

(require racket/function)
(require lens)
(require "predicates.rkt")
(require "print-util.rkt")

(define (combine cords [i 0])
  (if (empty? cords)
      null
      (let* ([current-cord-lens (list-ref-lens i)]
             [current-cord (lens-view current-cord-lens
                                      cords)])
        (if (null? current-cord)
            (let* ([remaining-cords (remq current-cord
                                          cords)])
              (if (null? remaining-cords)
                  (combine remaining-cords)
                  (combine remaining-cords
                           (modulo i
                                   (length remaining-cords)))))
            (cons (first current-cord)
                  (combine (lens-set current-cord-lens
                                     cords
                                     (rest current-cord))
                           (modulo (add1 i)
                                   (length cords))))))))

(define (divide lst n)
  (let loop ([remaining-list lst]
             [slices-remaining n])
    (define slice-length
      (ceiling (/ (length remaining-list)
                  slices-remaining)))
    (cond [(= slices-remaining 1)
           (list remaining-list)]
          [else (append (list (take remaining-list slice-length))
                        (loop (drop remaining-list slice-length)
                              (sub1 slices-remaining)))])))

(define (braid lst #:n [n 3])
  (if (<= (length lst)
          1)
      lst
      (let ([cords (divide lst n)])
        (let ([result (combine (map (curry braid
                                           #:n n)
                                    cords))])
          result))))

(provide braid
         divide
         combine)
