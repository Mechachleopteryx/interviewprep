#lang racket

(require racket/function)
(require lens)
(require "predicates.rkt")
(require "print-util.rkt")

(define (combine cords [i 0])
  ;; combine multiple lists into one list by selecting
  ;; one element from each list in turn
  (if (empty? cords)
      null
      (let ([current-cord (lens-view (list-ref-lens i)
                                     cords)])
        (if (empty? current-cord)
            (let ([remaining-cords (remq current-cord
                                         cords)])
              (combine remaining-cords
                       (remainder i
                                  (if (empty? remaining-cords)
                                      1
                                      (length remaining-cords)))))
            (cons (first current-cord)
                  (combine (lens-set (list-ref-lens i)
                                     cords
                                     (rest current-cord))
                           (remainder (add1 i)
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
        (combine (map (curry braid
                             #:n n)
                      cords)))))

(provide braid
         divide
         combine)
