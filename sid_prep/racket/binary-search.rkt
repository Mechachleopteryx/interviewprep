#lang racket

(define (search array key)
  (let ([len (vector-length array)])
    (when (> len 0)
      (let* ([middle-index (floor (/ len 2))]
             [middle-element (vector-ref array middle-index)])
        (if (equal? key
                    middle-element)
            middle-index
            (if (< key
                   middle-element)
                (search (vector-take array middle-index) key)
                (let ([found-index (search (vector-take-right array middle-index) key)])
                  (when (not (void? found-index))
                    (+ found-index
                       middle-index
                       1)))))))))


(provide search)
