#lang racket

(define (merge left right)
  (cond [(empty? left) right]
        [(empty? right) left]
        [else (match* (left right)
                [((cons l ls) (cons r rs))
                 (if (< l r)
                     (cons l (merge ls right))
                     (cons r (merge left rs)))])]))

(define (sort my-list)
  (match my-list
    [(or (list) (list _)) my-list]
    [_ (let ([middle (quotient (length my-list)
                               2)])
         (let-values ([(left right) (split-at my-list middle)])
           (merge (sort left)
                  (sort right))))]))

(provide sort)
