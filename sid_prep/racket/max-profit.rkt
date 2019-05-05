#lang racket

(define (max-profit prices)
  (let ([max 0]
        [low (first prices)]
        [high (first prices)])
    (for ([p (in-list prices)])
      (if (> p high)
          (set! high p)
          (when (< p low)
            (set! max (- high
                         low))
            (set! low p)
            (set! high p))))
    (let ([latest-max (- high low)])
      (if (> latest-max
            max)
         latest-max
         max))))


(define (max-profit prices)
  (let ([max-profit (apply min prices)])
    (for* ([i (- (length prices) 1)]
           [j (in-range (add1 i) (length prices))])
      (let ([profit (- (list-ref prices j)
                       (list-ref prices i))])
        (when (> profit
                 max-profit)
          (set! max-profit profit))))
    max-profit))
