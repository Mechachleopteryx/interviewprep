#lang racket

(define (count-valleys hike-steps)
  (let ([steps (map (λ (c)
                      (match c
                        [#\U 1]
                        [#\D -1]))
                    (string->list hike-steps))])
    (let loop ([remaining-steps steps]
               [sum 0])
      (if (null? remaining-steps)
          0
          (let* ([current-step (first remaining-steps)]
                 [new-sum (+ sum current-step)])
            (if (and (= new-sum
                        0)
                     (= current-step
                        1))
                (+ (loop (rest remaining-steps)
                         new-sum)
                   1)
                (loop (rest remaining-steps)
                      new-sum)))))))
