#lang racket

(require racket/list)

(define (partition pred lst)
  ;; iterative
  (define low null)
  (define high null)
  (for ([elem (in-list lst)])
    (if (pred elem)
        (set! low (cons elem low))
        (set! high (cons elem high))))
  (values low high))

(define (partition pred lst)
  ;; tail recursive
  (let loop ([lst lst]
             [low null]
             [high null])
    (if (empty? lst)
        (values low high)
        (let ([elem (first lst)])
          (if (pred elem)
              (loop (rest lst)
                    (cons elem low)
                    high)
              (loop (rest lst)
                    low
                    (cons elem high)))))))

(define (partition pred lst)
  ;; recursive
  (if (empty? lst)
      (values null null)
      (let-values ([(low high)
                    (partition pred
                               (rest lst))])
        (let ([elem (first lst)])
          (if (pred elem)
              (values (cons elem low)
                      high)
              (values low
                      (cons elem high)))))))

(define (sort lst)
  (if (empty? lst)
      lst
      (let ([pivot (first lst)])
        (define-values (low high)
          (partition (λ (x) (< x pivot)) ; racket also has a built-in `partition`
                     (rest lst)))
        (append (sort low)
                (list pivot)
                (sort high)))))

(provide sort)
