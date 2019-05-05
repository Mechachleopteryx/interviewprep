#lang racket

(define (merge l r)
  (if (null? l)
      r
      (if (null? r)
          l
          (let ([l-head (car l)]
                [r-head (car r)])
            (if (< l-head r-head)
                (cons l-head
                      (merge (cdr l)
                             r))
                (cons r-head
                      (merge l
                             (cdr r))))))))

(define (sort my-list)
  (let ([len (length my-list)])
    (if (= len
           1)
        my-list
        (let ([mid (floor (/ len 2))])
          (let-values ([(l r) (split-at my-list mid)])
            (merge (sort l)
                   (sort r)))))))

(provide sort)
