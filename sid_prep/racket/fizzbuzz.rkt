#lang racket

;; check using nested (prioritized) ifs
(define (fizzbuzz-check n)
  (if (= (remainder n 15)
         0)
      "fizzbuzz"
      (if (= (remainder n 5)
             0)
          "buzz"
          (if (= (remainder n 3)
                 0)
              "fizz"
              n))))

;; check using pattern matching
(define (fizzbuzz-check n)
  (match (gcd n 15)
    [15 "fizzbuzz"]
    [5 "buzz"]
    [3 "fizz"]
    [_ n]))

;; accumulated list
(define (fizzbuzz n)
  #| Return the first n fizzbuzz numbers
  fizbuzz :: int? -> [int? | str?]
  |#
  (let loop ([i n]
             [res null])
    (if (> i 0)
        (loop (sub1 i)
              (cons (fizzbuzz-check i)
                    res))
        res)))

;; list comprehension
(define (fizzbuzz n)
  #| Return the first n fizzbuzz numbers
  fizbuzz :: int? -> [int? | str?]
  |#
  (for/list ([i (in-range 1 n)])
    (fizzbuzz-check i)))

;; map
(define (fizzbuzz n)
  #| Return the first n fizzbuzz numbers
  fizbuzz :: int? -> [int? | str?]
  |#
  (map fizzbuzz-check (stream->list (in-range 1 n))))

;; generator
(define (fizzbuzz n)
  #| Return the first n fizzbuzz numbers
  fizbuzz :: int? -> [int? | str?]
  |#
  (let ([f (fizzbuzz-generator)])
    (for/list ([i (in-range 1 n)])
      (f))))

(define (fizzbuzz-generator)
  (generator ()
             (for ([i (stream-rest (in-naturals))])
               (yield (fizzbuzz-check i)))))

;; stream
(define (fizzbuzz n)
  #| Return the first n fizzbuzz numbers
  fizbuzz :: int? -> [int? | str?]
  |#
  (let ([f (in-fizzbuzz)])
    (stream->list (stream-take f n))))

(define (in-fizzbuzz)
  (stream-map fizzbuzz-check (stream-rest (in-naturals))))
