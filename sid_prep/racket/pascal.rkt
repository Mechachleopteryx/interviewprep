#lang racket

(define DEFAULT-OFFSET 100)

(define (prime? num)
  (let loop ([i 2])
    (if (<= i
            (sqrt num))
        (if (= (remainder num i)
            0)
         #f
         (loop (add1 i)))
        #t)))

(define (multiple-of? n num)
  (= (remainder num n)
     0))

(define (render-sierpinski num
                           #:selector [selector? odd?])
  (if (selector? num)
      "●"
      " "))

(define (split-into-pairs lst)
  (let loop ([remaining-elements (cons 0 lst)])
    (if (null? remaining-elements)
        null
        (cons (cons (first remaining-elements)
                    (with-handlers ([exn:fail? (λ (exn) 0)])
                      (second remaining-elements)))
              (loop (rest remaining-elements))))))

(define (render-row row offset renderer)
  (define separators (stream-cons "" separators))
  (displayln
   (string-append (string-join
                   (stream->list
                    (stream-take separators
                                 offset)))
                  (string-join (map renderer row)))))

(define (pascal n [renderer number->string])
  (let loop ([i 0] [current-row '(1)])
    (when (or (< i n)
              (= n -1))
      (let ([next-row
             (for/list ([pair (in-list (split-into-pairs current-row))])
               (+ (car pair)
                  (cdr pair)))])
        (let ([offset (if (> n -1) n DEFAULT-OFFSET)])
          (render-row current-row
                      (modulo (- offset i)
                              (* DEFAULT-OFFSET 2))
                      renderer))
        (sleep 0.1)
        (loop (add1 i) next-row)))))

(pascal -1
        (curry render-sierpinski
               #:selector (curry multiple-of? 9)))
