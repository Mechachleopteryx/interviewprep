#lang racket

(require racket/stream)

(define (render-list-as-string lst renderer [offset 0])
  (define separators (stream-cons " " separators))
  (displayln
   (string-append (string-join
                   (stream->list
                    (stream-take separators
                                 offset))
                   "")
                  (string-join (map renderer lst)
                               (stream-first separators)))))

(define (render-sierpinski num
                           #:selector [selector? odd?])
  (if (selector? num)
      "●"
      " "))

(provide render-list-as-string
         render-sierpinski)
