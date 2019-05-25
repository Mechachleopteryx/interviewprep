#lang racket

(require racket/stream)
(require racket/function)
(require racket/string)

(define DEFAULT-DISPLAY-WIDTH 272)
(define separators (stream-cons " " separators))

(define (render-as-string lst renderer)
  (string-join (map renderer lst)
               (stream-first separators)))

(define (extract-visible string-to-render
                         [display-width DEFAULT-DISPLAY-WIDTH])
  (define string-width (string-length string-to-render))
  (define middle-of-string (floor (/ string-width
                                     2)))
  (define center-offset (floor (/ display-width
                                  2)))
  (define provisional-left-index (- middle-of-string
                                    center-offset))
  (define provisional-right-index (+ middle-of-string
                                     center-offset))

  (define left-index (if (> provisional-left-index
                            0)
                         provisional-left-index
                         0))
  (define right-index (if (< provisional-right-index
                             string-width)
                          provisional-right-index
                          (- string-width
                             1)))
  (substring string-to-render
             left-index
             right-index))

(define (center-in-display string-to-render
                           [display-width DEFAULT-DISPLAY-WIDTH])
  (define string-width (string-length string-to-render))
  (define center-offset (floor (/ display-width
                                  2)))
  (define provisional-prefix-length
    (- center-offset
       (/ string-width
          2)))
  (define prefix-length (if (> provisional-prefix-length
                               0)
                            (floor provisional-prefix-length)
                            0))
  (define prefix (string-join
                  (stream->list
                   (stream-take separators
                                prefix-length))
                  ""))
  (string-append prefix string-to-render))

(define (render-list-in-display lst
                                renderer
                                [display-width DEFAULT-DISPLAY-WIDTH])
  (define string-to-render (render-as-string lst renderer))
  (define visible-string (extract-visible string-to-render
                                          display-width))
  (define centered-string (center-in-display visible-string
                                             display-width))
  (displayln centered-string))

(define (render-sierpinski num
                           #:selector [selector? odd?])
  (if (selector? num)
      "●"
      " "))

(provide render-list-in-display
         render-sierpinski)
