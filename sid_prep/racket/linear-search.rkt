#lang racket

(define (search my-list elem)
  (let loop ([current-index 0])
    (when (< current-index
             (length my-list))
      (if (= (list-ref my-list current-index)
             elem)
          current-index
          (loop (+ current-index
                   1))))))

(provide search)
