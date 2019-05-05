#lang racket

(provide stack%)

(define stack%
  (class object%
    (super-new)
    (define contents '())
    (define/public (push elem)
      (set! contents (cons elem contents)))
    (define/public (pop)
      (with-handlers ([exn:fail? (λ (exn) null)])
        (let ([elem (car contents)])
          (set! contents (cdr contents))
          elem)))
    (define/public (peek)
      (match contents
        ['() null]
        [_   (car contents)]))
    (define/public (show)
      contents)))
