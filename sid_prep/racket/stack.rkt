#lang racket

(provide stack%)

(define stack%
  (class object%
    (super-new)

    (field [contents '()])

    (define/public (push elem)
      (set! contents (cons elem contents)))

    (define/public (pop)
      (with-handlers ([exn:fail? (λ (exn) (void))])
        (let ([elem (car contents)])
          (set! contents (cdr contents))
          elem)))

    (define/public (peek)
      (match contents
        ['() (void)]
        [_   (car contents)]))

    (define/public (empty?)
      (void? (peek)))

    (define/public (show)
      contents)))
