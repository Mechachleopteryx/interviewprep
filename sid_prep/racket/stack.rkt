#lang racket

(define stack%
  (class* object% (printable<%>)
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

    (define/public (size)
      (length contents))

    (define/public (custom-print port quote-depth)
      (print contents port quote-depth))

    (define/public (custom-write port)
      (write contents port))

    (define/public (custom-display port)
      (display contents port))))

(define ordered-stack%
  (class stack%
    (super-new)

    (inherit-field contents)
    (field [comparator? <])

    (define/override (push elem)
      (let ([top (send this peek)])
        (if (or (void? top)
                (comparator? elem
                             top))
            (super push elem)
            (error "Pushing onto stack would violate order!"))))))

(provide stack%
         ordered-stack%)
