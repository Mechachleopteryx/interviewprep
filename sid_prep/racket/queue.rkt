#lang racket

(provide queue%)

(define queue%
  (class object%
    (super-new)

    (define contents '())

    (define/public (enqueue elem)
      (set! contents (append contents (list elem))))

    (define/public (enqueue-all elem-list)
      (set! contents (append contents elem-list)))

    (define/public (dequeue)
      (with-handlers ([exn:fail:contract?
                       (λ (exn) (void))])
        (let ([elem (car contents)])
          (set! contents (cdr contents))
          elem)))

    (define/public (peek)
      (match contents
        ['() (void)]
        [_   (car contents)]))

    (define/public (empty?)
      (void? (peek)))

    (define/public (as-list)
      contents)

    (define/public (show)
      contents)))
