#lang racket

(define hashtable%
  (class object%
    (init [table-size 100])
    (super-new)
    (define size table-size)
    (define contents
      (build-vector size
                    (λ args (mutable-set))))
    (define/public (contains? key)
      (not (void? (lookup-association key))))
    (define/private (hash-fn-int key-int)
      (remainder key-int size))
    (define/private (hash-fn key)
      (cond [(number? key) (hash-fn-int key)]
            [(string? key) (hash-fn-int (foldl +
                                               0
                                               (map char->integer
                                                    (string->list key))))]
            [else (error "Unsupported key type! Must be a number.")]))
    (define/public (insert key value)
      (let* ([index (hash-fn key)]
             [destination (vector-ref contents index)])
        (remove key)
        (set-add! destination
                  (cons key value))))
    (define/public (remove key)
      (let* ([index (hash-fn key)]
             [destination (vector-ref contents index)]
             [item (lookup-association key)])
        (when (not (void? item))
          (set-remove! destination item))))
    (define/private (lookup-association key)
      (define (find-key remaining-elements)
        (when (not (stream-empty? remaining-elements))
          (let ([current-element (stream-first remaining-elements)])
           (if (equal? (car current-element)
                       key)
               current-element
               (find-key (stream-rest remaining-elements))))))
      (let* ([index (hash-fn key)]
             [destination (vector-ref contents index)])
        (find-key (set->stream destination))))
    (define/public (lookup key)
      (let ([assoc (lookup-association key)])
        (cdr assoc)))
    (define/public (show)
      contents)))

(provide hashtable%)
