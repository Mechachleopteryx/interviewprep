#lang racket

(require "hashtable.rkt")

(define hashset%
  (class object%
    (init [set-size 100])
    (super-new)
    (define size set-size)
    (define contents (new-empty-set))
    (define/private (new-empty-set)
      (new hashtable% [table-size size]))
    (define/public (add key)
      (send contents insert key (void)))
    (define/public (remove key)
      (send contents remove key))
    (define/public (contains? key)
      (send contents contains? key))
    (define/public (pick)
      (let* ([members (send contents show)]
             [random-members (shuffle (vector->list members))])
        (let loop ([current-members random-members])
          (when (not (empty? current-members))
            (let ([current-member (car current-members)])
              (if (not (set-empty? current-member))
                  (car (set-first current-member))
                  (loop (cdr current-members))))))))
    (define/public (pop)
      (let ([result (pick)])
        (remove result)
        result))
    (define/public (show)
      (send contents show))
    (define/public (clear)
      (set! contents (new-empty-set)))))

(provide hashset)
