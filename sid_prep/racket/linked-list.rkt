#lang racket

(define linked-list%
  (class object%
    (super-new)
    (define contents '())
    (define/public (insert elem)
      (set! contents (mcons elem contents)))
    (define/public (remove elem)
      (define (remove-recur current-sublist)
        (let ([remaining-sublist (mcdr current-sublist)])
          (when (not (empty? remaining-sublist))
            (if (equal? elem
                        (mcar remaining-sublist))
                (begin (set-mcdr! current-sublist
                                  (mcdr remaining-sublist))
                       elem)
                (remove-recur remaining-sublist)))))
      (when (not (empty? contents))
        (if (equal? elem
                   (head))
           (begin (set! contents (mcdr contents))
                  elem)
           (remove-recur contents))))
    (define/public (remove-all elem)
      (define (remove-until-end)
        (let ([removed-element (remove elem)])
          (when (not (void? removed-element))
            (remove-until-end))))
      (remove-until-end))
    (define/public (head)
      (mcar contents))
    (define/public (show)
      contents)))

(provide linked-list%)
