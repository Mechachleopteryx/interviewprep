#lang racket

(require "linked-list.rkt")

(define (make-list node)
  (mcons null node))

(define (get-tail lst)
  (mcdr lst))

(define (split lst)
  (let loop ([slow-position lst]
             [fast-position lst])
    (if (null? fast-position)
        (let ([second-list (make-list (mcdr slow-position))])
          (set-mcdr! slow-position null)
          (values lst second-list))
        (begin
          (with-handlers
            ([exn:fail?
              (λ (exn)
                (set! fast-position
                      (mcdr fast-position)))])
            (set! fast-position (mcdr (mcdr fast-position)))
            (set! slow-position (mcdr slow-position)))
          (loop slow-position fast-position)))))

(define (merge list-l list-r)
  (let ([l-node (get-tail list-l)]
        [r-node (get-tail list-r)])
    (cond [(null? l-node) (make-list r-node)]
          [(null? r-node) (make-list l-node)]
          [else (let ([l (mcar l-node)]
                      [r (mcar r-node)])
                  (if (< l r)
                      (make-list
                       (mcons l
                              (get-tail
                               (merge (make-list (mcdr l-node))
                                      list-r))))
                      (make-list
                       (mcons r
                              (get-tail
                               (merge list-l
                                      (make-list (mcdr r-node))))))))])))

(define (sort lst)
  (cond [(null? (get-tail lst))
         lst]
        [(null? (get-tail (get-tail lst)))
         (make-list (get-tail lst))]
        [else (let-values ([(first-list second-list)
                            (split lst)])
                (merge (sort first-list)
                       (sort second-list)))]))

(define (sort-list lst)
  (let ([head (get-field head lst)])
    (define result (sort head))
    (send lst from-list (get-tail result))))

(provide sort-list)
