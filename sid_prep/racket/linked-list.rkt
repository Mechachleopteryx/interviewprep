#lang racket

(define linked-list%
  (class object%
    (super-new)

    (field [head (mcons null null)])  ; sentinel head

    (define/public (from-list l)
      (set-mcdr! head l)) ; do via init arg somehow

    (define/public (insert elem)
      (set-mcdr! head
                 (mcons elem
                        (mcdr head))))

    (define/private (find-spot elem)
      (let loop ([current-node head])
        (let ([next-node (mcdr current-node)])
          (when (not (null? next-node))
            (if (equal? elem
                        (mcar next-node))
                current-node
                (loop next-node))))))

    (define/public (find elem)
      (let ([spot (find-spot elem)])
        (when (not (void? spot))
          (mcdr spot))))

    (define/public (remove elem)
      (let ([spot (find-spot elem)])
        (when (not (void? spot))
          (let ([next-node (mcdr spot)])
            (set-mcdr! spot
                       (mcdr next-node))
            (set-mcdr! next-node
                       null)
            next-node))))

    (define/public (remove-all elem)
      (let loop ()
        (let ([removed-element (remove elem)])
          (when (not (void? removed-element))
            (loop)))))

    (define/public (length)
      (let loop ([current-node head]
                 [len 0])
        (let ([next-node (mcdr current-node)])
          (if (null? next-node)
              len
              (loop next-node (+ len 1))))))

    (define/public (empty?)
      (null? (mcdr head)))))

(define sorted-linked-list%
  (class linked-list%
    (inherit-field head)
    (inherit length)
    (super-new)
    (init [comparator <])

    (define less-than? comparator)

    (define/private (find-spot elem)
      (let loop ([current-node head])
        (let ([next-node (mcdr current-node)])
          (if (or (null? next-node)
                  (not (less-than? (mcar next-node)
                                   elem)))
              current-node
              (loop next-node)))))

    (define/override (find elem)
      (let ([spot (find-spot elem)])
        (when (not (void? spot))
          (let ([next-node (mcdr spot)])
            (when (equal? (mcar next-node)
                          elem)
              next-node)))))

    (define/override (insert elem)
      (let* ([spot (find-spot elem)]
             [node (mcons elem (mcdr spot))])
        (set-mcdr! spot node)))

    (define/override (remove elem)
      (let ([spot (find-spot elem)])
        (when (not (void? spot))
          (let ([next-node (mcdr spot)])
            (when (and (not (null? next-node))
                       (equal? (mcar next-node)
                               elem))
              (set-mcdr! spot (mcdr next-node))
              next-node)))))))

(provide linked-list%
         sorted-linked-list%)
