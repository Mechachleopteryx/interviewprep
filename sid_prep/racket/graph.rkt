#lang racket

(require racket/generator)

(define graph%
  (class* object% (printable<%>)
    (init)
    (super-new)

    (define nodes (make-hash))

    (define/public (node-cls)
      graph-node%) ; do this a better way

    (define generate-id
      (generator () (for ([i (in-naturals)])
                      (yield i))))

    (define/public (get-nodes)
      nodes)

    (define/public (add-node node-value)
      (let* ([node-id (generate-id)]
             [node (new (send this node-cls)
                        [node-id node-id]
                        [node-value node-value])])
        (hash-set! nodes node-id node)
        node))

    (define/public (get-node node-id)
      (hash-ref nodes node-id))

    (define/public (add-edge node-1 node-2)
      (when (and (hash-ref nodes (send node-1 get-id) #f)
                 (hash-ref nodes (send node-2 get-id) #f))
        (send node-1 add-edge-to node-2)
        (send node-2 add-edge-from node-1)))

    (define/public (remove-edge node-1 node-2)
      (when (and (hash-ref nodes (send node-1 get-id) #f)
                 (hash-ref nodes (send node-2 get-id) #f))
        (send node-1 remove-edge-to node-2)
        (send node-2 remove-edge-from node-1)))

    (define/public (custom-print port quoting-depth)
      (for ([n (in-dict-values nodes)])
        (println n port)))

    (define/public (custom-write port)
      (write nodes port))

    (define/public (custom-display port)
      (display nodes port))))

(define graph-node%
  (class* object% (printable<%>)
    (init node-id node-value)
    (super-new)

    (define id node-id)
    (define value node-value)
    (define in-neighbors (mutable-set))
    (define out-neighbors (mutable-set))

    (define/public (get-id)
      id)

    (define/public (get-value)
      value)

    (define/public (get-in-neighbors)
      in-neighbors)

    (define/public (get-out-neighbors)
      out-neighbors)

    (define/public (get-neighbors)
      (let ([result (mutable-set)])
        (set-union! result in-neighbors out-neighbors)
        result))

    (define/public (add-edge-to other-node)
      (set-add! out-neighbors other-node))

    (define/public (remove-edge-to other-node)
      (set-remove! out-neighbors other-node))

    (define/public (add-edge-from other-node)
      (set-add! in-neighbors other-node))

    (define/public (remove-edge-from other-node)
      (set-remove! in-neighbors other-node))

    (define/public (custom-print port quoting-depth)
      (define out (map (λ (n) (send n get-value))
                       (set->list out-neighbors)))
      (if (empty? out)
          (fprintf port "(~a)" value)
          (fprintf port "(~a) → ~a" value out)))

    (define/public (custom-write port)
      (write value port))

    (define/public (custom-display port)
      (display value port))))

(define ugraph%
  (class graph%
    (super-new)

    (define/override (node-cls)
      ugraph-node%)
    ; TODO: look into augmenting to avoid
    ; the duplication (- graph/ugraph) here

    (define/override (add-edge node-1 node-2)
      (send node-1 add-edge node-2)
      (send node-2 add-edge node-1))

    (define/override (remove-edge node-1 node-2)
      (send node-1 remove-edge node-2)
      (send node-2 remove-edge node-1))))

(define ugraph-node%
  (class graph-node%
    (super-new)

    (define/public (add-edge other-node)
      (send this add-edge-from other-node)
      (send this add-edge-to other-node))

    (define/public (remove-edge other-node)
      (send this remove-edge-from other-node)
      (send this remove-edge-to other-node))))

(provide graph%
         graph-node%
         ugraph%
         ugraph-node%)
