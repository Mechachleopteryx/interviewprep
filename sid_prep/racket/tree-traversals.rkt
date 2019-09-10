#lang racket

#|
Data definitions
  tree: struct(contents, children)
  elements: any
Contract (-> write template)
  preorder-traverse :: [any] -> procedure?
  list->tree :: list? -> tree?
Docstring
  Given a tree, provides a generator that sequences its elements
  in a certain order (a preorder)
Examples
  '(1 (2 3 4) (5 (6 7)) 8) ->
    '(1 (2 3 4) (5 (6 7)) 8)
    1
    '(2 3 4)
    2
    3
    4
    '(5 (6 7))
    5
    '(6 7)
    6
    7
    8
    (define ss (traverse '(1 (2 3) 4))
    (let loop ([item (ss)])
      (println item)
      (loop (ss)))
    (for ([i (in-producer ss (void))])
      (println i))
Body
Tests
|#

(require "tree.rkt")

(define (list->tree tree-as-list)
  (if (list? tree-as-list)
      (tree tree-as-list
            (map list->tree tree-as-list))
      (tree tree-as-list null)))

(define (tree->list tree)
  (tree-data tree))

;; can use this function to test lazy evaluation
;; of some of the approaches below
(define (cons-list n)
  (let loop ([i 0])
    (if (< i n)
        (list (loop (add1 i)) (loop (add1 i)) (loop (add1 i)))
        empty)))

(module preorder racket/base
  (define (traverse tree)
    (cons (tree-data tree)
          (traverse-children (tree-children tree))))

  ;;; approach #1
  (define (traverse-children children)
    (if (empty? children)
        empty
        (append (traverse (car children))
                (traverse-children (cdr children)))))

  ;;; approach #2
  (define (traverse-children children)
    (apply append
           (map traverse children)))

  ;; combined approach #2
  (define (traverse tree)
    (cons (tree-data tree)
          (apply append
                 (map traverse (tree-children tree)))))

  ;; lazy approach with streams
  (define (traverse tree)
    (stream-cons (tree-data tree)
                 (apply stream-append
                        (map traverse (tree-children tree)))))

  ;; lazy approach with generator
  (define (traverse tree)
    (generator ()
               (yield (tree-data tree))
               (for ([g (in-list (map traverse (tree-children tree)))])
                 (for ([c (in-producer g (void))])
                   (yield c)))))

  (provide (rename-out [traverse preorder-traverse])))

(module postorder racket/base
  (define (traverse tree)
    (stream-append (apply stream-append
                          (map traverse
                               (tree-children tree)))
                   (stream (tree-data tree))))
  (provide (rename-out [traverse postorder-traverse])))

(module inorder racket/base
  ;; assumes each node has either no children or exactly 2 children
  (define (traverse tree)
    (let ([children (tree-children tree)])
      (if (empty? children)
          (stream (tree-data tree))
          (stream-append (traverse (first children))
                         (stream (tree-data tree))
                         (traverse (second children))))))
  (provide (rename-out [traverse inorder-traverse])))

(module levelorder racket/base
  ;; approach #1: queue + lists
  (define (traverse tree)
    (let loop ([result null]
               [queue (list tree)])
      (if (empty? queue)
          result
          (let ([current-node (first queue)]
                [remaining-nodes (rest queue)])
            (loop (append result
                         (list (tree-data current-node)))
                 (append remaining-nodes
                          (tree-children current-node)))))))

  ;; naive lazy approach #2 using streams, converting all
  ;; list references to streams. This WILL NOT do what we think
  ;; since the stream is returned at the tail, i.e. the stream
  ;; must be fully constructed before a single element
  ;; can be returned
  (define (traverse tree)
    (let loop ([result empty-stream]
               [queue (stream tree)])
      (if (stream-empty? queue)
          result
          (let ([current-node (stream-first queue)]
                [remaining-nodes (stream-rest queue)])
            (loop (stream-append result
                                 (stream (tree-data current-node)))
                  (stream-append remaining-nodes
                                 (in-list (tree-children current-node))))))))

  ;; properly lazy version. Stream is constructed at the head
  ;; so it can be lazily evaluated, one value at a time
  (define (traverse tree)
    (let loop ([queue (list tree)])
      (if (empty? queue)
          empty-stream
          (let ([current-node (first queue)]
                [remaining-nodes (rest queue)])
            (stream-cons (tree-data current-node)
                         (loop (append remaining-nodes
                                       (tree-children current-node))))))))

  (provide (rename-out [traverse levelorder-traverse])))


(provide preorder-traverse
         postorder-traverse
         inorder-traverse
         levelorder-traverse)
