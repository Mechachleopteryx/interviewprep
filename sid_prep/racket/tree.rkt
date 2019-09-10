#lang racket

(struct tree (data children)
  #:transparent)

(struct binary-tree (data left right)
  #:transparent)

(provide tree
         binary-tree)
