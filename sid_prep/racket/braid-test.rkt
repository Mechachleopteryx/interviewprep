#lang racket/base

(require rackunit
         "braid.rkt")

(test-case
    "Combine test"

  (check-pred null?
              (combine (list '() '() '())))

  (check-pred null?
              (combine null))

  (check equal?
         (combine (list '(1) '(2) '(3)))
         '(1 2 3))

  (check equal?
         (combine (list '(1 2) '(3)))
         '(1 3 2))

  (check equal?
         (combine (list '(1 2) '()))
         '(1 2))

  (check equal?
         (combine (list '(1 2 3) '(4 5 6) '(7 8 9)))
         '(1 4 7 2 5 8 3 6 9))

  (check equal?
         (combine (list '(1 2 3) '(4 5) '(6) '()))
         '(1 4 6 2 5 3)))

(test-case
    "Divide test"

  (check equal?
         (divide null 1)
         '(()))

  (check equal?
         (divide null 2)
         '(() ()))

  (check equal?
         (divide '(1) 1)
         '((1)))

  (check equal?
         (divide '(1) 2)
         '((1) ()))

  (check equal?
         (divide '(1 2) 1)
         '((1 2)))

  (check equal?
         (divide '(1 2) 2)
         '((1) (2)))

  (check member
         (divide '(1 2 3) 2)
         (list '((1 2) (3))
               '((1) (2 3))))

  (check equal?
         (divide '(1 2 3) 3)
         '((1) (2) (3)))

  (check equal?
         (divide '(1 2 3) 4)
         '((1) (2) (3) ()))

  (check equal?
         (divide '(1 2 3 4) 2)
         '((1 2) (3 4)))

  (check member
         (divide '(1 2 3 4) 3)
         (list '((1) (2) (3 4))
               '((1 2) (3) (4))
               '((1) (2 3) (4)))))
