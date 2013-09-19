;; Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

(require rackunit)

(define (free_ref)
  free)

(check-exn exn:fail?
  (lambda ()
    (eval
      '(free_ref))))   ; ERROR

(define free "bird")
(check-eq? "bird" (free_ref))

(define free "ball")
(check-eq? "ball" (free_ref))

(module M racket
  (define free "money")
  (provide free))

(require 'M)

(check-eq? "money" free)
(check-eq? "ball" (free_ref))

(define free "beer")
(check-eq? "beer" free)
(check-eq? "beer" (free_ref))

(printf "success~n")
