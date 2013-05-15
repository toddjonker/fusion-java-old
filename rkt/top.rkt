;; Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

(require rackunit)

(define my_top "at-top")
(check-eq? (#%top . my_top) "at-top")


(let [(my_top "shadow")]
  (check-eq? (#%top . my_top) "at-top"))


(check-exn exn:fail?
  (lambda ()
    (eval
      '(module M racket
         (define my_top "in-M")
         (let [(my_top "shadow")]
           (#%top . my_top))))))   ; ERROR


(module M racket
  (require rackunit)
  (define my_top "in-M")
  (check-eq? (#%top . my_top) "in-M")

  ;; Here the #%top finds the "real" top-level definition,
  ;; despite having different marks.
  (define-syntax define_accessor
    (syntax-rules ()
      [(_ name)
       (define (name)
         (#%top . my_top))]))

  ;; Here the %top will find the accompanying definition
  ;; but that defn won't interfere with the "real" top-level
  ;; my_top because it has different marks.
  (define-syntax define_accessor2
    (syntax-rules ()
      [(_ name)
       (begin
         (define my_top "another one")
         (define (name)
           (#%top . my_top)))]))
  (provide define_accessor define_accessor2))

(require 'M)

; Macro-introduced #%top causes access to top-level, not module-level.
(define_accessor a)
(check-eq? (a) "at-top")
(check-eq? my_top "at-top")
(check-eq? (#%top . my_top) "at-top")

(define_accessor2 a2)
(check-eq? (a2) "another one")
(check-eq? my_top "at-top")
(check-eq? (#%top . my_top) "at-top")

(printf "success~n")
