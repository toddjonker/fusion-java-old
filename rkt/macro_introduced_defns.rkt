;; Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

(require rackunit)


;; Here we are making sure that the macro-introduced binding `c` is visible
;; to other code from the macro.
(define-syntax define_c
  (lambda (stx)
    (quasisyntax
      (define (c) c))))
(define_c)

;; Similar, but within a module.  Here `d` is unbound where it occurs.
(module M1 racket
  (define-syntax define_d
    (lambda (stx)
      (quasisyntax
        (define (d) d))))
  (provide define_d))
(require 'M1)
(define_d)


;;============================================================================


(define top "outside")

;; Macro that introducing a binding with "private" identifier.
(define-syntax define_top
  (lambda (stx)
    (quasisyntax
      (define top (unsyntax (cadr (syntax-e stx)))))))
(define_top "macro1")
(check-eq? top "outside")


;; Similar for macro exported from a module
(module M2 racket
  (define-syntax define_top
    (lambda (stx)
      (quasisyntax
        (begin
          (define top (unsyntax (cadr (syntax-e stx))))
          (define (unsyntax (caddr (syntax-e stx))) (lambda () top))))))
   (provide define_top))
(require 'M2)
(define_top "macro2" get_top_2)
(check-eq? top "outside")
(check-eq? (get_top_2) "macro2")


;; Now try using the macro at module level
(module M3 racket
  (require 'M2 rackunit)
  (define top "in_M3")
  (define_top "macro_in1" get_top_in1)
  (check-eq? top "in_M3")
  (check-eq? (get_top_in1) "macro_in1")
  (define_top "macro_in2" get_top_in2)
  (check-eq? top "in_M3")
  (check-eq? (get_top_in2) "macro_in2"))
(require 'M3)

(printf "success~n")
