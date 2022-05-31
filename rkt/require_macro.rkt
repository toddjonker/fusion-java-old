#lang racket

(provide (all-defined-out))



(define-for-syntax (capturing_syntax stx datum)
  ; Transforms datum into syntax objects, using lexical context from
  ; stx which should be the original syntax sexp.
  (datum->syntax stx datum))


; WRONG: Bindings are introduced using the lexical context of the macro definition.
(define-syntax require_grain_naive
  (lambda (stx)
    (quote-syntax (require "grain.rkt"))))

; WRONG: Copies the original context onto just the `require` identifier.
(define-syntax require_grain_wrong
  (lambda (stx)
    (quasisyntax ((unsyntax (capturing_syntax stx (quote require))) "grain.rkt"))))

; RISKY: Copies the original context onto the entire expansion.
;        Will fail if require isn't right at point of use.
(define-syntax require_grain_risky
  (lambda (stx)
    (capturing_syntax stx (quote (require "grain.rkt")))))

; CORRECT: Copies the original context onto the module path.
(define-syntax require_grain_best
  (lambda (stx)
    (quasisyntax (require (unsyntax (capturing_syntax stx "grain.rkt"))))))
