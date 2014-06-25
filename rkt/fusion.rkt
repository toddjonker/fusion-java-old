;; Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

#lang racket

(require rackunit)
(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))


(define RACKET #t)

(define === equal?)

(define-syntax lets (make-rename-transformer #'let*))

(define size length)
(define pair cons)
(define head car)
(define tail cdr)


;; Syntax

(define (datum_to_syntax datum (ctx #f) (loc #f))
  (datum->syntax ctx datum loc))

(define syntax_is_original syntax-original?)

(define-syntax define_syntax       (make-rename-transformer #'define-syntax))
(define-syntax syntax_unwrap       (make-rename-transformer #'syntax-e))
(define-syntax quote_syntax        (make-rename-transformer #'quote-syntax))
(define-syntax syntax_property     (make-rename-transformer #'syntax-property))

(define-syntax syntax_track_origin (make-rename-transformer #'syntax-track-origin))

;; TODO WORKAROUND FUSION-47 Should use interned symbol and remove this.
(define (syntax_origin stx)
  (syntax-property stx 'origin))

(define UNSET_STX_PROP_VALUE #f)


;; Testing

(define-syntax check_true  (make-rename-transformer #'check-true))
(define-syntax check_false (make-rename-transformer #'check-false))
(define-syntax check_same  (make-rename-transformer #'check-eq?))
