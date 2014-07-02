;; Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

#lang racket

(require rackunit)
(provide (all-defined-out))


(define === equal?)

(define-syntax lets                (make-rename-transformer #'let*))
(define-syntax syntax_unwrap       (make-rename-transformer #'syntax-e))
(define-syntax quote_syntax        (make-rename-transformer #'quote-syntax))
(define-syntax syntax_property     (make-rename-transformer #'syntax-property))
(define-syntax syntax_track_origin (make-rename-transformer #'syntax-track-origin))

(define size length)
(define pair cons)
(define head car)
(define tail cdr)

(define-syntax check_same (make-rename-transformer #'check-eq?))
