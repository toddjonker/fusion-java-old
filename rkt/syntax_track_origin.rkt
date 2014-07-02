;; Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

(require rackunit "fusion.rkt")


;; TODO WORKAROUND FUSION-47 Should use interned symbol and remove this.
(define (syntax_origin stx)
  (syntax-property stx 'origin))

(define UNSET #f)

(load "ftst/syntax_track_origin.rkt.fusion")
