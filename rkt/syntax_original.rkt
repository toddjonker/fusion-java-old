;; Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

(require rackunit "fusion.rkt")


(define-syntax if1head3
  (lambda (stx)
    #'(if 1 head 3)))


(load "ftst/syntax_original.rkt.fusion")
