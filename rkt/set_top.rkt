"Copyright (c) 2016 Amazon.com, Inc.  All rights reserved."

(require rackunit "fusion.rkt")


"============================================================================="
"Attempted mutation of imported bindings"

(module M1 racket
  (define m1_bound "in M1")
  (provide m1_bound))
(require 'M1)

(check-exn exn:fail:syntax?
  (lambda ()
    (eval '(set m1_bound "outside"))))
(check === "in M1" m1_bound)
