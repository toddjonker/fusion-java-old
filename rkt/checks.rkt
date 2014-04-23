;#lang racket
(require rackunit)

;(check-true #f)
;(check-false 6)

(define-check (check-aggr arg)
  (check-pred odd? arg "yow"))
(check-aggr 0)

(define-check (check-aggr arg)
  (with-check-info* (list (make-check-name "new-name"))
    (lambda () (check-pred odd? arg "yow"))))
(check-aggr 0)


