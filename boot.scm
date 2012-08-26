;
; boot.scm - bootstrap driver
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Bootstrapping process for asserting that the totality checker
; is itself total.
;
(load "std.scm")

(load "boot1.scm")

(load "util.scm")
(load "env.scm")
(load "canon.scm")
(load "crit.scm")
(load "total.scm")

(display "Re-loading with real definition of define-total") (newline)
(load "boot2.scm")

(load "util.scm")
(load "env.scm")
(load "canon.scm")
(load "crit.scm")
(load "total.scm")

(load "boot3.scm")

;; Define-total.

(define-total fac
  (lambda (n)
    (if (>= n 0)
      (cond
        ((zero? n)
          1)
        (else
          (* n (fac (- n 1))))))))
;
; This should be an error.
;

;(define-total ever1
;  (lambda (x)
;     (cond
;       ((eq? x 10000)
;         x)
;       (else
;         (ever1 (+ x 1))))))

(display (fac 5)) (newline)
