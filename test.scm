;
; test.scm - simple test suite
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

(load "std.scm")
(load "boot1.scm")
(load "util.scm")
(load "env.scm")
(load "canon.scm")
(load "crit.scm")
(load "total.scm")

;
; Definitions (as S-expressions) of procedures that we will test.
;

(define simple-proc
  '(lambda (a b)
     (if (> a b) (* 2 a) (* 3 b)))
)

(define unsafe-fac-proc
  '(lambda (n)
     (cond
       ((zero? n)
         1)
       (else
         (* n (fac (- n 1))))))
)

(define fac-proc
  '(lambda (n)
     (if (>= n 0)
       (cond
         ((zero? n)
           1)
         (else
           (* n (fac (- n 1)))))
       0))
)

(define unsafe-anbncn-proc
  '(lambda (l k)
     (cond
       ((null? l)
         (eq? k 0))
       ((eq? (car l) 'a)
         (anbncn (cdr l) (+ k 1)))
       ((eq? (car l) 'b)
         (anbncn (cdr l) (+ k 1)))
       ((eq? (car l) 'c)
         (anbncn (cdr l) (- k 2)))
       (else
         (anbncn (cdr l) k))))
)

(define anbncn-proc
  '(lambda (l k)
     (if (acyclic? l)
       (cond
         ((null? l)
           (eq? k 0))
         ((eq? (car l) 'a)
           (anbncn (cdr l) (+ k 1)))
         ((eq? (car l) 'b)
           (anbncn (cdr l) (+ k 1)))
         ((eq? (car l) 'c)
           (anbncn (cdr l) (- k 2)))
         (else
           (anbncn (cdr l) k)))))
)

(define ack-proc
  '(lambda (x y)
     (if (and (>= x 0) (>= y 0))
         (cond
           ((zero? x)
             (+ y 1))
           ((zero? y)
             (ack (- x 1) 1))
           (else
             (ack (- x 1) (ack x (- y 1)))))))
)

(define ever1-proc
  '(lambda (x)
     (cond
       ((eq? x 10000)
         x)
       (else
         (ever1 (+ x 1)))))
)

;
; The tests proper.
;

;; Tests for acyclicity.

(test acyclic-1
  (acyclic? '(1 2 3 4 5))
  #t
)

(test acyclic-2
  (acyclic? '(1 2 (a b c) 3 4 5))
  #t
)

(test acyclic-3
  (let* ((tail '(5 . TBA))
         (lst  (append '(1 2 3 4) tail)))
    (set-cdr! tail lst)
    (acyclic? lst))
  #f
)

(test acyclic-4
  (let* ((tail      '(5 . TBA))
         (inner-lst  (append '(1 2 3 4) tail))
         (outer-lst  (list 'a 'b inner-lst 'c 'd)))
    (set-cdr! tail inner-lst)
    (acyclic? outer-lst))
  #f
)

;; Tests for canonicalization.

(test canonicalize-simple
  (canonicalize simple-proc '())
  '(lambda (a b)
     (if (> a b)
       (* 2 a)
       (* 3 b)))
)

(test canonicalize-fac
  (canonicalize fac-proc '())
  '(lambda (n)
     (if (>= n 0)
       (if (zero? n)
         1
         (let ((z
                (let ((z (- n 1))) (fac z))
              ))
           (* n z)))
       0))
)

(test canonicalize-ack
  (canonicalize ack-proc '())
  '(lambda (x y)
     (if (let ((z (>= x 0)) (zz (>= y 0))) (and z zz))
         (if (zero? x)
             (+ y 1)
             (if (zero? y)
                 (let ((z (- x 1)))
                   (ack z 1))
                 (let ((z (- x 1))
                       (zz (let ((zz (- y 1))) (ack x zz))))
                   (ack z zz))))
         'undefined))
)

(test canonicalize-let*
  (canonicalize
   '(lambda (x y)
      (let* ((p (+ x y))
             (q (* p p)))
        (display q)))
   '())
  '(lambda (x y)
      (let ((p (+ x y)))
        (let ((q (* p p)))
          (display q))))
)

;; Tests for finding critical arguments.

(test crit-args-simple
  (find-crit (canonicalize simple-proc '()) '(simple))
  '()
)

(test crit-args-fac
  (find-crit (canonicalize fac-proc '()) '(fac))
  '((n . crit-nat))
)

(test crit-args-unsafe-fac
  (find-crit (canonicalize unsafe-fac-proc '()) '(fac))
  '()
)

(test crit-args-anbncn
  (find-crit (canonicalize anbncn-proc '()) '(anbncn))
  '((l . crit-list))
)

(test crit-args-ack
  (find-crit (canonicalize ack-proc '()) '(ack))
  '()
)

(test crit-args-ever1
  (find-crit (canonicalize ever1-proc '()) '(ever1))
  '()
)

;; Tests for determining totality.

(test proc-total-simple
  (proc-total? 'simple (canonicalize simple-proc '()) known-total-env)
  #t
)

(test proc-total-fac
  (proc-total? 'fac (canonicalize fac-proc '()) known-total-env)
  #t
)

(test proc-total-unsafe-fac
  (proc-total? 'fac (canonicalize unsafe-fac-proc '()) known-total-env)
  #f
)

(test proc-total-anbncn
  (proc-total? 'anbncn (canonicalize anbncn-proc '()) known-total-env)
  #t
)

(test proc-total-ack
  (proc-total? 'ack (canonicalize ack-proc '()) known-total-env)
  #f
)

(test proc-total-ever1
  (proc-total? 'ever1 (canonicalize ever1-proc '()) known-total-env)
  #f
)

;; Tests for determining totality of mutually-recursive procedures.

(test mutrec-total-even
  (procs-total? '(even odd) (list
    (canonicalize '(lambda (x)
      (if (>= x 0)
        (cond
          ((zero? x)
            1)
          (else
            (odd (- x 1)))))) '())
    (canonicalize '(lambda (x)
      (if (>= x 0)
        (cond
          ((zero? x)
            0)
          (else
            (odd (- x 1)))))) '()))
    known-total-env)
  #t
)

(test mutrec-total-bad
  (procs-total? '(foo bar) (list
    (canonicalize '(lambda (x y)
      (if (acyclic? x)
        (cond
          ((null? x)
            (bar '(1 2 3 4 5) y))
          (else
            (foo (cdr x) (+ y 1)))))) '())
    (canonicalize '(lambda (x y)
      (if (acyclic? x)
        (cond
          ((null? x)
            (foo '(1 2 3 4 5) y))
          (else
            (bar (cdr x) (+ y 1)))))) '()))
    known-total-env)
  #f
)
