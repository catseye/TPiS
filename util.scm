;
; util.scm - miscellaneous utility procedures
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Determine if a Scheme datum is acyclic.
;
(define acyclic?
  (lambda (pair)
    (acyclic-test pair '())))

(define acyclic-test
  (lambda (pair acc)
    (cond
      ((not (pair? pair))
        #t)
      ((memq pair acc)
        #f)
      (else
        (let ((fst     (car pair))
              (snd     (cdr pair))
              (new-acc (cons pair acc)))
          (and (acyclic-test fst new-acc) (acyclic-test snd new-acc)))))))

;
; XXX explain and/or borrow eopl:error
;
(define-total error
  (lambda (msg)
    (display msg) (newline)
    (read '())))

;
; Test case for test suite.
;
(define-syntax test
  (syntax-rules ()
    ((test test-name expr expected)
      (begin
        (display "Running test: ") (display (quote test-name)) (display "... ")
        (let ((result expr))
          (cond
            ((equal? result expected)
              (display "passed.") (newline))
            (else
              (display "FAILED!") (newline)
              (display "Expected: ") (display expected) (newline)
              (display "Actual:   ") (display result) (newline))))))))

;
; XXX there may be a problem doing NON-TAIL (self-)recursion, check it out.
;
(define-total bindings->names
  (lambda (bindings)
    (if (acyclic? bindings)
       (cond
         ((null? bindings)
           '())
         (else
           (let* ((binding (car bindings))
                  (rest    (cdr bindings))
                  (name    (car binding)))
             (cons name (bindings->names rest))))))))

(define-total bindings->values
  (lambda (bindings)
    (if (acyclic? bindings)
       (cond
         ((null? bindings)
           '())
         (else
           (let* ((binding (car bindings))
                  (rest    (cdr bindings))
                  (value   (cadr binding)))
             (cons value (bindings->values rest))))))))

(define-total is-lambda?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'lambda (car expr-rep)))))

(define-total is-let?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'let (car expr-rep)))))

(define-total is-let*?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'let* (car expr-rep)))))

(define-total is-if?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'if (car expr-rep)))))

(define-total is-cond?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'cond (car expr-rep)))))

(define-total is-begin?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'begin (car expr-rep)))))

(define-total is-call?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (not (eq? (car expr-rep) 'quote)))))  ;... and all others

(define-total is-identifier?
  (lambda (expr-rep)
    (symbol? expr-rep)))

(define-total is-quote?
  (lambda (expr-rep)
    (and
      (pair? expr-rep)
      (eq? 'quote (car expr-rep)))))
