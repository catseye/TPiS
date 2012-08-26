;
; boot3.scm - third stage bootstrap for define[rec]-total
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Given a new procedure name and a procedure (lambda) S-expression, define
; a new procedure with this name and this lambda, if and only if it can be
; established that the procedure is total.
;
; This has side-effects; an environment of known-total procedures is kept,
; and if the given procedure turns out to be total, it is added to this.
;
; A new environment is set up which initially associates each argument name
; with a nil value, indicating that we know nothing about it yet.
;
; These are the final, "real" definitions of define-total and definerec-total,
; used in the last stage of the bootstrapping process.
;
(define-syntax define-total
  (syntax-rules ()
    ((define-total proc-name lambda-expr)
      (begin
        (define proc-name 'TBA)
        (let* ((proc-rep (canonicalize (quote lambda-expr) known-total-env)))
          (cond
            ((not (proc-total? (quote proc-name) proc-rep known-total-env))
              (error "procedure could not be determined to be total")))
        (set! known-total-env
          (extend-env known-total-env (quote proc-name) 'total-proc))
        (set! proc-name lambda-expr))))))

(define-syntax definerec-total
  (syntax-rules ()
    ((definerec-total ((name val) ...))
      (definerec-total "split" (name ...) (val ...) () ()))
    ((definerec-total "split" (name1 name2 ...) (val1 val2 ...) nacc vacc)
      (definerec-total "split" (name2 ...) (val2 ...) (name1 . nacc) (val1 . vacc)))
    ((definerec-total "split" () () names exprs)
      (begin
        (cond
          ((not (procs-total? (quote names) (quote exprs) known-total-env))
            (error "procedures could not be determined to be total")))
        (set! known-total-env
          (names->env (quote names) 'total-proc known-total-env))
        (definerec-total "define" names exprs)))
    ((definerec-total "define" (name1 name2 ...) (val1 val2 ...))
      (begin
        (define name1 val1)
        (definerec-total "define" (name2 ...) (val2 ...))))
    ((definerec-total "define" () ())
      (display ""))))
