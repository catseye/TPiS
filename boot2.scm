;
; boot2.scm - second stage bootstrap for define[rec]-total
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; These macros are used in the second stage of bootstrapping, to
; replace the existing definitions of assumed total procedures with
; genuinely checked total procedures.
;
; Note that these DO affect known-total-env, by adding to it.
;
(define-syntax define-total
  (syntax-rules ()
    ((define-total proc-name lambda-expr)
      (begin
        (display "Redefining ") (display (quote proc-name)) (display " as total procedure") (newline)
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
        (display "Redefining ") (display (quote names)) (display " as total procedures") (newline)
        (cond
          ((not (procs-total? (quote names) (quote exprs) known-total-env))
            (error "procedures could not be determined to be total")))
        (set! known-total-env
          (names->env (quote names) 'total-proc known-total-env))
        (definerec-total "define" names exprs)))
    ((definerec-total "define" (name1 name2 ...) (val1 val2 ...))
      (begin
        (set! name1 val1)
        (definerec-total "define" (name2 ...) (val2 ...))))
    ((definerec-total "define" () ())
      (display ""))))
