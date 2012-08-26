;
; boot1.scm - first stage bootstrap for define[rec]-total
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; These macros are used in the first stage of bootstrapping
; to provide "reasonable" semantics for define-total before
; total procedure checking is actually defined.  i.e. it
; allows the procedure to be defined, without actually checking
; it, on the assumption that a future bootstrapping pass will
; do that.  ("This is Fake D.I.Y.")
;
; Note that these do not affect known-total-env in any way.
;
(define-syntax define-total
  (syntax-rules ()
    ((define-total proc-name lambda-expr)
      (begin
        (define proc-name lambda-expr)
        (display "WARNING: assuming ") (display (quote proc-name)) (display " is total for now, ")
        (display "please re-load file with real define-total later") (newline)))))

(define-syntax definerec-total
  (syntax-rules ()
    ((definerec-total ((name val) ...))
      (definerec-total "split" (name ...) (val ...) () ()))
    ((definerec-total "split" (name1 name2 ...) (val1 val2 ...) nacc vacc)
      (definerec-total "split" (name2 ...) (val2 ...) (name1 . nacc) (val1 . vacc)))
    ((definerec-total "split" () () nacc vacc)
      (definerec-total "define" nacc vacc))
    ((definerec-total "define" (name1 name2 ...) (val1 val2 ...))
      (begin
        (define-total name1 val1)
        (definerec-total "define" (name2 ...) (val2 ...))))
    ((definerec-total "define" () ())
      (display ""))))
