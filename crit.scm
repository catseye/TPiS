;
; crit.scm - critical argument locator
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Given an abstract environment and the test expression of an 'if',
; return the environment which should apply to the 'then' expression
; branch of the 'if'.  For example, if the test is '(zero? n)', our
; new environment is a copy of the given environment, but with the
; name 'n' now bound to 'crit-nat' because n is available as a critical
; natural number argument inside the 'then' branch of this if expression.
;
(define-total make-then-branch-env
  (lambda (env test-expr-rep)
    (cond
      ((is-call? test-expr-rep)
        (let ((proc-name (car test-expr-rep))
              (proc-args (cdr test-expr-rep)))
          (cond
            ((eq? proc-name 'acyclic?)
              (extend-env env (car proc-args) 'list))
            ((and (eq? proc-name '>=) (eq? (cadr proc-args) 0))
              (extend-env env (car proc-args) 'nat))
            ((and (eq? proc-name 'zero?) (eq? (get-env env (car proc-args)) 'nat))
              (extend-env env (car proc-args) 'crit-nat))
            ((and (eq? proc-name 'null?) (eq? (get-env env (car proc-args)) 'list))
              (extend-env env (car proc-args) 'crit-list))
            (else
                env))))
        (else
          env))))

;
; 'Update' the env to reflect the information accrued in args,
; after name changes to variables (let).
;
; OTOH, if renaming has taken place, we need to retrieve the
; bindings and re-associate the types with the previous names,
; aka "back propogate" the types.
;
; XXX still TODO?
;
(define-total back-propogate-crit
  (lambda (lower-env upper-env)
    lower-env))

(define-total merge-crit-envs
  (lambda (env1 env2 acc-env)
    (if (acyclic? env1)
      (cond
        ((null? env1)
          acc-env)
        (else
          (let* ((pair  (car env1))
                 (rest  (cdr env1))
                 (var   (car pair))
                 (type1 (cdr pair))
                 (type2 (get-env env2 var)))
            ;
            ; Note that this glosses over the possibility that the
            ; var is 'crit-nat' in one environment and 'crit-list' in
            ; the other, which, absurd as it is, can happen.
            ;
            (cond
              ((or (eq? type1 'crit-nat) (eq? type2 'crit-nat))
                (merge-crit-envs rest env2 (extend-env acc-env var 'crit-nat)))
              ((or (eq? type1 'crit-list) (eq? type2 'crit-list))
                (merge-crit-envs rest env2 (extend-env acc-env var 'crit-list)))
              (else
                (merge-crit-envs rest env2 acc-env)))))))))

;
; Given the representation and name of a procedure, find its
; critical arguments.  The critical arguments are the arguments
; that take on a predictable final value exactly when the
; procedure returns.
;
; We find critical arguments by first finding critical variables,
; then relating these variables to the arguments they came from.
;
; We find critical variables by creating new environments on
; each branch of a conditional statement, and noticing if a
; variable takes on a final value in any branch that terminates
; (rather than recursing.)
;
(define-total find-crit-expr
  (lambda (expr-rep proc-names env)
    (if (acyclic? expr-rep)
      (cond
        ;
        ; This branch is used to convince the totality checker that
        ; the expr-rep data structure really is well-founded, even
        ; though it will never in practice be taken.
        ;
        ((null? expr-rep)
          env)
        ((is-let? expr-rep)
          (let* ((body-rep   (caddr expr-rep))           ; get-let-body
                 (in-let-env (find-crit-expr body-rep proc-names env)))
            (back-propogate-crit in-let-env env)))
        ;
        ; A variable is critical if it has a critical type in either environment
        ; of an if expression.
        ;
        ; Note that this is the only place where we extend the
        ; environment.
        ;
        ((is-if? expr-rep)
          (let* ((test-expr-rep (cadr expr-rep))   ; get-test-expr
                 (then-expr-rep (caddr expr-rep))  ; get-then-expr
                 (else-expr-rep (cadddr expr-rep)) ; get-else-expr
                 (then-env      (make-then-branch-env env test-expr-rep))
                 (in-then-env   (find-crit-expr then-expr-rep proc-names then-env))
                 (in-else-env   (find-crit-expr else-expr-rep proc-names env))
                 (merged-env    (merge-crit-envs in-then-env in-else-env '())))
            merged-env))
        ;
        ; When we see a 'begin', we know that only the second expression
        ; can establish critical variables, since the first cannot return.
        ;
        ((is-begin? expr-rep)
          (let* ((sub-expr (caddr expr-rep)))  ; get=begin-second
            (find-crit-expr sub-expr proc-names env)))
        ((is-call? expr-rep)
          (let* ((called-proc-name (car expr-rep)))
            (cond
              ((memv called-proc-name proc-names)
                ;
                ; The procedure recursively calls itself here.
                ; Therefore none of the variables in this branch
                ; can be critical variables, so return an empty
                ; environment.
                ;
                '())
              ((not (is-identifier? called-proc-name))
                '())
              (else
                ;
                ; The procedure terminates non-recursively here,
                ; therefore some of the variables in this branch
                ; might be critical variables, so return (i.e.
                ; endorse) the environment we were given.
                ;
                env))))
        (else
          ;
          ; The procedure terminates here too; see above comment.
          ;
          env)))))

;
; Find the critical arguments of a procedure.
;
(define-total find-crit
  (lambda (expr-rep proc-names)
    (cond
      ((is-lambda? expr-rep)
        (let* ((body (caddr expr-rep))) ; get-canonicalized-lambda-body
          (find-crit-expr body proc-names '())))
      (else
        '()))))
