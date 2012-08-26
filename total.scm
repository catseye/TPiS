;
; total.scm - kernel of totality checker
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Create initial environment from the list of arguments of the lambda
; expression and from the environment of previously gathered critical
; arguments.  Each argument which is found in the critical arguments
; gets a type apropos to its critical nature; arguments that are not
; found in the critical argument environment get the type 'unknown'.
;
(define-total make-initial-args
  (lambda (formals crit-args env)
    (if (acyclic? formals)
      (cond
        ((null? formals)
          env)
        (else
          (let* ((formal (car formals))
                 (rest   (cdr formals))
                 (type   (get-env crit-args formal)))
            (cond
              (type
                (make-initial-args rest crit-args (extend-env env formal type)))
              (else
                (make-initial-args rest crit-args (extend-env env formal 'unknown))))))))))

;
; Create a new 'this' type.
;
(define-total make-this
  (lambda (formals crit-args acc)
    (if (acyclic? formals)
      (cond
        ((null? formals)
          (cons 'this (reverse acc)))
        (else
          (let* ((formal   (car formals))
                 (rest     (cdr formals))
                 (this-arg (or (get-env crit-args formal) 'unknown)))
            (make-this rest crit-args (cons this-arg acc))))))))

;
; Create a new environment that is suited for a base for the
; environments of a set of mutually recursive procedures.
; This environment has one 'this' in it for each procedure.
; These procedures can call each other, and so must be informed
; of each others' critical arguments.
;
(define-total make-mutual-env
  (lambda (proc-names expr-reps env)
    (if (acyclic? proc-names)
      (cond
        ((null? proc-names)
          env)
        (else
          (let* ((proc-name (car proc-names))
                 (expr-rep  (car expr-reps))
                 (formals   (cadr expr-rep)) ; get-lambda-args
                 (crit-args (find-crit expr-rep proc-names))
                 (this      (make-this formals crit-args '()))
                 (new-env   (extend-env env proc-name this)))
            (make-mutual-env (cdr proc-names) (cdr expr-reps) new-env)))))))

;
; A list of built-in functions that, when applied to an acyclic list
; (list or reduced-list type) always result in a reduced-list.
;
(define reducing-procs '(
       car
       cdr
      caar
      cadr
      cdar
      cddr
     caaar
     caadr
     cadar
     caddr
     cdaar
     cdadr
     cddar
     cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
  ))

;
; Determine the type of an expression, with respect to totality analysis.
;
; If the expression is (- foo 1) where foo is a critical natural number,
; the type is a reduced natural number.
;
; If the expression is (cdr foo) where foo is a critical list, the
; type is a reduced list.
;
(define-total get-expr-type
  (lambda (expr-rep env)
    (cond
      ((is-call? expr-rep)
        (let* ((proc-name (car expr-rep))
               (proc-args (cdr expr-rep))
               (proc-type (get-env env proc-name)))
          (cond
            ((eq? proc-name '-)
              (cond
                ((and (eq? (get-env env (car proc-args)) 'crit-nat)
                      (eq? (cadr proc-args) 1))
                  'reduced-nat)
                (else
                  'unknown)))
            ((memv proc-name reducing-procs)
              (cond
                ((or (eq? (get-env env (car proc-args)) 'crit-list)
                     (eq? (get-env env (car proc-args)) 'reduced-list))
                  'reduced-list)
                (else
                  'unknown)))
            (else
              'unknown))))
      (else
        'unknown))))

;
; When encountering a self-recursive call, check that each passed
; argument coincides with an identified critical argument.
;
(define-total check-recurse-args
  (lambda (actuals formal-types env)
    (if (acyclic? actuals)
      (cond
        ((null? actuals)
          #t)
        (else
          (let* ((actual      (car actuals))
                 (actual-type (get-env env actual))
                 (formal-type (car formal-types)))
            (cond
              ((and (eq? formal-type 'crit-nat) (not (eq? actual-type 'reduced-nat)))
                #f)
              ((and (eq? formal-type 'crit-list) (not (eq? actual-type 'reduced-list)))
                #f)
              (else
                (check-recurse-args (cdr actuals) (cdr formal-types) env)))))))))

;
; Create a new abstract environment from the existing one, in the context
; of the abstract execution of the body of the let.
;
(define-total make-let-env
  (lambda (names vals orig-env new-env)
    (if (acyclic? names)
      (cond
        ((null? names)
          (merge-envs orig-env new-env))
        (else
          (let* ((first-name (car names))
                 (rest-names (cdr names))
                 (first-val  (car vals))
                 (rest-vals  (cdr vals))
                 (first-type (get-expr-type first-val orig-env))
                 (new-env    (extend-env new-env first-name first-type)))
            (cond
              (first-type
                (make-let-env rest-names rest-vals orig-env new-env))
              (else
                #f))))))))

(definerec-total (

;
; Determine if all bindings in a let-expression are total.
;
(bindings-total?
  (lambda (bindings env)
    (if (acyclic? bindings)
      (cond
        ((null? bindings)
          #t)
        (else
          (let* ((binding    (car bindings))
                 (rest       (cdr bindings))
                 (name       (car binding))
                 (val        (cadr binding)))
            (cond
              ((expr-total? val env)
                (bindings-total? rest env))
              (else
                #f))))))))

;
; Given a canonicalized representation of an expression and a type
; environment, return #t if we can establish that this expression is total
; (always terminates for any input,) or #f if we cannot establish that.
;
(expr-total?
  (lambda (expr-rep env)
    (if (acyclic? expr-rep)
      (cond
        ;
        ; Branch never taken but convinces analysis that we're total.
        ;
        ((null? expr-rep)
          #t)
        ;
        ; a) If we see a 'let', we abstractly evaluate each of its
        ;    initial values in the current environment; then we
        ;    create a new environment where each name is bound to its
        ;    newly associated (abstract) value, and abstractly evaluate
        ;    the body in the new environment.
        ;
        ((is-let? expr-rep)
          (let* ((bindings  (cadr expr-rep))
                 (names     (bindings->names bindings))
                 (vals      (bindings->values bindings))
                 (body-rep  (caddr expr-rep))
                 (new-env   (make-let-env names vals env '())))
            (cond
              (new-env
                (and (bindings-total? bindings env)
                     (expr-total? body-rep new-env)))
              (else
                (display expr-rep)
                #f))))
        ;
        ; b) If we see an 'if', we abstractly evaluate the test
	;    and both branches; the procedure is only total if
	;    all of these are total.
        ;
        ((is-if? expr-rep)
          (let* ((test-expr-rep (cadr expr-rep))  ; get-test-expr
                 (then-expr-rep (caddr expr-rep)) ; get-then-expr
                 (else-expr-rep (cadddr expr-rep)) ; get-else-expr
		 (test-total    (expr-total? test-expr-rep env))
                 (then-total    (expr-total? then-expr-rep env))
                 (else-total    (expr-total? else-expr-rep env)))
            (and test-total then-total else-total)))
        ;
        ; c) A sequence is only total if all of its constituent
        ;    expressions are total.
        ;
        ((is-begin? expr-rep)
          (let* ((first-expr (cadr expr-rep))     ; get-begin-first
                 (second-expr (caddr expr-rep)))  ; get-begin-second
          (and (expr-total? first-expr env)
               (expr-total? second-expr env))))
        ;
        ; d) If we see a call to a procedure, we insist that the
        ;    called procedure must be total as well.  There are three
        ;    sub-cases to consider here:
        ;
        ((is-call? expr-rep)
          (let* ((proc-name (car expr-rep))
                 (proc-type (get-env env proc-name))
                 (proc-args (cdr expr-rep)))
            (cond
              ;
              ; d.1) It's a call to a function that we already know is total.
              ;
              ((eq? proc-type 'total-proc)
                #t)
              ;
              ; d.2) It's a recursive call - either self-recursive or
              ;      to one of any number of mutually recursive procedures.
              ;      In either case, we must check that the arguments being
              ;      passed to the critical arguments of the procedure are
              ;      those that monotonically decrease.
              ;
              ((is-recursable? proc-type)
                (check-recurse-args proc-args (this-args proc-type) env))
              ;
              ; d.3) It's a call to a function that we know nothing about.
              ;      Perhaps it is an external function which was not
              ;      given in the list of known-total functions, or perhaps
              ;      it was passed to this function as a parameter.  Either
              ;      way, because of this call, we must assume that our
              ;      function might not be total.
              ;
              (else
                #f))))
        ;
        ; e) Otherwise, the expression is an atom, number, lambda, or
        ;    other primitive, and as such, is certainly total.
        ;
        (else
          #t)))))

))

;
; Given the name of a procedure, a canonicalized, syntax-checked S-expression
; representing that procedure, and a type environment, return #t if we can
; establish that this procedure is total (always terminates for any input,)
; or #f if we cannot establish that.
;
; We create a new environment from the given environment, which we use while
; checking the representation of the procedure.  In this new environment,
; each of the arguments of the procedure is associated with its type, and
; the name of the procedure is associated with a 'this' type, which knows
; the types that each of the arguments should be.
;
(define-total proc-total?
  (lambda (proc-name expr-rep env)
    (cond
      ((is-lambda? expr-rep)
        (let* ((crit-args (find-crit expr-rep (list proc-name)))
               (body-rep  (caddr expr-rep)) ; get-canonicalized-lambda-body
               (formals   (cadr expr-rep))  ; get-lambda-args
               (inner-env (make-initial-args formals crit-args '()))
               (this      (make-this formals crit-args '()))
               (outer-env (extend-env env proc-name this))
               (new-env   (merge-envs outer-env inner-env)))
          (expr-total? body-rep new-env)))
      (else
        #f))))

;
; Given a set of (possibly mutually recursive) procedures, determine if
; they are all total.  First form the mutual environment, then call
; each-proc-total? on the list of procedures, with that environment.
;
(define procs-total?
  (lambda (proc-names expr-reps env)
    (let* ((canon-expr-reps (canonicalize-all expr-reps env '()))
           (mutual-env      (make-mutual-env proc-names canon-expr-reps (make-empty-env))))
      (each-proc-total? proc-names canon-expr-reps (merge-envs env mutual-env)))))

;
; Do the actual work required by procs-total?.
;
(define-total each-proc-total?
  (lambda (proc-names expr-reps env)
    (if (acyclic? expr-reps)
      (cond
        ((null? expr-reps)
          #t)
        (else
          (let* ((proc-name (car proc-names))
                 (expr-rep  (car expr-reps))
                 (body-rep  (caddr expr-rep)) ; get-canonicalized-lambda-body
                 (formals   (cadr expr-rep))  ; get-lambda-args
                 (crit-args (find-crit expr-rep proc-names))
                 (inner-env (make-initial-args formals crit-args '()))
                 (new-env   (merge-envs env inner-env)))
            (and
              (expr-total? body-rep new-env)
              (each-proc-total? (cdr proc-names) (cdr expr-reps) env))))))))
