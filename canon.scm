;
; canon.scm - canonicalization of Scheme procedure S-expressions
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

(define forbidden-syntax '(
    set!
    letrec
    do
    delay
    let-syntax
    letrec-syntax
    syntax-rules
    define
    define-syntax
  ))

;
; Create the environment that the body of a let* statement would
; be evaluated with.
;
(define-total make-let*-env
  (lambda (names env)
    (if (acyclic? names)
      (cond
        ((null? names)
          env)
        (else
          (let* ((first   (car names))
                 (rest    (cdr names))
                 (new-env (extend-env env first 'defined)))
            (make-let*-env rest new-env)))))))
  
(definerec-total (

;
; Canonicalize the given S-expression representing a Scheme procedre.
; That is, return a new S-exp which is equivalent to the given S-exp,
; but "de-sugared" so that it is simpler and so we can more easily
; work with it during program analysis.
;
; Aborts with an error, as a side-effect, if there is invalid syntax
; in the procedure, that is, syntax which does not appear in the
; subset of Scheme which we can successfully canonicalize.
;
(canonicalize
  (lambda (expr-rep env)
    (if (acyclic? expr-rep)
      (cond
        ((null? expr-rep)  ; for totality-checker's benefit
          expr-rep)
        ((pair? expr-rep)
          (cond
	    ;
	    ; If it is defined in the environment, it might be a
	    ; local variable which we are calling, in which case
	    ; we must override the default meaning of the symbol.
	    ; (This code is a copy of the is-call? code below.)
	    ;
	    ((and (symbol? (car expr-rep)) (get-env env (car expr-rep)))
              (let* ((func-ref-rep (canonicalize (car expr-rep) env))
                     (args-rep     (cdr expr-rep))
                     (pair         (canonicalize-func-args args-rep env '() '()))
                     (new-bindings (car pair))
                     (new-args     (cdr pair))
                     (new-call-rep (cons func-ref-rep new-args)))
                (cond
                  ((null? expr-rep)
                    expr-rep)
                  ((null? new-bindings)
                    expr-rep)
                  (else
                    (list 'let new-bindings new-call-rep)))))	      
            ;
            ; 'if' with no else-branch is given an explicit undefined else branch.
            ;
            ((is-if? expr-rep)
              (let* ((test-expr (cadr expr-rep))     ; get-test-expr
                     (then-expr (caddr expr-rep)))   ; get-then-expr
                (if (> (length expr-rep) 3)
                    (let* ((else-expr (cadddr expr-rep))) ; get-else-expr
                      (list 'if (canonicalize test-expr env)
                                (canonicalize then-expr env)
                                (canonicalize else-expr env)))
                    (list 'if (canonicalize test-expr env)
                              (canonicalize then-expr env)
                              ''undefined))))
            ((is-cond? expr-rep)
              (canonicalize-cond-branches (cdr expr-rep) env)) ; get-cond-branches
	    ;
	    ; XXX TODO: disallow named let
	    ;
            ((is-let? expr-rep)
              (let* ((bindings     (cadr expr-rep))  ; get-let-bindings
                     (body-rep     (cddr expr-rep))  ; get-let-body
		     (names        (bindings->names bindings))
                     (new-bindings (canonicalize-bindings bindings env '()))
                     (new-env      (extend-env-many env (names->env names 'defined (make-empty-env))))
                     (new-body     (canonicalize-begin body-rep new-env)))
                (list 'let new-bindings new-body)))
            ((is-let*? expr-rep)
              (let* ((bindings   (cadr expr-rep))   ; get-let-bindings
                     (body       (cddr expr-rep))  ; get-let-body
                     (names      (bindings->names bindings))
                     (inner-env  (make-let*-env names env))
                     (canon-body (canonicalize-begin body inner-env)))
                (canonicalize-let* bindings canon-body env)))
            ;
            ; Canonicalizing a lambda expression doesn't make any significant
            ; structural changes by itself, but it does modify the environment.
            ;
            ((is-lambda? expr-rep)
              (let* ((args         (cadr expr-rep))           ; get-lambda-args
                     (body-rep     (cddr expr-rep))           ; get-lambda-body
                     (new-bindings (names->env args 'defined (make-empty-env)))
                     (new-env      (extend-env-many env new-bindings))
                     (new-body     (canonicalize-begin body-rep new-env)))
                (list 'lambda args new-body)))
            ((is-begin? expr-rep)
              (canonicalize-begin (cdr expr-rep) env)) ; get-begin-list
            ;
            ; Evaluation of parameters inside a function call are brought outside the
            ; function call, i.e. (foo (bar baz)) -> (let ((x (bar baz))) (func x))
            ; This only brings out nontrivial (non-symbol, int etc) parameters.
            ;
            ((is-call? expr-rep)
              (let* ((func-ref-rep (canonicalize (car expr-rep) env))
                     (args-rep     (cdr expr-rep))
                     (pair         (canonicalize-func-args args-rep env '() '()))
                     (new-bindings (car pair))
                     (new-args     (cdr pair))
                     (new-call-rep (cons func-ref-rep new-args)))
                (cond
                  ((null? expr-rep)
                    expr-rep)
                  ((null? new-bindings)
                    expr-rep)
                  (else
                    (list 'let new-bindings new-call-rep)))))
            ((memv (car expr-rep) forbidden-syntax)
              #f)
            (else
              expr-rep)))
        (else
          expr-rep)))))

;
; 'cond' is transformed into a series of nested 'if's.
;
(canonicalize-cond-branches
  (lambda (branch-reps env)
    (if (acyclic? branch-reps)
      (cond
        ((null? branch-reps)
          ''undefined)
        (else
          (let* ((first-branch     (car branch-reps))
                 (rest-of-branches (cdr branch-reps))
                 (first-test       (car first-branch))
                 (first-result     (cdr first-branch))
                 (canon-result     (canonicalize-begin first-result env)))
            (cond
              ((eq? first-test 'else)
                canon-result)
              (else
                (list 'if
                      (canonicalize first-test env)
                      canon-result
                      (canonicalize-cond-branches rest-of-branches env))))))))))

(canonicalize-bindings
  (lambda (bindings env acc)
    (if (acyclic? bindings)
      (cond
        ((null? bindings)
          (reverse acc))
        (else
          (let* ((binding     (car bindings))
                 (rest        (cdr bindings))
                 (name        (car binding))
                 (value       (cadr binding))
                 (new-value   (canonicalize value env))
                 (new-binding (list name new-value)))
            (canonicalize-bindings rest env (cons new-binding acc))))))))

;
; 'let*' is transformed into a series of nested 'let's.
; Note that body-rep is given to us pre-canonicalized.
;
(canonicalize-let*
  (lambda (bindings body-rep env)
    (if (acyclic? bindings)
      (cond
        ((null? bindings)
          body-rep)
        (else
          (let* ((binding   (car bindings))
                 (rest      (cdr bindings))
                 (name      (car binding))
                 (value     (cadr binding))
                 (canon-val (canonicalize value env))
                 (new-env   (extend-env env name '())))
            (list 'let
                  (list (list name canon-val))
                  (canonicalize-let* rest body-rep env))))))))

;
; The list of expressions contained inside a 'begin' expression
; is transformed into a series of nested, two-statement 'begin's.
;
(canonicalize-begin
  (lambda (exprs env)
    (if (acyclic? exprs)
      (cond
        ((null? exprs)
          ''undefined)
        ((null? (cdr exprs))
          (canonicalize (car exprs) env))
        (else
          (let* ((first (canonicalize (car exprs) env))
                 (rest  (canonicalize-begin (cdr exprs) env)))
            (list 'begin first rest)))))))

(canonicalize-func-args
  (lambda (args env acc-let acc-args)
    (if (acyclic? args)
      (cond
        ((null? args)
          (cons (reverse acc-let) (reverse acc-args)))
        ((list? (car args))
          (let* ((canonicalized-arg (canonicalize (car args) env))
                 (new-name          (get-fresh-name env))
                 (new-env           (extend-env env new-name '()))
                 (new-acc-let       (cons (list new-name canonicalized-arg) acc-let))
                 (new-acc-args      (cons new-name acc-args)))
            (canonicalize-func-args (cdr args) new-env new-acc-let new-acc-args)))
        (else
          (canonicalize-func-args (cdr args) env acc-let (cons (car args) acc-args)))))))

))

;
; Given a list of S-expressions representing Scheme expressions, return
; a corresponding list of canonicalized expressions.
;
(define-total canonicalize-all
  (lambda (expr-reps env acc)
    (if (acyclic? expr-reps)
      (cond
        ((null? expr-reps)
          (reverse acc))
        (else
          (let* ((expr-rep       (car expr-reps))
                 (rest           (cdr expr-reps))
                 (canon-expr-rep (canonicalize expr-rep env))
                 (new-acc        (cons canon-expr-rep acc)))
            (canonicalize-all rest env new-acc)))))))
