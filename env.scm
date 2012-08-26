;
; env.scm - environment support for abstract interpretation
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; Each environment associates a name with a type.
;
; For canonicalization, we only need to know if a name has been
; defined at all.  In this case, the only type is 'defined'.
; For the termination analysis proper, we need a more refined system:
;
; unknown         a variable which we know nothing about.
;
; list            a variable that we know to be an acyclic list.
;                 (true branch of an 'acyclic-list?' test.)
;
; reduced-list    cdr of an acylic-list.
;
; null            null list.
;
; nat             a variable which has tested for positive integerness.
;
; reduced-nat     a positive-int that has been decremented.
;
; zero            a variable that we know must be zero (true branch
;                 of a 'zero?' test.)
;
; total-proc      a procedure that we know always terminates.
;


(define-total make-empty-env
  (lambda ()
    '()))

(define-total extend-env
  (lambda (env name value)
    (cons (cons name value) env)))

(define-total names->env
  (lambda (names value env)
    (if (acyclic? names)
        (cond
          ((null? names)
            env)
          (else
            (names->env (cdr names) value (extend-env env (car names) value)))))))
  
(define-total extend-env-many
  (lambda (env list-of-bindings)
    (append list-of-bindings env)))

(define-total get-env
  (lambda (env name)
    (let* ((result (assq name env)))
      (cond
        (result
          (cdr result))
        (else
          #f)))))

(define-total get-fresher-name
  (lambda (env acc)
    (if (acyclic? env)
      (cond
        ((null? env)
          acc)
        ((get-env env acc)
          (get-fresher-name (cdr env) (string->symbol
              (string-append (symbol->string acc) (symbol->string (caar env))))))
        (else
          acc)))))

(define-total get-fresh-name
  (lambda (env)
    (get-fresher-name env 'z)))
    
; Entries in env2 override those in env1.
(define-total merge-envs
  (lambda (env1 env2)
    (if (acyclic? env1)
        (cond
          ((null? env1)
            env2)
          ((get-env env2 (caar env1))
            (merge-envs (cdr env1) env2))
          (else
            (merge-envs (cdr env1) (cons (car env1) env2)))))))

(define-total display-env-loop
  (lambda (env)
    (if (acyclic? env)
      (cond
        ((null? env)
         'ok)
        (else
          (let* ((pair (car env))
                 (key  (car pair))
                 (val  (cdr pair)))
            (if (not (get-env orig-known-total-env key))
              (begin (display key) (display " -> ") (display val) (display "; "))
              'whatever)
            (display-env-loop (cdr env))))))))

(define-total display-env
  (lambda (env)
    (display "{")
    (display-env-loop env)
    (display "}")))

;
; There is only one type complex enough to require helper functions -
; the type of the procedure currently being analyzed, called a "this".
; A this tracks the types of all of its arguments.
;

(define-total is-this?
  (lambda (type)
    (and (pair? type) (eq? (car type) 'this))))

(define-total this-args
  (lambda (type)
    (cdr type)))

(define-total all-unknown?
  (lambda (types)
    (if (acyclic? types)
        (cond
          ((null? types)
            #t)
          (else
            (cond
              ((eq? (car types) 'unknown)
                (all-unknown? (cdr types)))
              (else
                #f)))))))

;
; A type is "recursable" if it is a this and at least one of the types
; of its arguments is not "unknown".
;
(define-total is-recursable?
  (lambda (type)
    (and (is-this? type) (not (all-unknown? (this-args type))))))
