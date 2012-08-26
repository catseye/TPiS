;
; std.scm - standard environment of known-total procedures.
; Total Procedures in Scheme, May 2006, Chris Pressey
; For license information, see the file LICENSE in this directory.
;

;
; These are mostly Scheme builtins.
;
; We make some assumptions about this list.  The basic assumption
; is that all of these builtins are implemented correctly and are
; thus guaranteed to return.  Another assumption is that those
; builtins that operate on lists (for example, 'length', 'memv',
; 'list->vector', etc) always return, even when given a cyclic list
; (even though R5RS does not specify this behaviour.)  Should this
; assumption fail for some given implementation of some of these
; builtins, those builtins should be removed from this list, and
; if those builtins are used in the totality checker itself
; (notably 'reverse' is used in several places,) "hand-rolled"
; versions that are guaranteed to terminate should be substituted.
;
; Note that this file should NOT be reloaded during bootstrapping.
;
; It might be nice if these were local to define-total, but
; it's more useful for testing purposes if we can access them
; from outside there too.
;
(define orig-known-total-env
  (map (lambda (name) (cons name 'total-proc)) '(
    eqv? eq? equal?
    number? complex? real? rational? integer?
    exact? inexact?
    = < > <= >=
    zero? positive? negative? odd? even? max min
    + * - /
    abs quotient remainder modulo
    gcd lcm
    numerator denominator
    floor ceiling truncate round
    rationalize
    exp log sin cos tan asin acos atan
    sqrt expt
    make-rectangular make-polar real-part imag-part magnitude angle
    exact->inexact inexact->exact
    number->string string->number
    not boolean? and or
    pair? cons car cdr
    ;set-car! set-cdr!            ; no side effects
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    null? list?
    list length append reverse    ; NOTE: can depend on implementation
    list-tail list-ref            ; NOTE: can depend on implementation
    memq memv member              ; NOTE: can depend on implementation
    assq assv assoc               ; NOTE: can depend on implementation
    symbol? symbol->string string->symbol
    char?
    char=? char<? char>? char<=? char>=? 
    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
    char-alphabetic? char-numeric? char-whitespace?
    char-upper-case? char-lower-case?
    char->integer integer->char
    char-upcase char-downcase
    string? make-string string string-length string-ref
    ;string-set!                  ; no side effects
    string=? string-ci=?
    string<? string>? string<=? string>=? 
    string-ci<? string-ci>? string-ci<=? string-ci>=?
    substring string-append
    string->list list->string
    string-copy
    ;string-fill!                 ; no side effects
    vector? make-vector vector vector-length vector-ref
    ;vector-set!                  ; no side effects
    vector->list list->vector
    ;vector-fill!                 ; no side effects
    procedure?
    ;apply                        ; The following builtins call for
    ;map for-each                 ; more complex flow analysis than
    ;force                        ; our analyzer does at present.
    ;call-with-current-continuation values call-with-values dynamic-wind
    ;eval scheme-report-environment null-environment interaction-environment
    ;call-with-input-file call-with-output-file
    input-port? output-port?
    current-input-port current-output-port
    ;with-input-from-file with-output-to-file
    ;open-input-file open-output-file
    ;close-input-port close-output-port
    read read-char peek-char
    eof-object? char-ready?
    write display newline write-char
    ;load transcript-on transcript-off
    acyclic?                      ; part of the analyzer
  )))

;
; This variable changes over time, as define-total and definerec-total
; 'learn' about new user-defined procedures which are also total.
;
(define known-total-env orig-known-total-env)
