(module eopl mzscheme
  (require "datatype.ss"
	   "private/sllgen.ss")
  (require-for-syntax "private/slldef.ss")

  (provide define-datatype
	   cases)

  ;; Special def that saves a quoted value at compile time in case
  ;; it's needed for `sllgen:make-define-datatypes':
  (define-syntax (eopl-define stx)
    (syntax-case stx (quote)
      [(_ name (quote def))
       (identifier? (syntax name))
       (begin
	 (hash-table-put! sllgen-def
			  (syntax-e (syntax name))
			  (syntax def))
	 (syntax/loc stx (define name (quote def))))]
      [(_ . rest)
       (syntax/loc stx (define . rest))]))

  (provide (rename eopl-define define))

  (provide (all-from "private/sllgen.ss"))

  (provide error
	   (rename error eopl:error)
	   printf
	   (rename printf eopl:printf))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide always? list-of)

  (define always?
    (lambda (x) #t))
  
  (define list-of
    (lambda (pred . l)
      (let ((all-preds (cons pred l)))
	(lambda (obj)
	  (let loop ((obj obj) (preds '()))
	    (or 
	     ;; if list is empty, preds should be, too
	     (and (null? obj) (null? preds))
	     (if (null? preds)
		 ;; if preds is empty, but list isn't, then recycle
		 (loop obj all-preds)
		 ;; otherwise check and element and recur.
		 (and (pair? obj)
		      ((car preds) (car obj))
		      (loop (cdr obj) (cdr preds))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define empty null)

  (provide time            ;; useful to compare implementations
	   collect-garbage ;; useful with `time'
	   empty)          ;; for constructor-based printing

  ;; Unfortunately, most of the rest is cut-and-pasted from R5RS:

  ;; values
  (provide car cdr caar cadr cdar cddr
	   caaar caadr cadar caddr cdaar cdadr cddar cdddr
	   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	   map = < > <= >= max min + - * / 
	   abs gcd lcm exp log sin cos tan not eq?
	   call-with-current-continuation make-string
	   symbol->string string->symbol make-rectangular 
	   exact->inexact inexact->exact number->string string->number 
	   rationalize output-port? current-input-port current-output-port current-error-port 
	   open-input-file open-output-file close-input-port close-output-port
	   with-output-to-file transcript-on transcript-off flush-output
	   string-length string-ci<=? string-ci>=? string-append 
	   string->list list->string string-fill! 
	   vector-length vector->list list->vector vector-fill!
	   char-alphabetic? char-numeric? char-whitespace? 
	   char-upper-case? char-lower-case? char->integer integer->char char-downcase
	   call-with-output-file call-with-input-file with-input-from-file
	   apply for-each symbol? pair? cons set-car! set-cdr! null? list? list length append reverse
	   list-tail list-ref memq memv member assq assv assoc procedure?
	   number? complex? real? rational? integer? exact? inexact? zero?
	   positive?  negative? odd? even? 
	   quotient remainder modulo floor ceiling truncate round 
	   numerator denominator asin acos atan sqrt
	   expt make-polar real-part imag-part angle magnitude input-port?
	   read read-char peek-char eof-object?
	   char-ready? write display newline write-char load 
	   string? string string-ref string-set! string=? substring string-copy
	   string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
	   vector? make-vector vector vector-ref vector-set! 
	   char? char=? char<? char>? char<=? char>=? 
	   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
	   char-upcase boolean? eqv? equal? force
	   call-with-values values eval port? scheme-report-environment null-environment 
	   interaction-environment dynamic-wind)

  ;; syntax
  (provide quasiquote unquote unquote-splicing 
	   if let and or cond case delay do
	   letrec let* begin lambda quote set!
	   define-syntax

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
	   #%app #%datum #%top 
	   (rename synrule-in-stx-module-begin #%module-begin))

  (define-syntax synrule-in-stx-module-begin
    (lambda (stx)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%plain-module-begin)
	      (quote-syntax 
	       (require-for-syntax (rename mzscheme syntax-rules syntax-rules)))
	      (cdr (syntax-e stx)))
       stx))))
