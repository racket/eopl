
(module datatype mzscheme

  (require-for-syntax "private/utils.ss")

  (define-struct element (tag fieldvals))

  (define-syntax define-datatype
    (lambda (stx)
      (syntax-case stx ()
	[(_ name pred-name 
	    (variant-name (field-name field-pred) ...) 
	    ...)
	 (begin
	   ;; More syntax checks...
	   (unless (identifier? (syntax name))
	     (raise-syntax-error #f
				 "datatype name is not an indentifier"
				 stx (syntax name)))
	   (unless (identifier? (syntax name))
	     (raise-syntax-error #f
				 "predicate name is not an indentifier"
				 stx (syntax pred-name)))
	   (for-each (lambda (vt fields)
		       (unless (identifier? vt)
			 (raise-syntax-error 
			  'cases 
			  "variant name is not an indentifier"
			  stx vt))
		       (for-each (lambda (field)
				   (unless (identifier? field)
				     (raise-syntax-error 
				      'cases 
				      "field name is not an indentifier"
				      stx field)))
				 (syntax->list fields)))
		     (syntax->list (syntax (variant-name ...)))
		     (syntax->list (syntax ((field-name ...) ...))))
	   ;; Count the fields for each variant:
	   (with-syntax ([(variant-field-count ...) 
			  (map (lambda (n) 
				 (datum->syntax-object (quote-syntax here) n #f))
			       (map length
				    (map
				     syntax->list
				     (syntax->list 
				      (syntax ((field-name ...) ...))))))])
	     (syntax
	      (begin
		(define-syntax name 
		  ;; Note: we're back to the transformer environment, here.
		  ;; Also, this isn't a transformer function, so any direct
		  ;;  use of the name will trigger a syntax error. The name
		  ;;  can be found by `syntax-local-value', though.
		  (make-dt (syntax pred-name)
			   (list
			    (make-vt (syntax variant-name)
				     variant-field-count)
			    ...)))
		;; Bind the predicate and selector functions:
		(define-values (pred-name
				variant-name ...)
		  ;; Create a new structure for the datatype (using the
		  ;; datatype name in `struct', so it prints nicely).
		  (let-values ([(struct:x make-x x? acc mut)
				(make-struct-type 'name struct:element 0 0)])
		    ;; User-available functions:
		    (values
		     x? ;; The datatype predicate
		     ;; Create the constructor functions:
		     (let ([vname (quote variant-name)])
		       (let ([variant-name 
			      (lambda (field-name ...)
				(unless (field-pred field-name)
				  (error vname 
					 "bad value for ~a field"
					 (quote field-name)))
				...
				(make-x vname (list field-name ...)))])
			 variant-name))
		     ...)))))))])))
  
  (define-syntax cases
    (lambda (stx)
      (syntax-case stx ()
	[(_ datatype expr 
	    clause
	    ...)
	 ;; Get datatype information:
	 (let ([dt (syntax-local-value (syntax datatype) (lambda () #f))])
	   (unless (dt? dt)
	     (raise-syntax-error 
	      'cases 
	      "not a datatype name" 
	      stx
	      (syntax datatype)))
	   
	   ;; Parse clauses:
	   (let-values ([(orig-variants field-idss bodys else-body)
			 (let loop ([clauses (syntax->list (syntax (clause ...)))][saw-cases null])
			   (cond
			    [(null? clauses)
			     (values null null null #f)]
			    [else
			     (let ([clause (car clauses)])
			       (syntax-case clause (else)
				 [(variant (field-id ...) body)
				  (let* ([variant (syntax variant)]
					 [orig-variant
					  (ormap (lambda (dtv) 
						   (let ([vt-name (vt-name-stx dtv)])
						     (and (module-identifier=? variant vt-name)
							  vt-name)))
						 (dt-variants dt))])
				    (unless orig-variant
				      (raise-syntax-error 
				       #f
				       (format "not a variant of `~a'"
					       (syntax-object->datum (syntax datatype)))
				       stx
				       variant))

				    (let ([field-ids (syntax->list (syntax (field-id ...)))])
				      (for-each (lambda (fid)
						  (unless (identifier? fid)
						    (raise-syntax-error
						     #f
						     "expected an identifier for a field"
						     stx
						     fid)))
						field-ids)
				      (let ([dtv (variant-assq variant (dt-variants dt))])
					(unless (= (length field-ids)
						   (vt-field-count dtv))
					  (raise-syntax-error
					   #f
					   (format
					    "variant case `~a' for `~a' has wrong field count (expected ~a, found ~a)"
					    (syntax-object->datum variant)
					    (syntax-object->datum (syntax datatype))
					    (vt-field-count dtv)
					    (length field-ids))
					   stx
					   clause)))

				      ;; Check for duplicate local field ids:
				      (let ([dup (check-duplicate-identifier field-ids)])
					(when dup
					  (raise-syntax-error
					   #f
					   "duplicate field identifier"
					   stx
					   dup)))

				      ;; Check for redundant case:
				      (when (memq orig-variant saw-cases)
					(raise-syntax-error
					 #f
					 "duplicate case"
					 stx
					 clause))
				    
				      ;; This clause is ok:
				      (let-values ([(ov idss bodys else)
						    (loop (cdr clauses) (cons orig-variant saw-cases))])
					(values (cons orig-variant ov)
						(cons field-ids idss)
						(cons (syntax body) bodys)
						else))))]
				 [(else body)
				  (begin
				    (unless (null? (cdr clauses))
				      (raise-syntax-error
				       #f
				       "else clause must be last"
				       stx
				       clause))
				    (values null null null (syntax body)))]

				 [_else (raise-syntax-error
					 #f
					 "bad clause"
					 stx
					 clause)]))]))])
	     
	     ;; Create the result:
	     (with-syntax ([pred (dt-pred-stx dt)]
			   [(orig-variant ...) orig-variants]
			   [((field-id ...) ...) field-idss]
			   [(body ...) bodys]
			   [else-body (or else-body
					  (syntax 
					   (error 'cases "no variant case matched")))])
	       (syntax
		(let ([v expr])
		  (if (not (pred v))
		      (error 'case "not a ~a: ~s" 
			     (quote datatype) v)
		      (let* ([t (element-tag v)]
			     [flds (element-fieldvals v)])
			(case t
			  [(orig-variant)
			   (apply
			    (lambda (field-id ...)
			      body)
			    flds)]
			  ...
			  [else else-body]))))))))])))
     
  (define-syntax provide-datatype
    (lambda (stx)
      (syntax-case stx ()
	[(_ datatype)
	 (let ([dt (syntax-local-value (syntax datatype) (lambda () #f))])
	   (unless (dt? dt)
	     (raise-syntax-error
	      #f
	      "not a datatype name" 
	      stx
	      (syntax datatype)))
	   (with-syntax ([pred (dt-pred-stx dt)]
			 [(orig-variant ...) 
			  (map vt-name-stx (dt-variants dt))])
	     (syntax
	      (provide datatype
		       pred
		       orig-variant ...))))])))

  (provide define-datatype cases provide-datatype))
