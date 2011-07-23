
(define (Diagram:module-initialize module-name enter-module find-module eval-env )
  
  (define path-sigfun   (Pdot (Pident (ident-create "Signal")) "sigfun"))
  (define sigfun-type   (Tcon path-sigfun '()))
  
  (define ident-diagram   (ident-create "diagram"))
  (define path-diagram    (Pident ident-diagram))
  (define diagram-type    (Tcon path-diagram '()))

  (define ident-pure   (ident-create "pure"))
  (define path-pure    (Pident ident-pure))
  (define pure-type    (Tcon path-pure '()))

  (define-values (type-variables reset-type-variables
				 find-type-variable 
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type bot-type
				 )
    (core-utils))

  (let (
	(sig
	  (list
	   (Type_sig ident-pure (make-typedecl (make-kind 0) #f))
	   (Type_sig ident-diagram (make-typedecl (make-kind 0) #f))

	   (Value_sig (ident-create "IDENTITY") 
		      (make-valtype '() (arrow-type diagram-type diagram-type)))

	   (Value_sig (ident-create "SENSE") 
		      (make-valtype '() (arrow-type (list-type label-type) (arrow-type diagram-type diagram-type))))

	   (Value_sig (ident-create "ACTUATE") 
		      (make-valtype '() (arrow-type (list-type label-type) (arrow-type diagram-type diagram-type))))

	   (Value_sig (ident-create "ASSIGN") 
		      (make-valtype '() (arrow-type (list-type label-type) (arrow-type pure-type diagram-type))))
	   
	   (Value_sig (ident-create "ODE") 
		      (make-valtype '() (arrow-type (list-type sigfun-type) (arrow-type sigfun-type (arrow-type sigfun-type  (arrow-type pure-type diagram-type))))))

	   (Value_sig (ident-create "PURE") 
		      (make-valtype '() (arrow-type sigfun-type pure-type)))
	   
	   (Value_sig (ident-create "GROUP") 
		      (make-valtype '() (arrow-type pure-type (arrow-type pure-type pure-type))))
	   
	   (Value_sig (ident-create "RELATION") 
		      (make-valtype '() (arrow-type label-type (arrow-type label-type (arrow-type sigfun-type (arrow-type pure-type pure-type))))))

	   (Value_sig (ident-create "SEQUENCE") 
		      (make-valtype '() (arrow-type diagram-type (arrow-type diagram-type diagram-type))))
	   
	   (Value_sig (ident-create "UNION") 
		      (make-valtype '() (arrow-type diagram-type (arrow-type diagram-type diagram-type))))

	   (Value_sig (ident-create "TRANSITION") 
		      (make-valtype '() (arrow-type diagram-type (arrow-type diagram-type (arrow-type sigfun-type diagram-type)))))
	   
	   (Value_sig (ident-create "TRANSIENT") 
		      (make-valtype '() (arrow-type diagram-type (arrow-type diagram-type (arrow-type sigfun-type diagram-type)))))
	   
	   (Value_sig (ident-create "RTRANSITION") 
		      (make-valtype '() (arrow-type diagram-type 
						    (arrow-type diagram-type
								(arrow-type sigfun-type (arrow-type sigfun-type diagram-type))))))

	   ))


	(struct
	 (list

	  (Type_def ident-diagram (make-kind 0) 
		    (make-deftype '() (Tcon path-diagram '()) ))

	  (datacon 'diagram 'SENSE   2)
	  (datacon 'diagram 'ACTUATE 2)
	  (datacon 'diagram 'ASSIGN 2)
	  (datacon 'diagram 'ODE 4)
	  (datacon 'pure 'RELATION 4)
	  (datacon 'pure 'PURE 1)
	  (datacon 'pure 'GROUP 2)
	  (datacon 'diagram 'IDENTITY 1)
	  (datacon 'diagram 'TRANSITION 3)
	  (datacon 'diagram 'TRANSIENT 3)
	  (datacon 'diagram 'RTRANSITION 4)
	  (datacon 'diagram 'SEQUENCE 2)
	  (datacon 'diagram 'UNION 2)

	 ))
	  
	)

    (let ((modname (ident-create module-name)))
      (enter-module modname (Signature sig))
      (eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
    )
  ))


