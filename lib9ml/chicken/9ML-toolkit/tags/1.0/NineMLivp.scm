
(define (IVP:module-initialize module-name enter-module find-module eval-env)

  (define ident-ivp        (ident-create "ivp"))
  (define path-ivp         (Pident ident-ivp))
  (define ivp-type         (Tcon path-ivp '()))

  (define path-real        (Pident (ident-create "real")))
  (define real-type        (Tcon path-real '()))

  (define path-label       (Pident (ident-create "label")))
  (define label-type       (Tcon path-label '()))

  (define path-diagram   (Pdot (Pident (ident-create "Diagram")) "diagram"))
  (define diagram-type   (Tcon path-diagram '()))
  
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
	   (append
	    (list

	     (Type_sig ident-ivp (make-typedecl (make-kind 0) #f))
	     
	     (Value_sig (ident-create "construct") 
			(make-valtype `() 
			      (arrow-type diagram-type
                                    (arrow-type label-type
				      (arrow-type label-type 
					  (arrow-type real-type
					     (arrow-type real-type
							 ivp-type)))))))
	     )))
	  
	  (struct 
	   (append
	    (list 
	     
	     (Type_def ident-ivp (make-kind 0) 
		       (make-deftype '() (Tcon path-ivp '()) ))
	     
	     (datacon 'ivp 'construct 5)
	     
	     )))
	  )
    
      (let ((modname (ident-create module-name)))
	(enter-module modname  (Signature sig))
	(eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
	)

      ))
