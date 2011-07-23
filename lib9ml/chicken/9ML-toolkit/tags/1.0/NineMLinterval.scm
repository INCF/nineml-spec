
(define (Interval:module-initialize module-name enter-module find-module eval-env)

  (define ident-interval   (ident-create "interval"))
  (define path-interval    (Pident ident-interval))
  (define interval-type    (Tcon path-interval `()))

  (define path-nat   (Pident (ident-create "nat")))
  (define nat-type   (Tcon path-nat '()))

  (define-values (type-variables reset-type-variables
				 find-type-variable 
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type bot-type
				 )
    (core-utils))


    (let

      (
	(sig
	 (append
	  (list

	   (Type_sig ident-interval (make-typedecl (make-kind 0) #f))
	   
	   (Value_sig (ident-create "empty") 
		      (make-valtype '() interval-type))

	   (Value_sig (ident-create "singleton") 
		      (make-valtype '() (arrow-type nat-type interval-type)))

	   (Value_sig (ident-create "closed_interval") 
		      (make-valtype '() (arrow-type nat-type (arrow-type nat-type interval-type))))

	   (Value_sig (ident-create "add") 
		      (make-valtype '() (arrow-type nat-type (arrow-type interval-type interval-type))))

	   (Value_sig (ident-create "union") 
		      (make-valtype '() (arrow-type interval-type interval-type)))

	  )))

	(struct 
	 (append
	  (list 
	   
	   (Type_def ident-interval (make-kind 0) 
		     (make-deftype '() (Tcon path-interval '()) ))

	   (datacon 'graph 'empty 0)
	   (datacon 'graph 'singleton 1)
	   (datacon 'graph 'closed_interval 2)
	   (datacon 'graph 'add 2)
	   (datacon 'graph 'union 2)

	  
	 )))
	)
    
    (let ((modname (ident-create module-name)))
      (enter-module modname  (Signature sig))
      (eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
      )
    ))
    


