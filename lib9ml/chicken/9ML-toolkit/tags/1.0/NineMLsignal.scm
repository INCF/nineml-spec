
(define (Signal:module-initialize module-name enter-module find-module eval-env)



  (define ident-sigfun   (ident-create "sigfun"))
  (define path-sigfun    (Pident ident-sigfun))
  (define sigfun-type    (Tcon path-sigfun '()))

  (define path-label   (Pident (ident-create "label")))
  (define label-type   (Tcon path-label '()))

  (define path-real   (Pident (ident-create "real")))
  (define real-type   (Tcon path-real '()))

  (define path-bool   (Pident (ident-create "bool")))
  (define bool-type   (Tcon path-bool '()))

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
	   (Type_sig ident-sigfun (make-typedecl (make-kind 0) #f))
	   
	   (Value_sig (ident-create "signal") 
		      (make-valtype '() (arrow-type label-type sigfun-type)))
	   
	   (Value_sig (ident-create "relation") 
		      (make-valtype '() (arrow-type label-type (arrow-type sigfun-type sigfun-type))))
	   
	   (Value_sig (ident-create "realconst") 
		      (make-valtype '() (arrow-type real-type sigfun-type)))
	   
	   (Value_sig (ident-create "boolconst") 
		      (make-valtype '() (arrow-type bool-type sigfun-type)))
	   
	   (Value_sig (ident-create "realsig") 
		      (make-valtype '() (arrow-type label-type (arrow-type sigfun-type sigfun-type))))
	   
	   (Value_sig (ident-create "boolsig") 
		      (make-valtype '() (arrow-type label-type (arrow-type sigfun-type sigfun-type))))
	   
	   (Value_sig (ident-create "if")
		      (make-valtype '() (arrow-type sigfun-type (arrow-type sigfun-type (arrow-type sigfun-type sigfun-type))))))

	  (map 
	   (lambda (name)
	     (Value_sig (ident-create name)
			(make-valtype '() (arrow-type sigfun-type (arrow-type sigfun-type sigfun-type)))))
	   '("add" "sub" "mul" "div" "gte" "lte" "gt" "lt" ))

	  (map 
	   (lambda (name)
	     (Value_sig (ident-create name)
			(make-valtype '() (arrow-type sigfun-type sigfun-type))))
	   '("neg" "log" "ln" "cosh" "tanh" ))

	  ))

	(struct 
	 (append
	  (list 
	   
	   (Type_def ident-sigfun (make-kind 0) 
		     (make-deftype '() (Tcon path-sigfun '()) ))

	   (datacon 'sigfun 'signal 1)
	   (datacon 'sigfun 'relation 2)
	   (datacon 'sigfun 'realsig 2)
	   (datacon 'sigfun 'boolsig 2)
	   (datacon 'sigfun 'realconst 1)
	   (datacon 'sigfun 'boolconst 1)
	   (datacon 'sigfun 'if 3))
	  
	  (map (lambda (name op) (datacon 'sigfun name 2 op))
	       '(add sub mul div gte lte gt lt)
	       '(+ - * / >= <= > <))

	  (map (lambda (name) (datacon 'sigfun name 1))
	       '(log ln tanh cosh neg))

	  
	 ))
	)
    
    (let* ((modname (ident-create module-name))
	   (msig    (Signature sig))
	   (mdef    (Module_def modname (Structure struct))))
      (enter-module modname msig)
      (eval-env (mod-eval-cbv (eval-env) (list mdef)))
      )
    ))

    


