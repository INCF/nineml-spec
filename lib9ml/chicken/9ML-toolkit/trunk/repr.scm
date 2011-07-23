
;;
;; Different external representations of NineML.
;;
;; Copyright 2010-2011 Ivan Raikov and the Okinawa Institute of
;; Science and Technology.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module 9ML-repr

	(repr-verbose 
	 sxml-value->sexpr sexpr->diagram+initial print-fragments 
         print-eval-env print-type-env print-source-defs 
	 generate-diagram html-report traverse-definitions
	 )

	(import scheme chicken )

	(require-library srfi-1 srfi-13 data-structures extras utils files irregex mathh)
	(import (only srfi-1 fold combine every zip unzip2 filter-map partition delete-duplicates)
		(only srfi-13 string-downcase )
		(only data-structures conc compose identity atom? intersperse string-intersperse ->string )
		(only extras fprintf pp)
		(only utils system*)
		(only files make-pathname pathname-directory)
		(only mathh cosh tanh log10)
		)

	(require-extension datatype static-modules miniML miniMLvalue miniMLeval signal-diagram signal-diagram-dynamics
			   regex ssax sxml-transforms sxpath sxpath-lolevel object-graph 9ML-parse)


(include "SXML.scm")
(include "SXML-to-XML.scm")



(define repr-verbose (make-parameter 0))

(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (repr-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))


(define (run:execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd) (system (->string cmd)))
	    (map smooth explist)))

(define (run:execute* explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd) (system* "~a" cmd))
	    (map smooth explist)))


(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (begin
       (d "running ~A ...~%" (list `exp ...))
       (run:execute* (list `exp ...))))))

(define-syntax run-
  (syntax-rules ()
    ((_ exp ...)
     (begin
       (d "running ~A ...~%" (list `exp ...))
       (run:execute (list `exp ...))))))

(define (enumvars expr ax)
  (if (pair? expr)
      (case (car expr)
	((cond)  (fold (lambda (x ax) (enumvars x ax)) ax (cdr expr)))
	(else  (if (symbol? (car expr))  (fold (lambda (x ax) (enumvars x ax)) ax (cdr expr)) ax)))
      (if (symbol? expr) (cons expr ax) ax)))

(define (sexpr->function sexpr)  (make-function (enumvars sexpr '()) sexpr))


(define (sxml-value->sexpr tree)
    (let* ((tree 
	    (pre-post-order* 
	    tree
	    `(
	      (Tuple *macro* .
		     ,(lambda (tag elems) 
			(let ((node (cons tag elems)))
			  (let ((left (sxml:kidn-cadr 'left node))
				(right (sxml:kidn-cdr 'right node)))
			    (cons left right)))))
	      
	      (Const . ,(lambda (tag elems) (car elems)))
	      
	      (label . ,(lambda (tag elems) (string->symbol (car elems))))
	      
	      (string . ,(lambda (tag elems) (car elems)))
	      
	      (real . ,(lambda (tag elems) (car elems)))
	      
	      (nat  . ,(lambda (tag elems) (car elems)))
	      
	      (bool . ,(lambda (tag elems) (if (string=? (car elems) "true") #t #f)))
	      
	      (null . ,(lambda (tag elems) '()))
	      
	      (*text* . ,(lambda (trigger str) str))
	      
	      (*default* . ,(lambda (tag elems) (cons tag elems)))
	      )))

    	   (tree
	    (pre-post-order* 
	     tree
	     `(
	       (signal . ,(lambda (tag elems) (caar elems)))
	       
	       (sigfun . ,(lambda (tag elems) (car elems)))

	       (*text* . ,(lambda (trigger str) str))
	       
	       (*default* . ,(lambda (tag elems) (cons tag elems)))
	       )))

	   (tree
	    (let flatten ((tree tree))
	      (cond ((atom? tree) tree)
		    (else (cons (flatten (car tree)) (flatten (cadr tree))))))))
      tree))


;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b)
  (let loop ((fragments b) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
       (display (car fragments))
       (loop (cdr fragments) #t)))))

      
(define (print-eval-env env . rest)
  (let-optionals rest ((output-type #f)  (component-filter identity))
          (let ((env (filter-map component-filter env)))

		 (case output-type
		   ((sxml )
		    (pp (eval-env->sxml env)))


		   ((xml )
		    (let* ((doc1   `(Toplevel ,@(eval-env->sxml env)))
			   (doc2  (ensure-xmlns  doc1))
			   (doc3  (ensure-xmlver doc2)))
		      (print-fragments (generate-XML `(begin ,doc3)))))
		       
		   
		   (else 
		    (for-each
		     (lambda (x) 
		       (let ((id (car x))
			     (v  (cdr x)))
			 (pp `(,id ,v))
			 ))
		     env))
		   ))))


      
(define (print-type-env env . rest)
  (let-optionals rest ((output-type #f) (component-filter identity))
          (let ((env (filter-map component-filter env)))
	    (case output-type
	      ((sxml )
	       (pp (map (compose modspec->sxml cdr) env)))
	      
	      ((xml )
	       (let* ((doc1   `(Toplevel ,@(map (compose modspec->sxml cdr) env)))
		      (doc2  (ensure-xmlns doc1))
		      (doc3  (ensure-xmlver doc2)))
		 (print-fragments (generate-XML `(begin ,doc3)))))
	      
	      (else  (pp env))
	      
	      ))))
      
(define (print-source-defs defs . rest)
  (let-optionals rest ((output-type #f))

		 (case output-type
		   ((sxml )
		    (pp (map moddef->sxml defs)))

		   ((xml )
		    (let* ((doc1   `(Toplevel ,@(map moddef->sxml defs)))
			   (doc2  (ensure-xmlns doc1))
			   (doc3  (ensure-xmlver doc2)))
		      (print-fragments (generate-XML `(begin ,doc3)))))
		       
		   (else  (pp defs))

		   )))

(define (signal-op->mathml op)
  (case op
    ((add) 'plus)
    ((sub) 'minus)
    ((mul) 'multiply)
    ((div) 'divide)
    (else op)))


(define (function->nxml f)
  `(lambda ,(map (lambda (x) `(bvar ,x)) (function-formals f))
     ,(signal->nxml (function-body f))))


(define (signal->nxml tree)
    (let recur ((sexpr tree))
      (or (and (pair? sexpr)
	       (case (car sexpr)

		 ((signal) 
		  (let ((sexpr (cdr sexpr)))
		    
		    (case (car sexpr)
		      
		      ((signal)   
		       (let ((name (cadr sexpr)))
			 `(ci ,name)))
		      
		      ((realsig)   
		       (let ((name (cadr sexpr))
			     (value (caddr sexpr)))
			 `(ci (@ (type real)) ,name)))
		      
		      ((boolsig)   
		       (let ((name (cadr sexpr))
			     (value (caddr sexpr)))
			 `(ci (@ (type real)) ,name)))

		      ((if)
		       `(if ,(recur (cadr sexpr)) 
			    ,(recur (caddr sexpr))
			    ,(recur (cadddr sexpr))))
		      
		      ((add sub mul div gte lte gt lt)
		       (let ((name (signal-op->mathml (car sexpr))))
			 `(apply (,name) ,(recur (cadr sexpr)) 
				 ,(recur (caddr sexpr)))))
		       
		      ((neg log ln cosh tanh)
		       (let ((name (signal-op->mathml (car sexpr))))
			 `(apply (,name) ,(recur (cadr sexpr)) )))

		      (else (error 'signal->nxml "invalid signal function constructor" sexpr))

		      )))

		 (else (map recur sexpr))
		 )))

             sexpr))


(define (diagram->nxml sexpr)

    (let recur ((sexpr sexpr))
      (or (and (pair? sexpr)
	       (case (car sexpr)
		 ((diagram) 
		  (let ((sexpr (cdr sexpr)))
		    
		    (case (car sexpr)
		      
		      ((RTRANSITION)  
		       (let ((f (cadr sexpr)) (fk (caddr sexpr))
			     (e (cadddr sexpr)) (ek (car (cddddr sexpr))))
			 `(DiagramLib:Rtransition 
			   (@ (e ,e) (e ,ek) ,(recur f) ,(recur fk)))
			 ))
		      
		      ((TRANSITION)  
		       (let ((f (cadr sexpr)) (fk (caddr sexpr))
			     (e (cadddr sexpr))) 
			 `(DiagramLib:Transition 
			   (@ (e ,e) ,(recur f) ,(recur fk)))
			 ))
		      
		      ((TRANSIENT)  
		       (let ((f (cadr sexpr)) (fk (caddr sexpr))
			     (e (cadddr sexpr))) 
			 `(DiagramLib:Transient 
			   (@ (e ,e) ,(recur f) ,(recur fk)))
			 ))

		      ((IDENTITY)       
		       (let ((f (cadr sexpr)))
			 `(DiagramLib:Identity ,(recur f))))

		      ((RELATION)           
		       (let ((n (cadr sexpr)) (x (caddr sexpr))
			     (f (sexpr->function (cadddr sexpr)))
			     (d (car (cddddr sexpr))))
			 `(DiagramLib:Relation (@ (name ,n) (arg ,x))
					       ,(function->nxml f)
					       ,(recur d))))
			 
		      ((PURE)           
		       (let ((f (sexpr->function (cadr sexpr))))
			 `(DiagramLib:Function
			   ,(function->nxml f))))

		      ((GROUP)           
		       (let ((n1 (cadr sexpr)) (n2 (caddr sexpr)))
			 `(DiagramLib:Group
			   ,(recur n1) ,(recur n2))))

		      ((SEQUENCE)       
		       (let ((n1  (cadr sexpr))
			     (n2  (caddr sexpr)))
			 `(DiagramLib:Sequence ,(recur n1) ,(recur n2))
			 ))

		      ((UNION)          
		       (let ((n1 (cadr sexpr))
			     (n2 (caddr sexpr)))
			 `(DiagramLib:Regime ,(recur n1) ,(recur n2))
			 ))

		      ((SENSE)          
		       (let ((sns (cadr sexpr)) (n (caddr sexpr)))
			 `(DiagramLib:Sense ,(map (lambda (s) `(signal ,s)) sns) 
					    ,(recur n))
			 ))
						   
		      ((ACTUATE)        
		       (let ((sns (cadr sexpr)) (n (caddr sexpr)))
			 `(DiagramLib:Actuate ,(map (lambda (s) `(signal ,s)) sns) 
					      ,(recur n))))
		      
		      ((ODE)            
		       (let ((ivar (cadr sexpr)) (dvar (caddr sexpr))
			     (rhs (cadddr sexpr)))
			 `(DiagramLib:ODE `(independent_variable ,ivar)
					  `(dependent_variable ,dvar)
					   ,(recur rhs))))

		      ((ASSIGN)         
		       (let ((var (cadr sexpr)) 
			     (rhs (recur (caddr sexpr))))
			 `(DiagramLib:Assign `(variable ,var)
					     ,(recur rhs))))

		      
		      (else (error 'diagram->nxml "invalid diagram constructor" sexpr))
		      )))

		 (else (map recur sexpr))
		 ))

             sexpr)))


(define (print-nxml prefix uenv)

    (let (
	  (path-ss
	   `(
	     (path
	      *macro*
	      . ,(lambda (tag elems) elems))
	     
	    (Pident
	     *macro*
	     . ,(lambda (tag elems)
		  (let ((node (cons tag elems)))
		    (let ((name (sxml:text node)))
		      (if (not name) (error 'print-nxml "Pident element requires text content" node))
		      name))))
	     
	     (Pdot
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'print-nxml "Pdot element requires name attribute"))
		       `(,(sxml:kids node) "." ,name)))))

	     
	     ,@alist-conv-rules*
	     ))


	  (moddef-ss
	   
	   `(
	     (Type_def
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name))
			   (deftype (sxml:kidn* 'deftype node)))
		       `(Type (@ (name ,name)) ,deftype)))
		   ))

	     (Component
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		       (let ((name (sxml:attr node 'name))
			     (members ((sxpath '(Component (*or* Val Component))) `(*TOP* ,node))))
			 `(Namespace (@ (name ,name)) . ,members)
			 ))
		   ))

	     (Val
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let* ((name (sxml:attr node 'name))
			    (value (sxml:kid node))
			    (tuple-label ((sxpath '(Tuple left Const label *text*)) `(*TOP* ,value))))

		       (if (not name) (error 'type-env-report "binding element requires name attribute"))

		       (cond ((and (pair? tuple-label) (equal? (car tuple-label) "diagram")) ;; value is a diagram
			      (let* ((diagram-id (gensym 'diagram)))
				`(Binding (@ (name ,name)) ,(diagram->nxml (sxml-value->sexpr value)))))

			     (else
			      `(Binding (@ (name ,name))  ,value)))
		       ))))
	     
	  ,@alist-conv-rules*

	  ))


	  (term-ss
	   `(

	     (Longid 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (sxml:kids node)
		     )))

	     (Function
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((formal (sxml:attr node 'formal))
			   (body   (sxml:kid node)))
		       `(Term:Function (@ (x ,formal)) ,body)
		       ))))


	     (Let0 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name))
			   (value (sxml:kidn-cadr 'value node))
			   (body (sxml:kidn-cadr 'body node)))
		       `(Term:Let (@ (name ,name)) (value ,value) (body ,body))
		       ))))

	     (Apply 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((left (sxml:kidn-cdr 'left node))
			   (right (sxml:kidn-cdr 'right node)))
		       `(Term:Apply (left ,left) (right ,right))
		       ))))
	     
	     ,@alist-conv-rules*
	     ))


	  )

  (let (  
	(filename    (string-append prefix ".xml"))
	(source-defs (car uenv))
	(type-env    (cadr uenv))
	(eval-env    (caddr uenv)))

    (let ((eval-env-sxml (eval-env->sxml eval-env))
	  (eval-env-rulesets `(,moddef-ss
			       ,term-ss
			       ,path-ss
			       )))
      
      (let* (
	     (eval-env-sxml  (sxml-transform eval-env-sxml eval-env-rulesets))
	     (content        `(Toplevel ,eval-env-sxml))
	     )
	
	(with-output-to-file filename
	  (lambda () (print-fragments (generate-XML content))))
	    
	)))
  ))


(define (sexpr->diagram+initial h sexpr)

    (define initenv  (make-parameter '()))

    (define (realsig-value x)
      (cond ((number? x) x)
	    ((equal? 'realsig (car x)) (caddr x))
	    (else (error 'realsig-value "invalid real signal" x))))
    (define (realsig-name x)
      (if (and (pair? x) (equal? 'realsig (car x))) (cadr x)
	  (error 'realsig-name "invalid real signal" x)))
    (define (boolsig-value x)
      (cond ((boolean? x) x)
	    ((equal? 'boolsig (car x)) (caddr x))
	    (else (error 'boolsig-value "invalid boolean signal" x))))
    (define (boolsig-name x)
      (if (and (pair? x) (equal? 'boolsig (car x))) (cadr x)
	  (error 'boolsig-value "invalid boolean signal" x)))

    (define (sigfun-eval sexpr)
      (let recur ((sexpr sexpr))
	(if (pair? sexpr)
	    (case (car sexpr)
	      ((realconst)   (let ((value (cadr sexpr))) value))
	      ((boolconst)   (let ((value (cadr sexpr))) value))
	      ((realsig)     (let ((name (cadr sexpr))
				   (value (recur (caddr sexpr))))
			       (if (not (number? value)) (error 'realsig "real signal value is not a real" name value))
			       (initenv (cons (cons name value) (initenv)))
			       `(realsig ,name ,value)))
	      ((boolsig)   (let ((name (cadr sexpr))
				 (value0 (recur (caddr sexpr))))
			     (let ((value (if (boolean? value0) value0
					      (else (error 'boolsig "boolean signal value is not a boolean" name value0)))))
			       (initenv (cons (cons name value) (initenv)))
			       `(boolsig ,name ,value))))
	      ((neg)       (let ((x (recur (cadr sexpr))))
			     (- (realsig-value x))))
	      ((log)       (let ((x (recur (cadr sexpr))))
			     (log10 (realsig-value x))))
	      ((ln)        (let ((x (recur (cadr sexpr))))
			     (log (realsig-value x))))
	      ((cosh)      (let ((x (recur (cadr sexpr))))
			     (cosh (realsig-value x))))
	      ((tanh)      (let ((x (recur (cadr sexpr))))
			     (tanh (realsig-value x))))
	      ((+)       (let ((x (recur (cadr sexpr)))
			       (y (recur (caddr sexpr))))
			   (+ (realsig-value x) (realsig-value y))))
	      ((-)       (let ((x (recur (cadr sexpr))) 
			       (y (recur (caddr sexpr))))
			   (- (realsig-value x) (realsig-value y))))
	      ((*)       (let ((x (recur (cadr sexpr)))  
			       (y (recur (caddr sexpr))))
			   (* (realsig-value x) (realsig-value y))))
	      ((/)       (let ((x (recur (cadr sexpr)))  
			       (y (recur (caddr sexpr))))
			   (/ (realsig-value x) (realsig-value y))))
	      ((>=)       (let ((x (recur (cadr sexpr)))  
				(y (recur (caddr sexpr))))
			    (>= (realsig-value x) (realsig-value y))))
	      ((<=)       (let ((x (recur (cadr sexpr)))  
				(y (recur (caddr sexpr))))
			    (<= (realsig-value x) (realsig-value y))))
	      ((>)        (let ((x (recur (cadr sexpr)))  
				(y (recur (caddr sexpr))))
			    (> (realsig-value x) (realsig-value y))))
	      ((<)        (let ((x (recur (cadr sexpr)))  
				(y (recur (caddr sexpr))))
			    (< (realsig-value x) (realsig-value y))))
	      (else (map recur sexpr))
	      ) sexpr)))


    (let ((diagram
	   (let recur ((sexpr sexpr))
	     (if (pair? sexpr)
		 (case (car sexpr)
		   
		   ((diagram) 
		    (let ((sexpr (cdr sexpr)))
		      (case (car sexpr)
			
			((PURE)           (let ((f  (sexpr->function (cadr sexpr))))  (PURE f)))
			((GROUP)          (UNION (recur (cadr sexpr)) (recur (caddr sexpr))))
			((IDENTITY)       (IDENTITY (recur (cadr sexpr))))
			((SEQUENCE)       (SEQUENCE (recur (cadr sexpr)) (recur (caddr sexpr))))
			((UNION)          (UNION (recur (cadr sexpr)) (recur (caddr sexpr))))
			((SENSE)          (SENSE (cadr sexpr) (recur (caddr sexpr))))
			((ACTUATE)        (ACTUATE (cadr sexpr) (recur (caddr sexpr))))
			((TRANSIENT)      (TRANSIENT (recur (cadr sexpr)) (recur (caddr sexpr))
						     (recur (cadddr sexpr))))
			((TRANSITION)     (TRANSITION (recur (cadr sexpr)) (recur (caddr sexpr))
						      (recur (cadddr sexpr))))
			((RTRANSITION)    (RTRANSITION (recur (cadr sexpr)) (recur (caddr sexpr))
						       (recur (cadddr sexpr))
						       (recur (cadddr (cdr sexpr)))))
			
			((ODE)            (let ((deps  (map recur (cadr sexpr)))
						(indep (recur (caddr sexpr)))
						(tstep (recur (cadddr sexpr)))
						(rhs   (cadddr (cdr sexpr))))
					    
					    (if (not (equal? tstep h))
						(error 'sexpr->diagram "mismatch between independent variable step of ODE and IVP" h tstep))
					    
					    (let-values (((rhs-list relation-list)
							  (let rhs-recur ((rhs-list '()) (relation-list '()) (rhs rhs))
							    (case (car rhs)
							      ((pure)
							       (let ((d (cdr rhs)))
								 (case (car d)
								   ((GROUP)  
								    (let-values (((rhs-list1 relation-list1) 
										  (rhs-recur rhs-list relation-list (cadr d))))
								      (rhs-recur rhs-list1 relation-list1 (caddr d))))
								   ((PURE)      
								    (let ((expr (recur (cadr d))))
								      (values (cons expr rhs-list) relation-list)))
								   ((RELATION)  
								    (let ((r (cdr d)))
								      (rhs-recur rhs-list (cons (list (car r) (list (cadr r)) (recur (caddr r))) relation-list) (cadddr r))))
								   (else (error 'sexpr->diagram "invalid ODE subelement" d)))))
							      (else
							       (error 'sexpr->diagram "invalid ODE subelement" rhs))))))
					      (make-dae-system h indep (append (reverse relation-list) (zip deps (reverse rhs-list))))
					      )))
			
			((ASSIGN)         (let ((vars  (cadr sexpr))
						(rhs   (caddr sexpr)))
					    (let ((rhs-list
						   (let rhs-recur ((rhs-list '()) (rhs rhs))
						     (case (car rhs)
						       ((pure)
							(let ((d (cdr rhs)))
							  (case (car d)
							    ((GROUP)  (rhs-recur (rhs-recur rhs-list  (cadr d)) (caddr d)))
							    ((PURE)   (cons (recur (cadr d)) rhs-list))
							    (else (error 'sexpr->diagram "invalid ASSIGN subelement" d)))))
						       (else (error 'sexpr->diagram "invalid ASSIGN subelement" rhs))))))
					      
					      (make-assign-system (zip vars (reverse rhs-list))))))
			
			((RELATION)      (let ((n (cadr sexpr)) (x (caddr sexpr))
					       (f (sexpr->function (recur (cadddr sexpr)))))
					   (RELATION (list n x f) (recur (cadddr (cdr sexpr))))))
			
			(else             (error 'sexpr->diagram "invalid diagram constructor" sexpr))
			)))
		   
		   ((relation)    (let ((op (cadr sexpr))) (cons op (map recur (cddr sexpr)))))
		   
		   ((realsig)     (let ((name (cadr sexpr))
					(value (sigfun-eval (caddr sexpr))))
				    (if (not (number? value)) (error 'realsig "real signal value is not a real" name value))
				    (initenv (cons (cons name value) (initenv)))
				    name))

		   ((realconst)   (cadr sexpr))
		   
		   ((boolsig)   (let ((name (cadr sexpr))
				      (value0 (sigfun-eval (caddr sexpr))))
				  (let ((value (if (boolean? value0) value0
						   (case (car value0) 
						     ((boolconst) (cadr value0))
						     (else (error 'boolsig "boolean signal value is not a boolean" name value0))))))
				    (initenv (cons (cons name value) (initenv)))
				    name)))

		   ((boolconst)   (if (cadr sexpr) 'true 'false))
		   
		   (else (map recur sexpr)))
		 sexpr)
	  )))
    (initenv (delete-duplicates (initenv) (lambda (x y) (equal? (car x) (car y)))))
    (list diagram (initenv))))

    
  


(define (generate-diagram prefix diagram-id tree)

    (let ((sexpr (sxml-value->sexpr tree)))

      (reset-graph)
      (let recur ((sexpr sexpr))
        (or (and (pair? sexpr)
                 (case (car sexpr)
			((diagram) 
                         (let ((sexpr (cdr sexpr)))

       	                    (case (car sexpr)

                              ((RTRANSITION)  
                                (let ((f (cadr sexpr)) (fk (caddr sexpr))
                                      (e (recur (cadddr sexpr))) (ek (recur (car (cddddr sexpr)))))
				  (let ((node (register-node (gensym 'rtransition)))
					(fnode (recur f))
					(fknode (recur fk)))
				    (set-label node "RTRANSITION")
				    (let ((edge1  (register-edge node fnode))
					  (edge2  (register-edge node fknode)))
				      (set-label edge1 e)
				      (set-label edge2 ek)
				      node
				      ))))
                               
                              ((TRANSITION)  
                                (let ((f (cadr sexpr)) (fk (caddr sexpr))
                                      (e (recur (cadddr sexpr))) )
				  (let ((node (register-node (gensym 'transition)))
					(fnode (recur f))
					(fknode (recur fk)))
				    (set-label node "TRANSITION")
				    (let ((edge1  (register-edge node fnode)))
				      (set-label edge1 e)
				      node
				      ))))
                               
                              ((TRANSIENT)  
                                (let ((f (cadr sexpr)) (fk (caddr sexpr))
                                      (e (recur (cadddr sexpr))) )
				  (let ((node (register-node (gensym 'transient)))
					(fnode (recur f))
					(fknode (recur fk)))
				    (set-label node "TRANSIENT")
				    (let ((edge1  (register-edge node fnode)))
				      (set-label edge1 e)
				      node
				      ))))
                               
                               ((IDENTITY)       (let ((n1 (recur (cadr sexpr))))
						   (let ((node (register-node (gensym 'IDENTITY))))
						     (set-label node "IDENTITY")
						     (let ((edge1 (register-edge node n1)))
						       (set-label edge1 "n1")
						       node))))
			       ((PURE)           (let ((f (sexpr->function (cadr sexpr))))
						   (let ((node (register-node (gensym 'function))))
						     (set-label node (sprintf "fn ~A => ~A" 
									      (function-formals f) 
									      (function-body f)))
						     node)))
			       ((GROUP)          (let ((n1 (recur (cadr sexpr))) 
						       (n2 (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'UNION))))
						     (set-label node "UNION")
						     (let ((edge1 (register-edge node n1))
							   (edge2 (register-edge node n2)))
						       (set-label edge1 "n1")
						       (set-label edge1 "n2")
						       node
						       ))))
			       ((SEQUENCE)       (let ((n1 (recur (cadr sexpr))) 
						       (n2 (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'sequence))))
						     (set-label node "SEQUENCE")
						     (let ((edge1 (register-edge node n1))
							   (edge2 (register-edge node n2)))
						       (set-label edge1 "n1")
						       (set-label edge1 "n2")
						       node
						       ))))
			       ((UNION)          (let ((n1 (recur (cadr sexpr))) 
						       (n2 (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'UNION))))
						     (set-label node "UNION")
						     (let ((edge1 (register-edge node n1))
							   (edge2 (register-edge node n2)))
						       (set-label edge1 "n1")
						       (set-label edge1 "n2")
						       node
						       ))))
			       ((SENSE)          (let ((sns (cadr sexpr)) (n (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'SENSE))))
						     (set-label node (sprintf "SENSE ~A" sns))
						     (let ((edge (register-edge node n)))
						       node
						       ))))
						   
			       ((ACTUATE)        (let ((sns (cadr sexpr)) (n (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'ACTUATE))))
						     (set-label node (sprintf "ACTUATE ~A" sns))
						     (let ((edge (register-edge node n)))
						       node
						       ))))
			       ((ODE)            (let ((ivar (cadr sexpr)) (dvar (caddr sexpr))
						       (rhs (recur (cadddr sexpr))))
						   (let ((node (register-node (gensym 'ODE))))
						     (set-label node (sprintf "D (~A ~A) = ~A" dvar ivar rhs))
						     node
						     )))
			       ((ASSIGN)         (let ((var (cadr sexpr)) 
						       (rhs (recur (caddr sexpr))))
						   (let ((node (register-node (gensym 'ASSGIN))))
						     (set-label node (sprintf "~A = ~A" var rhs))
						     node
						     )))

                               (else (error 'generate-diagram "invalid diagram constructor" sexpr)))))

			((realsig)   (let ((name (cadr sexpr))
					   (value (caddr sexpr)))
				       name))

			((boolsig)   (let ((name (cadr sexpr))
					   (value (caddr sexpr)))
				       name))

                        (else (map recur sexpr))
                        ))
             sexpr))

      (let* ((dir (pathname-directory prefix))
             (dot-path (make-pathname dir (string-append (->string diagram-id) ".dot")))
	     (png-path (make-pathname dir (string-append (->string diagram-id) ".png"))))
	(with-output-to-file  dot-path
	  (lambda ()
	    (render-graph/dot (current-output-port))
	    ))
	
	(run (dot -Tpng ,dot-path > ,png-path))
	)
	
      ))


(define variable-names (make-parameter '()))


(define (html-report prefix uenv #!key (value-hook #f))

  (let-syntax 
      (
       (line (syntax-rules ()
	       ((_ x ...) (list (list 'span '(@ (class "hl_line")) `x ...) nl))))
       (code (syntax-rules ()
	       ((_ x ...) (list 'code '(@ (class "hl_code")) `x ...))))
       )

    (let (
	  (path-ss
	   `(
	     (path
	      *macro*
	      . ,(lambda (tag elems) elems))
	     
	    (Pident
	     *macro*
	     . ,(lambda (tag elems)
		  (let ((node (cons tag elems)))
		    (let ((name (sxml:text node)))
		      (if (not name) (error 'html-report "Pident element requires text content" node))
		      name))))
	     
	     (Pdot
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'html-report "Pdot element requires name attribute"))
		       `(,(sxml:kids node) "." ,name)))))

	     
	     ,@alist-conv-rules*
	     ))

	  (simple-type-ss
	   `(
	     (Tcon
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((path (sxml:kidn-cadr 'path node))
			   (ts (map cdr (sxml:kidsn 't node))))
		       (cond 
			((equal? path `(pident (@ (name "->"))))
			 `(,(car ts) " -> " ,(cadr ts)))
			((pair? ts)
			 `("(" ,@(intersperse ts " ") ") " ,path))
			(else path))))))
	     
	     
	     (Tvar
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((repres (sxml:kidn 'repres node)))
		       (cond 
			(repres (cdr repres))
			(else (let* ((name (or (assq elems (variable-names))))
				     (name (if (not name)
					       (let* ((n  (+ 1 (length (variable-names))))
						     (s  (string-append "'t" (number->string n))))
						 (variable-names (cons (list n s) (variable-names)))
						 s))))
				name)))))))
	     
	     ,@alist-conv-rules*
	     ))

	  (const-ss
	   `(

	     (Const
	      *macro*
	      . ,(lambda (tag elems) 
		   (let ((node (cons tag elems)))
		     (sxml:kids node))))

	     (label 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (code ,(sxml:text node)))))
		   
	     ,@alist-conv-rules*
	     ))
	    
	  (typedef-ss
	   `(
	     (Valtype
	      *macro* 
	      . ,(lambda (tag elems)
		   (let ((body (sxml:kidn-cdr 'body elems)))
		     body)
		   ))

	     (Deftype 
	       *macro*
	       . ,(lambda (tag elems)
		    (let ((node (cons tag elems)))
		      (let ((b (sxml:kidn-cdr 'body node)))
			b)
		      )))

	     ,@alist-conv-rules*

	     ))
	  
	  (modspec-ss
	   `(
	     (Value_sig 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'type-env-report "value_sig element requires name attribute"))
		       (line "Value " (b ,name) " : " ,(sxml:kids node))))))
	     
	     
	     (Type_sig 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'type-env-report "type_sig element requires name attribute"))
		       (line "Type " (b ,name) " = " ,(sxml:kids node))))))
	     
	     (Typedecl
	      *macro* 
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((m (sxml:kidn* 'manifest node)))
		       m)
		     )))
	     
	     (manifest 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((dt (sxml:kidn* 'deftype node)))
		       dt)
		     )))
	     
	     (Module_sig 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'type-env-report "module_sig element requires name attribute"))
		       `(,(line "Component signature " (b ,name) " : ") 
			 ,(sxml:kids node))))))
	     
	     (Signature
	      *macro* 
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     `(ul . ,(map (lambda (x) `(li ,x)) (sxml:kids node ))))))
	     
	     
	     (Functorty
	      *macro* 
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'type-env-report "functorty elements requires name attribute"))
		       `("Functor " (b ,name)
			 (ul . ,(map (lambda (x) `(li ,x)) (sxml:kids node ))))))))
	     
	     
	     ,@alist-conv-rules*
	     ))

	  (moddef-ss
	   
	   `(
	     (Type_def
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name))
			   (deftype (sxml:kidn* 'deftype node)))
		       (code  "type " ,name " = " ,deftype)
		   ))))

	     (Component
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'type-env-report "component element requires name attribute"))
		       `(,(line "Component " (b ,name) " = ") (ul . ,(map (lambda (x) `(li ,x)) (sxml:kids node))))))))

	     (Val
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let* ((name (sxml:attr node 'name))
			    (value (sxml:kid node))
			    (tuple-label ((sxpath '(Tuple left Const label *text*)) `(*TOP* ,value))))

		       (if (not name) (error 'type-env-report "binding element requires name attribute"))

		       (cond ((and value-hook (pair? tuple-label) (value-hook prefix name (car tuple-label) value)) =>
			      (lambda (x) `(,(line "binding " (b ,name) " = ") (p ,x))))

			     (else
			      `(,(line "binding " (b ,name) " = ") ,value)))
		       ))))

	     (Prim
	      *macro*
	      . ,(lambda (tag elems) 
		  (code "primitive procedure")))
	     
	     (null
	      *macro*
	      . ,(lambda (tag elems) 
		  (code "null")))
	     
	     (Tuple
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((left (sxml:kidn-cadr 'left node))
			   (right (sxml:kidn-cdr 'right node)))
		        `( " ( "  ,left " " ,@right " ) " )
			))))
	     
	     (Closure
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((body (sxml:kidn-cdr 'body node))
			   (env (sxml:kidn-cdr 'env node)))
		       `(,(line "Closure: ") ,@body ,(line "where ") ,env)))))

	     
	     
	  ,@alist-conv-rules*

	  ))


	  (term-ss
	   `(

	     (Longid 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (sxml:kids node))))

	     (Function
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((formal (sxml:attr node 'formal)))
		       (let-values (((formals body)
				     (let recur ((formals (list formal)) 
						 (body (sxml:kid node)))
				       (case (car body) 
					 ((function)
					  (recur 
					   (cons (sxml:attr body 'formal) formals)
					   (sxml:kid body)))
					 (else (values (reverse formals) body))))))
			 `(,(line (code "Function " ,(intersperse formals " ") " => ")) 
			   ,body))
		       ))))


	     (Let0 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name))
			   (value (sxml:kidn-cadr 'value node))
			   (body (sxml:kidn-cadr 'body node)))
		       `(,(line (code "binding " (b ,name) " = ") ,value)
			 ,body)))))
	     
	     (Apply 
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((left (sxml:kidn-cdr 'left node))
			   (right (sxml:kidn-cdr 'right node)))
		       (code ,left " (" ,right ") ")))))
	     
	     ,@alist-conv-rules*
	     ))


	  )

  (let ((filename (string-append prefix ".html"))
	(source-defs (car uenv))
	(type-env    (cadr uenv))
	(eval-env    (caddr uenv)))


    (let ((type-env-sxml (map (compose modspec->sxml cdr) type-env))
	  (eval-env-sxml (eval-env->sxml eval-env))
	  (type-env-rulesets `(,modspec-ss
			       ,typedef-ss
			       ,simple-type-ss
			       ,path-ss
			       ))
	  (eval-env-rulesets `(,moddef-ss
			       ,modspec-ss
			       ,typedef-ss
			       ,term-ss
			       ,const-ss
			       ,simple-type-ss
			       ,path-ss
			       )))
      
      (with-output-to-file filename
	(lambda ()
	  (let* ((type-env-shtml (sxml-transform type-env-sxml type-env-rulesets))
		 (eval-env-shtml (sxml-transform eval-env-sxml eval-env-rulesets))
		 (content        `(html:begin ,prefix (body (section* 1 ,prefix)
							    (toc)
							    (section 2 "Type environment") 
							    ,type-env-shtml 
							    (section 2 "Value environment")
							    ,eval-env-shtml
							    )))
		 (internal-link
		  (lambda (r)
		    (post-order 
		     r
		     `(
		       (*default* . ,(lambda (tag elems) (cons tag elems)))
		       
		       (*text* . ,(lambda (trigger str) 
				    (string-substitute* (string-downcase str) 
							'(("[^A-Za-z0-9_ \t-]" . "")
							 ("[ \t]+" . "-"))))))
		     )))
		 )

	    (print-fragments
	     (generate-XML content
			   rulesets:
			   `(((html:begin . ,(lambda (tag elems)
					       (let ((title (car elems))
						     (elems (cdr elems)))
						 (list "<HTML><HEAD><TITLE>" title "</TITLE></HEAD>"
						       "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\" />"
						       "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"

						       "<link rel=\"stylesheet\" type=\"text/css\" href=\"highlight.css\" />"
						       elems
						       "</HTML>"))))
			      (section
			       *macro*
			       . ,(lambda (tag elems)
				    (let ((level (car elems))
					  (head-word (cadr elems))
					  (contents (cddr elems)))
				      (cond ((and (integer? level) head-word)
					     `((,(string->symbol (string-append "h" (number->string level)))
						(@ (id ,(internal-link head-word)))
						,head-word ) . ,contents))
					    (else
					     (error 'html-transformation-rules
						    (conc "section elements must be of the form (section level head-word . contents), got " elems))))
					   )))

			      (section*
			       *macro*
			       . ,(lambda (tag elems)
				    (let ((level (car elems))
					  (head-word (cadr elems))
					  (contents (cddr elems)))
				      (cond ((and (integer? level) head-word)
					     `((,(string->symbol (string-append "h" (number->string level)))
						,head-word ) . ,contents))
					    (else
					     (error 'html-transformation-rules
						    (conc "section elements must be of the form (section level head-word . contents), got " elems))))
				      )))


			      (toc ;; Re-scan the content for "section" tags and generate
			       *macro*
			       . ,(lambda (tag rest) ;; the table of contents
				    `(div (@ (id "toc"))
					  ,rest
					  (ol ,(let find-sections ((content content))
						 (cond
						  ((not (pair? content)) '())
						  ((pair? (car content))
						   (append (find-sections (car content))
							   (find-sections (cdr content))))
						  ((eq? (car content) 'section)
						   (let* ((level (cadr content))
							  (head-word (caddr content))
							  (href (conc "#" (internal-link head-word)))
							  (subsections (find-sections (cdddr content))))
						     (cond ((and (integer? level) head-word)
							    `((li (a (@ (href ,href)) ,head-word)
								  ,@(if (null? subsections)
									'()
									`((ol ,subsections))))))
							   (else
							    (error 'html-transformation-rules
								   "section elements must be of the form (section level head-word . contents)")))))
						  (else (find-sections (cdr content)))))))))


			      ,@alist-conv-rules*
			      ))
			   protect: #t
			   ))
	    
	    )))

	  ;;eval-env-sxml
      )))))



(define (traverse-definitions prefix uenv #!key (type-hook #f) (component-hook #f) (value-hook #f))

  (let (
	  (moddef-ss
	   
	   `(
	     (Type_def
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name))
			   (deftype (sxml:kidn* 'deftype node)))
		       (and type-hook (type-hook prefix name deftype))
		   ))))

	     (Component
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     (let ((name (sxml:attr node 'name)))
		       (if (not name) (error 'process-definition "component element requires name attribute"))
		       (and component-hook (component-hook prefix name (sxml:kid node)))
		       ))))

	     (Val
	      *macro*
	      . ,(lambda (tag elems)
		   (let ((node (cons tag elems)))
		     
		     (let* ((name (sxml:attr node 'name))
			    (value (sxml:kid node))
			    (tuple-label ((sxpath '(Tuple left Const label *text*)) `(*TOP* ,value))))

		       (if (not name) (error 'process-definitions "binding element requires name attribute"))

		       (cond ((and value-hook (pair? tuple-label) (value-hook prefix name (car tuple-label) value)) =>
			      (lambda (x) x))

			     (else #f))
		       ))))
	     
	     ,@alist-conv-rules*
	  
	     (*text* . ,(lambda (trigger str) str))
	     
	     (*default* . ,(lambda (tag elems) (cons tag elems)))
	     
	     ))
	  )

  (let ((source-defs (car uenv))
	(type-env    (cadr uenv))
	(eval-env    (caddr uenv)))
    (let recur ((eval-env eval-env))
      (if (pair? eval-env)
	  (let ((entry (car eval-env)))
	    (let ((v (cdr entry)))
	      (cond ((MLvalue? v) 
		     (let ((sxml-value (MLvalue->sxml v)))
		       (let* ((name (sxml:attr sxml-value 'name))
			      (value (sxml:kid sxml-value))
			      (tuple-label ((sxpath '(Tuple left Const label *text*)) `(*TOP* ,sxml-value))))
			 (if (pair? tuple-label)
			     (value-hook prefix name (car tuple-label) sxml-value)))))
		    (else
		     (if (modval? v)
			 (cases modval v
				(Structure_v (env) (recur env)))))))
	    (recur (cdr eval-env))
	    ))
      ))
  ))

      

)