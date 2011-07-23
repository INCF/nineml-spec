;;
;;  A parser for NineML + syntactic sugar.
;;
;;  Based on the code and paper by Xavier Leroy (2000): A modular
;;  module system. Journal of Functional Programming, 10, pp 269-303
;;  doi:10.1017/S0956796800003683
;;
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


(module 9ML-parse

	(parse parse-sexpr-macro parse-string-expr parse-sym-expr nineml-xmlns parse-al-sxml-component parse-al-sxml)

	(import scheme chicken 
		(only srfi-1 fold combine every unzip2 filter-map partition delete-duplicates cons*)
		(only srfi-13 string-null?)
		(only data-structures conc ->string)
		(only extras fprintf))
	(require-extension extras matchable static-modules miniML miniMLsyntax 
			   ssax sxml-transforms sxpath sxpath-lolevel)

  (define-values (type-variables reset-type-variables
				 find-type-variable 
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type string-type bot-type
				 )
    (core-utils))


(define (safe-car x) (and (pair? x) (car x)))

(include "SXML.scm")

(define-record token symbol value line)

(define-record-printer (token x out)
  (fprintf out "#(token  ~S ~S)"
	   (token-symbol x)
	   (token-value x) ))


(define (token p line)
  (cons (car p)
	(cond  [(pair? (cdr p))  (make-token (car p) (cadr p) line)]
	       [else (make-token (car p) #f line)])))


(define-syntax tok
  (syntax-rules ()
    ((tok t)   (token (quasiquote t) 0))
    ((tok t l) (token (quasiquote t) l))))


(define (make-parse-error loc)
  (lambda (msg #!optional arg)
    (let ((loc-str (or (and loc (if (list? loc) (conc " " loc " ") (conc " (" loc ") "))) "")))
      (cond  [(not arg) (error loc-str msg)]
	     [(token? arg)
	      (error (conc "line " (token-line arg) ": " msg) loc-str
		     (conc (token-symbol arg) 
			   (if (token-value arg) (conc " " (token-value arg)) "")))]
	     [else (error loc-str (conc msg arg))]
	     ))))

(define lexer-error error)

(include "NineML.l.scm")
(include "NineML.grm.scm")
(include "expr-parser.scm")


(define-record-type algebraic-eqn
  (make-algebraic-eqn quantity rhs)
  algebraic-eqn? 
  (quantity algebraic-eqn-quantity)
  (rhs algebraic-eqn-rhs))


(define-record-type ode-eqn
  (make-ode-eqn indep dep tstep rhs)
  ode-eqn? 
  (indep ode-eqn-indep)
  (dep   ode-eqn-dep)
  (tstep ode-eqn-tstep)
  (rhs   ode-eqn-rhs))


(define-record-type relation
  (make-relation quantity var rhs)
  relation? 
  (quantity relation-quantity)
  (var      relation-var)
  (rhs      relation-rhs))


(define (ode-eqn-or-relation? x)
  (or (ode-eqn? x) (relation? x)))


(define diagram-pure         (Longid (Pdot (Pident (ident-create "Diagram")) "PURE")))
(define diagram-group        (Longid (Pdot (Pident (ident-create "Diagram")) "GROUP")))
(define diagram-assign       (Longid (Pdot (Pident (ident-create "Diagram")) "ASSIGN")))
(define diagram-ode          (Longid (Pdot (Pident (ident-create "Diagram")) "ODE")))
(define diagram-sequence     (Longid (Pdot (Pident (ident-create "Diagram")) "SEQUENCE")))
(define diagram-union         (Longid (Pdot (Pident (ident-create "Diagram")) "UNION")))
(define diagram-transient     (Longid (Pdot (Pident (ident-create "Diagram")) "TRANSIENT")))
(define diagram-transition    (Longid (Pdot (Pident (ident-create "Diagram")) "TRANSITION")))
(define diagram-relation      (Longid (Pdot (Pident (ident-create "Diagram")) "RELATION")))

(define signal-realconst     (Longid (Pdot (Pident (ident-create "Signal")) "realconst")))
(define signal-boolconst     (Longid (Pdot (Pident (ident-create "Signal")) "boolconst")))
(define signal-boolsig       (Longid (Pdot (Pident (ident-create "Signal")) "boolsig")))

(define (make-group rhs-list)
  (let ((n (length rhs-list)))
    (cond ((= n 1)  (car rhs-list))
	  ((= n 2)  (Apply (Apply diagram-group (car rhs-list)) (cadr rhs-list)))
	  (else     (make-group
		     (list (make-group (list (car rhs-list) (cadr rhs-list)) )
			   (make-group (cddr rhs-list))))))))

(define (make-list value-list)
  (let recur ((value-list (reverse value-list)) 
	      (value (Longid (Pident (ident-create "null")))))
    (if (null? value-list) value
	(recur (cdr value-list) 
	       (Apply (Apply (Longid (Pident (ident-create "cons"))) (car value-list)) 
		      value)))
    ))


(define (make-relations relation-list value)
  (if (null? relation-list) value
      (let ((relation (car relation-list)))
	(Apply
	 (Apply
	  (Apply
	   (Apply diagram-relation (Const `(label ,(relation-quantity relation))))
	   (Const `(label ,(relation-var relation))))
	  (relation-rhs relation))
	 (make-relations (cdr relation-list) value)))
      ))


(define (op->signal-function op)
  (let ((name (case op
		((+)   "add")
		((*)   "mul")
		((/)   "div")
		((>)   "gt")
		((<)   "lt")
		((>=)  "gte")
		((<=)  "lte")
		(else (->string op)))))
    (Longid (Pdot (Pident (ident-create "Signal")) name))))
    

(define (op->relation op)
  (Apply
   (Longid (Pdot (Pident (ident-create "Signal")) "relation"))
   (Const `(label ,op))))

(define (signal-operation? op)
  (case op
    ((add mul div gt gte lte neg cosh tanh log ln) #t)
    (else #f)))

    
(define (make-pure sf) (Apply diagram-pure sf))

(define (make-signal-expr expr)
  (cond ((number? expr) (Apply signal-realconst (Const `(real ,expr))))
	((symbol? expr) (case expr 
			  ((false) (Apply signal-boolconst (Const `(bool #f))))
			  ((true)  (Apply signal-boolconst (Const `(bool #t))))
			  (else (Longid (Pident (ident-create (->string expr)))))))
	(else
	 (match expr

		(('- a)  
		 (Apply (op->signal-function "neg") (make-signal-expr a)))

		(('- a b)  
		 (Apply (Apply (op->signal-function "sub") (make-signal-expr a))
			(make-signal-expr b)))
		
		(('if a b c)  
		 (Apply
		  (Apply (Apply (op->signal-function "if")
				(make-signal-expr a))
			 (make-signal-expr b))
		  (make-signal-expr c)))
		
		(((and op (? symbol?)) a b)
		 (Apply
		  (Apply (op->signal-function op) 
			 (make-signal-expr a))
		  (make-signal-expr b)))
		
		(((and op (? symbol?)) a)
		 (if (signal-operation? op)
		     (Apply (op->signal-function op) 
			    (make-signal-expr a))
		     (Apply (op->relation op) 
			    (make-signal-expr a))))
		
		(else (error 'make-signal-expr "invalid signal expression" expr))))
	))


(define (parse-sexpr-eqn x)
  (match x
	 (((or 'D 'd) (dep indep tstep) '= . rhs)
	  (let ((rhs   (parse-string-expr (->string rhs))))
	    (make-ode-eqn indep dep tstep (make-signal-expr rhs))))

	 (((and quantity (? symbol?)) (var) '= . rhs)
	  (let ((rhs (parse-string-expr (->string rhs))))
	    (make-relation quantity var (make-signal-expr rhs))))

	(((and quantity (? symbol?))  '= . rhs)
	 (let ((rhs  (parse-string-expr (->string rhs))))
	   (make-algebraic-eqn quantity (make-signal-expr rhs))))

	(else
	 (error 'parse-sexpr-eqn "invalid equation" x))
	))
		    

(define (make-ode-eqn-expr eqn)
  (and (ode-eqn? eqn) 
       (let ((rhs (ode-eqn-rhs eqn))
	     (dep (ode-eqn-dep eqn))
	     (indep (ode-eqn-indep eqn))
	     (tstep (ode-eqn-tstep eqn)))
	 (Apply
	  (Apply
	   (Apply
	    (Apply diagram-ode (make-list (list (Longid (Pident (ident-create (->string dep)))))))
	    (Longid (Pident (ident-create (->string indep)))))
	   (Longid (Pident (ident-create (->string tstep)))))
	  (make-pure rhs))
	 )))


(define (make-relation-expr eqn)
  (let ((rhs (relation-rhs eqn))
	(var (relation-var eqn))
	(quantity (relation-quantity eqn)))
    (Apply
     (Apply
      (Apply diagram-relation (Const `(label ,quantity)))
      (Const `(label ,var)))
     (make-pure rhs))
    ))


(define (make-algebraic-eqn-expr eqn)
  (let ((rhs (algebraic-eqn-rhs eqn))
	(quantity (algebraic-eqn-quantity eqn)))
    (Apply
     (Apply diagram-assign (make-list (list (Const `(label ,quantity)))))
     (make-pure rhs))
    ))


(define (make-algebraic-eqn-lst-expr eqlst)
  (let ((qs (map (lambda (x) (Const `(label ,(algebraic-eqn-quantity x)))) eqlst)))
    (Apply (Apply diagram-assign (make-list qs))
	   (make-group (map make-pure (map algebraic-eqn-rhs eqlst))))))


(define (make-ode-eqn-lst-expr eqlst)
  (let ((tsteps (delete-duplicates (map ode-eqn-tstep eqlst)))
	(indeps (delete-duplicates (map ode-eqn-indep eqlst)))
	(deps   (map  ode-eqn-dep eqlst)))
    (match (list deps indeps tsteps)
	   (((dep . _) (indep) (tstep))
	    (Apply
	     (Apply
	      (Apply 
	       (Apply diagram-ode (make-list (map (lambda (x) (Longid (Pident (ident-create (->string x))))) deps)))
	       (Longid (Pident (ident-create (->string indep)))))
	      (Longid (Pident (ident-create (->string tstep)))))
	     (make-group (map make-pure (map ode-eqn-rhs eqlst)))))
	   (else (error 'parse-sexpr-macro "invalid system of ODE equations" eqlst)))))


(define (make-dae-eqn-lst-expr eqlst)
  (let-values (((relations ode-eqs) (partition relation? eqlst)))
    (let ((tsteps (delete-duplicates (map ode-eqn-tstep ode-eqs)))
	  (indeps (delete-duplicates (map ode-eqn-indep ode-eqs)))
	  (deps   (map ode-eqn-dep ode-eqs)))
      (match (list deps indeps tsteps)
	     (((dep . _) (indep) (tstep))
	      (Apply
	       (Apply
		(Apply
		 (Apply diagram-ode (make-list (map (lambda (x) (Longid (Pident (ident-create (->string x))))) deps)))
		 (Longid (Pident (ident-create (->string indep)))) )
		(Longid (Pident (ident-create (->string tstep)))))
	       (make-relations relations (make-group (map make-pure (map ode-eqn-rhs ode-eqs))))))
	     
	     (else (error 'parse-sexpr-macro "invalid system of DAE equations" eqlst))
	     ))))

(define (parse-sexpr-macro lst)
  (match lst

	 (((? symbol?) . rest)
	  (let ((eqn (parse-sexpr-eqn lst)))
	    (cond ((ode-eqn? eqn)   (make-ode-eqn-expr eqn))

		  ((relation? eqn) (make-relation-expr eqn))

		  ((algebraic-eqn? eqn) (make-algebraic-eqn-expr eqn))
		  
		  )))


	 (((? pair?) . rest)

	  (let ((eqlst (map parse-sexpr-eqn lst)))

	    (cond ((every algebraic-eqn? eqlst) 
		   (make-algebraic-eqn-lst-expr eqlst))

		 ((every ode-eqn? eqlst)
		  (make-ode-eqn-lst-expr eqlst))

		 ((every ode-eqn-or-relation? eqlst)
		  (make-dae-eqn-lst-expr eqlst))
			  
		 (else
		  (error 'parse-sexpr-macro "invalid system of equations" eqlst)))))
		
	(else (error 'parse-sexpr-macro "invalid equational expression" lst))
	))


(define (parse loc s)
  (cond ((port? s)   (lexer-init 'port s))
	((string? s) (lexer-init 'string s))
	(else (error 'parse "bad argument type; not a string or port" s)) )
   (parser lexer (make-parse-error loc)))



(define nineml-xmlns "http://nineml.org/9ML/0.1")

(define (parse-al-sxml-dynamics sxml)
  (let ((state-variables  ((sxpath `(// nml:StateVariable)) sxml))
	(regimes ((sxpath `(// nml:Regime)) sxml)))

;; TODO: ensure that parameters and state variables are consistent in the equations

    (if (pair? regimes)
	(cond
	 ((= (length regimes) 1)
	       (let ((r (car regimes)))
		 (let ((time-derivatives  ((sxpath `(// nml:TimeDerivative)) r))
		       (on-conditions     ((sxpath `(// nml:OnCondition)) r)))

		   (if (> (length on-conditions) 1)
		       (error 'parse-al-sxml-dynamics "multiple on-conditions blocks in regime" r))
		   (if (null? time-derivatives)
		       (error 'parse-al-sxml-dynamics "regime does not contain time derivative blocks" r))
		   (let ((ode-variables (map (lambda (x) 
					       (string->symbol (sxml:attr x 'variable )))
					     time-derivatives))
			 (ode-rhss      (map (lambda (x)
					       (parse-string-expr 
						(sxml:kidn-cadr 'nml:MathInline x )
						'parse-al-sxml-dynamics))
					     time-derivatives)))
		     (if (null? on-conditions)

			 (make-ode-eqn-lst-expr
			  (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs)))  ode-variables ode-rhss)))

			 (let ((c (car on-conditions)))
			   (let (( trigger (sxml:kidn-cadr 'nml:Trigger c))
				 ( event-out (sxml:kidn 'nml:EventOut c))
				 ( state-assignments ((sxpath `(// nml:StateAssignment)) c)))
				 
			     (if (not trigger) (error 'parse-al-sxml-dynamics "on-condition without trigger" c))
			     (if (not event-out) (error 'parse-al-sxml-dynamics "on-condition without event-out" c))

			     (let ((trigger-rhs (parse-string-expr 
						 (sxml:text trigger) 
						 'parse-al-sxml-dynamics))
				   (trigger-name (string->symbol (sxml:attr event-out 'port )))
				   (assign-variables (map (lambda (x) 
							    (string->symbol (sxml:attr  x 'variable))) 
							  state-assignments))
				   (assign-rhss      (map (lambda (x)
							    (parse-string-expr 
							     (sxml:kidn-cadr 'nml:MathInline x) 
							     'parse-al-sxml-dynamics))
							  state-assignments)))
			       (Apply

				(Apply
				 
				 (Apply diagram-transient
					
					(Apply
					 (Apply diagram-union 
						(Apply (Apply diagram-assign (make-list (list (Const `(label ,trigger-name)))))
						       (make-pure (make-signal-expr trigger-rhs))))
					 
					 (make-ode-eqn-lst-expr
					  (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs)))  
					       ode-variables ode-rhss))))
				 
				 (make-algebraic-eqn-lst-expr
				  (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs))  )
				       assign-variables assign-rhss)))

				(Apply
				 (Apply signal-boolsig (Const `(label ,trigger-name)))
				 (Apply signal-boolconst (Const `(bool #f)))))

				))
			   ))
			 )))
#|
	      ((= (length regimes) 2)
	       (let ((r (car regimes))
		     (s (cadr regimes)))
		 (let ((time-derivatives  ((sxpath `(// TimeDerivative)) r))
		       (on-condition      ((sxpath `(// OnCondition)) s)))
		   (if (null? time-derivatives)
		       (error 'parse-al-sxml-dynamics "regime does not contain time derivative blocks" r))
		   (let ((variables (map (lambda (x) (sxml:attr 'variable x)) time-derivatives))
			 (rhss      (map (lambda (x)
					   (PURE (sexpr->function
						  (parse-string-expr 
						   (sxml:kidn-cadr 'MathInline x) 
						   'parse-al-sxml-dynamics))))
					 time-derivatives)))

		     (make-ode-system 'h 't rhss)
		     ))
		   ))
|#
	      ((> (length regimes) 2)
	       (error 'parse-al-sxml-dynamics "maximum of two regimes is supported" sxml))

	      )
	'())))
  


(define (parse-al-sxml-component sxml)
  
  (let ((dynamics    (safe-car ((sxpath `(// nml:Dynamics)) sxml)))
	(parameters  ((sxpath `(// nml:Parameter))  sxml))
	(ports       ((sxpath `(// (*or* nml:AnalogPort nml:EventPort)))  sxml))
	(name        (sxml:attr sxml 'name)))

   (let ((dynamics-body 
	   (cond (dynamics => parse-al-sxml-dynamics)
		 (else '())))

	 (dynamics-args
	  (cons* "t" "h" (map (lambda (x) (sxml:attr x 'name)) (append parameters ports))))
	 )


     (Value_def (ident-create name) 
		(let recur ((args dynamics-args) (ax dynamics-body))
		  (if (null? args) ax
		      (recur (cdr args) (Function (ident-create (car args)) ax)))))
     )))



(define (parse-al-sxml al-sxml)
  (let ((al-sxml-defs ((sxpath `(// nml:ComponentClass))  al-sxml)) )

    (map parse-al-sxml-component al-sxml-defs)

    ))



)
