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

	(parse)

	(import scheme chicken 
		(only srfi-1 fold combine every unzip2 filter-map partition delete-duplicates)
		(only srfi-13 string-null?)
		(only data-structures conc ->string)
		(only extras fprintf))
	(require-extension extras matchable static-modules miniML miniMLsyntax)

  (define-values (type-variables reset-type-variables
				 find-type-variable 
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type bot-type
				 )
    (core-utils))


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
(include "expr.grm.scm")

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
(define diagram-relation     (Longid (Pdot (Pident (ident-create "Diagram")) "RELATION")))

(define signal-realconst     (Longid (Pdot (Pident (ident-create "Signal")) "realconst")))
(define signal-boolconst     (Longid (Pdot (Pident (ident-create "Signal")) "boolconst")))

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


(define (sexpr-eqn-parser x)
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
	 (error 'sexpr-eqn-parser "invalid equation" x))
	))
		    

(define (sexpr-parser lst)
  (match lst

	 (((? symbol?) . rest)
	  (let ((eqn (sexpr-eqn-parser lst)))
	    (cond ((ode-eqn? eqn) 
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
		     ))

		  ((relation? eqn) 
		   (let ((rhs (relation-rhs eqn))
			 (var (relation-var eqn))
			 (quantity (relation-quantity eqn)))
		     (Apply
		      (Apply
		       (Apply diagram-relation (Const `(label ,quantity)))
		       (Const `(label ,var)))
		      (make-pure rhs))
		     ))

		  ((algebraic-eqn? eqn) 
		   (let ((rhs (algebraic-eqn-rhs eqn))
			 (quantity (algebraic-eqn-quantity eqn)))
		     (Apply
		      (Apply diagram-assign (make-list (list (Const `(label ,quantity)))))
		      (make-pure rhs))
		     ))
		  
		  )))


	 (((? pair?) . rest)

	  (let ((eqlst (map sexpr-eqn-parser lst)))

	    (cond ((every algebraic-eqn? eqlst) 
		   (let ((qs (map (lambda (x) (Const `(label ,(algebraic-eqn-quantity x)))) eqlst)))
		     (Apply (Apply diagram-assign (make-list qs))
			    (make-group (map make-pure (map algebraic-eqn-rhs eqlst))))))

		 ((every ode-eqn? eqlst)
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
			   (else (error 'sexpr-parser "invalid system of ODE equations" eqlst)))))

		 ((every ode-eqn-or-relation? eqlst)
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

			     (else (error 'sexpr-parser "invalid system of DAE equations" eqlst))
			     ))))
			  
		 (else
		  (error 'sexpr-parser "invalid system of equations" eqlst)))))
		
	(else (error 'sexpr-parser "invalid equational expression" lst))
	))


(define (parse loc s)
  (cond ((port? s)   (lexer-init 'port s))
	((string? s) (lexer-init 'string s))
	(else (error 'parse "bad argument type; not a string or port" s)) )
   (parser lexer (make-parse-error loc)))

)
