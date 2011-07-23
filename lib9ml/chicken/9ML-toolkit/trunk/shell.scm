;
;;  A type checker and test report generator for NineML.
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

(require-extension setup-api srfi-13 datatype matchable static-modules miniML miniMLsyntax miniMLeval )
(require-extension getopt-long object-graph)
(require-extension 9ML-parse 9ML-repr)


(define-values (env-binding? env-empty env-add-signature env-add-module env-add-type env-add-spec env-add-value
	        env-find-value env-find-type env-find-module env-find)
  (make-mod-env core-syntax))

(define-values (scope-typedecl scope-modtype scope-signature scope-modterm scope-moddef)
  (make-mod-scoping core-syntax core-scoping))

(define-values (check-modtype check-signature type-modterm type-moddef type-definition)
  (make-mod-typing core-syntax core-typing))

(include "NineMLcore.scm")
(include "NineMLsignal.scm")
(include "NineMLdiagram.scm")
(include "NineMLinterval.scm")
(include "NineMLgraph.scm")

(define init-scope      (make-parameter st-empty))
(define init-type-env   (make-parameter env-empty))
(define init-eval-env   (make-parameter env-empty))

(define (enter-typedecl id decl)
  (init-scope (st-enter-type id (init-scope)))
  (init-type-env   (env-add-type id decl (init-type-env))))

(define (enter-valtype name ty)
  (let ((id (ident-create name)))
    (init-scope (st-enter-value id (init-scope)))
    (init-type-env   (env-add-value id ty (init-type-env)))))

(define (enter-val name val)
  (let ((id (or (and (ident? name) name) (ident-create name))))
    (init-eval-env (ident-add id val (init-eval-env)))))

(core-initialize enter-typedecl enter-valtype)
(eval-cbv-initialize enter-val)

(define (enter-module id mty)
  (init-scope (st-enter-module id (init-scope)))
  (init-type-env (env-add-module id mty (init-type-env))))


(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))

(define opt-defaults
  `(
    (prompt . "9ML> ")
    ))

(define (defopt x)
  (lookup-def x opt-defaults))
                 
(define opt-grammar
  `(

    (prompt           "sets command prompt to the given string"
		      (value (required STRING)
			     (default ,(defopt 'prompt))
			     ))

    (output-sxml       "sets output format to SXML")
    (output-xml        "sets output format to XML")

    (help             (single-char #\h))           

    ))

(define variable-names (make-parameter '()))

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
     (run:execute* (list `exp ...)))))

(define-syntax run-
  (syntax-rules ()
    ((_ exp ...)
     (run:execute (list `exp ...)))))



;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (shell:usage)
  (print "Usage: " (car (argv)) " [options...] file1... ")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))

;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define (interpreter in #!key
		     (scope    (init-scope))
		     (type-env (init-type-env))
		     (eval-env (init-eval-env)))

    (let ((defs (parse 'NineML in)))

	     (let* (
		    (scoped-defs      (scope-moddef scope defs))
		    (mty              (type-moddef type-env '() scoped-defs))
		    (eval-env1        (mod-eval-cbv eval-env scoped-defs))
		    (type-env1        (fold (lambda (x ax) 
					      (cases modspec x 
						     (Value_sig  (id vty)   (env-add-value  id vty ax))
						     (Type_sig   (id decl)  (env-add-type   id decl ax))
						     (Module_sig (id mty)   (env-add-module id mty ax))
						     )) 
					    type-env mty))
		    (scope1          (fold (lambda (x ax) 
					     (cases modspec x
						    (Value_sig  (id vty)   (st-enter-value id ax))
						    (Type_sig   (id decl)  (st-enter-type id ax))
						    (Module_sig (id mty)   (st-enter-module id ax))
						    )) 
					   scope mty))
		    )
	       

	       (list (list scoped-defs mty) 
		     (list scope1 type-env1 eval-env1) ))
	))


(define (handle thunk kont)
  (condition-case (thunk)
		  [(exn arity)      (begin 
				      (print "wrong number of arguments")
				      (kont))]
		  [var ()           (begin 
				      (print-error-message var)
				      (kont))]
		  ))

(define (repl #!key 
	      (prompt "9ML> ") 
	      (output-type #f)
	      (scope    (init-scope))
	      (type-env (init-type-env))
	      (eval-env (init-eval-env)))

  (define (complete-phrase? line)
    (let ((s (string-trim-right line)))
      (string-index line #\; )))

  (letrec
      ((recur
	(lambda (scope type-env eval-env)

	  (printf "~A " prompt)
	  
	  (let line-recur ((line (read-line (current-input-port)))
			   (lines '()))
	    
	    (if (eof-object? line)

		(list scope type-env eval-env)
		
		(if (not (complete-phrase? line))
		    
		    (begin
		      (printf "-> " )
		      (line-recur (read-line (current-input-port)) (cons line lines)))
		    
		    (let ((text (string-concatenate (reverse (cons line lines)))))
		      
		      (handle 

		       (lambda ()
			 (match-let ((((scoped-defs mty) 
				       (scope1 type-env1 eval-env1))
				      
				      (interpreter text 
						   scope: scope 
						   type-env: type-env 
						   eval-env: eval-env)))
				 
				 (let* ((type-alist (map (lambda (x) (cases modspec x
									    (Value_sig (id vty) (cons id x))
									    (Type_sig (id decl) (cons id x))
									    (Module_sig (id mty) (cons id x))
									    )) 
							 mty))
					(type-alist (filter (lambda (x) (not (assoc (car x) (init-type-env)))) type-alist)))
				   (print-type-env  type-alist output-type))
				 (newline)
				 (recur scope1 type-env1 eval-env1)
				 ))

		       (lambda ()
			 (recur scope type-env eval-env)))

		    ))))
	  )))

    (recur scope type-env eval-env)))



(define (main options operands)

  (if (options 'help) (shell:usage))

  (let ((find-module (lambda (x) (env-find-module x (init-type-env)))))

    (for-each (lambda (init name) (init name enter-module find-module init-eval-env))
	      (list Signal:module-initialize   
		    Diagram:module-initialize  
		    Interval:module-initialize 
		    Graph:module-initialize
		    )
	      (list "Signal" "Diagram" "Interval" "Graph" )) )

  (let ((output-type (cond ((options 'output-xml)  'xml)
			   ((options 'output-sxml) 'sxml)
			   (else #f))))

    (let* ((uenv (list (init-scope) (init-type-env) (init-eval-env)))
	   (uenv (if (pair? operands) 
		     (fold (lambda (x uenv) 
			     (let ((in (open-input-file x)))
			       (match-let (((scope type-env eval-env) uenv))
					  (match-let (((_ uenv1)
						       (interpreter in 
								    scope: scope 
								    type-env: type-env 
								    eval-env: eval-env)))
						     (close-input-port in)
						     uenv1))))
			   (list (init-scope) (init-type-env) (init-eval-env))
			   operands)
		     uenv)))

      (match-let (((scope type-env eval-env) uenv))
		 
		 (repl prompt:     (or (options 'prompt) (defopt 'prompt))
		       output-type: output-type
		       scope:       scope
		       type-env:    type-env
		       eval-env:    eval-env)
	))

    ))

(main opt (opt '@))
