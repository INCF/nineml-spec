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

(require-extension setup-api srfi-13 datatype static-modules miniML miniMLsyntax miniMLeval )
(require-extension getopt-long ssax sxml-transforms sxpath sxpath-lolevel object-graph)
(require-extension 9ML-parse 9ML-repr )

(include "SXML.scm")
(include "SXML-to-XML.scm")

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
    ))

(define (defopt x)
  (lookup-def x opt-defaults))
                 
(define opt-grammar
  `(

    (print-type-env  "prints the type environment of each operand"
		     (single-char #\t)
		     (value (optional COMPONENT-LIST)
			    (default all)
			    (transformer 
			     ,(lambda (x) 
				(if (string=? x "all") x
				    (list (string-split x ",")))))))

    (print-eval-env  "prints the evaluation environment of each operand"
		     (single-char #\e)
		     (value (optional COMPONENT-LIST)
			    (default all)
			    (transformer 
			     ,(lambda (x) 
				(if (string=? x "all") x
				    (list (string-split x ",")))))))

    (print-source-defs  "prints the source definitions of each operand"
			(single-char #\s))

    (nxml            "prints canonical NineML XML representation of each operand"
		     (single-char #\n))

    (html-report        "prints out an HTML report of the unified environments of each operand")

    (output-sxml       "sets output format to SXML")
    (output-xml        "sets output format to XML")

    (help         (single-char #\h))           

    ))




(define (ensure-xmlns doc)
  (let ((doc1 (sxml:add-attr doc '(xmlns:nineml "nineml"))))
    (sxml:add-attr doc1 '(xmlns nineml))))

(define (ensure-xmlver doc)
  (let ((doc1 (sxml:add-attr doc '(nineml:version "Chicken:20101129"))))
    doc1))

(define-record-printer (value x out)
  (fprintf out "#<value ~S>"
	   (cases value x
		  (Const_v (c) `(Const ,c))
		  (Closure_v (body env) 
			     (if (null? env) `(Closure ,body ())
				 `(Closure ,body (,(car env) ...))))
		  (Prim_v (p) `(Prim ,p))
		  (Tuple_v (d) `(Data ,d)))))

      
(define (print-eval-env env . rest)
  (let-optionals rest ((output-type #f)  (component-filter identity))
          (let ((env (filter-map component-filter env)))

		 (case output-type
		   ((sxml )
		    (pp (eval-env->sxml env)))


		   ((xml )
		    (let* ((doc1   `(toplevel ,@(eval-env->sxml env)))
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
	       (let* ((doc1   `(toplevel ,@(map (compose modspec->sxml cdr) env)))
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
		    (let* ((doc1   `(toplevel ,@(map moddef->sxml defs)))
			   (doc2  (ensure-xmlns doc1))
			   (doc3  (ensure-xmlver doc2)))
		      (print-fragments (generate-XML `(begin ,doc3)))))
		       
		   (else  (pp defs))

		   )))

(define nl "\n")



;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (report:usage)
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


(define (interpreter operand)
  (let ((in (open-input-file operand)))
    (let ((defs (parse 'NineML in)))
      (close-input-port in)
      (let* ((scoped-defs      (scope-moddef (init-scope) defs))
	     (mty              (type-moddef (init-type-env) '() scoped-defs))
	     (type-env         (map (lambda (x) (cases modspec x
						       (Value_sig (id vty) (cons id x))
						       (Type_sig (id decl) (cons id x))
						       (Module_sig (id mty) (cons id x))
						       )) mty))
	     (eval-env         (mod-eval-cbv (init-eval-env) scoped-defs))
	     (unified-env      (list scoped-defs 
				     (filter (lambda (x) (not (assoc (car x) (init-type-env)))) type-env) 
				     (filter (lambda (x) (not (assoc (car x) (init-eval-env)))) eval-env) ))
	     
	     )
	unified-env
	))))


(define (diagram-hook prefix label value)
  (and (pair? label) (string=? (car label) "diagram") ;; value is a diagram
       (let* ((diagram-id (gensym 'diagram))
	      (diagram-link `(img (@ (src ,(string-append (->string diagram-id) ".png"))) (alt "NineML diagram"))))
	 (generate-diagram prefix diagram-id value)
	 `(,(line "binding " `(b ,name) " = ") ,diagram-link))))


(define (main options operands)

  (if (options 'help) (report:usage))

  (let ((find-module (lambda (x) (env-find-module x (init-type-env)))))
    (for-each (lambda (init name) (init name enter-module find-module init-eval-env))
	      (list Signal:module-initialize   
		    Diagram:module-initialize  
		    Interval:module-initialize 
		    Graph:module-initialize )
	      (list "Signal" "Diagram" "Interval" "Graph" )))

  (let ((output-type (cond ((options 'output-xml)  'xml)
			   ((options 'output-sxml) 'sxml)
			   (else #f))))
    (cond ((null? operands)  (report:usage))
	  (else
	   (let ((unified-envs (map interpreter operands)))
	     (for-each
	      (lambda (operand uenv)
		(let ((source-defs (car uenv))
		      (type-env    (cadr uenv))
		      (eval-env    (caddr uenv)))
	     
		  (let ((type-env-opt    (options 'print-type-env))
			(eval-env-opt    (options 'print-eval-env))
			(source-defs-opt (options 'print-source-defs))
			(html-report-opt (options 'html-report))
			(nxml-opt        (options 'nxml)))

		    (if type-env-opt
			(if (and (string? type-env-opt) (string=?  type-env-opt "all"))
			    (print-type-env type-env output-type)
			    (let ((fc (lambda (x) (and (member (ident-name (car x)) type-env-opt) x))))
			      (print-type-env type-env output-type fc))))

		    (if eval-env-opt
			(if (and (string? eval-env-opt) (string=? eval-env-opt "all"))
			    (print-eval-env eval-env output-type)
			    (let ((fc (lambda (x) (and (member (ident-name (car x)) eval-env-opt) x))))
			      (print-eval-env eval-env output-type fc))))

		    (if source-defs-opt (print-source-defs source-defs output-type))
		    
		    (if html-report-opt (html-report operand uenv value-hook: diagram-hook))

		    (if nxml-opt (print-nxml operand uenv))
		  )))

	      operands unified-envs)))
	  )))

(width 40)
(main opt (opt '@))
