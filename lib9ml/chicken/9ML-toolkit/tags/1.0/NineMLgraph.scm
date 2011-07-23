
(define (Graph:module-initialize module-name enter-module find-module eval-env)

  (define ident-graph   (ident-create "graph"))
  (define path-graph    (Pident ident-graph))
  (define (graph-type n e)   (Tcon path-graph `(,n ,e)))

  (define path-nat   (Pident (ident-create "nat")))
  (define nat-type   (Tcon path-nat '()))

  (define path-interval   (Pdot (Pident (ident-create "Interval")) "interval"))
  (define interval-type   (Tcon path-interval '()))

  (define path-real   (Pident (ident-create "real")))
  (define real-type   (Tcon path-real '()))

  (define-values (type-variables reset-type-variables
				 find-type-variable 
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type bot-type
				 )
    (core-utils))

  (let* ((alpha (newvar)) (beta (newvar)) 
	 (talpha (Tvar alpha)) (tbeta (Tvar beta)) )

    (let

      (
	(sig
	 (append
	  (list

	   (Type_sig ident-graph (make-typedecl (make-kind 2) #f))
	   
	   (Value_sig (ident-create "empty") 
		      (make-valtype `(,alpha ,beta) (graph-type talpha tbeta)))


	   (Value_sig (ident-create "add_node") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) (arrow-type nat-type (graph-type talpha tbeta)))))

	   (Value_sig (ident-create "add_node_interval") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) (arrow-type interval-type (graph-type talpha tbeta)))))


	   (Value_sig (ident-create "nodes") 
		      (make-valtype `(,alpha ,beta) (arrow-type nat-type (graph-type talpha tbeta))))


	   (Value_sig (ident-create "add_edge") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type nat-type
							    (arrow-type nat-type (graph-type talpha tbeta))))))


	   (Value_sig (ident-create "node_property") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type nat-type
							    (arrow-type label-type
									(Tvar alpha) )))))
	   
	   (Value_sig (ident-create "set_node_property") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type nat-type
							    (arrow-type label-type
									(arrow-type (Tvar alpha) 
										    (graph-type talpha tbeta)))))))
	   
	   (Value_sig (ident-create "set_node_interval_property") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type interval-type
							    (arrow-type label-type
									(arrow-type (Tvar alpha) 
										    (graph-type talpha tbeta)))))))
	   
	   (Value_sig (ident-create "set_edge_property") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type nat-type
							    (arrow-type nat-type
									(arrow-type label-type
										    (arrow-type (Tvar beta) 
												(graph-type talpha tbeta))))))))
	   
	   (Value_sig (ident-create "set_edge_interval_property") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type interval-type
							    (arrow-type interval-type
									(arrow-type label-type
										    (arrow-type (Tvar beta) 
												(graph-type talpha tbeta))))))))
	   
	   
	   (Value_sig (ident-create "union") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type (graph-type talpha tbeta)
							    (graph-type talpha tbeta)))))


	   (Value_sig (ident-create "disjoint_union") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type (graph-type talpha tbeta) 
						(arrow-type (graph-type talpha tbeta) 
							    (graph-type talpha tbeta)))))

	   
	   (Value_sig (ident-create "complete_graph") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type nat-type (graph-type talpha tbeta))))


	   (Value_sig (ident-create "complete_bipartite_graph") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type nat-type (arrow-type nat-type (graph-type talpha tbeta)))))

	   
	   (Value_sig (ident-create "gnp_graph") 
		      (make-valtype `(,alpha ,beta) 
				    (arrow-type nat-type (arrow-type real-type (graph-type talpha tbeta)))))


	   (Value_sig (ident-create "small_world_graph") 
		      (make-valtype `(,alpha ,beta) 

				    (arrow-type nat-type
						(arrow-type nat-type
							    (arrow-type real-type (graph-type talpha tbeta))))))

	   
	   

	  )))

	(struct 
	 (append
	  (list 
	   
	   (Type_def ident-graph (make-kind 2) 
		     (make-deftype `(,alpha ,beta) (Tcon path-graph '()) ))

	   (datacon 'graph 'empty 0)
	   (datacon 'graph 'add_node 2)
	   (datacon 'graph 'nodes 1)
	   (datacon 'graph 'add_edge 3)
	   (datacon 'graph 'node_property 3)
	   (datacon 'graph 'set_node_property 4)
	   (datacon 'graph 'set_node_interval_property 4)
	   (datacon 'graph 'set_edge_property 5)
	   (datacon 'graph 'set_edge_interval_property 5)

	   (datacon 'graph 'union 2)
	   (datacon 'graph 'disjoint_union 2)
	   (datacon 'graph 'complete_graph 1)
	   (datacon 'graph 'complete_bipartite_graph 2)
	   (datacon 'graph 'gnp_graph 2)
	   (datacon 'graph 'small_world_graph 3)
	  
	 )))
	)
    
    (let ((modname (ident-create module-name)))
      (enter-module modname  (Signature sig))
      (eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
      )
    )))
    


