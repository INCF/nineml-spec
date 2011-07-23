

(define pair (Longid (Pident (ident-create "pair"))))

(define (datacon type label arity . rest)
  (let-optionals rest ((op label))
		 
		 (Value_def (ident-create (->string label))
			    (let ((body (lambda (x) (Apply (Apply pair (Const `(label ,type))) 
							   (Apply (Apply pair (Const `(label ,op))) x))))
				  (xs  (list-tabulate arity (lambda (i) (ident-create (symbol->string (gensym 'x)))))))
			      
			      (let recur ((xs xs) (rxs (reverse xs)) (body body))
				(if (pair? xs)
				    (recur (cdr xs) (cdr rxs)
					   (lambda (y) (Function (car rxs) (body (Apply (Apply pair (Longid (Pident (car xs)))) y)))))
				    (body (Longid (Pident (ident-create "empty"))))))

			      ))))
