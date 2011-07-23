;;
;;  NineML IVP code generator for Chicken Scheme.
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


(module 9ML-ivp-chicken

	(ivp-chicken)

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file)
	(only data-structures conc))
(require-extension setup-api datatype signal-diagram 9ML-repr)


(define nl "\n")
	

(define chicken-run
#<<EOF

(define-syntax run
  (syntax-rules ()
    ((_ f indep dep end input parameters)
     (let ((nstate input))
       (printf "# ~A " indep)
       (for-each (lambda (x) (printf "~A " x)) dep)
       (printf "~%")
       (let recur ((nstate nstate))
	 (let ((ival (alist-ref indep nstate)))
	   (printf "~A " ival)
	   (for-each (lambda (x)
		       (let ((v (alist-ref x nstate)))
			 (printf "~A " (if (boolean? v) (or (and v 1) 0) v))))
		     dep)
	   (printf "~%")
	   (if (> ival end)
	       (print "# All done!")
	       (recur (append (f nstate) parameters))))))
     )))
EOF
)

(define (ivp-chicken prefix ivp-id ivar dvars pvars start end ic sd)

  (let* ((dir (or (pathname-directory prefix) "."))
	 (solver-path (make-pathname dir (conc ivp-id "_solver.scm")))
	 (run-path    (make-pathname dir (sprintf "~A_run.scm" ivp-id)))
	 (exec-path   (make-pathname dir (sprintf "~A_run" ivp-id)))
	 (log-path    (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (csc-path    (make-pathname (program-path) "csc")))
    
    (make 
	(
	 (solver-path (prefix)
		      (with-output-to-file solver-path
			(lambda () (codegen/scheme ivp-id sd solver: 'rk3))))
	 
	 (run-path (prefix)
		   (with-output-to-file run-path
		     (lambda () 
		       (print-fragments
			(list
			 (sprintf "(include \"~A_solver.scm\")~%~%" ivp-id)
			 chicken-run nl
			 (sprintf "(define initial (quote ~A))~%~%" (cons (cons ivar start) ic))
			 (sprintf "(define parameters (quote ~A))~%~%" (map (lambda (x) (assoc x ic)) pvars))
			 (sprintf "(run ~A (quote ~A) (quote ~A) ~A initial parameters)~%~%" ivp-id ivar dvars end)
			 )))))
	 
	 (exec-path (run-path solver-path)
		    (run (,csc-path -w -I ,dir -b -S -O3 -d0 -heap-initial-size 4096k ,run-path)))
	 
	 (log-path (exec-path) (run (,exec-path > ,log-path)))
	 )
      
      (list log-path) )
    ))


)
