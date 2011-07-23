;;
;;  NineML IVP code generator for MLton.
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


(module 9ML-ivp-mlton

	(ivp-mlton mlton-initial)

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file)
	(only data-structures conc alist-ref intersperse)
	(only srfi-13 string-concatenate))
(require-extension setup-api datatype signal-diagram 9ML-repr)


(define nl "\n")

	
(define (mlton-initial ic)
  (let ((mlvalue (lambda (v) 
		   (cond ((and (number? v) (negative? v)) (string-append "~" (sprintf "~A" (abs v))))
			 ((boolean? v)  (if v "true" "false"))
			 (else (sprintf "~A" v))))))
    (let ((ic (map (lambda (x) (let ((v (cdr x))) (cons (car x) (mlvalue v)))) ic)))
      (string-append "val initial = {" (string-concatenate (intersperse (map (lambda (x) (sprintf "~A=(~A)" (car x) (cdr x))) ic) ",")) "}"))))


 
(define mlton-run-prelude
#<<EOF

fun putStrLn str = 
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))
    
fun putStr str = 
    (TextIO.output (TextIO.stdOut, str))
    
fun showBoolean b = (if b then "1" else "0")

fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end

EOF
)


(define (mlton-printstate ivar dvars ic)
  (string-append
   (sprintf "fun printstate (input) = ~%")
   "("
   (sprintf "(showReal (#~A(input)))"  ivar)
   (string-concatenate
    (map (lambda (dvar)
	   (let ((v (alist-ref dvar ic)))
	     (let ((show (cond ((number? v) "showReal")
			       ((boolean? v) "showBoolean")
			       (else         ""))))
	       (sprintf "^ \" \" ^ (~A (#~A(input)))" show dvar)))) dvars))
   ")" ))


(define (mlton-run-start ivar dvars ic)
  (let* ((states (cons ivar dvars))
	 (f       (lambda (x) (let ((n (car x)))
				(sprintf "~A=(#~A(~A))" n n (if (member n states) "nstate" "initial") ))))
	 (nstate1 (string-append "{" (string-concatenate (intersperse (map f ic) ",")) "}")))
    (sprintf
#<<EOF

fun start (tmax,f,initial) =
let
  fun run (input) =
    let val nstate = f input
        val nstate1 = ~A
    in putStrLn (printstate nstate1);
       if (#~A nstate)  > tmax
       then (putStrLn "# All done!"; nstate1)
       else (run nstate1)
    end
in
  run (initial)
end
 
EOF
   nstate1 ivar)))


(define (ivp-mlton prefix ivp-id ivar dvars pvars start end ic sd)
  (let* ((dir          (or (pathname-directory prefix) "."))
	 (shared-dir   (chicken-home))
	 (flsim-dir    (make-pathname shared-dir "flsim"))
	 (solver-path  (make-pathname (pathname-directory prefix) (conc ivp-id "_solver.sml")))
	 (run-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.sml")))
	 (mlb-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.mlb")))
	 (exec-path    (make-pathname dir (sprintf "~A_run" ivp-id)))
	 (log-path     (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (mlton-path  "mlton"))
    
    (make (
	   (solver-path (prefix)
			(with-output-to-file solver-path (lambda () (codegen/ML ivp-id sd solver: 'rk3))))
	   
	   (run-path (prefix)
		     (with-output-to-file run-path 
		       (lambda () 
			 (print-fragments
			  `(
			    (,mlton-run-prelude)
			    (,(mlton-printstate ivar dvars ic) ,nl)
			    (,(mlton-run-start ivar dvars ic) ,nl)
			    (,(mlton-initial ic) ,nl)
			    ,(sprintf "val _ = (printstate initial; start (~A, Model.~A, initial))~%~%" end ivp-id)
			    )))))
	   
	   (mlb-path ()
		     (with-output-to-file mlb-path
		       (lambda () 
			 (print-fragments
			  `(("$(SML_LIB)/basis/basis.mlb" ,nl )
			    ("$(RK_LIB)/rk.mlb" ,nl )
			    ("local " ,nl)
			    (,(sprintf "    ~A_solver.sml" ivp-id) ,nl)
			    ("in" ,nl)
			    ("    structure Model" ,nl)
			    ("end" ,nl)
			    ,(sprintf "~A_run.sml" ivp-id) ,nl))
			 )))
	   
	   (exec-path (solver-path run-path mlb-path)
		      (run (,mlton-path -link-opt -s 
					-mlb-path-var ,(string-append "'RK_LIB " flsim-dir "/sml-lib/rk'") 
					,mlb-path)))
	   
	   (log-path (exec-path)
		     (run (,exec-path > ,log-path)))
	   )
      (list log-path) )
    ))

)
