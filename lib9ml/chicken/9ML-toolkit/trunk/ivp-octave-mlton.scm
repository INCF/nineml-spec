;;
;;  NineML IVP code generator for Octave/MLton.
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


(module 9ML-ivp-octave-mlton

	(ivp-octave-mlton)

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file absolute-pathname? )
	(only data-structures conc alist-ref intersperse)
	(only posix current-directory)
	(only srfi-1 filter list-index)
	(only srfi-13 string-concatenate))
(require-extension setup-api datatype signal-diagram 9ML-repr 9ML-ivp-mlton)


(define nl "\n")


(define (octave/mlton-m ivp-id ivar hvar dvars ic imax log-path adir)
  (let* ((states (cons ivar dvars))
	 (parameters (filter (lambda (x) (not (member (car x) states))) ic)))
  `(,(sprintf "addpath (~S);~%" adir )
    ,(sprintf "i = 1; t = 0; tmax = ~A;~%" imax)
    ,(sprintf "~A_open();~%" ivp-id)
    ,(sprintf "[state parameters] = ~A_initial();~%" ivp-id)
    ,(sprintf "N = size(state)(1);~%" )
    ,(sprintf "log = repmat(state,[1 ((tmax-t+1)/(parameters(~A)))]);~%" 
	      (+ 1 (list-index (lambda (x) (eq? (car x) hvar)) parameters))) 
    ,(sprintf "tic;~%")
    ,(sprintf "while (t < tmax) ~%")
    ,(sprintf "  ~A_run(state,parameters,log,i);~%" ivp-id)
    ,(sprintf "  t  = state(1);~%" )
    ,(sprintf "  i = i+1;~%")
    ,(sprintf "endwhile~%")
    ,(sprintf "toc;~%")
    ,(sprintf "~A_close();~%" ivp-id)
    ,(sprintf "log = resize(log,[N (i-1)]);~%" )
    ,(sprintf "data = log';~%" )
    ,(sprintf "save (\"-ascii\", ~S, \"data\");~%" log-path)
    ))
  )

(define (mlton-clib-oct-cc ivp-id N P)
  `(,(sprintf
#<<EOF

#include <octave/oct.h>
#include "~A_clib.h"

void print_usage (void)
{
}

EOF
ivp-id)

    ,(sprintf
#<<EOF

DEFUN_DLD (~A_open, args, nargout, "NineML IVP open")
{
     const char *libargs = "";

     ~A_clib_open(0,(const char **)&libargs);
     
     return octave_value_list();
}

DEFUN_DLD (~A_close, args, nargout, "NineML IVP close")
{
     ~A_clib_close();
     
     return octave_value_list();
}
EOF
ivp-id ivp-id ivp-id ivp-id)

    ,(sprintf
#<<EOF

DEFUN_DLD (~A_initial, args, nargout, "NineML IVP initial values")
{
     int nargin = args.length ();
     double *Avec, *Bvec;
     const char *libargs = "";
     octave_value_list retval;

     dim_vector Adv (2);
     Adv(0) = ~A; Adv(1) = 1;

     dim_vector Bdv (2);
     Bdv(0) = ~A; Bdv(1) = 1;

     NDArray A (Adv);
     NDArray B (Bdv);
     
     Avec = A.fortran_vec();
     Bvec = B.fortran_vec();

     ~A_clib_initial ((void *)Avec);
     ~A_clib_parameters ((void *)Bvec);

     nargout = 2;
     
     retval(0) = octave_value(A);
     retval(1) = octave_value(B);

     return retval;
}

EOF
ivp-id N P ivp-id ivp-id)

,(sprintf 
#<<EOF

DEFUN_DLD (~A_run, args, , "NineML IVP")
{
     int nargin = args.length ();
     const double *Avec, *Pvec, *res;
     const char *libargs = "";
     unsigned int i, Bi, N, base;

     if (nargin != 4)
     {
	  print_usage ();
	  return octave_value_list ();
     }
     else
     {
	  const NDArray A = args(0).array_value();
	  const NDArray P = args(1).array_value();
	  const NDArray B = args(2).array_value();

          Bi = args(3).uint_value();

	  dim_vector Bdims = B.dims();
	  dim_vector Adims = A.dims();

	  N = Adims(0);

          Avec = A.fortran_vec();
          Pvec = P.fortran_vec();
	  ~A_clib_run1 ((void *)Avec,(void *)Pvec);

	  res = B.fortran_vec();
	  base = Bi*N;
	  for (i=0; i<N; i++)
	  {
	        ((double *)res)[base+i] = Avec[i];
	  }

	  return octave_value_list();
     }
}

EOF
ivp-id ivp-id)))


(define mlton-clib-prelude
#<<EOF

val getReal = MLton.Pointer.getReal64
val setReal = MLton.Pointer.setReal64
val setBoolean = (fn (p,i,v) => if v then setReal(p,i,1.0) else setReal(p,i,0.0))
val getBoolean = (fn (p,i) => if Real.==(getReal(p,i),1.0) then true else false)

EOF
)


(define (mlton-clib-exports ivp-id ivar dvars ic)
  `(
    ,(sprintf "val e = _export \"~A_clib_initial\" public: (MLton.Pointer.t -> unit) -> unit;~%~%" ivp-id)
    ,(sprintf "val _ = e (fn(p) => (coutputstate (p,initial)))~%~%")
    ,(sprintf "val e = _export \"~A_clib_parameters\" public: (MLton.Pointer.t -> unit) -> unit;~%~%" ivp-id)
    ,(sprintf "val _ = e (fn(p) => (coutputparameters (p,initial)))~%~%")
    ,(sprintf "val e = _export \"~A_clib_run1\" public: (MLton.Pointer.t * MLton.Pointer.t -> unit) -> unit;~%~%" ivp-id)
    ,(sprintf "val _ = e (run1(Model.~A,coutputstate))~%~%" ivp-id)
    ))


(define (mlton-clib-run1 ivar dvars ic)
  (let* ((states (cons ivar dvars))
	 (sic    (cons (cons ivar (alist-ref ivar ic))
		       (append (filter (lambda (x) (member (car x) dvars)) ic)
			       (filter (lambda (x) (not (member (car x) states))) ic))))
	 (iv       (lambda (vars)
		     (let recur ((vars vars) (sindex 0) (pindex 0) (ax '()))
		       (if (null? vars) (reverse ax)
			   (let ((x (car vars)))
			     (let ((n (car x)) (v (cdr x)))
			       (let ((get (cond ((number? v) "getReal")
						((boolean? v) "getBoolean")
						(else         ""))))
				 (let ((asgn (if (member n states) 
						 (sprintf "~A=(~A(s,~A))" n get sindex)
						 (sprintf "~A=(~A(p,~A))" n get pindex)))
				       (sindex (if (member n states) (+ 1 sindex) sindex))
				       (pindex (if (member n states) pindex (+ 1 pindex))))
						 
				     (recur (cdr vars) sindex pindex (cons asgn ax))
				     )))
				 ))
			     )))
	 (input    (string-append "{" (string-concatenate (intersperse (iv sic) ",")) "}"))
	 (ov       (lambda (vars)
		     (let recur ((vars vars) (pindex 0) (ax '()))
		       (if (null? vars) (reverse ax)
			   (let ((x (car vars)))
			     (let ((n (car x)) (v (cdr x)))
			       (let ((get (cond ((number? v) "getReal")
						((boolean? v) "getBoolean")
						(else         ""))))
				 (let ((asgn (if (member n states) 
						 (sprintf "~A=(#~A(~A))" n n "nstate" )
						 (sprintf "~A=(~A(p,~A))" n get pindex)))
				       (pindex (if (member n states) pindex (+ 1 pindex))))
				     (recur (cdr vars) pindex (cons asgn ax))
				     )))
				 ))
			     )))
	 (nstate1 (let recur ((vars sic) (pindex 0) (ax '()))
		    (string-append "{" (string-concatenate (intersperse (ov sic) ",")) "}"))))
    (sprintf
#<<EOF

fun run1 (f,output) (s,p) =
let val input   = ~A
    val nstate  = f input
    val nstate1 = ~A
    in 
       output (s,nstate1)
    end
 
EOF
   input nstate1)))


(define (mlton-coutputstate ivar dvars ic)
  (let* ((states (cons ivar dvars))
	 (sic    (cons (cons ivar (alist-ref ivar ic))
		       (append (filter (lambda (x) (member (car x) dvars)) ic)
			       (filter (lambda (x) (not (member (car x) states))) ic)))))
 
    (string-append
     (sprintf "fun coutputstate (p,input) = ~%")
     "("
     (string-concatenate
      (let recur ((vars sic) (index 0) (ax '()))
	(if (null? vars) (intersperse (reverse ax) ";\n")
	    (let ((var (car vars)))
	      (let ((n (car var)) (v (cdr var)))
		(if (member n states)
		    (let ((set (cond ((number? v) "setReal")
				     ((boolean? v) "setBoolean")
				     (else         ""))))
		      (recur (cdr vars) (+ 1 index)
			     (cons (sprintf "~A (p,(~A),#~A(input))" set index n) ax)))
		    (recur  (cdr vars) index ax))
		))
	    )))
     ")" )))

(define (mlton-coutputparameters  pvars ic)
  (let ((pic (filter (lambda (x) (member (car x) pvars)) ic)))
 
    (string-append
     (sprintf "fun coutputparameters (p,input) = ~%")
     "("
     (string-concatenate
      (let recur ((vars pic) (index 0) (ax '()))
	(if (null? vars) (intersperse (reverse ax) ";\n")
	    (let ((var (car vars)))
	      (let ((n (car var)) (v (cdr var)))
		(let ((set (cond ((number? v) "setReal")
				 ((boolean? v) "setBoolean")
				 (else         ""))))
		  (recur (cdr vars) (+ 1 index)
			 (cons (sprintf "~A (p,(~A),#~A(input))" set index n) ax)))
		))
	    )))
     ")" )))




(define (ivp-octave-mlton prefix ivp-id hvar ivar dvars pvars start end ic sd)

  (let* (			
	 (N (+ 1 (length dvars)))
	 (P (- (length ic) N))
	 (dir        (or (pathname-directory prefix) "."))
	 (adir       (if (absolute-pathname? dir) dir
			 (make-pathname (current-directory) dir)))
	 (shared-dir         (chicken-home))
	 (flsim-dir          (make-pathname shared-dir "flsim"))
	 (solver-path        (make-pathname dir (sprintf "~A_solver.sml" ivp-id)))
	 (clib-path          (make-pathname dir (sprintf "~A_clib.sml" ivp-id)))
	 (h-path             (make-pathname dir (sprintf "~A_clib.h" ivp-id)))
	 (mlb-path           (make-pathname dir (sprintf "~A_clib.mlb" ivp-id)))
	 (clib-so-path       (make-pathname dir (sprintf "~A_clib.so" ivp-id)))
	 (lib-so-path        (make-pathname dir (sprintf "lib~A_clib.so" ivp-id)))
	 (oct-cc-path        (make-pathname dir (sprintf "~A_run.cc" ivp-id)))
	 (oct-run-path       (make-pathname dir (sprintf "~A_run.oct" ivp-id)))
	 (oct-initial-path   (make-pathname dir (sprintf "~A_initial.oct" ivp-id)))
	 (oct-open-path      (make-pathname dir (sprintf "~A_open.oct" ivp-id)))
	 (oct-close-path     (make-pathname dir (sprintf "~A_close.oct" ivp-id)))
	 (m-path             (make-pathname dir (sprintf "~A_~A.m" (pathname-file prefix) ivp-id)))
	 (log-path           (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (mlton-path         "mlton")
	 (mkoctfile-path     "mkoctfile")
	 (octave-path        "octave")
	 )
    
    (make (
	   (solver-path (prefix)
			(with-output-to-file solver-path
			  (lambda () (codegen/ML ivp-id sd solver: 'rk3))) )
	   
	   
	   (clib-path (prefix)
		      (with-output-to-file clib-path
			(lambda () 
			  (print-fragments
			   `(
			     (,mlton-clib-prelude)
			     ,(sprintf "val N = ~A~%~%" N)
			     (,(mlton-coutputstate ivar dvars ic) ,nl)
			     (,(mlton-coutputparameters pvars ic) ,nl)
			     (,(mlton-clib-run1 ivar dvars ic) ,nl)
			     (,(mlton-initial ic) ,nl ,nl)
			     ,(mlton-clib-exports ivp-id ivar dvars ic)
			     )))) )
	   
	   
	   (mlb-path ()
		     (with-output-to-file mlb-path
		       (lambda () 
			 (print-fragments
			  `(("$(SML_LIB)/basis/basis.mlb" ,nl )
			    ("$(SML_LIB)/basis/mlton.mlb" ,nl)
			    ("$(RK_LIB)/rk.mlb" ,nl )
			    ("local " ,nl)
			    (,(sprintf "    ~A_solver.sml" ivp-id) ,nl)
			    ("in" ,nl)
			    ("    structure Model" ,nl)
			    ("end" ,nl)
			    ,(sprintf "~A_clib.sml" ivp-id) ,nl))
			 )) )
	   
	   
	   (oct-cc-path (h-path)
			(with-output-to-file oct-cc-path
			  (lambda () 
			    (print-fragments
			     `(,(mlton-clib-oct-cc ivp-id N P))))) )
	   
	   
	   (m-path ()
		   (with-output-to-file m-path
		     (lambda () 
		       (print-fragments
			`(,(octave/mlton-m ivp-id ivar hvar dvars ic end log-path adir))))) )
	   
	   
	   (h-path (clib-so-path) )
	   
	   
	   (clib-so-path (mlb-path solver-path clib-path)
			 (run (,mlton-path -inline 1024
			                   -format library -export-header 
					   ,h-path -default-ann "'allowFFI true'" -link-opt -s 
					   -mlb-path-var ,(string-append "'RK_LIB " flsim-dir "/sml-lib/rk'") 
					   ,mlb-path)) )
	   
	   
	   (lib-so-path (clib-so-path)
			(run (ln -sf ,(sprintf "~A_clib.so" ivp-id) ,lib-so-path)) )
	   
	   
	   (oct-run-path (oct-cc-path lib-so-path)
			 (run ( "CXXFLAGS=\"-O2\"" 
				,mkoctfile-path ,oct-cc-path -s  -o ,oct-run-path
				,(sprintf "-l~A_clib" ivp-id) ,(sprintf "-L~A" dir) 
				,(sprintf "\"-Wl,-rpath=~A\"" adir))) )
	   
	   
	   (oct-initial-path (oct-run-path)
			     (run (ln -sf ,(sprintf "~A_run.oct" ivp-id) ,oct-initial-path)) )
	   
	   
	   (oct-open-path (oct-run-path)
			  (run (ln -sf ,(sprintf "~A_run.oct" ivp-id) ,oct-open-path)) )
	   
	   
	   (oct-close-path (oct-run-path)
			   (run (ln -sf ,(sprintf "~A_run.oct" ivp-id) ,oct-close-path)) )
	   
	   
	   (log-path (m-path oct-run-path oct-initial-path oct-open-path oct-close-path)
		     (run (octave -q  ,m-path)) )

#|	   
	   (clean ()
		  (run rm ,solver-path ,clib-path ,h-path
		       ,mlb-path ,clib-so-path ,lib-so-path
		       ,oct-cc-path ,oct-run-path ,oct-initial-path
		       ,oct-open-path ,oct-close-path ,m-path ,log-path))
|#
	   
	   )
      (list log-path) )))

)

