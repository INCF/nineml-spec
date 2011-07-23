;; 
;; SXML.scm
;;
;; Auxilliary functions for SXML manipulation.
;;
;; Copyright Ivan Raikov and the Okinawa Institute of Science and Technology
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


;; obtain the first non-attribute child 
(define (sxml:kid node)
  (let ((v ((select-first-kid (lambda (x) (not (eq? (car x) '@)))) node)))
    (if (not v)  (error 'sxml:kid "node does not have children" node)  v)))
  
;; obtain the cadr of the first non-attribute child 
(define (sxml:kid-cadr node)
  (let ((v ((select-first-kid (lambda (x) (not (eq? (car x) '@)))) node)))
    (if (not v)  (error 'sxml:kid-cadr "node does not have children" node)  (cadr v))))
  
;; obtain all non-attribute children of a node
(define (sxml:kids node)
  ((select-kids (lambda (x) (not (eq? (car x) '@)))) node))
  
;; obtain all children of a node named n
(define (sxml:kidsn name node)
  ((select-kids (lambda (x) (eq? (car x) name))) node))
  
;; obtain  child named n of a node
(define (sxml:kidn name node)
  ((select-first-kid (lambda (x)  (eq? (car x) name))) node))

;; obtain  non-empty child named n of a node
(define (sxml:kidn* name node)
  ((select-first-kid (lambda (x) (and (eq? (car x) name) (not (null? (cdr x)))))) node))
  
;; obtain  the cdr of child named n
(define (sxml:kidn-cdr name node)
  (let ((v ((select-first-kid (lambda (x)  (eq? (car x) name))) node)))
    (if (not v)  (error 'sxml-kidn-cdr "node does not have children" node)  (cdr v))))
  
  
;; obtain  the cadr of child named n
(define (sxml:kidn-cadr name node)
  (let ((v ((select-first-kid (lambda (x) (eq? (car x) name))) node)))
    (if (not v)  (error 'sxml:kidn-cadr "node does not have children" name node)  (cadr v))))
  
(define (sxml:if-number x)
  (and x (sxml:number x)))

(define (sxml:attrv name node . lst)
  (if (null? lst)  (sxml:attr node name)
      (map (lambda (node) (sxml:attr node name)) (cons node lst))))


(define (ensure-xmlns doc)
  (let ((doc1 (sxml:add-attr doc '(xmlns:nineml "nineml"))))
    (sxml:add-attr doc1 '(xmlns nineml))))

(define (ensure-xmlver doc)
  (let ((doc1 (sxml:add-attr doc '(nineml:version "Chicken:20101129"))))
    doc1))
