
; Generating XML

(define nl #\newline)

(define (sxml-transform content rulesets)
   (fold (lambda (ruleset content)
		     (pre-post-order* content ruleset))
	 content rulesets))

(define (generate-XML content #!key (rulesets '()) (protect #f))
  (let*
      (
       (content (fold (lambda (ruleset content)
			(pre-post-order* content ruleset))
		      content rulesets))
       )

   (post-order content
   `(
     ,@(if protect universal-protected-rules universal-conversion-rules)

     (begin
      . ,(lambda (tag . body)
	   (list "<?xml version=\"1.0\" ?>" nl
		  body
		  nl)))
 
     ; Description of the Resources
     ; A resource is declared by a SXML element
     ;	(Resource name title date version)
     ; where name is a filename for a document resource,
     ; name of an element or an attribute
     ; The Resource element by itself does not generate any
     ; XML. It is used as a container of information about the
     ; resource. This information is when evaluating Resource-ref
     ; and Resource-descr elements.

     (Resource
      . ,(lambda (tag name title date version)
	   '()))		; null expansion


     
     (Described-by	; (Described-by res-name) 
      . ,(lambda (tag name)	; A higher-order tag
	   `(DescribedBy
	     (Resource-ref ,name))))

     (DescribeDoc	; Describe a document resource
      . ,(lambda (tag name)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeDocument
	       (InformationResourceLocation ,name)
	       )))
	   ))

     (DescribeSample	; Describe an XML sample doc
      . ,(lambda (tag name . relations)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLSample
	       (InformationResourceLocation ,name)
	       (Relationships
		,@relations))))
	   ))

     (DescribeDTD	; Describe a DTD
      . ,(lambda (tag name . relations)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLschema
	       (@ (schemaType "DTD"))
	       (InformationResourceLocation ,name)
	       (Relationships
		,@relations))))
	   ))

     (XMLElement	; Describe an XML element
      . ,(lambda (tag name content descr-by . attlist)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLElement
	       ,content
	       (Relationships
		,@attlist
		(DescribedBy
		 (Resource-ref ,descr-by))
		))))))

     (XMLSpecFor
      . ,(lambda (tag name lit-version)
	   (list 'IsXMLSpecFor
	    '(Namespace "MET")
	    (list 'InformationResourceName name)
	    (list 'InformationResourceVersion lit-version))))

     ; Describe datatypes of elements and attributes
     (DTString		; A string datatype
      . ,(lambda (tag length)
	   `(DataTypeString (StringLength ,length))))

     (DTInt		; An int datatype
      . ,(lambda (tag length unit)
	   `(DataTypeInteger (IntegerLength ,length)
			     (IntegerUnitMeasure ,unit))))

     (DTFloat		; A floating-point datatype
      . ,(lambda (tag length precision unit)
	   `(DataTypeFloat (FloatLength ,length)
			   (FloatPrecision ,precision)
			   (FloatUnitMeasure ,unit))))


     (DTContainer	; A container of other elements
      . ,(lambda (tag . elem-names)
	   (cons 'DataTypeContainer
		 (map 
		  (lambda (elem-name) 
		    (list 'Contains
			  (list 'Resource-ref elem-name)))
		  elem-names))))

     (Attlist
      . ,(lambda (tag . attr-names)
	   (map (lambda (attr-name) 
		  (list 'IsQualifiedByAttribute
			(list 'Resource-ref attr-name)))
	   attr-names)))


     (XMLAttr	; default-value may be #f if omitted
      . ,(lambda (tag name content default-value descr-by)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLAttribute
	       ,content
	       ,(and default-value (list 'DefaultValue default-value))
	       (Relationships
		(DescribedBy
		 (Resource-ref ,descr-by))
		))))))

  ))))

