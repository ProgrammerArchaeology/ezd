;;; A middle of the road structure package.  This structure system falls
;;; somewhere between a simple structure system and an object system like sos.
;;;
;;; (define-structure <name>
;;;	slot-name
;;;	(slot-name [ (define (<name>-<slot-name> self) ...) ]
;;;	    	   [ (define (<name>-<slot-name>! self value) ...) ]
;;;		   [ initial-value ]))
;;;
;;; Instances of a structure are created by (make-<name> slot-values...), where
;;; slot-values is a list of values for each slot that does not have an initial
;;; value.  Slots are initialized in the order that they are defined.
;;;
;;; If no slot access functions are explicitly defined, then ones are
;;; automatically generated that simply load and store into the structure.
;;;
;;; Finally, a type predicate ISA-<name>? is defined that will return #t iff
;;; its argument is a structure of that type.

;*           Copyright 1990-1993 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;* 
;*                       Director of Licensing
;*                       Western Research Laboratory
;*                       Digital Equipment Corporation
;*                       250 University Avenue
;*                       Palo Alto, California  94301  
;* 
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.  
;* 
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

(define-macro DEFINE-STRUCTURE
    (lambda (exp expander)
	    (define NAME (cadr exp))
	    (define SLOTS (cddr exp))
	    (define (MAKE-SYM . x)
		    (string->symbol
			(apply string-append
			       (map (lambda (x)
					    (if (symbol? x)
						(symbol->string x)
						x))
				    x))))
	    (define (SLOT-NAME slot) (if (pair? slot) (car slot) slot))
	    (define (MATCH proc l fail)
		    (let loop ((l l))
			 (if (pair? l)
			     (if (proc (car l)) (car l) (loop (cdr l)))
			     fail)))
	    (define (INITIAL-VALUE slot)
		    (if (pair? slot)
			(match (lambda (x)
				       (not (and (pair? x)
						 (eq? (car x) 'define))))
			       (cdr slot)
			       (car slot))
			slot))
	    (define (INITIAL-ARGS)
		    (let loop ((slots slots))
			 (if (pair? slots)
			     (if (eq? (initial-value (car slots))
				      (slot-name (car slots)))
				 (cons (slot-name (car slots))
				       (loop (cdr slots)))
				 (loop (cdr slots)))
			     '())))
	    (define (CHECK) `(if (eq? (vector-ref self 0) ',name) self #f))
	    (define (GET-PUT slot index fname default)
		    (if (pair? slot)
			(let ((func (match (lambda (x)
						   (and (pair? x)
							(eq? (car x) 'define)
							(eq? (caadr x) fname)))
					   (cdr slot)
					   #f)))
			     (if func
				 `(define ,(cadr func)
					  (let ((,(slot-name slot) ,index)
						(self ,(check)))
						,default
					       ,@(cddr func)))
				 default))
			default))
	    (define (GET slot index)
		    (let ((fname (make-sym name "-" (slot-name slot))))
			 (get-put slot index fname
			     `(define (,fname self)
				      (vector-ref ,(check) ,index)))))
	    (define (PUT slot index)
		    (let ((fname (make-sym name "-" (slot-name slot) "!")))
			 (get-put slot index fname
			     `(define (,fname self value)
				      (vector-set! ,(check) ,index value)))))
	    (expander
		`(begin (define (,(make-sym "MAKE-" name) ,@(initial-args))
				(let ((self (make-vector
						,(+ 1 (length slots)))))
				     (vector-set! self 0 ',name)
				     ,@(let loop ((i 1) (slots slots))
					    (if (pair? slots)
						(cons `(vector-set! self ,i
							   ,(initial-value
								(car slots)))
						      (loop (+ i 1)
							    (cdr slots)))
						'()))
				     self))
			(define (,(make-sym "ISA-" name "?") x)
				(and (vector? x)
				     (eq? (vector-ref x 0) ',name)))
			,@(let loop ((i 1) (slots slots))
			       (if (pair? slots)
				   (cons (get (car slots) i)
					 (cons (put (car slots) i)
					       (loop (+ i 1) (cdr slots))))
				   '()))
			',name)
		expander)))

;;; In-line versions of the default slot load/store procedures can be
;;; constructed by the following macro.
;;;
;;; (define-in-line-structure-access <name> {slot-name | #} ...)
;;;
;;; For each slot in the structure, either the slot name (indicating that
;;; access procedures should be constructed) or a #F (indicating that no
;;; access procedures are to be constructed) must be supplied.

(define-macro DEFINE-IN-LINE-STRUCTURE-ACCESS
    (lambda (exp expander)
	    
	    (define STRUCT-NAME (cadr exp))
	    
	    (define STRUCT-LEN (length (cdr exp)))
	    
	    (define (MAKE-SYM . x)
		    (string->symbol
			(apply string-append
			       (map (lambda (x)
					    (if (symbol? x)
						(symbol->string x)
						x))
				    x))))
	    (define (MAKE-ISA)
		    `(define-in-line (,(make-sym "ISA-" struct-name "?") x)
			 ((lap (x y)
			       (BOOLEAN (AND (EQ (TSCPTAG x) EXTENDEDTAG)
					     (AND (EQ (UNSI_GNED x)
						      (BITOR VECTORTAG
							     ,(* struct-len
								 256)))
						  (EQ (VECTOR_ELEMENT x 0)
						      y)))))
			  x ',struct-name)))
	    
	    (define (MAKE-LOAD slot n)
		    `(define-in-line (,(make-sym struct-name "-" slot) x)
			 (if (,(make-sym "ISA-" struct-name "?") x)
			     ((lap (x) (VECTOR_ELEMENT x ,n)) x)
			     (error ',(make-sym struct-name "-" slot)
				    ,(string-append "Argument is not a "
					  (symbol->string struct-name)
					  ": ~s")
				    x))))
	    
	    (define (MAKE-STORE slot n)
		    `(define-in-line (,(make-sym struct-name "-" slot "!") x y)
			 (if (,(make-sym "ISA-" struct-name "?") x)
			     ((lap (x y) (SETGEN (VECTOR_ELEMENT x ,n) y)) x y)
			     (error ',(make-sym struct-name "-" slot "!")
				    ,(string-append "Argument is not a "
					 (symbol->string struct-name)
					 ": ~s")
				    x))))
	    
	    (expander
		`(begin ,(make-isa)
			,@(let loop ((slots (cddr exp)) (n 4))
			       (if (pair? slots)
				   (if (car slots)
				       (cons (make-load (car slots) n)
					     (cons (make-store (car slots) n)
						   (loop (cdr slots) (+ n 4))))
				       (loop (cdr slots) (+ n 4)))
				   '())))
		expander)))

;;; The previously defined macro only makes sense in compiled code, so disable
;;; it in the interpreter.

(eval-when (eval)
    (define-macro DEFINE-IN-LINE-STRUCTURE-ACCESS
	(lambda (exp expander)
		(expander (list 'quote exp) expander))))
							  
					      
