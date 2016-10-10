;;; ezd - easy drawing for X11.
;;;
;;; A DRAWING contains a set of graphical objects.  These objects are displayed
;;; by drawing them with a view into a window.  The view into a window also
;;; allows events to be mapped back into the drawing.

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

(module drawing)

(include "struct.sch")
(include "display.sch")
(include "window.sch")
(include "view.sch")
(include "graphic.sch")
(include "events.sch")
(include "commands.sch")
(include "xternal.sch")

;;; A DRAWING is a structured object consisting of the following fields:
;;;
;;;	NAME		symbolic name of the drawing.
;;;	HEAD		head of the list of objects in the drawing.  Since this
;;;			list is in drawing order, the objects at the "bottom"
;;;			of the drawing are at the head of the list.
;;;	TAIL		tail of the list of objects in the drawing.
;;;	ADDED-HEAD	head of the list of recent additions to the drawing.
;;;	ADDED-TAIL	tail of the list of recent additions to the drawing.
;;;	ZMOTION		objects have been rearranged in the drawing.
;;;	DAMAGED		head of the list of functions computing bounding boxes
;;;			describing objects damaged in the drawing.
;;;	CLEARED		boolean indicating that the drawing has been cleared.
;;;	IS-CLEAR	boolean indicating that nothing but clear objects have
;;;			been drawn in the drawing.
;;;	WINDOW-WATCH	list of object names that may have an object specific
;;;			event handler for RESIZE, EXPOSE, OVERLAY, or VISIBLE
;;;			events.
;;;	EVENTS		list of events that are for the object "*".

(define-structure DRAWING
    name
    (head '())
    (tail '())
    (added-head '())
    (added-tail '())
    (zmotion #f)
    (damaged '())
    (cleared #f)
    (is-clear #t)
    (window-watch '())
    (events (let* ((name (drawing-name self))
		   (x (assoc name *drawings*)))
		  (if x (set! *drawings* (remove x *drawings*)))
		  (set! *drawings* (cons (list name self) *drawings*))
		  '())))

(define-in-line-structure-access DRAWING
    name
    head
    tail
    added-head
    added-tail
    zmotion
    damaged
    cleared
    is-clear
    window-watch
    events)

;;; A list of lists associating the name of each drawing with the data
;;; structure is maintained in the global *DRAWINGS*.

(define *DRAWINGS* '())

;;; A drawing name can be converted to the appropriate data structure by the
;;; function NAME->DRAWING.

(define (NAME->DRAWING name)
    (let ((x (assoc name *drawings*)))
	 (if x (cadr x) (error 'name->drawing "undefined DRAWING: ~s" name))))

;;; Boolean to test if a drawing already exists.

(define (DRAWING-EXISTS? name)
    (if (assoc name *drawings*) #t #f))

;;; The name of an object in the current drawing is coverted to the graphic
;;; structure representing it by the following function.  It is an error to
;;; look up a non-existent object.

(define (NAME->GRAPHIC name)
    (let ((g (getprop name (drawing-name *current-drawing*))))
	 (if g g (error 'NAME->GRAPHIC "OBJECT does not exist: ~s" name))))

;;; An object is verified to be the name of a graphic object by the following
;;; procedure.

(define (NAME-OF-GRAPHIC? name)
    (and *current-drawing*
	 (symbol? name)
	 (getprop name (drawing-name *current-drawing*))))

;;; Most drawing commands have an implied argument, the current drawing.  The
;;; global *CURRENT-DRAWING* represents it.

(define *CURRENT-DRAWING* #f)

;;; The ezd command SET-DRAWING is used to set the current drawing.  If a
;;; drawing by that name does not exist, then one is created.

(define (SET-DRAWING name)
    (let ((drawing (if (drawing-exists? name)
		       (name->drawing name)
		       (make-drawing name))))
	 (set! *current-drawing* drawing)))

(define-ezd-command
    `(set-drawing ,symbol?)
    "(set-drawing drawing-name)"
    set-drawing)

;;; The ezd commands SAVE-DRAWING and RESTORE-DRAWING push and pop the current
;;; drawing on a stack.  

(define *SAVED-DRAWINGS* '())

(define-ezd-command
    `(save-drawing)
    "(save-drawing)"
    (lambda ()
	    (if *current-drawing*
		(set! *saved-drawings*
		      (cons *current-drawing* *saved-drawings*)))))

(define-ezd-command
    `(restore-drawing)
    "(restore-drawing)"
    (lambda ()
	    (unless (null? *saved-drawings*)
		    (set! *current-drawing* (car *saved-drawings*))
		    (set! *saved-drawings* (cdr *saved-drawings*)))))

;;; A drawing is cleared by the following procedure.

(define (DRAWING-CLEAR drawing)
    (let ((dname (drawing-name drawing)))
	 (for-each
	     (lambda (g)
		     (let ((object-name (graphic-name g)))
			  (if object-name
			      (putprop object-name dname #f))))
	     (drawing-head drawing))
	 (drawing-head! drawing '())
	 (drawing-tail! drawing '())
	 (drawing-added-head! drawing '())
	 (drawing-added-tail! drawing '())
	 (drawing-zmotion! drawing #f)
	 (drawing-damaged! drawing '())
	 (drawing-cleared! drawing #t)
	 (drawing-is-clear! drawing #t)
	 (drawing-window-watch! drawing '())
	 (drawing-events! drawing '())
	 (set! *update-display* #t)))

;;; The currently selected drawing is cleared by the ezd command CLEAR.

(define-ezd-command
    '(CLEAR)
    "(clear)"
    (lambda () (if *current-drawing* (drawing-clear *current-drawing*))))

;;; Graphic objects are moved to either the top or the bottom of the current
;;; drawing or relative to another object by the following procedure and
;;; commands.

(define (FLOAT/SINK-OBJECT drawing obj-name ref-name float)
    (let ((object (name->graphic obj-name))
	  (reference (and ref-name (name->graphic ref-name)))
	  (prev-reference (not ref-name))
	  (object-deleted #f))

	 ;;; Delete object and correct pointers, find reference object.
	 (let loop ((prev #t) (gl (drawing-head drawing)))
	      (if (pair? gl)
		  (let ((g (car gl)))
		       (cond ((and (eq? g object)
				   (not (eq? (drawing-head drawing)
					     (drawing-tail drawing))))
			      (let ((oh (drawing-head drawing))
				    (ot (drawing-tail drawing))
				    (oah (drawing-added-head drawing))
				    (oat (drawing-added-tail drawing)))
				   (if (eq? oh gl)
				       (drawing-head! drawing (cdr gl)))
				   (if (eq? ot gl)
				       (if (eq? ot oh)
					   (drawing-tail! drawing '())
					   (drawing-tail! drawing prev)))
				   (if (eq? oah gl)
				       (drawing-added-head! drawing (cdr gl)))
				   (if (eq? oat gl)
				       (if (eq? oat oah)
					   (drawing-added-tail! drawing '())
					   (drawing-added-tail! drawing prev)))
				   (if (pair? prev) (set-cdr! prev (cdr gl)))
				   (set! object-deleted #t)
				   (if (not prev-reference)
				       (loop prev (cdr gl)))))
			     ((eq? g reference)
			      (set! prev-reference prev)
			      (if (not object-deleted) (loop gl (cdr gl))))
			     (else (loop gl (cdr gl)))))))
	 
	 ;;; Insert object relative to reference object and correct pointers.
	 (let ((oh (drawing-head drawing))
	       (ot (drawing-tail drawing))
	       (oah (drawing-added-head drawing))
	       (oat (drawing-added-tail drawing))
	       (lob (list object)))
	      (if float
		  (cond ((pair? prev-reference)
			 (set-cdr! lob (cddr prev-reference))
			 (set-cdr! (cdr prev-reference) lob))
			((and (eq? prev-reference #t) ref-name)
			 (set-cdr! lob (cdr oh))
			 (set-cdr! oh lob))
			(else (set-cdr! ot lob)
			      (drawing-tail! drawing lob)))
		  (cond ((pair? prev-reference)
		         (set-cdr! lob (cdr prev-reference))
			 (set-cdr! prev-reference lob))
			(else (drawing-head! drawing (cons object oh)))))
	      (if (eq? oh oah)
		  (drawing-added-head! drawing (drawing-head drawing)))
	      (if (eq? ot oat)
		  (drawing-added-tail! drawing (drawing-tail drawing))))
	 
	 ;;; Mark area contained the moved object as damaged.
	 (if *clean-mouse-window*
	     (for-each (lambda (v)
			       (if (eq? (view-drawing v) drawing)
				   (set! *clean-mouse-window* #f)))
		 (window-views *mouse-window*)))
	 (drawing-damaged! drawing (cons (graphic-compute-bb object)
					 (drawing-damaged drawing)))
	 (drawing-zmotion! drawing #t)
	 (set! *update-display* #t)))

;;; Command parsers and definition.

(define NAME-OF-GRAPHIC1? #f)

(define NAME-OF-GRAPHIC2?
    (let ((name-of-first #f))
	 (set! name-of-graphic1?
	       (lambda (x)
		       (if (name-of-graphic? x)
			   (begin (set! name-of-first x) #t)
			   #f)))
	 (lambda (x) (and (name-of-graphic? x) (not (eq? x name-of-first))))))

(define-ezd-command
    `(FLOAT ,name-of-graphic1? (optional ,name-of-graphic2?))
    "(float object-name [object-name])"
    (lambda (o-name1 o-name2)
	    (float/sink-object *current-drawing* o-name1 o-name2 #t)))

(define-ezd-command
    `(SINK ,name-of-graphic1? (optional ,name-of-graphic2?))
    "(sink object-name [object-name])"
    (lambda (o-name1 o-name2)
	    (float/sink-object *current-drawing* o-name1 o-name2 #f)))

;;; A graphic object is added to a drawing by the following procedure.

(define (DRAWING-ADD drawing graphic)
    (let ((name (drawing-name drawing))
	  (object-name (graphic-name graphic)))
	 
	 (define (ADD-TO-DRAWING)
		 (let ((tail (drawing-tail drawing))
		       (added-tail (drawing-added-tail drawing))
		       (graphic-list (list graphic)))
		      (if (null? tail)
			  (drawing-head! drawing graphic-list)
			  (set-cdr! tail graphic-list))
		      (drawing-tail! drawing graphic-list)
		      (if (null? added-tail)
			  (drawing-added-head! drawing graphic-list)
			  (set-cdr! added-tail graphic-list))
		      (drawing-added-tail! drawing graphic-list)))
	 
	 (define (GRAPHIC-DAMAGED g)
		 (drawing-damaged! drawing
		     (cons (graphic-compute-bb g) (drawing-damaged drawing))))
	 
	 (if (and (drawing-is-clear drawing)
		  (not (eq? (graphic-xdraw graphic) draw-clear)))
	     (drawing-is-clear! drawing #f))
	 (if *clean-mouse-window*
	     (for-each (lambda (v)
			       (if (eq? (view-drawing v) drawing)
				   (set! *clean-mouse-window* #f)))
		 (window-views *mouse-window*)))
	 (if object-name
	     (let ((old-graphic (getprop object-name name)))
		  (if old-graphic
		      ;;; Object is being replaced by a new one.
		      (let ((old-events (graphic-events old-graphic)))
			   (graphic-damaged old-graphic)
			   (graphic-damaged graphic)
			   (set-graphic! old-graphic graphic)
			   (graphic-events! old-graphic old-events))
		      (begin (putprop object-name name graphic)
			     (add-to-drawing))))
	     (add-to-drawing))
	 (set! *update-display* #t)))

;;; Module reset/initialization.

(define (DRAWING-MODULE-INIT)
    (for-each
	(lambda (name-drawing)
		(for-each
		    (lambda (graphic)
			    (let ((name (graphic-name graphic)))
				 (if name
				     (putprop name (car name-drawing) #f))))
		    (drawing-head (cadr name-drawing))))
	*drawings*)
    (set! *drawings* '())
    (set! *saved-drawings* '())
    (set! *current-drawing* #f))
