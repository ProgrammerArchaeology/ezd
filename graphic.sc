;;; ezd - easy drawing for X11 displays.
;;;
;;; A DRAWING is composed of GRAPHIC objects.

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

(module graphic)

(include "struct.sch")
(include "window.sch")
(include "view.sch")
(include "drawing.sch")
(include "commands.sch")
(include "ezd.sch")

;;; Drawings are composed of GRAPHIC structures with the following fields:
;;;
;;;	NAME		object name or #f
;;;	COMPUTE-BB	procedure to compute the bounding box for the graphical
;;;			object in X coordinates in terms of the *current-view*
;;;	XDRAW		procedure to draw the object into an X window via the
;;;			current view.
;;;	PSDRAW		procedure to draw the object to a Postscript file via
;;;			the current view.
;;;	INTERSECT?	boolean procedure to determine whether a bounding
;;;			rectangle expressed in the drawings coordinate system
;;;			intersects the object.
;;;	EVENTS		list of events specific to the graphical object.
;;;	REDRAW-SEQ	sequence number when object added to drawing.

(define-structure GRAPHIC
    name
    compute-bb
    xdraw
    psdraw
    intersect?
    (events '())
    (redraw-seq *redraw-seq*))

(define-in-line-structure-access GRAPHIC
    name
    compute-bb
    xdraw
    psdraw
    intersect?
    events
    redraw-seq)

;;; The slots of one graphical object are copied to another by the following
;;; function.

(define (SET-GRAPHIC! to from)
    (graphic-name! to (graphic-name from))
    (graphic-compute-bb! to (graphic-compute-bb from))
    (graphic-xdraw! to (graphic-xdraw from))
    (graphic-psdraw! to (graphic-psdraw from))
    (graphic-intersect?! to (graphic-intersect? from))
    (graphic-events! to (graphic-events from))
    (graphic-redraw-seq! to *redraw-seq*))

;;; A graphic is recognized as representing a clear object by having 
;;; DRAW-CLEAR as it's XDRAW procedure.

(define (DRAW-CLEAR) #t)

;;; The null graphic is a graphic that never intersects or draws.

(define NULL-GRAPHIC (make-graphic 'null-graphic (lambda () '(0 0 0 0))
			 draw-clear draw-clear
			 (lambda (minx miny maxx maxy) #f)))

;;; A BBGRAPHIC represents the mapping of a GRAPHIC object into the current
;;; VIEW.  It contains the following slots.
;;;
;;;	GRAPHIC		graphic object
;;;	MINX		bounding box in X coordinates for the graphic object
;;;	MINY
;;;	MAXX
;;;	MAXY

(define-structure BBGRAPHIC
    graphic
    (minx 0)
    (miny 0)
    (maxx 0)
    (maxy (bbgraphic-bounding-box self)))

(define-in-line-structure-access BBGRAPHIC
    graphic
    minx
    miny
    maxx
    maxy)

(define (BBGRAPHIC-BOUNDING-BOX self)
    (let* ((bb ((graphic-compute-bb (bbgraphic-graphic self))))
	   (minx (inexact->exact (floor (car bb))))
	   (miny (inexact->exact (floor (cadr bb))))
	   (maxx (inexact->exact (ceiling (caddr bb))))
	   (maxy (inexact->exact (ceiling (cadddr bb)))))
	  (bbgraphic-minx! self minx)
	  (bbgraphic-miny! self miny)
	  (bbgraphic-maxx! self maxx)
	  (bbgraphic-maxy! self maxy)
	  maxy))

;;; A BBGRAPHIC's bounding box is updated as required by the following
;;; procedure.

(define (UPDATE-BBGRAPHIC bbg)
    (let ((g (bbgraphic-graphic bbg)))
	 (if (eq? (graphic-redraw-seq g) *redraw-seq*)
	     (bbgraphic-bounding-box bbg))))

;;; A list of BBGRAPHICs is drawn to an X window via the current view by the
;;; following procedure.

(define (XDRAW-BBGRAPHIC-LIST bbgl)
    (for-each (lambda (bbg) ((graphic-xdraw (bbgraphic-graphic bbg)))) bbgl))

;;; A BBGRAPHIC is drawn to an X window via the current view by the following
;;; procedure.

(define (XDRAW-BBGRAPHIC bbg) ((graphic-xdraw (bbgraphic-graphic bbg))))

;;; A list of BBGRAPHICs is drawn in Postscript via the current view by the 
;;; following procedure.

(define (PSDRAW-BBGRAPHIC-LIST bbgl)
    (for-each (lambda (bbg) ((graphic-psdraw (bbgraphic-graphic bbg)))) bbgl))

;;; A BBGRAPHIC is drawn in Postscript to the current view by the following
;;; procedure.

(define (PSDRAW-BBGRAPHIC bbg) ((graphic-psdraw (bbgraphic-graphic bbg))))

;;; The minimum and maximum of pairs of coordinates are computed by the
;;; following functions that allow one or both of the arguments to be #F.

(define (BBMIN x y)
    (if (and x y) (min x y) (or x y)))

(define (BBMAX x y)
    (if (and x y) (max x y) (or x y)))

;;; An ACTION is applied to all members of the BBGRAPHICs list intersecting a
;;; rectangle by the following procedure.  The bounding box coordinates are
;;; X coordinates.

(define (BBGRAPHICS-INTERSECT bbgl minx miny maxx maxy action)    
    (if minx
	(let loop ((bbgl bbgl))
	     (if (pair? bbgl)
		 (let* ((bbg (car bbgl))
			(g (bbgraphic-graphic bbg)))
		       (if (eq? (graphic-redraw-seq g) *redraw-seq*)
			   (bbgraphic-bounding-box bbg))
		       (if (not (or (>= (bbgraphic-minx bbg) maxx)
				    (>= (bbgraphic-miny bbg) maxy)
				    (<= (bbgraphic-maxx bbg) minx)
				    (<= (bbgraphic-maxy bbg) miny)))
			   (action bbg))
		       (loop (cdr bbgl)))
		 #f))
	#f))

;;; An ACTION is applied to all members of the BBGRAPHICs list not intersecting
;;; a rectangle by the following procedure.  The bounding box coordinates are
;;; X coordinates.

(define (BBGRAPHICS-NOT-INTERSECT bbgl minx miny maxx maxy action)    
    (if minx
	(let loop ((bbgl bbgl))
	     (if (pair? bbgl)
		 (let* ((bbg (car bbgl))
			(g (bbgraphic-graphic bbg)))
		       (if (eq? (graphic-redraw-seq g) *redraw-seq*)
			   (bbgraphic-bounding-box bbg))
		       (if (or (>= (bbgraphic-minx bbg) maxx)
			       (>= (bbgraphic-miny bbg) maxy)
			       (<= (bbgraphic-maxx bbg) minx)
			       (<= (bbgraphic-maxy bbg) miny))
			   (action bbg))
		       (loop (cdr bbgl)))
		 #f))
	#f))

;;; The top most object in a view that intersects a bounding box is returned
;;; by the following function.  Objects currently drawn as well as objects to
;;; be drawn are examined.

(define (BBGRAPHICS-REALLY-INTERSECT view minx miny maxx maxy)    
    (set-view view '())
    (let ((uminx (min (x->user minx) (x->user maxx)))
	  (uminy (min (y->user miny) (y->user maxy)))
	  (umaxx (max (x->user minx) (x->user maxx)))
	  (umaxy (max (y->user miny) (y->user maxy))))
	 (let loop ((bbgl (view-bb-head view)) (match #f))
	      (if (pair? bbgl)
		  (let* ((bbg (car bbgl))
			 (g (bbgraphic-graphic bbg)))
			(if (eq? (graphic-redraw-seq g) *redraw-seq*)
			    (bbgraphic-bounding-box bbg))
			(if (or (>= (bbgraphic-minx bbg) maxx)
				(>= (bbgraphic-miny bbg) maxy)
				(<= (bbgraphic-maxx bbg) minx)
				(<= (bbgraphic-maxy bbg) miny)
				(not ((graphic-intersect? g) uminx uminy umaxx
				      umaxy)))
			    (loop (cdr bbgl) match)
			    (loop (cdr bbgl) g)))
		  (let loop ((gl (if (view-new view)
				     (drawing-head (view-drawing view))
				     (drawing-added-head (view-drawing view))))
			     (match match))
		       (if (pair? gl)
			   (let* ((g (car gl))
				  (bbox ((graphic-compute-bb g))))
				 (if (or (>= (car bbox) maxx)
					 (>= (cadr bbox) maxy)
					 (<= (caddr bbox) minx)
					 (<= (cadddr bbox) miny)
					 (not ((graphic-intersect? g) uminx
					       uminy umaxx umaxy)))
				     (loop (cdr gl) match)
				     (loop (cdr gl) g)))
			   match))))))

;;; Named graphical objects are constructed by the following function.

(define (EZD-OBJECT name commands)
    (let* ((gl (let loop ((cl commands))
		    (if (pair? cl)
			(let ((g (ezd-one (car cl))))
			     (if (isa-graphic? g)
				 (cons g (loop (cdr cl)))
				 (begin (ezd-error 'ezd-object
			       "OBJECTs may only contain graphics commands: ~s"
					    (car cl))
					(loop (cdr cl)))))
			'())))
	   (clear (let loop ((gl gl))
		       (if (pair? gl)
			   (and (eq? (graphic-xdraw (car gl)) draw-clear)
				(loop (cdr gl)))
			   #t))))
	  
	  (define (BB-REALLY-INTERSECT? uminx uminy umaxx umaxy)
		  (let* ((x1 (user->x uminx))
			 (y1 (user->y uminy))
			 (x2 (user->x umaxx))
			 (y2 (user->y umaxy))
			 (xminx (min x1 x2))
			 (xminy (min y1 y2))
			 (xmaxx (max x1 x2))
			 (xmaxy (max y1 y2)))
			(let loop ((gl gl))
			     (if (null? gl)
				 #f
				 (or (let ((bb ((graphic-compute-bb
						    (car gl)))))
					  (and (not (or (>= (car bb) xmaxx)
							(>= (cadr bb) xmaxy)
							(<= (caddr bb) xminx)
							(<= (cadddr bb)
							    xminy)))
					       ((graphic-intersect? (car gl))
						uminx uminy umaxx umaxy)))
				     (loop (cdr gl)))))))
	  
	  (case (length gl)
		((0) (make-graphic
			 name
			 (lambda () '(0 0 0 0))
			 draw-clear
			 draw-clear
			 (lambda (minx miny maxx maxy) #f)))
		((1) (graphic-name! (car gl) name)
		 (car gl))
		(else  (make-graphic
			   name
			   (lambda ()
				   (let loop ((gl gl) (minx #f) (miny #f)
					      (maxx #f) (maxy #f))
					(if (pair? gl)
					    (let ((bb ((graphic-compute-bb
							   (car gl)))))
						 (loop (cdr gl)
						       (bbmin minx (car bb))
						       (bbmin miny (cadr bb))
						       (bbmax maxx (caddr bb))
						       (bbmax maxy
							      (cadddr bb))))
					    (list minx miny maxx maxy))))
			   (if clear
			       draw-clear
			       (lambda () (for-each
					      (lambda (g) ((graphic-xdraw g)))
					      gl)))
			   (if clear
			       draw-clear
			       (lambda () (for-each
					      (lambda (g) ((graphic-psdraw g)))
					      gl)))
			   bb-really-intersect?)))))

(define-ezd-command
    `(object ,(lambda (x) (and (symbol? x) (not (eq? x '*)))) (repeat ,any?))
    "(object name commands...)"
    ezd-object)

;;; Module reset/initialization procedure.

(define (GRAPHIC-MODULE-INIT)
    #t)
