;;; ezd - easy drawing for X11 displays.
;;;
;;; A VIEW object maps a DRAWING onto a WINDOW object.

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

(module view)

(include "struct.sch")
(include "display.sch")
(include "window.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "commands.sch")
(include "events.sch")
(include "xternal.sch")

;;; Creates a VIEW composed of the following fields:
;;;
;;;	DRAWING-NAME	drawing name associated with the view.
;;;	WINDOW-NAME	window name associated with the view.
;;;	CLIP-MINX	X window bounding box for the view or #f.
;;;	CLIP-MINY
;;;	CLIP-MAXX
;;;	CLIP-MAXY
;;;	DRAWING		drawing associated with the view.
;;;	WINDOW		window associated with the view.
;;;	USER->X		convert user x coordinate to X coordinate system.
;;;	USER->Y		convert user y coordinate to X coordinate system.
;;;	USER->LW	convert user line width coordinate to X pixels.
;;;	X->USER		convert X x coordinate to user coordinate system.
;;;	Y->USER		convert X y coordinate to user coordinate system.
;;;	USER->WIDTH	convert user x units to X pixels.
;;;	USER->HEIGHT	convert user y units to X pixels.
;;;     WIDTH->USER	convert X pixels to user x units.
;;;	HEIGHT->USER	convert X pixels to user y units.
;;;	ORIGINX		X coordinates for drawing's origin.
;;;	ORIGINY
;;;	SCALEX		Scale factors to convert drawing units to X.
;;;	SCALEY
;;;	SCALELW
;;;	NEW		indicates that this is a newly created view.
;;;	BB-HEAD		head of list of BBGRAPHICs in the view.
;;;	BB-TAIL		tail of list of BBGRAPHICS in the view.
;;;	STIPPLE-X	X coordinates of stipple offset
;;;	STIPPLE-Y
;;;	NEW-TRANSFORM	argument list for new transformation

(define-structure VIEW
    drawing-name
    window-name
    (clip-minx #f)
    (clip-miny #f)
    (clip-maxx #f)
    (clip-maxy #f)
    (drawing (define (view-drawing self)
		     (name->drawing (view-drawing-name self)))
	     #f)
    (window (define (view-window self)
		    (name->window (view-window-name self)))
	    #f)
    (user->x (lambda (x) (inexact->exact x)))
    (user->y (lambda (x) (inexact->exact x)))
    (user->lw (lambda (x) (if x (inexact->exact x) 0)))
    (x->user (lambda (x) x))
    (y->user (lambda (x) x))
    (user->width (lambda (x) (inexact->exact x)))
    (user->height (lambda (x) (inexact->exact x)))
    (width->user (lambda (x) x))
    (height->user (lambda (x) x))
    (originx 0)
    (originy 0)
    (scalex 1)
    (scaley 1)
    (scalelw 1)
    (new #t)
    (bb-head '())
    (bb-tail '())
    (stipple-x 0)
    (stipple-y 0)
    (new-transform #f))

(define-in-line-structure-access VIEW
    drawing-name
    window-name
    clip-minx
    clip-miny
    clip-maxx
    clip-maxy
    #f
    #f
    user->x
    user->y
    user->lw
    x->user
    y->user
    user->width
    user->height
    width->user
    height->user
    originx
    originy
    scalex
    scaley
    scalelw
    new
    bb-head
    bb-tail
    stipple-x
    stipple-y
    new-transform)

;;; All procedures that draw into objects assume a "current view".  The
;;; information related to the current view is stored in the following
;;; global variables.

(define *CURRENT-VIEW* #f)

(define *WINDOW* #f)		;;; From the WINDOW object.
(define *WIDTH* #f)
(define *HEIGHT* #f)
(define *NAME* #f)
(define *FOREGROUND-NAME* #f)
(define *BACKGROUND-NAME* #f)
(define *FOREGROUND* #f)
(define *BACKGROUND* #f)
(define *XWINDOW* #f)

(define USER->X #f)		;;; From the VIEW object.
(define USER->Y #f)
(define USER->LW #f)
(define X->USER #f)
(define Y->USER #f)
(define USER->WIDTH #f)
(define USER->HEIGHT #f)
(define WIDTH->USER #f)
(define HEIGHT->USER #f)
(define STIPPLE-X #f)
(define STIPPLE-Y #f)

(define *CLIP-BBL* #f)		;;; From the call to SET-VIEW.

;;; The current view is swapped by the following function.  One can force
;;; the cached values to be returned to their structure by supplying #f as the
;;; argument.  The previous value of *CURRENT-VIEW* is returned as the
;;; value of the function.

(define (SET-VIEW cview clip-bbl)
    (unless (eq? cview *current-view*)
	    (when *current-view*
		  (view-user->x! *current-view* user->x)
		  (view-user->y! *current-view* user->y)
		  (view-user->lw! *current-view* user->lw)
		  (view-x->user! *current-view* x->user)
		  (view-y->user! *current-view* y->user)
		  (view-user->width! *current-view* user->width)
		  (view-user->height! *current-view* user->height)
		  (view-width->user! *current-view* width->user)
		  (view-height->user! *current-view* height->user)
		  (view-stipple-x! *current-view* stipple-x)
		  (view-stipple-y! *current-view* stipple-y))
	    (when cview
		  (set! user->x (view-user->x cview))
		  (set! user->y (view-user->y cview))
		  (set! user->lw (view-user->lw cview))
		  (set! x->user (view-x->user cview))
		  (set! y->user (view-y->user cview))
		  (set! user->width (view-user->width cview))
		  (set! user->height (view-user->height cview))
		  (set! width->user (view-width->user cview))
		  (set! height->user (view-height->user cview))
		  (set! stipple-x (view-stipple-x cview))
		  (set! stipple-y (view-stipple-y cview))
		  (set! *current-drawing* (view-drawing cview))
		  (set! *window* (view-window cview))
		  (set! *width* (window-width *window*))
		  (set! *height* (window-height *window*))
		  (set! *name* (window-name *window*))
		  (set! *foreground-name* (window-foreground-name *window*))
		  (set! *background-name* (window-background-name *window*))
		  (set! *foreground* (window-foreground *window*))
		  (set! *background* (window-background *window*))
		  (set! *xwindow* (window-xwindow *window*))))
    (let ((return *current-view*))
	 (set! *current-view* cview)
	 (if cview (set! *clip-bbl*
			 (let ((clipped (clip-bbl-to-view cview clip-bbl)))
			      (if (and (null? clipped) (view-clip-minx cview))
				  (list (list (view-clip-minx cview)
					      (view-clip-miny cview)
					      (view-clip-maxx cview)
					      (view-clip-maxy cview)))
				  clipped))))
	 return))

;;; A bounding box list is clipped to a view by the following procedure.  A
;;; newly constructed list of bounding boxes is returned.

(define (CLIP-BBL-TO-VIEW view bbl)
    (if (view-clip-minx view)
	(let ((clip-minx (view-clip-minx view))
	      (clip-miny (view-clip-miny view))
	      (clip-maxx (view-clip-maxx view))
	      (clip-maxy (view-clip-maxy view)))
	     (let loop ((bbl bbl))
		  (if (pair? bbl)
		      (let* ((bb (car bbl))
			     (minx (car bb))
			     (miny (cadr bb))
			     (maxx (caddr bb))
			     (maxy (cadddr bb)))
			    (if (or (<= maxx clip-minx)
				    (<= maxy clip-miny)
				    (>= minx clip-maxx)
				    (>= miny clip-maxy))
				(loop (cdr bbl))
				(cons (list (max minx clip-minx)
					    (max miny clip-miny)
					    (min maxx clip-maxx)
					    (min maxy clip-maxy))
				      (loop (cdr bbl)))))
		      '())))
	bbl))

;;; A bounding box is clipped to a view by the following procedure.  Either a
;;; bounding box of #f is returned.

(define (CLIP-BB-TO-VIEW view minx miny maxx maxy)
    (if (view-clip-minx view)
	(let ((clip-minx (view-clip-minx view))
	      (clip-miny (view-clip-miny view))
	      (clip-maxx (view-clip-maxx view))
	      (clip-maxy (view-clip-maxy view)))
	     (if (or (<= maxx clip-minx)
		     (<= maxy clip-miny)
		     (>= minx clip-maxx)
		     (>= miny clip-maxy))
		 #f
		 (list (max minx clip-minx)
		       (max miny clip-miny)
		       (min maxx clip-maxx)
		       (min maxy clip-maxy))))
	(list minx miny maxx maxy)))

;;; The following function converts a list of views into a list of lists of
;;; views, where each list is a list of intersecting views.  Each sublist is
;;; ordered as was the original list of views.

(define (PARTITION-VIEWS views)
    
    (define (INTERSECT? view views)
	    (if (pair? views)
		(let ((v2 (car views)))
		     (if (and (view-clip-minx view)
			      (view-clip-minx v2)
			      (or  (<= (view-clip-maxx view)
				       (view-clip-minx v2))
				   (<= (view-clip-maxy view)
				       (view-clip-miny v2))
				   (>= (view-clip-minx view)
				       (view-clip-maxx v2))
				   (>= (view-clip-miny view)
				       (view-clip-maxy v2))))
			 (intersect? view (cdr views))
			 #t))
		#f))
    
    (if (pair? views)
	(let loop ((intersect (list (car views)))
		   (disjoint '())
		   (views (cdr views)))
	     (if (pair? views)
		 (let ((view (car views)))
		      (if (intersect? view intersect)
			  (loop (append intersect (list view)) disjoint
				(cdr views))
			  (loop intersect (append disjoint (list view))
				(cdr views))))
		 (cons intersect (partition-views disjoint))))
	'()))

;;; Graphics contexts are managed by the following procedure.  Given the
;;; appropriate options, it will return a graphics context.  If needed a
;;; new one will be created.  Note that the GC's are actually owned and
;;; managed by the view's display object.

(define (CV-GC width color stipple dash font arc)
    (display-gc *display* width (or color *foreground*) *background*
	stipple stipple-x stipple-y dash font arc *clip-bbl*))

;;; Points are converted to pixels by the following function.  A line width
;;; of #f converts to 0.

(define (POINTS->PIXELS x)
    (if x (inexact->exact (round (* *pixels/point* x))) 0))

;;; The following procedure checks to see if a drawing name exists in the
;;; last checked window name.  It is used in conjuction with WINDOW-EXISTS?
;;; to parse the window and drawing within the window names in a command.

(define (DRAWING-IN-LAST-EXISTING-WINDOW? x)
    (and (symbol? x)
	 (let loop ((vl (window-views
			    (name->window last-existing-window-name))))
	      (if (pair? vl)
		  (let ((view (car vl)))
		       (if (eq? x (view-drawing-name view))
			   #t
			   (loop (cdr vl))))
		  #f))))

;;; A drawing is shown in a window by defining a "view" of a drawing in a
;;; window.  This is done by the OVERLAY and UNDERLAY commands that place the
;;; drawing over or under the drawings already in the window.  If the drawing
;;; is already visible in the window, it will be repositioned.

(define (OVER/UNDER-LAY wname dname over bb)
    (let* ((window (name->window wname))
	   (drawing (name->drawing dname))
	   (views (window-views window)))
	  
	  (define (CVT x) (if (list-ref bb 4) (points->pixels x) x))
	  
	  (define (ADD-VIEW views view)
		  (window-views! window (if over
					    (append views (list view))
					    (cons view views)))
		  (when bb
			(view-clip-minx! view (cvt (car bb)))
			(view-clip-miny! view (cvt (cadr bb)))
			(view-clip-maxx! view (cvt (+ (car bb) (caddr bb))))
			(view-clip-maxy! view (cvt (+ (cadr bb) (cadddr bb)))))
		  (handle-view-events view 'overlay #f
		      (if (view-clip-minx view)
			  `(,(view-clip-minx view) ,(view-clip-miny view)
			    ,(- (view-clip-maxx view) (view-clip-minx view))
			    ,(- (view-clip-maxy view)(view-clip-miny view)))
			  `(0 0 ,(window-width (view-window view))
				,(window-height (view-window view)))))
		  (handle-visible-events view))
		      
	  
	  (let loop ((vl views))
	       (if (pair? vl)
		   (let ((view (car vl)))
			(if (eq? (view-drawing-name view) dname)
			    (begin (damage-view-area view)
				   (set-view #f '())
				   (add-view (remq view views) view))
			    (loop (cdr vl))))
		   (let ((view (make-view dname wname)))
			(if (null? views)
			    (xmapraised *dpy* (window-xwindow window))
			    (if (not over) (damage-view-area view)))
			(add-view views view)))))
    (set! *update-display* #t))

(define-ezd-command
    `(overlay ,window-exists? ,drawing-exists?
	 (optional ,non-negative? ,non-negative? ,positive-number?
	     ,positive-number? (optional POINTS)))
    "(overlay window-name drawing-name [ x y width height [ POINTS ] ])"
    (lambda (w-name d-name bb) (over/under-lay w-name d-name #t bb)))

(define-ezd-command
    `(underlay ,window-exists? ,drawing-exists?
	 (optional ,non-negative? ,non-negative? ,positive-number?
	     ,positive-number? (optional POINTS)))
    "(underlay window-name drawing-name [ x y width height [ POINTS ] ])"
    (lambda (w-name d-name bb) (over/under-lay w-name d-name #f bb)))

;;; The X window area occupied by a view is "damaged" by the following
;;; procedure to force it to be updated when the window is redrawn.

(define (DAMAGE-VIEW-AREA view)
    (let ((drawing (view-drawing view))
	  (window (view-window view)))
	 (set-view view '())
	 (if (not (drawing-is-clear drawing))
	     (for-each
		 (lambda (bbl)
			 (window-damage-bbl! window
			     (merge-bbl (car bbl) (cadr bbl) (caddr bbl)
				 (cadddr bbl) (window-damage-bbl window))))
		 (clip-bbl-to-view view
		     (map (lambda (g) ((graphic-compute-bb g)))
			  (drawing-head drawing)))))))

;;; A window name and drawing name is translated to a view by the following
;;; procedure.

(define (WINDOW-DRAWING->VIEW window-name drawing-name)
    (let loop ((vl (window-views (name->window window-name))))
	 (if (pair? vl)
	     (let ((view (car vl)))
		  (if (eq? drawing-name (view-drawing-name view))
		      view
		      (loop (cdr vl))))
	     (ezd-error 'WINDOW-DRAWING->VIEW
		    "DRAWING ~a is not visible in WINDOW ~s"
		    drawing-name window-name))))

;;; A view is deleted by the ezd command DELETE-VIEW.

(define (DELETE-VIEW window-name drawing-name)
    (let* ((view (window-drawing->view window-name drawing-name))
	   (window (view-window view)))
	  (window-views! window (remq view (window-views window)))
	  (if (null? (window-views window))
	      (begin (window-exposed! window #f)
		     (xunmapwindow *dpy* (window-xwindow window)))
	      (damage-view-area view))
	  (handle-view-events view 'overlay #f (list #f #f #f #f))
	  (handle-view-events view 'visible #f (list #f #f #f #f))
	  (set! *update-display* #t)))

(define-ezd-command
    `(delete-view ,window-exists? ,drawing-in-last-existing-window?)
    "(delete-view window-name drawing-name)"
    delete-view)

;;; When an area of a drawing is made visible by either making it a view, or
;;; changing it's coordinate system, then this event must be reported to any
;;; objects in the drawing that expect it.  The arguments are a bounding box
;;; (in the drawing's coordinate system) of the area of the drawing visible
;;; in the view.

(define (HANDLE-VISIBLE-EVENTS view)
    (handle-view-events view 'visible #f
	(list ((view-x->user view) (or (view-clip-minx view) 0))
	      ((view-y->user view) (or (view-clip-miny view) 0))
	      ((view-width->user view)
	       (- (or (view-clip-maxx view) (window-width (view-window view)))
		  (or (view-clip-minx view) 0)))
	      ((view-height->user view)
	       (- (or (view-clip-maxy view)(window-height (view-window view)))
		  (or (view-clip-miny view) 0))))))

;;; The coordinate system of a view is modified by the ezd commands
;;; ORIGIN and SCALE.  The ORIGIN command provides the X coordinates of the
;;; origin of the user's coordinate system.  The SCALE command provide the
;;; scale factors for scaling the x and y coordinates.  The coordinate
;;; transformations are:
;;;
;;;	x11 = user-x * scale-x + origin-x
;;;	y11 = user-y * scale-y + origin-y

(define-ezd-command
    `(origin ,window-exists? ,drawing-in-last-existing-window?
	     ,number? ,number? (optional points))
    "(origin window-name drawing-name x y [ points ])"
    (lambda (w d x y points)
	    (if points
		(queue-transform w d (points->pixels x)
		    (points->pixels y) #f #f #f)
		(queue-transform w d x y #f #f #f))))

(define-ezd-command
    `(scale ,window-exists? ,drawing-in-last-existing-window?
	    ,non-zero? ,non-zero? ,positive-number? (optional points))
    "(scale window-name drawing-name x-scale y-scale line-width-scale [ points ])"
    (lambda (w d x y lw points)
	    (if points
		(queue-transform w d #f #f (* *pixels/point* x)
		    (* *pixels/point* y) (* *pixels/point* lw))
		(queue-transform w d #f #f x y lw))))

;;; The first step when performing a transformation is to save the new
;;; transform in the view.  The actual transformation occurs the next time the
;;; display is updated.

(define (QUEUE-TRANSFORM window-name drawing-name originx originy scalex
	    scaley scalelw)
    (let ((view (window-drawing->view window-name drawing-name))
	  (originx (and originx (inexact->exact (round originx))))
	  (originy (and originy (inexact->exact (round originy)))))
	 (when (or (and originx
			(or (not (equal? originx (view-originx view)))
			    (not (equal? originy (view-originy view)))))
		   (and scalex
			(or (not (equal? scalex (view-scalex view)))
			    (not (equal? scaley (view-scaley view)))
			    (not (equal? scalelw (view-scalelw view))))))
	       (view-new-transform! view
		   (let loop ((old (or (view-new-transform view)
				       '(#f #f #f #f #f)))
			      (new (list originx originy scalex scaley
					 scalelw)))
			(if (pair? new)
			    (cons (or (car new) (car old))
				  (loop (cdr old) (cdr new)))
			    '())))
	       (set! *update-display* #t))))

;;; When the display is updated, the transformations on all views in a window
;;; are done together.  The first step is to compute the new coordinate
;;; transformation functions.  Once this is done, the display is updated by
;;; moving existing bits, or damaging it and forcing it to be redrawn.  A list
;;; of views needing visible events is returned.  The events are sent after
;;; drawing completes.

(define VISIBLE-EVENT-QUEUE '())

(define (TRANSFORM-VIEWS views)
    (set! visible-event-queue '())
    (for-each transform-a-partition (partition-views views))
    visible-event-queue)

(define (TRANSFORM-A-PARTITION views)
    
    (define (MERGE x l) (if (member x l) l (cons x l)))
    
    (define (ACCUM func l x default)
	    (let loop ((v #f) (l l))
		 (if (pair? l)
		     (let ((next-v (or (list-ref (car l) x) default)))
			  (loop (if v (func v next-v) next-v) (cdr l)))
		     v)))
    
    (let ((rescaled #f)
	  (deltax '())
	  (deltay '())
	  (clip '()))
	 (for-each
	     (lambda (view)
		     (let ((transform (view-new-transform view)))
			  (view-new-transform! view #f)
			  (cond ((drawing-is-clear (view-drawing view))
				 (if (pair? transform)
				     (apply transform-a-view view transform)))
				((pair? transform)
				 (let* ((result (apply transform-a-view
						       view transform))
					(rs (car result))
					(dx (cadr result))
					(dy (caddr result)))
				       (set! rescaled (or rs rescaled))
				       (set! deltax (merge dx deltax))
				       (set! deltay (merge dy deltay))
				       (set! clip
					     (merge `(,(view-clip-minx view)
						      ,(view-clip-miny view)
						      ,(view-clip-maxx view)
						      ,(view-clip-maxy view))
						    clip))))
				(else (set! rescaled #t)))))
	     views)
	 (if (not (null? deltax))
	     (let* ((window (view-window (car views)))
		    (minx (accum min clip 0 0))
		    (miny (accum min clip 1 0))
		    (maxx (accum max clip 2 (window-width window)))
		    (maxy (accum max clip 3 (window-height window))))
		   (transform-redisplay views  
		       (or rescaled (> (length deltax) 1)
			   (> (length deltay) 1) (> (length clip) 1))
		       (car deltax) (car deltay) minx miny maxx maxy)))))

;;; Coordinate transformations are performed on an existing view by the
;;; following function.

(define (TRANSFORM-A-VIEW view originx originy scalex scaley scalelw)
    (set-view view '())
    (let ((was-originx (view-originx *current-view*))
	  (was-originy (view-originy *current-view*)))
	 (if originx (view-originx! *current-view* originx))
	 (if originy (view-originy! *current-view* originy))
	 (if scalex (view-scalex! *current-view* scalex))
	 (if scaley (view-scaley! *current-view* scaley))
	 (if scalelw (view-scalelw! *current-view* scalelw))
	 (let ((originx (view-originx *current-view*))
	       (originy (view-originy *current-view*))
	       (rescaled (or scalex scaley scalelw))
	       (scalex (view-scalex *current-view*))
	       (scaley (view-scaley *current-view*))
	       (scalelw (view-scalelw *current-view*)))
	      (set! user->x
		    (lambda (x) (inexact->exact (+ (* x scalex) originx))))
	      (set! user->y
		    (lambda (y) (inexact->exact (+ (* y scaley) originy))))
	      (set! user->lw
		    (lambda (x) (if x (inexact->exact (* scalelw x)) 0)))
	      (set! x->user (lambda (x) (/ (- x originx) scalex)))
	      (set! y->user (lambda (y) (/ (- y originy) scaley)))
	      (set! user->width
		    (lambda (w) (inexact->exact (abs (* w scalex)))))
	      (set! user->height
		    (lambda (h) (inexact->exact (abs (* h scaley)))))
	      (set! width->user (lambda (w) (abs (/ w scalex))))
	      (set! height->user (lambda (h) (abs (/ h scaley))))
	      (set-view #f '())
	      (set-view view '())
	      (set! visible-event-queue (cons view visible-event-queue))
	      (list rescaled (- originx was-originx)
		    (- originy was-originy)))))

;;; Once the coordinate transformation is complete, the following procedure
;;; is called to change the window.  When a partition of a window is being
;;; uniformly transformed and no scaling is being done, then bits will be
;;; moved using xcopyarea.  All other transformations will result in the
;;; window being damaged and redrawn.
;;;
;;; N.B.: VIEW-COMPILED is required as the optimum case that uses XIFEVENT
;;; can only be used if TRANSFORM-REDISPLAY is compiled.

(eval-when (load) (define VIEW-COMPILED #t))
(eval-when (eval) (define VIEW-COMPILED #f))

(define (TRANSFORM-REDISPLAY views damage-all deltax deltay
	    minx miny maxx maxy)
    
    (define (GRAPHICS-EVENT? dpy event any)
	    (let* ((event (cons 'xeventp
				((lap (x) (POINTER_TSCP (UNSIGNED x))) event)))
		   (type (xevent-type event)))
		  (if (or (and (eq? type graphicsexpose)
			       (eq? (xevent-xgraphicsexpose-drawable event)
				    *xwindow*))
			  (and (eq? type noexpose)
			       (eq? (xevent-xnoexpose-drawable event)
				    *xwindow*)))
		      1
		      0)))
    
    (define (HANDLE-GRAPHICS-EVENTS)
	    (let ((event (xifevent *dpy* graphics-event? (cons 'charp 0))))
		 (when (eq? (xevent-type event) graphicsexpose)
		       (window-expose-bbl! *window*
			   (merge-bbl (xevent-xgraphicsexpose-x event)
			       (xevent-xgraphicsexpose-y event)
			       (+ (xevent-xgraphicsexpose-x event)
				  (xevent-xgraphicsexpose-width
				      event))
			       (+ (xevent-xgraphicsexpose-y event)
				  (xevent-xgraphicsexpose-height
				      event))
			       (window-expose-bbl *window*)))
		       (unless (zero? (xevent-xgraphicsexpose-count
					  event))
			       (handle-graphics-events)))))
    
    (define (DAMAGEAREA x y width height)
	    (window-damage-bbl! *window*
		(merge-bbl x y (+ x width) (+ y height)
		    (window-damage-bbl *window*))))
    
    (set-view (car views) '())
    (let ((width (- maxx minx))
	  (height (- maxy miny)))
	 (if (or damage-all (not view-compiled) (not (window-exposed *window*))
		 (>= (abs deltax) width) (>= (abs deltay) height))
	     (begin (for-each
			(lambda (view)
				(set-view view '())
				(view-new! *current-view* #t)
				(view-bb-head! *current-view* '())
				(view-bb-tail! *current-view* '()))
			views)
		    (if (window-exposed *window*)
			(damagearea minx miny width height)))
	     (begin  (xcopyarea *dpy* *xwindow* *xwindow*
			 (cv-gc #f #f #f #f #f #f) minx miny width height
			 (+ minx deltax) (+ miny deltay))
		     (set! stipple-x (+ stipple-x deltax))
		     (set! stipple-y (+ stipple-y deltay))
		     (if (negative? deltax)
			 (damagearea (+ maxx deltax) miny (abs deltax) height))
		     (if (positive? deltax)
			 (damagearea minx miny deltax height))
		     (if (negative? deltay)
			 (damagearea minx (+ maxy deltay) width (abs deltay)))
		     (if (positive? deltay)
			 (damagearea minx miny width deltay))
		     (if (eq? *clean-mouse-window* *window*)
			 (set! *clean-mouse-window* #f))
		     (for-each
			 (lambda (view)
				 (set-view view '())
				 (for-each
				     bbgraphic-bounding-box
				     (view-bb-head *current-view*)))
			 views)
		     (handle-graphics-events)))
	 (set! *update-display* #t)))

;;; Redraw those portions of the drawing inside the update bounding boxes as
;;; well as those additions to the drawing outside the bounding boxes.  The
;;; bounding boxes are specified in X's coordinate system.  If no bounding
;;; box list is specified, then simply write the additions.

(define (REDRAW-A-VIEW view bbl)
    
    (define (RECOMPUTE-BBGL gl stop)
	    (let loop ((gl gl) (head '()) (tail '()))
		 (if (and (pair? gl) (not (eq? gl stop)))
		     (let* ((g (car gl))
			    (bbgl (list (make-bbgraphic g))))
			   (if (null? head)
			       (loop (cdr gl) bbgl bbgl)
			       (loop (cdr gl) head (begin (set-cdr! tail bbgl)
							  bbgl))))
		     (begin (view-bb-head! view head)
			    (view-bb-tail! view tail)))))
    
    (define (ADD-AND-DRAW gl)
	    (let loop ((gl gl) (head (view-bb-head view))
		       (tail (view-bb-tail view)))
		 (if (pair? gl)
		     (let* ((g (car gl))
			    (bbg (make-bbgraphic g))
			    (bbgl (list bbg)))
			   ((graphic-xdraw g))
			   (if (null? head)
			       (loop (cdr gl) bbgl bbgl)
			       (loop (cdr gl) head (begin (set-cdr! tail bbgl)
							  bbgl))))
		     (begin (view-bb-head! view head)
			    (view-bb-tail! view tail)))))
    
    (define (XDRAW-INTERSECTING-BBGRAPHICS)
	    (let loop ((minx #f) (miny #f) (maxx #f) (maxy #f) (l bbl))
		 (if (pair? l)
		     (let ((h (car l)))
			  (loop (bbmin minx (car h)) (bbmin miny (cadr h))
				(bbmax maxx (caddr h)) (bbmax maxy (cadddr h))
				(cdr l)))
		     (bbgraphics-intersect (view-bb-head view)
			 minx miny maxx maxy
			 (if (= (length bbl) 1)
			     xdraw-bbgraphic
			     if-in-any-xdraw-bbgraphic)))))
    
    (define (IF-IN-ANY-XDRAW-BBGRAPHIC bbg)
	    (let ((minx (bbgraphic-minx bbg))
		  (miny (bbgraphic-miny bbg))
		  (maxx (bbgraphic-maxx bbg))
		  (maxy (bbgraphic-maxy bbg)))
		 (let loop ((bbl bbl))
		      (if (pair? bbl)
			  (let ((bb (car bbl)))
			       (if (or (>= (car bb) maxx) (>= (cadr bb) maxy)
				       (<= (caddr bb) minx)
				       (<= (cadddr bb) miny))
				   (loop (cdr bbl))
				   (xdraw-bbgraphic bbg)))))))
    
    (let ((drawing (view-drawing view)))
	 (set-view view '())
	 (cond ((view-new view)
		(view-new! view #f)
		(add-and-draw (drawing-head drawing)))
	       ((drawing-cleared drawing)
		(view-bb-head! view '())
		(view-bb-tail! view '())
		(add-and-draw (drawing-head drawing)))
	       ((not (null? bbl))
		(if  (drawing-zmotion drawing)
		     (recompute-bbgl (drawing-head drawing)
			 (drawing-added-head drawing)))
		(set-view view bbl)
		(xdraw-intersecting-bbgraphics)
		(set-view view '())
		(add-and-draw (drawing-added-head drawing)))
	       (else (add-and-draw (drawing-added-head drawing))))))

;;; Module reset/initialization procedure.

(define (VIEW-MODULE-INIT)
    (set! *current-view* #f))
