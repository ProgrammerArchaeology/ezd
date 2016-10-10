;;; ezd - easy drawing for X11 displays.
;;;
;;; A WINDOW object maintains the information required for an ezd drawing
;;; window.

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

(module window)

(include "struct.sch")
(include "display.sch")
(include "view.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "commands.sch")
(include "ginfo.sch")
(include "events.sch")
(include "ezd.sch")
(include "xternal.sch")

;;; Each WINDOW object is represented by a structure with the following
;;; fields:
;;;
;;;	DISPLAY		display object for X display.
;;;	X		initial window position in pixels
;;;	Y
;;;	WIDTH		initial window size in pixels
;;;	HEIGHT
;;;	NAME		ezd name for the window.
;;;	TITLE		title string for the X window.
;;;	FOREGROUND-NAME	foreground color name.
;;;	BACKGROUND-NAME	background color name.
;;;	FOREGROUND	X pixel for the foreground color.
;;;	BACKGROUND	X pixel for the background color.
;;;	EXPOSED		boolean indicating that the window has been
;;;			initially exposed.
;;;	EXPOSE-BBL	list of bounding boxes defining areas that were
;;;			exposed.
;;;	DAMAGE-BBL	list of bounding boxes defining areas that need to be
;;;			redrawn, but weren't exposed.
;;;	VIEWS		list of views of drawings displayed in the window.  The
;;;			head of the list is the "bottom" view.
;;;	CURSOR		current window cursor
;;;	CURSORS		stack of cursors used by SAVE-CURSOR and RESTORE-CURSOR
;;;			commands.
;;;	GC		graphics context for pixmap operations
;;;	XWINDOW		X windowid for the window.

(define-structure WINDOW
    display
    x
    y
    width
    height
    name
    title
    foreground-name
    background-name
    (foreground (if foreground-name
		    (display-color->pixel display foreground-name)
		    (begin (color? 'black)
		    	   (window-foreground-name! self 'black)
			   (display-black display))))
    (background (if background-name
		    (display-color->pixel display background-name)
		    (begin (color? 'white)
		    	   (window-background-name! self 'white)
			   (display-white display))))
    (variable-width (if (<= width 0) width #f))
    (variable-height (if (<= height 0) height #f))
    (exposed #f)
    (expose-bbl '())
    (damage-bbl '())
    (views '())
    (cursor (display-font->cursor display xc_left_ptr))
    (cursors '())
    (gc #f)
    (xwindow (let* ((dpy (display-dpy display))
		    (screen (display-screen display))
		    (wa (let ((wa (make-xsetwindowattributes)))
			     (xsetwindowattributes-background_pixel! wa
				 (window-background self))
			     (xsetwindowattributes-border_pixel! wa
				 (window-foreground self))
			     (xsetwindowattributes-colormap! wa
				 (display-colormap display))
			     wa))
		    (xwindow (xcreatewindow dpy
				 (xrootwindow dpy screen)
				 (window-x self)
				 (window-y self)
				 (window-width self)
				 (window-height self)
				 2
				 (display-visual-depth display)
				 inputoutput
				 (display-visual display)
				 (bit-or cwbackpixel cwborderpixel cwcolormap)
				 wa))
		    (gc (xcreategc dpy xwindow 0 (make-xgcvalues))))
		   (let ((wmh (make-xwmhints)))
			(xwmhints-flags! wmh 1)
			(xwmhints-input! wmh 1)
			(xsetwmhints dpy xwindow wmh))
		   (xstorename dpy xwindow title)
		   (xseticonname dpy xwindow (symbol->string name))
		   (xselectinput dpy xwindow
		       (bit-or keypressmask keyreleasemask
			       exposuremask
			       structurenotifymask
			       ownergrabbuttonmask
			       buttonpressmask buttonreleasemask
			       enterwindowmask leavewindowmask
			       pointermotionmask))
		   (xdefinecursor dpy xwindow (window-cursor self))
		   (xsetforeground dpy gc (window-background self))
		   (xsetgraphicsexposures *dpy* gc 0)
		   (window-gc! self gc)
		   (set! *name-windows*
			 (cons (list (window-name self) self) *name-windows*))
		   (set! *xwindow-windows*
			 (cons (list xwindow self) *xwindow-windows*))
		   xwindow)))

(define-in-line-structure-access WINDOW
    display
    x
    y
    width
    height
    name
    title
    foreground-name
    background-name
    foreground
    background
    variable-width
    variable-height
    exposed
    expose-bbl
    damage-bbl
    views
    cursor
    cursors
    gc
    xwindow)
    
;;; A list of lists of window name and the appropriate WINDOW data structure
;;; is kept in *NAME-WINDOWS*.

(define *NAME-WINDOWS* '())

;;; Convert a window name to the WINDOW data structure.

(define (NAME->WINDOW name)
    (let ((x (assoc name *name-windows*)))
	 (if x (cadr x) (error 'name->window "WINDOW not defined: ~s" name))))

;;; See DRAWING-IN-LAST-EXISTING-WINDOW? (view.sc) to see how
;;; LAST-EXISTING-WINDOW-NAME is used to parse commands.

(define LAST-EXISTING-WINDOW-NAME #f)

;;; Boolean to check if a window exists.

(define (WINDOW-EXISTS? name)
    (if (assoc name *name-windows*)
	(begin (set! last-existing-window-name name)
	       #t)
	#f))

;;; A list of lists of X window id and the appropriate WINDOW data structure
;;; is kept in *XWINDOW-WINDOWS*

(define *XWINDOW-WINDOWS* '())

;;; Convert a X window id to a WINDOW data structure.

(define (XWINDOW->WINDOW xwindow)
    (let ((x (assoc xwindow *xwindow-windows*)))
	 (if x (cadr x) #f)))

;;; A drawing window is created by the following procedure.  If the window
;;; already exists, it is deleted and recreated.

(define (EZD-WINDOW name x-y width height fixed-size points title
	    foreground-name background-name)
    (let* ((x (if (pair? x-y)
		  (if points (points->pixels (car x-y)) (car x-y))
		  (points->pixels 144)))
	   (y (if (pair? x-y)
		  (if points (points->pixels (cadr x-y)) (cadr x-y))
		  (points->pixels 144)))
	   (width (if points (points->pixels width) width))
	   (height (if points (points->pixels height) height)))
	  (if (window-exists? name) (window-delete name))
	  (let ((w (make-window *display* x y width height name
		       (or title (symbol->string name))
		       foreground-name background-name))
		(hints (make-xsizehints)))
	       (xsizehints-flags! hints ussize)
	       (xsizehints-width! hints width)
	       (xsizehints-height! hints height)
	       (when (pair? x-y)
		     (xsizehints-flags! hints
			 (bit-or (xsizehints-flags hints) usposition))
		     (xsizehints-x! hints x)
		     (xsizehints-y! hints y))
	       (when fixed-size
		     (xsizehints-flags! hints
			 (bit-or (xsizehints-flags hints)
				 pminsize pmaxsize))
		     (xsizehints-min_width! hints width)
		     (xsizehints-max_width! hints width)
		     (xsizehints-min_height! hints height)
		     (xsizehints-max_height! hints height))
	       (xsetnormalhints *dpy* (window-xwindow w) hints)
	       w)))

(define-ezd-command
    `(window ,symbol?
	     (optional ,non-negative? ,non-negative?)
	     ,positive-number? ,positive-number?
	     (optional fixed-size) (optional points)
	     (optional ,string?) (optional ,color?) (optional ,color?))
    "(window name [ x y ] width height [ FIXED-SIZE ] [ POINTS ] [\"<title>\"] [<color> [<color>] ])"
    ezd-window)

;;; A WINDOW is deleted by the following procedure.

(define (WINDOW-DELETE name)
    (let ((self (name->window name)))
	 (for-each
	     (lambda (view) (delete-view name (view-drawing-name view)))
	     (window-views self))
	 (set! *xwindow-windows*
	       (remove (list (window-xwindow self) self) *xwindow-windows*))
	 (set! *name-windows*
	       (remove (list (window-name self) self) *name-windows*))
	 (xdestroywindow (display-dpy (window-display self))
	     (window-xwindow self))
	 (set! *update-display* #t)))

(define-ezd-command
    `(delete-window ,window-exists?)
    "(delete-window window)"
    window-delete)

;;; Cursors are saved and restored by the ezd commands SAVE-CURSOR and
;;; RESTORE-CURSOR.

(define-ezd-command
    `(save-cursor ,window-exists?)
    "(save-cursor window-name)"
    (lambda (name)
	    (let ((self (name->window name)))
		 (window-cursors! self (cons (window-cursor self)
					     (window-cursors self))))))

(define-ezd-command
    `(restore-cursor ,window-exists?)
    "(restore-cursor window-name)"
    (lambda (name)
	    (let* ((self (name->window name))
		   (cursors (window-cursors self)))
		  (when cursors
			(let ((cursor (car cursors)))
			     (xdefinecursor *dpy* (window-xwindow self) cursor)
			     (window-cursor! self cursor)
			     (window-cursors! self (cdr cursors))
			     (xflush *dpy*))))))

;;; A new cursor is installed in a window by the ezd command SET-CURSOR.

(define-ezd-command
    `(set-cursor ,window-exists? ,cursor-name?)
    "(set-cursor window-name cursor-name)"
    (lambda (name shape)
	    (let ((self (name->window name))
		  (cursor (display-font->cursor *display*
			      (cursor-name? shape))))
		 (xdefinecursor *dpy* (window-xwindow self) cursor)
		 (window-cursor! self cursor)
		 (xflush *dpy*))))

;;; A bounding box is merged onto a list of non-intersecting bounding boxes by
;;; the following function.  Overlapping bounding boxes are merged into one
;;; that contains both.  Adjacent boxes that are equal in size on the one
;;; dimension are merged.

(define (MERGE-BBL minx miny maxx maxy bbl)
    (let loop ((old bbl) (new '()))
	 (if (pair? old)
	     (let* ((h (car old))
		    (h-minx (car h))
		    (h-miny (cadr h))
		    (h-maxx (caddr h))
		    (h-maxy (cadddr h)))
		   (cond ((or (>= h-minx maxx) (>= h-miny maxy)
			      (<= h-maxx minx) (<= h-maxy miny))
			  (loop (cdr old) (cons h new)))
			 ((and (= minx h-minx) (= maxx h-maxx) (= maxy h-miny))
			  (merge-bbl minx miny maxx h-maxy (remq h bbl)))
			 ((and (= minx h-minx) (= maxx h-maxx) (= h-maxy miny))
			  (merge-bbl minx h-miny maxx maxy (remq h bbl)))
			 ((and (= miny h-miny) (= maxy h-maxy) (= maxx h-minx))
			  (merge-bbl minx miny h-maxx maxy (remq h bbl)))
			 ((and (= miny h-miny) (= maxy h-maxy) (= h-maxx minx))
			  (merge-bbl h-minx miny maxx maxy (remq h bbl)))
			 (else (merge-bbl (min minx h-minx) (min miny h-miny)
				   (max maxx h-maxx) (max maxy h-maxy)
				   (remq h bbl)))))
	     (cons (list minx miny maxx maxy) new))))

;;; Events related to a WINDOW are processed by the following procedure.  The
;;; only event handling "hardwired" into ezd is for expose events and window
;;; resizing.  The rest of the events are handled by user event handlers.

(define (WINDOW-EVENT-HANDLER window event)
    (cond ((eq? (xevent-type event) expose)
	   (set! *update-display* #t)
	   (window-exposed! window #t)		
	   (window-expose-bbl! window
	       (merge-bbl (xevent-xexpose-x event) (xevent-xexpose-y event)
		   (+ (xevent-xexpose-x event) (xevent-xexpose-width event))
		   (+ (xevent-xexpose-y event) (xevent-xexpose-height event))
		   (window-expose-bbl window))))
	  ((eq? (xevent-type event) configurenotify)
	   (let ((old-width (window-width window))
		 (old-height (window-height window))
		 (width (xevent-xconfigure-width event))
		 (height (xevent-xconfigure-height event)))
		(when (and (or (not (= width old-width))
			       (not (= height old-height))))
		      (when (eq? window *window*)
			    (set! *width* width)
			    (set! *height* height))
		      (window-width! window width)
		      (window-height! window height)
		      (handle-window-events window 'resize event
			  (list old-width old-height width height))))))
    (handle-when-events window event))

;;; Once there are no pending events, the display's event handler calls the
;;; following procedure to redraw all views in all windows as needed.

(define (REDRAW-ALL-WINDOWS)
    (let ((visible-event-views '())) 
	 (for-each
	     (lambda (name-window)
		     (let* ((window (cadr name-window))
			    (partitions (partition-views
					    (window-views window))))
			   (set! visible-event-views
				 (append (transform-views
					     (window-views window))
					 visible-event-views))
			   (for-each
			       (lambda (views)
				       (if (pair? views)
					   (let ((view (car views)))
						(redraw-a-partition window
						    views))))
			       partitions)
			   (window-damage-bbl! window '())
			   (window-expose-bbl! window '())))
	     *name-windows*)
	 (drawings-redrawn)
	 (for-each handle-visible-events visible-event-views)))

;;; When changes must be made to a drawing, or additions made to an overlayed
;;; drawing, the image is rendered to a pixmap and then copied to the screen
;;; to reduce screen flashes.

(define *PIXMAP* #f)

(define *PIXMAP-HEIGHT* #f)

(define *PIXMAP-WIDTH* #f)

(define (REDRAW-A-PARTITION window views)
    (let ((solid-views (let loop ((views views))
			    (if (pair? views)
				(if (and (drawing-is-clear
					     (view-drawing (car views)))
					 (not (drawing-cleared
						  (view-drawing
						      (car views)))))
				    (loop (cdr views))
				    (cons (car views) (loop (cdr views))))
				'())))
	  (bbl '())
	  (clip-minx #f)
	  (clip-miny #f)
	  (clip-maxx #f)
	  (clip-maxy #f))
	 
	 (define (SET-CLIP view)
		 ;;; Define the current clipping region.
		 (set! clip-minx (or (and view (view-clip-minx view)) 0))
		 (set! clip-miny (or (and view (view-clip-miny view)) 0))
		 (set! clip-maxx (or (and view (view-clip-maxx view))
				     (window-width window)))
		 (set! clip-maxy (or (and view (view-clip-maxy view))
				     (window-height window))))
	 
	 (define (ADD-BBL minx miny maxx maxy)
		 ;;; Add a clipped bounding box to the bounding box list.
		 (if (not (or (<= maxx clip-minx)
			      (<= maxy clip-miny)
			      (>= minx clip-maxx)
			      (>= miny clip-maxy)))
		     (set! bbl (merge-bbl (max minx clip-minx)
				   (max miny clip-miny)
				   (min maxx clip-maxx)
				   (min maxy clip-maxy) bbl))))
	 
	 (define (UNION-VIEW-GRAPHIC compute-bb)
		 ;;; Add a deleted object to the bounding box list.
		 (let* ((bb (compute-bb))
			(minx (car bb))
			(miny (cadr bb))
			(maxx (caddr bb))
			(maxy (cadddr bb)))
		       (if (not (eq? minx maxx))
			   (add-bbl minx miny maxx maxy))))
	 
	 (define (UNION-VIEW view)
		 ;;; Add changes to a view to the bounding box list.
		 (cond ((view-new view)
			(set-view view '())
			(set-clip view)
			(let loop ((gl (drawing-head (view-drawing view)))
				   (minx #f) (miny #f) (maxx #f) (maxy #f))
			     (if (pair? gl)
				 (let ((bb ((graphic-compute-bb (car gl)))))
				      (loop (cdr gl)
					    (bbmin minx (car bb))
					    (bbmin miny (cadr bb))
					    (bbmax maxx (caddr bb))
					    (bbmax maxy (cadddr bb))))
				 (if minx (add-bbl minx miny maxx maxy)))))
		       ((drawing-cleared (view-drawing view))
			(set-clip view)
			(add-bbl 0 0 (window-width window)
			    (window-height window)))
		       (else (set-view view '())
			     (set-clip view)
			     (for-each
				 union-view-graphic
				 (drawing-damaged (view-drawing view))))))
	 
	 (define (ADD-ADDITIONS-TO-BBL view)
		 ;;; Add additions in a view to the bounding box list.
		 (set-view view '())
		 (set-clip view)
		 (for-each
		     (lambda (g) (union-view-graphic (graphic-compute-bb g)))
		     (drawing-added-head (view-drawing view))))
	 
	 (define (UNION-ADDITIONS-TO-UNDERLAYS vl)
		 ;;; Add additions to lower drawings to the bounding box list.
		 (when (and (pair? vl) (pair? (cdr vl)))
		       (add-additions-to-bbl (car vl))
		       (union-additions-to-underlays (cdr vl))))
	 
	 (define (ADD-EXPOSE-TO-BBL)
		 ;;; Add window expose regions to the bounding box list.
		 (for-each
		     (lambda (bb) (add-bbl (car bb) (cadr bb)
				      (caddr bb) (cadddr bb)))
		     (window-expose-bbl window)))
	 
	 (define (ADD-DAMAGE-TO-BBL)
		 ;;; Add window expose regions to the bounding box list.
		 (for-each
		     (lambda (bb) (add-bbl (car bb) (cadr bb)
				      (caddr bb) (cadddr bb)))
		     (window-damage-bbl window)))
	 
	 (define (REDRAW)
		 ;;; Redraw the union of the damaged and exposed areas in
		 ;;; each view in order.
		 (for-each
		     (lambda (view)
			     (redraw-a-view view (clip-bbl-to-view view bbl)))
		     solid-views))
	 
	 (when (window-exposed window)
	       ;;; Compute the union of the view's damaged areas and added
	       ;;; areas to underlaying drawings.
	       (for-each union-view solid-views)
	       (union-additions-to-underlays solid-views)
	       (if (and (not nopixmap) solid-views)
		   ;;; OK to use a Pixmap to avoid flashing the screen.
		   (let ((xwindow (window-xwindow window))
			 (width (window-width window))
			 (height (window-height window)))
			;;; Add additions to the top drawing to bbl
			(add-additions-to-bbl (car (last-pair solid-views)))
			;;; Add exposed and damaged regions clipped by each
			;;; view to bbl
			(for-each
			    (lambda (view)
				    (set-clip view)
				    (add-expose-to-bbl)
				    (add-damage-to-bbl))
			    solid-views)
			;;; Get a pixmap.
			(when (or (not *pixmap*) (< *pixmap-width* width)
				  (< *pixmap-height* height))
			      (if *pixmap* (xfreepixmap *dpy* *pixmap*))
			      (set! *pixmap*
				    (xcreatepixmap *dpy*
					(window-xwindow window)
					width height
					(display-visual-depth *display*)))
			      (set! *pixmap-width* width)
			      (set! *pixmap-height* height))
			(set-view #f '())
			(window-xwindow! window *pixmap*)
			;;; Build clip list and fill pixmap with background.
			(let loop ((l bbl) (rl '()))
			     (if (pair? l)
				 (let ((bb (car l))
				       (r (make-xrectangle)))
				      (xrectangle-x! r (car bb))
				      (xrectangle-y! r (cadr bb))
				      (xrectangle-width! r (- (caddr bb)
							      (car bb)))
				      (xrectangle-height! r (- (cadddr bb)
							       (cadr bb)))
				      (loop (cdr l) (cons r rl)))
				 (xsetcliprectangles *dpy* (window-gc window)
				     0 0 (xrectangle-list->xrectanglea rl)
				     (length rl) Unsorted)))
			(xfillrectangle *dpy* *pixmap* (window-gc window)
			    0 0 width height)
			;;; Draw to pixmap and then copy to the window.
			(redraw)
			(xcopyarea *dpy* *pixmap* xwindow (window-gc window)
			    0 0 width height 0 0)
			(set-view #f '())
			(window-xwindow! window xwindow))
		   ;;; No pixmap, draw directly to the window.
		   (begin (for-each
			      (lambda (view)
				      (set-clip view)
				      (add-damage-to-bbl))
			      solid-views)
			  (for-each
			      (lambda (bb)
				      (xcleararea *dpy*
					  (window-xwindow window)
					  (car bb) (cadr bb)
					  (- (caddr bb) (car bb))
					  (- (cadddr bb) (cadr bb)) 0))
			      bbl)
			  (set-clip #f)
			  (add-expose-to-bbl)
			  (redraw))))))

;;; Once all drawings have been redrawn, then the additions list and the
;;; redraw area can be cleared.

(define *REDRAW-SEQ* 0)

(define (DRAWINGS-REDRAWN)
    (for-each
	(lambda (name-drawing)
		(let ((drawing (cadr name-drawing)))
		     (drawing-added-head! drawing '())
		     (drawing-added-tail! drawing '())
		     (drawing-zmotion! drawing #f)
		     (drawing-cleared! drawing #f)
		     (drawing-damaged! drawing '())))
	*drawings*)
    (set! *redraw-seq* (+ 1 *redraw-seq*)))
		      
;;; Module reset/initialization

(define (WINDOW-MODULE-INIT)
    (set! *name-windows* '())
    (set! *xwindow-windows* '())
    (set! *pixmap* #f))
