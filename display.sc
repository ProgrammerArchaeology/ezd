;;; ezd - easy drawing for X11 displays.
;;;
;;; A DISPLAY object maintains a connection to an X display and provides event
;;; dispatching via a system file task.

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

(module display)

(include "struct.sch")
(include "commands.sch")
(include "window.sch")
(include "drawing.sch")
(include "view.sch")
(include "xternal.sch")

;;; Display structure holds information specific to a display.  When a display
;;; object is created, it will install a system file task to handle events.

(define-structure DISPLAY
    name
    (dpy (let ((dpy (xopendisplay name)))
	      (if (null-pointer? dpy)
		  (if (equal? name "")
		      (error 'open-display
			     "Environment variable DISPLAY is not defined")
		      (error 'open-display "Can't open ~a" name)) )
	      (let ((system-file (xconnectionnumber dpy)))
		   (define-system-file-task system-file
		       (lambda () (display-event-handler self))
		       (lambda () (display-event-handler self)))
		   (enable-system-file-tasks #t))
	      (set! *pixels/point*
		    (* (/ (xdisplaywidth dpy (xdefaultscreen dpy))
			  (xdisplaywidthmm dpy (xdefaultscreen dpy)))
		       .3525))
	      (set! *display* self)
	      (set! *dpy* dpy)
	      dpy))
    (screen (xdefaultscreen (display-dpy self)))
    (visual-depth (xdefaultdepth (display-dpy self) (display-screen self)))
    (colormap (xdefaultcolormap (display-dpy self) (display-screen self)))
    (visual (let* ((dpy (display-dpy self))
		   (screen (display-screen self))
		   (default (xdefaultvisual dpy screen))
		   (depth (xdefaultdepth dpy screen))
		   (class (visual-class default)))
		  (if (and (eq? depth 8) (eq? class pseudocolor))
		      default
		      (let* ((return (xmatchvisualinfo dpy screen 8
					 pseudocolor))
			     (ok (car return))
			     (vi (cadr return)))
			    (if (not (zero? ok))
				(begin (display-visual-depth! self 8)
				       (display-colormap! self
					   (xcreatecolormap dpy
					       (xrootwindow dpy screen)
					       (xvisualinfo-visual vi)
					       allocnone))
				       (xvisualinfo-visual vi))
				default)))))
    (tiny-pixmap (xcreatepixmap (display-dpy self)
		     (xrootwindow (display-dpy self) (display-screen self))
		     1 1 (display-visual-depth self)))
    (black (display-color->pixel self 'black))
    (white (display-color->pixel self 'white))
    (handling-events #f)
    (defered-delete #f)
    (fonts '())
    (cursors '())
    (private-colors '())
    (cgcs '()))

(define-in-line-structure-access DISPLAY
    name
    dpy
    screen
    visual-depth
    colormap
    visual
    tiny-pixmap
    black
    white
    handling-events
    defered-delete
    fonts
    cursors
    private-colors
    cgcs)

;;; Graphics contexts are represented by a list of CLIPGC objects on the
;;; display object.

(define-structure CLIPGC
    width		;;; Line width
    color		;;; Foreground color
    background		;;; Background color
    stipple		;;; Stipple
    dash		;;; Boolean indicating dash
    font		;;; Font
    arc			;;; Arc mode
    xgc			;;; X graphics context
    bbl			;;; Bounding box list for clip region
    (stipple-x 0)	;;; Stipple offset
    (stipple-y 0))

(define-in-line-structure-access CLIPGC
    width
    color
    background
    stipple
    dash
    font
    arc
    xgc
    bbl
    stipple-x
    stipple-y)

;;; ezd supports one display and it is stored in the following globals.  A
;;; display structure is used to facilitate extensions to ezd that allow
;;; multiple displays.  The boolean *UPDATE-DISPLAY* is set true when windows
;;; need to be redrawn and/or drawing commands are queued.

(define *DISPLAY* #f)

(define *DPY* #f)

(define *PIXELS/POINT* #f)

(define *UPDATE-DISPLAY* #f)

;;; When an event is available for a display, the following procedure is
;;; called.  It will return once it has processed all events. 

(define (DISPLAY-EVENT-HANDLER self)
    (let ((event (make-xevent))
	  (dpy (display-dpy self))
	  (save-current-drawing *current-drawing*))
	 (if (not (display-handling-events self))
	     (let loop ()
		  (unless (zero? (xeventsqueued dpy queuedafterreading))
			  (ynextevent dpy event)
			  (display-handling-events! self #t)
			  (if (eq? (xevent-type event) mappingnotify)
			      (xrefreshkeyboardmapping event)
			      (let ((window (xwindow->window
						(xevent-xany-window event))))
				   (if window
				       (window-event-handler window event))))
			  (display-handling-events! self #f)
			  (if (display-defered-delete self)
			      (display-delete self)
			      (loop)))))
	 (let loop ()
	      (when *update-display*
		    (set! *update-display* #f)
		    (redraw-all-windows)
		    (xflush dpy)
		    (loop)))
	 (set! *current-drawing* save-current-drawing)))

;;; Normally, drawing and X event handling is done when there are no further
;;; commands to be processed.  However, the (DRAW-NOW) command causes drawing
;;; and event handling to be done at that point.

(define-ezd-command
    '(draw-now)
    "(draw-now)"
    (lambda () (display-event-handler *display*)))

;;; A display object is deleted by the following procedure.  The procedure
;;; may be called from an event handler.

(define (DISPLAY-DELETE self)
    (if (or (display-handling-events self) (not (display-dpy self)))
	(display-defered-delete! self #t)
	(let ((dpy (display-dpy self)))
	     (define-system-file-task (xconnectionnumber dpy) #f #f)
	     (xclosedisplay (display-dpy self))
	     (display-dpy! self #f))))

;;; Converts a string font name into a xfontstruct.  An error message is
;;; printed on stderr and the xfontstruct for "fixed" is returned on an error.

(define (DISPLAY-FONT->XFONTSTRUCT display font-name)
    (let* ((font-name (or font-name "fixed"))
	   (x (assoc font-name (display-fonts display))))
	  (if x
	      (cdr x)
	      (let ((f (let ((fa (xloadqueryfont (display-dpy display)
				    font-name)))
			    (if (null-pointer? fa)
				(let ((fd (xloadqueryfont (display-dpy display)
					     "fixed")))
				     (format stderr-port
					     "ezd can't load font: ~a~%"
					     font-name)
				     fd)
				fa))))
		   (display-fonts! display (cons (cons font-name f)
						 (display-fonts display)))
		   f))))

;;; Turns a color into a pixel value.  Colors that can't be allocated result
;;; in an error message.

(define (DISPLAY-COLOR->PIXEL display color)
    (let ((pc (getprop color 'private-color)))
	 (if pc
	     pc
	     (let ((x (xallocnamedcolor (display-dpy display)
			  (display-colormap display)
			  (symbol->string color))))
		  (if (not (zero? (car x)))
		      (xcolor-pixel (caddr x))
		      (begin (format stderr-port
				     "Can't allocate color: ~a~%" color)
			     #f))))))

;;; Load a cursor into the display and return the Cursor data structure.

(define (DISPLAY-FONT->CURSOR display font)
    (let ((font-cursor (assoc font (display-cursors display))))
	 (if font-cursor
	     (cdr font-cursor)
	     (let ((cursor (xcreatefontcursor (display-dpy display) font)))
		  (display-cursors! display
		      (cons (cons font cursor) (display-cursors display)))
		  cursor))))

;;; Define a new color in the shared color map.

(define (DISPLAY-DEFINE-COLOR display color color-value)
    (let ((xc (make-xcolor))
	  (dpy (display-dpy display))
	  (screen (display-screen display))
	  (rgb (color-value->rgb color-value)))
	 (xcolor-red! xc (* 256 (car rgb)))
	 (xcolor-green! xc (* 256 (cadr rgb)))
	 (xcolor-blue! xc (* 256 (caddr rgb)))
	 (if (zero? (xalloccolor dpy (display-colormap display) xc))
	     (begin (format stderr-port "Can't allocate color: ~a~%" color)
		    (xcolor-pixel! xc (display-black display))))
	 (putprop color 'isa-color rgb)
	 (putprop color 'private-color (xcolor-pixel xc))
	 (display-private-colors! display
	     (cons color (display-private-colors display)))))

;;; Define a new color with a mutable private color cell.

(define (DISPLAY-DEFINE-VARIABLE-COLOR display color initial-color)
    (let* ((dpy (display-dpy display))
	   (pixel (let ((buffer (make-string 4)))
		       (if (zero? (xalloccolorcells dpy
				      (display-colormap display)
				      0 (type/value->pointer 'unsignedlongap 0)
				      0
				      (type/value->pointer 'unsignedlongap
					  buffer)
				      1))
			   (begin (ezd-error 'define-variable-color
				      "Can't allocate a private color cell")
				  #f)
			   (c-int-ref buffer 0)))))
	  (when pixel
		(putprop color 'private-color pixel)
		(putprop color 'variable-color #t)
		(display-private-colors! display
		    (cons color (display-private-colors display)))
		(display-set-variable-color display color initial-color))))

;;; Set the value of a color with a private color cell.

(define (DISPLAY-SET-VARIABLE-COLOR display color color-value)
    (let ((dpy (display-dpy display))
	  (xc (make-xcolor))
	  (rgb (color-value->rgb color-value)))
	 (xcolor-red! xc (* 256 (car rgb)))
	 (xcolor-green! xc (* 256 (cadr rgb)))
	 (xcolor-blue! xc (* 256 (caddr rgb)))
	 (xcolor-flags! xc (bit-or dored dogreen doblue))
	 (xcolor-pixel! xc (getprop color 'private-color))
	 (xstorecolor dpy (display-colormap display) xc)
	 (putprop color 'isa-color rgb)
	 (set! *update-display* #t)))

;;; Convert a color-value to a list of RGB values.

(define (COLOR-VALUE->RGB cv)

    (define (COLOR shift) (bit-and 255 (bit-rsh cv shift)))

    (if (symbol? cv)
	(getprop cv 'isa-color)
	(list (color 16) (color 8) (color 0))))

;;; Drawing contexts are shared between objects managed by the display by the
;;; following function.  GC's that differ only by their clipping region or
;;; stipple offset are shared.

(define (DISPLAY-GC display width color background stipple stipple-x stipple-y
	    dash font arc bbl)
    (let ((dpy (display-dpy display))
	  (cgc (let loop ((cgcs (display-cgcs display)))
		    (if (null? cgcs)
			(let ((cgc (make-clipgc width color background stipple
				       dash font arc #f '())))
			     (display-cgcs! display
				 (cons cgc (display-cgcs display)))
			     cgc)
			(let ((cgc (car cgcs)))
			     (if (and (equal? width (clipgc-width cgc))
				      (equal? color (clipgc-color cgc))
				      (equal? background
					      (clipgc-background cgc))
				      (equal? stipple (clipgc-stipple cgc))
				      (equal? dash (clipgc-dash cgc))
				      (equal? font (clipgc-font cgc))
				      (equal? arc (clipgc-arc cgc)))
				 cgc
				 (loop (cdr cgcs))))))))
	 (if (not (clipgc-xgc cgc))
	     (let* ((window (display-tiny-pixmap display))
		    (gc (xcreategc dpy window 0 (make-xgcvalues))))
		   (xsetlineattributes dpy gc (or width 0)
		       (if dash lineonoffdash linesolid) capnotlast joinmiter)
		   (xsetarcmode dpy gc (or arc arcchord))
		   (xsetbackground dpy gc background)
		   (if (symbol? color)
		       (xsetforeground dpy gc
			   (or (display-color->pixel display color)
			       (display-black display)))
		       (xsetforeground dpy gc color))
		   (if stipple
		       (let ((buffer (make-string 128))
			     (bitmapsize 0))
			    (let loop ((i 0)
				       (bits (getprop stipple 'isa-stipple)))
				 (if (not (null? bits))
				     (begin (c-shortunsigned-set! buffer i
						(car bits))
					    (loop (+ i 2) (cdr bits)))
				     (set! bitmapsize (sqrt (* i 8)))))
			    (xsetstipple dpy gc
				(xcreatebitmapfromdata dpy window
				    (cons 'charp buffer) bitmapsize
				    bitmapsize))
			    (xsetfillstyle dpy gc fillstippled)))
		   (if font
		       (xsetfont dpy gc
			   (xfontstruct-fid (display-font->xfontstruct display
						font))))
		   (clipgc-xgc! cgc gc)))
	 (unless (equal? bbl (clipgc-bbl cgc))
		 (if bbl
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
			      (xsetcliprectangles dpy (clipgc-xgc cgc) 0 0
				  (xrectangle-list->xrectanglea rl) (length rl)
				  Unsorted)))
		     (xsetclipmask dpy (clipgc-xgc cgc) NONE))
		 (clipgc-bbl! cgc bbl))
	 (when (and stipple
		    (or (not (eq? stipple-x (clipgc-stipple-x cgc)))
			(not (eq? stipple-y (clipgc-stipple-y cgc)))))
	       (xsettsorigin dpy (clipgc-xgc cgc) stipple-x stipple-y)
	       (clipgc-stipple-x! cgc stipple-x)
	       (clipgc-stipple-y! cgc stipple-y))
	 (clipgc-xgc cgc)))

;;; Module reset/initialization.

(define (DISPLAY-MODULE-INIT)
    (when *display*
	  (for-each
	      (lambda (c)
		      (putprop c 'private-color #f)
		      (putprop c 'variable-color #f)
		      (putprop c 'isa-color #f))
	      (display-private-colors *display*))
	  (display-handling-events! *display* #f)
	  (display-delete *display*)
	  (set! *display* #f)
	  (set! *update-display* #f)))
