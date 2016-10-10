;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module generate the GRAPHIC objects representing
;;; arcs.

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

(module arc)

(include "struct.sch")
(include "commands.sch")
(include "ginfo.sch")
(include "display.sch")
(include "view.sch")
(include "psdraw.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "xternal.sch")

;;; Drawing arcs in Postscript may require the saving and restoration of the
;;; current transformation.  This is done by the following procedures.

(define (ARC-IN scale-y)
    (if (= scale-y 1)
	"newpath"
	`("newpath" "matrix" "currentmatrix" 1 ,scale-y "scale")))

(define (ARC-OUT scale-y)
    (if (= scale-y 1) "" "setmatrix"))

;;; Any of the types of arcs are drawn in X by the following procedure.

(define (XDRAW-ANY-ARC todraw gc x y width height angle1 angle2)
    (let ((ux1 (user->x x))
	  (ux2 (user->x (+ x width)))
	  (uy1 (user->y y))
	  (uy2 (user->y (+ y height)))
	  (a1 angle1)
	  (a2 angle2))
	 (if (<= ux1 ux2)
	     (if (<= uy1 uy2)
		 (begin (set! a1 (- a1))
			(set! a2 (- a2)))
		 #f)
	     (if (<= uy1 uy2)
		 (set! a1 (+ a1 180))
		 (begin (set! a1 (- 180 a1))
			(set! a2 (- a2)))))
	 (todraw *dpy* *xwindow* gc (min ux1 ux2) (min uy1 uy2)
		 (user->width width) (user->height height)
		 (* 64 a1) (* 64 a2))))

;;; An arc is created by the following procedure.

(define (ARC x y width height angle1 angle2 line-width color dash)
    (make-graphic
	#f
	(lambda ()
		(let ((ux1 (user->x x))
		      (ux2 (user->x (+ x width)))
		      (uy1 (user->y y))
		      (uy2 (user->y (+ y height)))
		      (lwdiv2 (quotient (user->lw line-width) 2)))
		     (list (- (min ux1 ux2) lwdiv2)
			   (- (min uy1 uy2) lwdiv2)
			   (+ (max ux1 ux2) lwdiv2 1)
			   (+ (max uy1 uy2) lwdiv2 1))))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (xdraw-any-arc xdrawarc
			(cv-gc (user->lw line-width) color #f dash #f #f)
			x y width height angle1 angle2)))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (let ((scale-y (/ height width))
			  (correct-y (/ width height)))
			 (pscolor color)
			 (pscommand (arc-in scale-y)
			     (+ x (/ width 2))
			     (* (+ y (/ height 2)) correct-y)
			     (/ width 2) angle1 (+ angle1 angle2) "arc"
			     (arc-out scale-y))
			 (psstroke (user->lw line-width) dash))))
	(lambda (minx miny maxx maxy)
		(let* ((lwdiv2 (quotient (user->lw line-width) 2))
		       (dx (width->user (max lwdiv2 2)))
		       (dy (height->user (max lwdiv2 2)))
		       (fx (- maxx minx))
		       (fy (- maxy miny)))
		      (and (in-arc-angle? minx miny maxx maxy x y
			       width height angle1 angle2)
			   (in-radius? minx miny maxx maxy (- x dx) (- y dy)
			       (+ width dx dx) (+ height dy dy))
			   (not (in-radius? minx miny maxx maxy
				    (+ x dx fx) (+ y dy fy)
				    (- width dx dx fx fx)
				    (- height dy dy fy fy))))))))

(define-ezd-command
    `(arc ,number? ,number? ,non-negative? ,non-negative? ,non-negative?
	  ,non-negative? (optional ,non-negative?) (optional ,color?)
	  (optional ,dash?))
    "(arc x y width height angle1 angle2 [<line width>] [<color>] [dash])"
    arc)

;;; An filled-arc is created by the following procedure.

(define (FILL-ARC x y width height angle1 angle2 color stipple)
    (make-graphic
	#f
	(lambda ()
		(let ((ux1 (user->x x))
		      (ux2 (user->x (+ x width 1)))
		      (uy1 (user->y y))
		      (uy2 (user->y (+ y height 1))))
		     (list (min ux1 ux2) (min uy1 uy2) (max ux1 ux2)
			   (max uy1 uy2))))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (xdraw-any-arc xfillarc
			(cv-gc #f color stipple #f #f arcchord)
			x y width height angle1 angle2)))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (let ((scale-y (/ height width))
			  (correct-y (/ width height)))
			 (pscolor color)
			 (pscommand (arc-in scale-y)
			     (+ x (/ width 2))
			     (* (+ y (/ height 2)) correct-y)
			     (/ width 2)  angle1 (+ angle1 angle2) "arc"
			     "closepath" "fill" (arc-out scale-y)))))
	(lambda (minx miny maxx maxy)
		(let ((aok (in-arc-angle? minx miny maxx maxy x y width height
			       angle1 angle2))
		      (rok (in-radius? minx miny maxx maxy x y width height)))
		     (or (and  (< angle2 180) aok rok
			       (in-segment? minx miny maxx maxy x y
				   width height angle1 angle2))
			 (and (<= 180 angle2 360) aok rok)
			 (and (< 180 angle2 360) (not aok) rok
			      (not (in-segment? minx miny maxx maxy x y width
				       height (+ angle1 angle2)
				       (- 360 angle2))))
			 (and (>= angle2 360) rok))))))

(define-ezd-command
    `(fill-arc ,number? ,number? ,non-negative? ,non-negative? ,number?
	 ,number? (optional ,color?) (optional ,stipple?))
    "(fill-arc x y width height angle1 angle2 [<color>] [<stipple>])"
    fill-arc)

;;; An pie-slice-arc is created by the following procedure.

(define (PIE-ARC x y width height angle1 angle2 color stipple)
    (make-graphic
	#f
	(lambda ()
		(let ((ux1 (user->x x))
		      (ux2 (user->x (+ x width 1)))
		      (uy1 (user->y y))
		      (uy2 (user->y (+ y height 1))))
		     (list (min ux1 ux2) (min uy1 uy2) (max ux1 ux2)
			   (max uy1 uy2))))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (xdraw-any-arc xfillarc
			(cv-gc #f color stipple #f #f arcpieslice)
			x y width height angle1 angle2)))
	(if (eq? color 'clear)
	    draw-clear
	    (lambda ()
		    (let ((scale-y (/ height width))
			  (correct-y (/ width height)))
			 (pscolor color)
			 (pscommand (arc-in scale-y)
			     (+ x (/ width 2))
			     (* (+ y (/ height 2)) correct-y)
			     "moveto" 
			     (+ x (/ width 2))
			     (* (+ y (/ height 2)) correct-y)
			     (/ width 2)
			     angle1 (+ angle1 angle2) "arc" "closepath"
			     "fill" (arc-out scale-y)))))
	(lambda (minx miny maxx maxy)
		(and (in-arc-angle? minx miny maxx maxy x y width height
			 angle1 angle2)
		     (in-radius? minx miny maxx maxy x y width height)))))

(define-ezd-command
    `(pie-arc ,number? ,number? ,non-negative? ,non-negative?
	 ,number? ,number? (optional ,color?) (optional ,stipple?))
    "(pie-arc x y width height angle1 angle2 [<color>] [<stipple>])"
    pie-arc)

;;; Intersection computation is done by the following procedures.  The first
;;; is a boolean that decides whether or not the bounding box is within the
;;; angle of the arc.

(define (IN-ARC-ANGLE? minx miny maxx maxy x y width height angle1 angle2)
    (let ((cx (+ x (/ width 2)))
	  (cy (+ y (/ height 2))))
	 
	 (define (IN? x y)
		 (let ((correct (if (< x cx)
				    180
				    (if (< y cy)
					360
					0)))
		       (alpha (if (= x cx)
				  90
				  (* (/ 180 3.14159 )
				     (atan (/ (- y cy) (- x cx)))))))
		      (if (> (+ angle1 angle2) 360)
			  (not (< (- (+ angle1 angle2) 360) (+ correct alpha)
				  angle1))
			  (<= angle1 (+ correct alpha) (+ angle1 angle2)))))
	 
	 (or (in? minx miny) (in? minx maxy) (in? maxx miny) (in? maxx maxy)
	     (in? (+ minx (/ (- maxx minx) 2)) (+ miny (/ (- maxy miny) 2))))))

;;; IN-RADIUS? determines whether or not the point is within radial distance
;;; of the center of the arc.

(define (IN-RADIUS? minx miny maxx maxy x y width height)
    (let* ((h (+ x (/ width 2)))
	   (k (+ y (/ height 2)))
	   (a (/ width 2))
	   (b (/ height 2)))
	  
	  (define (IN? x y)
		  (<= (+ (/ (* (- x h) (- x h)) (* a a))
			 (/ (* (- y k) (- y k)) (* b b)))
		      1))
	  (or (in? minx miny) (in? minx maxy) (in? maxx miny) (in? maxx maxy)
	      (in? (+ minx (/ (- maxx minx) 2))
		   (+ miny (/ (- maxy miny) 2))))))

;;; IN-SEGMENT? determines for points within the arc, whether or not they are
;;; actually in the segment of the filled arc.
;;;
;;;	xc, yc	= center of arc
;;;	x1, y1	= point at one end of the arc
;;;	x2, y2	= point at the other end of the arc
;;;	slope	= slope of the line between x1,y1 and x2,y2
;;;	y - yc = slope*(x - xc) line parallel to x1,y1 x2,y2 through xc,yc
;;;	-slope*x + y + slope*xc-yc = 0	general form for the above

(define (IN-SEGMENT? minx miny maxx maxy x y width height angle1 angle2)
    (let* ((w2 (/ width 2))
	   (h2 (/ height 2))
	   (radians1 (* angle1 (/ 3.14159 180)))
	   (radians2 (* angle2 (/ 3.14159 180)))
	   (xc (+ x w2))
	   (yc (+ y h2))
	   (x1 (+ xc (* (cos radians1) w2)))
	   (y1 (+ yc (* (sin radians1) h2)))
	   (x2 (+ xc (* (cos (+ radians1 radians2)) w2)))
	   (y2 (+ yc (* (sin (+ radians1 radians2)) h2)))
	   (slope (if (= x1 x2) #f (/ (- y2 y1) (- x2 x1))))
	   (yp (lambda (x) (if slope (+ (* slope (- x xc)) yc) yc)))
	   (distance (lambda (x y)
			     (if slope
				 (abs 
				      (/ (+ (* (- slope) x)
					    y
					    (- (* slope xc) yc))
					 (sqrt (+ (* slope slope) 1))))
				 (abs (- y yc)))))
	   (d1 (distance x1 y1)))
	  
	  (or (< d1 (distance minx miny)) (< d1 (distance minx maxy))
	      (< d1 (distance maxx miny)) (< d1 (distance maxx maxy))
	      (< d1 (distance (+ minx (/ (- maxx minx) 2))
			(+ miny (/ (- maxy miny) 2)))))))
