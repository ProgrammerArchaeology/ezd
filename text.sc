;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module generate the GRAPHIC objects representing
;;; text.

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

(module text)

(include "struct.sch")
(include "commands.sch")
(include "ginfo.sch")
(include "display.sch")
(include "view.sch")
(include "psdraw.sch")
(include "graphic.sch")
(include "xternal.sch")

;;; Text objects are created by the following procedure where x and y
;;; specify the minimum x,y coordinate of a rectangle containing the text.
;;; 
;;; If width, height, and positioning information are supplied, then that
;;; information describes the size of the rectangle and how the text is
;;; to be placed in it.  If the drawing is scaled such that the text won't
;;; fit in the box, then the text is not displayed.

(define (TEXT x y bbox words color stipple font)
    (let* ((width (if bbox (car bbox) #f))
	   (height (if bbox (cadr bbox) #f))
	   (center-x (if bbox (caddr bbox) #f))
	   (center-y (if bbox (cadddr bbox) #f))
	   (dxfs (display-font->xfontstruct *display* font))
	   (extent (xtextextents dxfs words (string-length words)))
	   (dim (cadddr extent))
	   (lbearing (xcharstruct-lbearing dim))
	   (rbearing (xcharstruct-rbearing dim))
	   (text-width (xcharstruct-width dim))
	   (ascent (xfontstruct-max_bounds-ascent dxfs))
	   (descent (xfontstruct-max_bounds-descent dxfs))
	   (text-height (+ ascent descent)))
	  
	  (define (BB-X)
		  (+ (min (user->x x)
			  (user->x (+ x (if bbox
					    width
					    (width->user text-width)))))
		     (case center-x
			   ((center) (quotient (- (user->width width)
						  text-width)
					 2))
			   ((right) (- (user->width width) text-width))
			   (else 0))))
	  
	  (define (BB-Y)
		  (+ (min (user->y y)
			  (user->y (+ y (if bbox
					    height
					    (height->user text-height)))))
		     (case center-y
			   ((center) (quotient (- (user->height height)
						  text-height)
					 2))
			   ((down) (- (user->height height) text-height))
			   (else 0))))
	  
	  (define (INSIDE-BBOX)
		  (or (not bbox)
		      (and (<= (width->user text-width) width)
			   (<= (height->user text-height) height))))
	  
	  (make-graphic
	      #f
	      (lambda ()
		      (let ((x (bb-x))
			    (y (bb-y)))
			   (list (- x 1) y
				 (+ x text-width) (+ y text-height))))
	      (if (eq? color 'clear)
		  draw-clear
		  (lambda ()
			  (if (inside-bbox)
			      (xdrawstring *dpy* *xwindow*
				  (cv-gc #f color stipple #f font #f)
				  (- (bb-x) lbearing) (+ (bb-y) ascent)
				  words (string-length words)))))
	      (if (eq? color 'clear)
		  draw-clear
		  (lambda ()
			  (when (inside-bbox)
				(pscolor color)
				(psfont font)
				(pscommand "xwindowmatrix" "setmatrix"
				    (- (bb-x) lbearing) (+ (bb-y) ascent)
				    "moveto" (psstring words) "show"
				    "viewmatrix" "setmatrix"))))
	      (lambda (minx miny maxx maxy) #t))))

(define (CENTER-X? x) (memq x '(left center right)))

(define (CENTER-Y? x) (memq x '(up center down)))

(define-ezd-command
    `(text ,number? ,number? (optional ,number? ,number? ,center-x? ,center-y?)
	 ,string? (optional ,color?) (optional ,stipple?) (optional ,string?))
    "(text x y [width height center-x center-y] [\"<text string>\" [<color>] [<stipple>] [\"fontname\"])"
    text)

;;; Utility function to compute the height and width in pixels of a text string
;;; for a given font (or #f for the default font).  Note that the height is
;;; always specified as the maximum height for the font.

(define (TEXT->HEIGHT-WIDTH words font)
    (let* ((dxfs (display-font->xfontstruct *display* font))
	   (extent (xtextextents dxfs words (string-length words)))
	   (dim (cadddr extent))
	   (text-width (xcharstruct-width dim))
	   (ascent (xfontstruct-max_bounds-ascent dxfs))
	   (descent (xfontstruct-max_bounds-descent dxfs))
	   (text-height (+ ascent descent)))
	  (list text-height text-width)))
