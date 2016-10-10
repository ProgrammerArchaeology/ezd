;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module produce a Postscript file representing the
;;; contents of a window.

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

(module psdraw)

(include "struct.sch")
(include "commands.sch")
(include "ezd.sch")
(include "ginfo.sch")
(include "display.sch")
(include "window.sch")
(include "view.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "xternal.sch")

;;; The views of drawings in a window are output in Postscript to a file by 
;;; the following procedure.

(define (PSFILE window-name file)
    (let ((window (name->window window-name))
	  (save-current-drawing *current-drawing*))
	 
	 (define (DEFINE-AND-CLIP-WINDOWBORDERPATH x y width height)
		 (pscommand "/windowborderpath" "{" "newpath"
		     (+ x width) (+ y height) "moveto" (- width) 0 "rlineto"
		     0 (- height) "rlineto" width 0 "rlineto" "closepath"
		     "}" "def")
		 (pscommand "windowborderpath" "clip"))
	 
	 (define (DRAW-VIEW view)
		 (let ((x (or (view-clip-minx view) 0))
		       (y (or (view-clip-miny view) 0))
		       (width (if (view-clip-minx view)
				  (- (view-clip-maxx view)
				     (view-clip-minx view))
				  (window-width window)))
		       (height (if (view-clip-minx view)
				   (- (view-clip-maxy view)
				      (view-clip-miny view))
				   (window-height window))))
		      (set! *psfont* #f)
		      (pscommand "gsave" "xwindowmatrix" "setmatrix")
		      (define-and-clip-windowborderpath x y width height)
		      (set-view view '())
		      (psscale view)
		      (bbgraphics-intersect (view-bb-head view) x y (+ x width)
			  (+ y height) psdraw-bbgraphic)
		      (pscommand "grestore")))
	 
	 (let loop ()
	      (unless (or (window-exposed window)
			  (null? (window-views window)))
		      (ezd '(pause 1000))
		      (loop)))
	 (redraw-all-windows)
	 (with-output-to-file file
	     (lambda ()
		     (psinit (window-width window) (window-height window))
		     (define-and-clip-windowborderpath 0 0
			 (window-width window) (window-height window))
		     (unless (eq? (window-background-name window) 'white)
			     (pscolor (window-background-name window))
			     (pscommand "windowborderpath" "fill"))
		     (for-each draw-view (window-views window))	
		     (psdone)))
	 (set! *current-drawing* save-current-drawing)))

;;; The ezd command POSTSCRIPT writes a Postscript representation of the
;;; contents of a window to a file.

(define-ezd-command
    `(postscript ,symbol? ,string?)
    "(postscript window-name file-name)"
    psfile)

;;; The header to the Postscript file is written by the following function.

(define *PSCOLOR* #f)

(define *PSFONT* #f)

(define *PSFONTS* '())

;;; Information about the PostScript imaging area is entered here.  The two
;;; values are the X and Y margin sizes in points.

(define *PSXOFFSET* 27)			;;; 3/8 inch
(define *PSYOFFSET* 99)			;;; 1-3/8 inch

;;; Widths and heights for available sizes of paper:

(define *PSPAPER* (list "8.5 x 11 inch" "11 x 17 inch"))
(define *PSWIDTH* (list (* 8.5 72) (* 11 72)))
(define *PSHEIGHT* (list (* 11 72) (* 17 72)))

(define (PSINIT width height)
    
    (define ROTATE #f)
    (define PSPAPER (car *pspaper*))
    (define PSWIDTH (car *pswidth*))
    (define PSHEIGHT (car *psheight*))
    (define PAPER-SELECTED #f)
    (define WIDTH-PTS (/ width *pixels/point*))
    (define HEIGHT-PTS (/ height *pixels/point*))
    
    (for-each
	(lambda (xpaper xwidth xheight)
		(cond (paper-selected)
		      ((and (<= 0 width-pts (- xwidth (* 2 *psxoffset*)))
			    (<= 0 height-pts (- xheight (* 2 *psyoffset*))))
		       (set! paper-selected #t)
		       (set! pswidth xwidth)
		       (set! psheight xheight)
		       (set! pspaper xpaper))
		      ((and (<= 0 height-pts (- xwidth (* 2 *psxoffset*)))
			    (<= 0 width-pts (- xheight (* 2 *psyoffset*))))
		       (set! rotate #t)
		       (set! paper-selected #t)
		       (set! pswidth xwidth)
		       (set! psheight xheight)
		       (set! pspaper xpaper))))
	*pspaper* *pswidth* *psheight*)
    (set! *pscolor* #f)
    (set! *psfont* #f)
    (set! *psfonts* '())
    (pscommand "%!PS-Adobe-")
    (pscommand "%%Creator: ezd - easy drawing for X11 displays."
	"*EZD-VERSION*" *ezd-version*)
    (pscommand "%%CreationDate:"
	(if (procedure? (top-level-value 'time-of-day))
	    ((top-level-value 'time-of-day))
	    (list->string
		(let loop ((p ((top-level-value 'open-input-process) "date")))
		     (let ((c (read-char p)))
			  (if (eq? c #\newline) '() (cons c (loop p))))))))
    
    (pscommand "%%BoundingBox:"
	(inexact->exact
	    (quotient (- pswidth (if rotate height-pts width-pts)) 2))
	(inexact->exact
	    (quotient (- psheight (if rotate width-pts height-pts)) 2))
	(inexact->exact
	    (- pswidth
	       (quotient (- pswidth (if rotate height-pts width-pts)) 2)))
	(inexact->exact
	    (- psheight
	       (quotient (- psheight (if rotate width-pts height-pts)) 2))))
    (pscommand "%%DocumentFonts: (atend)")
    (pscommand "%%EndComments")
    (pscommand "% [Rotate] and center on" pspaper "paper")
    (if rotate
	(pscommand 90 "rotate" 0 (- pswidth) "translate"
	    (inexact->exact (quotient (- psheight width-pts) 2))
	    (inexact->exact (quotient (- pswidth height-pts) 2))
	    "translate" "% INCLUDE-DELETE")
	(pscommand 
	    (inexact->exact (quotient (- pswidth width-pts) 2))
	    (inexact->exact (quotient (- psheight height-pts) 2))
	    "translate" "% INCLUDE-DELETE"))
    (pscommand
	"% Scale to reflect ?? dpi screen vs. 72 points/inch printer and save")
    (pscommand (/ *pixels/point*) (/ *pixels/point*) "scale" 1 "setlinewidth"
	0 height "translate" 1 -1 "scale"
	"/xwindowmatrix" "matrix" "currentmatrix" "def")
    (pscommand "%%EndProlog")
    (pscommand "%%Page: 0 1"))

;;; Scaling for each view is done by the following function.

(define (PSSCALE view)
    (pscommand "% Scale to reflect user distance and origin")
    (pscommand "xwindowmatrix" "setmatrix"
	(view-originx view) (view-originy view) "translate"
	(view-scalex view) (view-scaley view) "scale")
    (pscommand "/viewmatrix" "matrix" "currentmatrix" "def"))

;;; Complete the Postscript file.

(define (PSDONE)
    (pscommand "showpage" "% INCLUDE-DELETE")
    (pscommand "%%Trailer")
    (pscommand "%%DocumentFonts:"
	(let loop ((fonts *psfonts*))
	     (if fonts
		 (let ((rest (loop (cdr fonts)))
		       (x-ps-pts (assoc (car fonts) *translate-fonts*)))
		      (if x-ps-pts
			  (let ((font (cadr x-ps-pts)))
			       (if (member font rest) rest (cons font rest)))))
		 '("")))))

;;; Take a size in screen pixels and convert it to scaled units.

(define (PSXPIXEL x) (* x (/ *width* (user->width *width*))))

(define (PSYPIXEL x) (* x (/ *height* (user->height *height*))))

;;; Print a list of Postscript commands.

(define (PSCOMMAND . cl)
    (let loop ((cl cl))
	 (cond ((null? cl) #t)
	       ((or (pair? (car cl)) (null? (car cl)))
		(loop (append (car cl) (cdr cl))))
	       (else (display (car cl))
		     (cond ((null? (cdr cl)) (newline))
			   (else (display " ") (loop (cdr cl))))))))

;;; Set the current Postscript color.

(define (PSCOLOR color)
    (let ((color (or color *foreground-name*)))
	 (when (not (eq? color *pscolor*))
	       (set! *pscolor* color)
	       (let* ((rgb (getprop color 'isa-color))
		      (r (/ (car rgb) 255))
		      (g (/ (cadr rgb) 255))
		      (b (/ (caddr rgb) 255)))
		     (if (= r g b)
			 (pscommand r "setgray")
			 (pscommand r g b "setrgbcolor"))))))

;;; Stroke the currently set path in a given width and optional dash pattern.

(define (PSSTROKE width dash)
    (let ((width (max 1 width)))	     
	 (pscommand "gsave" "xwindowmatrix" "setmatrix"
	     (if (not (= width 1))
		 `(,width "setlinewidth")
		 '())
	     (if dash '("[ 3 ]" 0 "setdash") '())
	     "stroke" "grestore")))

;;; Turn a string into a Postscript string.

(define (PSSTRING text)
    (list->string
	(append '(#\()
		(let loop ((cl (string->list text)))
		     (if (null? cl)
			 '()
			 (case (car cl)
			       ((#\( #\) #\\ )
				(cons #\\ (cons (car cl) (loop (cdr cl)))))
			       (else (cons (car cl) (loop (cdr cl)))))))
		'(#\)))))

;;; Set the current PostScript font.  Transformation matrix corrects for
;;; user coordinate system and screen's dpi vs printers 72 pts/inch.

(define (PSFONT font)
    (let ((font (or font "fixed")))
	 (when (not (eq? font *psfont*))
	       (unless (member font *psfonts*)
		       (pscommand "xwindowmatrix" "setmatrix"
			   (string-append "/FONT-" font)
			   (let ((x (assoc font *translate-fonts*)))
				(if x
				    (list (string-append "/" (cadr x))
					  "findfont"
					  "["  (* *pixels/point* (caddr x)) 0 0
					  (* (- *pixels/point*) (caddr x)) 0 0
					  "]" "makefont")
				    (begin (format stderr-port
						   "Can't translate font: ~a~%"
						   font)
					   "FONT-fixed")))
			   "def"
			   "viewmatrix" "setmatrix")
		       (set! *psfonts* (cons font *psfonts*)))
	       (pscommand (string-append "FONT-" font) "setfont"))
	 (set! *psfont* font)))
