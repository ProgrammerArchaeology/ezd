;;; ezd - easy drawing for X11 displays.
;;;
;;; Variable colors are adjusted using the tool implemented in this module.

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

(module editcolor)

(include "struct.sch")
(include "events.sch")
(include "commands.sch")
(include "interact.sch")
(include "ezd.sch")
(include "ginfo.sch")
(include "xternal.sch")

(define EDIT-COLOR-DELTA 0)

;;; A variable color is "edited" by creating a control panel for it.  Any
;;; number of control panels may exist at any one time.

(define (MAKE-COLOR-CONTROL-PANEL color)
    
    ;;; Control panel configuration information.
    
    (define WS 10)		; whitespace
    (define SLIDE-W 256)	; slider width
    (define SLIDE-H 20)		; slider height
    (define HEX-W 50)
    
    (define COLOR-IND 20)	; indicator parameters
    (define COLOR-MIN 0)
    (define COLOR-MAX 255)
    (define COLOR-VALUE 0)
    (define COLOR-JUMP 1)
    
    (define (ROW-Y x) (+ ws (* (+ slide-h ws) x)))
    
    ;;; Information related to the variable color being edited.
    
    (define RED #f)
    (define GREEN #f)
    (define BLUE #f)
    (define GRAY #f)
    (define HUE #f)
    (define SATURATION #f)
    (define VALUE #f)
    
    (define PANEL
	    (string->symbol (string-append "EVC-" (symbol->string color))))
    
    ;;; Draw the hex value of a slider.
    
    (define (DRAW-HEX name row value)
	    (ezd `(object ,name
			  (text ,(+ ws slide-w ws) ,(row-y row) ,hex-w
				,slide-h right center
				,(number->string value 16)))))
    
    ;;; Set indicators on a change of RGB value.
    
    (define (SET-FROM-RGB slider r g b)
	    (unless (eq? red r)
		    (set! red r)
		    (if (not (eq? slider 'red))
			(set-attributes panel 'red `(value ,r)))
		    (draw-hex 'red-hex 0 r))
	    (unless (eq? green g)
		    (set! green g)
		    (if (not (eq? slider 'green))
			(set-attributes panel 'green `(value ,g)))
		    (draw-hex 'green-hex 1 g))
	    (unless (eq? blue b)
		    (set! blue b)
		    (if (not (eq? slider 'blue))
			(set-attributes panel 'blue `(value ,b)))
		    (draw-hex 'blue-hex 2 b))
	    (when (= r g b)
		  (set! gray r)
		  (if (not (eq? slider 'gray))
		      (set-attributes panel 'gray `(value ,gray))))
	    (let ((name-delta (rgb->color r g b)))
		 (ezd `(set-variable-color ,color
			   ,(+ (* 256 (+ (* 256 r) g)) b))
		      `(set-attributes ,panel name-key (text ,name-delta))))
	    (unless (memq slider '(hue saturation value))
		    (let* ((r (/ r 255))
			   (g (/ g 255))
			   (b (/ b 255))
			   (maxrgb (max r g b))
			   (minrgb (min r g b))
			   (delta (- maxrgb minrgb)))
			  (set! value maxrgb)
			  (if (positive? maxrgb)
			      (set! saturation (/ delta maxrgb))
			      (set! saturation 0))
			  (if (zero? saturation)
			      (set! hue 0)
			      (let ((rc (/ (- maxrgb r) delta))
				    (gc (/ (- maxrgb g) delta))
				    (bc (/ (- maxrgb b) delta)))
				   (cond ((= r maxrgb)
					  (set! hue (- bc gc)))
					 ((= g maxrgb)
					  (set! hue (+ 2 (- rc bc))))
					 (else (set! hue (+ 4 (- gc rc)))))
				   (set! hue (* hue 60))
				   (if (negative? hue)
				       (set! hue (+ hue 360)))))
			  (set-attributes panel 'hue `(value ,hue))
			  (set-attributes panel 'saturation
			      `(value ,saturation))
			  (set-attributes panel 'value `(value ,value)))))
    
    ;;; Set indicators on a keyboard input.
    
    (define (SET-FROM-KEYBOARD)
	    (let* ((value (car *user-event-misc*))
		   (number (string->number value))
		   (name (and (not number) (color->rgb value))))
		  (cond ((and number (exact? number) (>= 0 number))
			 (set-from-rgb #f
			     (bit-and 255 (bit-rsh number 16))
			     (bit-and 255 (bit-rsh number 8))
			     (bit-and 255 number)))
			(name (apply set-from-rgb #f name))
			(else (ezd `(bell))))))
    
    ;;; Set indicators on a change of HSV values.
    
    (define (SET-FROM-HSV slider h s v)
	    (let ((scaled-v (inexact->exact (* v 255))))
		 (set! hue h)
		 (set! saturation s)
		 (set! value v)
		 (if (zero? s)
		     (set-from-rgb slider scaled-v scaled-v scaled-v)
		     (let* ((h (/ (if (= h 360) 0 h) 60))
			    (i (inexact->exact (floor h)))
			    (f (- h i))
			    (p (inexact->exact (* 255 v (- 1 s))))
			    (q (inexact->exact (* 255 v (- 1 (* s f)))))
			    (t (inexact->exact (* 255 v (- 1 (* s (- 1 f)))))))
			   (case i
				 ((0) (set-from-rgb slider scaled-v t p))
				 ((1) (set-from-rgb slider q scaled-v p))
				 ((2) (set-from-rgb slider p scaled-v t))
				 ((3) (set-from-rgb slider p q scaled-v))
				 ((4) (set-from-rgb slider t p scaled-v))
				 ((5) (set-from-rgb slider scaled-v p q)))))))
    
    ;;; Load rgb.txt and draw the control panel. 
    
    (rgb-values)
    (ezd '(save-drawing)
	 `(set-drawing ,panel)
	 
	 '(object color-name)
	 
	 `(slider red ,ws ,(row-y 0) ,slide-w ,slide-h ,color-ind
		  ,color-min ,color-max ,color-value ,color-jump
		  ,(lambda () (set-from-rgb 'red (car *user-event-misc*)
				  green blue))
		  red)
	 
	 `(slider green ,ws ,(row-y 1) ,slide-w ,slide-h ,color-ind
		  ,color-min ,color-max ,color-value ,color-jump
		  ,(lambda () (set-from-rgb 'green red (car *user-event-misc*)
				  blue))
		  green)
	 
	 `(slider blue ,ws ,(row-y 2) ,slide-w ,slide-h ,color-ind
		  ,color-min ,color-max ,color-value ,color-jump
		  ,(lambda () (set-from-rgb 'blue red green
				  (car *user-event-misc*)))
		  blue)
	 
	 `(slider gray ,ws ,(row-y 3) ,slide-w ,slide-h ,color-ind
		  ,color-min ,color-max ,color-value ,color-jump
		  ,(lambda () (set-from-rgb 'gray (car *user-event-misc*)
				  (car *user-event-misc*)
				  (car *user-event-misc*)))
		  gray)
	 
	 `(slider hue ,ws ,(row-y 4) ,slide-w ,slide-h 28 0 360 0 3
		  ,(lambda () (set-from-hsv 'hue (car *user-event-misc*)
				  saturation value)))
	 
	 `(text ,(+ ws slide-w ws) ,(row-y 4) ,hex-w ,slide-h center center
		"Hue")
	 
	 `(slider saturation ,ws ,(row-y 5) ,slide-w ,slide-h .08 0 1 0 .01
		  ,(lambda () (set-from-hsv 'saturation hue
				  (car *user-event-misc*) value)))
	 
	 `(text ,(+ ws slide-w ws) ,(row-y 5) ,hex-w ,slide-h center center
		"Sat.")
	 
	 `(slider value ,ws ,(row-y 6) ,slide-w ,slide-h .08 0 1 0 .01
		  ,(lambda () (set-from-hsv 'value hue saturation
				  (car *user-event-misc*))))
	 
	 `(text ,(+ ws slide-w ws) ,(row-y 6) ,hex-w ,slide-h center center
		"Value")
	 
	 `(string-input name-key ,ws ,(row-y 7) ,slide-w ,slide-h
	      "" ,set-from-keyboard)
	 
	 `(push-button ok ,(+ ws slide-w ws) ,(row-y 7) ,hex-w ,slide-h
	      "OK" ,(lambda () (ezd '(save-drawing)
				    `(set-drawing ,panel)
				    '(clear)
				    `(delete-window ,panel)
				    '(restore-drawing))))
	 
	 `(window ,panel ,(+ 100 edit-color-delta) ,(+ 100 edit-color-delta)
		  ,(+ ws slide-w ws hex-w ws) ,(row-y 8) fixed-size
		  ,(string-append "Edit variable color "
		       (symbol->string color)))
	 `(overlay ,panel ,panel))
    (apply set-from-rgb #f (getprop color 'isa-color))
    (ezd '(restore-drawing))
    (set! edit-color-delta (remainder (+ edit-color-delta 20) 100)))
 
;;; R-G-B coordinates are converted to a color name by the following procedure.
;;; It returns a string that is the name of the closest color followed by
;;; a string of the form "+xx +xx +xx" that is the offsets to add to each
;;; pair of hex digits defining the color to get the named color.  The deltas
;;; are not supplied when the color is exact.

(define (RGB->COLOR red green blue)
    
    (let loop ((l (rgb-values)) (delta (* 3 (* 256 256 256)))
	       (delta-name "black") (deltas (list 256 256 256)))
	 (if (pair? l)
	     (let* ((rgb (caar l))
		    (name (cadar l))
		    (dr (- (car rgb) red))
		    (dg (- (cadr rgb) green))
		    (db (- (caddr rgb) blue))
		    (d (+ (abs (* dr dr dr)) (abs (* dg dg dg))
			  (abs (* db db db)))))
		   (cond ((zero? d)
			  (loop '() d name (list dr dg db)))
			 ((< d delta)
			  (loop (cdr l) d name (list dr dg db)))
			 (else (loop (cdr l) delta delta-name deltas))))
	     (string-append delta-name
		 (if (equal? deltas '(0 0 0))
		     ""
		     (string-append "   " (number->string (car deltas) 16) " "
			 (number->string (cadr deltas) 16) " "
			 (number->string (caddr deltas) 16)))))))

;;; A color name is converted to it's R-G-B coordinates by the following
;;; procedure.  Either a list of rgb values is returned, or #f indicating that
;;; the name was not found.

(define (COLOR->RGB name)
    (let loop ((l (rgb-values)))
	 (if (pair? l)
	     (if (equal? (cadar l) name) (caar l) (loop (cdr l)))
	     #f)))

;;; The following function returns a list of entries of the form:
;;;
;;;	((r-value g-value b-value) color-name)
;;;
;;; from the COLOR-DEFINITION-FILE.

(define COLOR-DEFINITION-FILE "/usr/lib/X11/rgb.txt")

(define RGB-VALUES
    (let ((values '()))
	 
	 (define NAME-BUFFER #f)
	 
	 (define (READ-NAME fh)
		 (let loop () (when (char-whitespace? (peek-char fh))
				    (read-char fh)
				    (loop)))
		 (let loop ((i 0))
		      (if (eq? (peek-char fh) #\newline)
			  (set! name-buffer (make-string i))
			  (let ((char (read-char fh)))
			       (loop (+ i 1))
			       (if name-buffer
				   (if (eq? char #\space)
				       (set! name-buffer #f)
				       (string-set! name-buffer i
					   (char-downcase char)))))))
		 name-buffer)
	 
	 (define (READ-RGB-FILE fh)
		 (let* ((r (read fh))
			(g (read fh))
			(b (read fh)))
		       (if (eof-object? r)
			   '()
			   (let ((name (read-name fh)))
				(if name
				    (cons (list (list r g b) name)
					  (read-rgb-file fh))
				    (read-rgb-file fh))))))
	 
	 (lambda ()
		 (if (null? values)
		     (let ((fh (open-input-file color-definition-file)))
			  (set! values (read-rgb-file fh))
			  (close-input-port fh)))
		 values)))

;;; A variable color is edited by the command EDIT-VARIABLE-COLOR.

(define-ezd-command
    `(edit-variable-color ,variable-color?)
    "(edit-variable-color variable-color)"
    make-color-control-panel)
