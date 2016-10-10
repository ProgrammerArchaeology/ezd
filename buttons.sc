;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module provide some basic interactive elements
;;; to provide event logging, keyboard input, clicks on objects, and popup
;;; menus.

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

(module buttons)

(include "commands.sch")
(include "ginfo.sch")
(include "interact.sch")
(include "events.sch")
(include "match.sch")
(include "ezd.sch")

;;; The CHECK-BUTTON command makes a check button in the current drawing.  The
;;; button is a circle drawn in the foreground color (default is black) that
;;; is filled with the background color (default is white) when the value is
;;; false, or the foreground color when the value is true.  The button text is
;;; written to the right of the button in the foreground color and the
;;; indicated font (default is the default X font).
;;;
;;; The button's value is complemented by clicking the mouse within either the
;;; circle or the button text.  The button action is taken when the button
;;; comes up.
;;;
;;; If the button is a member of a radio button set, then clicking on it when
;;; it is set will have no effect.  Clicking an unset button will set it and
;;; clear the set value.
;;;
;;; Button information can be accessed via the following attributes.
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	ACTION
;;;	TEXT
;;;	FONT
;;;	FOREGROUND
;;;	BACKGROUND
;;;	VALUE
;;;	RADIO-BUTTON-SET
;;;
;;;	ATTRIBUTES
;;;	DELETE-OBJECT

(define (MAKE-CHECK-BUTTON button-name x y width height text value action
	    opt-foreground opt-background font)
    
    (define FOREGROUND (or opt-foreground 'black))
    (define BACKGROUND (or opt-background 'white))
    (define DY 2)
    (define RADIO-BUTTON-SET '())
    (define DRAWING-NAME (current-drawing-name))
    
    (define READY #f)
    
    (define (EXIT)
	    (when ready
		  (set! value (not value))
		  (set! ready #f)
		  (draw-button)))
    
    (define (BUTTON-DOWN)
	    (unless (and value (pair? radio-button-set))
		    (set! ready #t)
		    (set! value (not value))
		    (draw-button)))
    
    (define (BUTTON-UP) 
	    (if ready
		(let ((drawing *user-event-drawing*))
		     (set! ready #f)
		     (set! *user-event-misc* (list value))
		     (ezd '(draw-now))
		     (if (procedure? action) (action) (eval action))
		     (for-each
			 (lambda (button)
				 (if (not (eq? button button-name))
				     (set-attributes drawing button
					 '(value #f))))
			 radio-button-set))
		(draw-button)))
    
    (define (DRAW-BUTTON)
	    (ezd '(save-drawing)
		 `(set-drawing ,drawing-name)
	         `(object ,button-name
			  (fill-arc ,x ,y ,(- height dy dy)
			      ,(- height dy dy) 0 360
			      ,(if value foreground background))
			  (arc ,x ,y ,(- height dy dy)
			       ,(- height dy dy) 0 360 ,foreground)
			  (text ,(+ x height dy dy) ,y ,width ,height left
				center ,text ,foreground
				,@(if font (list font) '())))
		 '(restore-drawing)))
    
    (define (GETATTRIBUTES)
	    (map (lambda (a)
			 (case a
			       ((X) x)
			       ((Y) y)
			       ((WIDTH) width)
			       ((HEIGHT) height)
			       ((TEXT) text)
			       ((ACTION) action)
			       ((FONT) font)
			       ((FOREGROUND) foreground)
			       ((BACKGROUND) background)
			       ((VALUE) value)
			       ((RADIO-BUTTON-SET) radio-button-set)
			       ((ATTRIBUTES) '(x y width height text action
						 font foreground background
						 value radio-button-set
						 attributes delete-object))
			       (else (ezd-error 'check-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    (define (OBJECT-LIST? x)
	    (or (null? x) (and (symbol? (car x)) (object-list? (cdr x)))))
    
    (define (SETATTRIBUTES)
	    (let ((delete #f)
		  (redraw #f)
		  (drawing *user-event-drawing*))
		 
		 (define (SET-RADIO-BUTTON-SET new-buttons)
			 (for-each
			     (lambda (button)
				     (if (and (not (memq button new-buttons))
					      (not (eq? button button-name)))
					 (set-attributes drawing button
					     '(RADIO-BUTTON-SET-VALUE))))
			     radio-button-set)
			 (for-each
			     (lambda (button)
				     (if (not (eq? button button-name))
					 (set-attributes drawing button
					     `(RADIO-BUTTON-SET-VALUE
						  ,@new-buttons))))
			     new-buttons)
			 (set! radio-button-set new-buttons))
		 
		 (for-each
		     (lambda (a)
			     (cond ((match? (X number?) a)
				    (set! redraw #t)
				    (set! x (cadr a)))
				   ((match? (Y number?) a)
				    (set! redraw #t)
				    (set! y (cadr a)))
				   ((match? (WIDTH positive-number?) a)
				    (set! redraw #t)
				    (set! width (cadr a)))
				   ((match? (HEIGHT positive-number?) a)
				    (set! redraw #t)
				    (set! height (cadr a)))
				   ((match? (TEXT string?) a)
				    (set! redraw #t)
				    (set! text (cadr a)))
				   ((match? (ACTION any?) a)
				    (set! action (cadr a)))
				   ((match? (FONT string?) a)
				    (set! redraw #t)
				    (set! font (cadr a)))
				   ((match? (FOREGROUND color?) a)
				    (set! redraw #t)
				    (set! foreground (cadr a)))
				   ((match? (BACKGROUND color?) a)
				    (set! redraw #t)
				    (set! background (cadr a)))
				   ((match? (VALUE boolean?) a)
				    (if (not (equal? value (cadr a)))
					(set! redraw #t))
				    (set! value (cadr a)))
				   ((and (eq? (car a) 'RADIO-BUTTON-SET)
					 (object-list? (cdr a)))
				    (set-radio-button-set (cdr a)))
				   ((and (eq? (car a) 'RADIO-BUTTON-SET-VALUE))
				    (set! radio-button-set (cdr a)))
				   ((equal? '(DELETE-OBJECT) a)
				    (set! delete #t))
				   (else (ezd-error 'check-button
					     "Illegal attribute: ~s" a))))
		     *user-event-misc*)
		 (if delete
		     (begin (set-radio-button-set
				(remq button-name radio-button-set))
			    (ezd `(object ,button-name)
				 `(when ,button-name * #f)))
		     (if redraw (draw-button)))))
    
    (draw-button)
    (ezd `(when ,button-name get-attributes ,getattributes)
	 `(when ,button-name set-attributes ,setattributes)
	 `(when ,button-name exit ,exit)
	 `(when ,button-name button1down ,button-down)
	 `(when ,button-name button1up ,button-up)))

(define-ezd-command
    `(check-button ,symbol? ,number? ,number? ,positive-number?
	 ,positive-number? ,string? ,boolean?
	 ,any? (optional ,color?) (optional ,color?) (optional ,string?))
    "(check-button name x y width height text value action [foreground [background]] [\"font\"])"
    make-check-button)

;;; The PUSH-BUTTON command makes a simple button in the current drawing.  The
;;; button is drawn as a filled rectangle in the background color (default is
;;; white) with a foreground colored (default is black) border.  The button
;;; text is written in the center of the button in the foreground color and
;;; font (default is X's).  If several button text's are provided, then each
;;; time the button action is taken, the "next" button text is displayed.
;;;
;;; When the mouse enters a button with an action not equal to #f, the
;;; rectangle border is thickened.  When mouse button 1 is pressed, the button
;;; colors are reversed.  When button 1 is released, the button action is taken
;;; and then the button is drawn normally.  When the button action is taken,
;;; the button event type is BUTTON1UP and the miscellaneous information is a
;;; list containing the button text when the button was pressed and the button
;;; text that will next be displayed.  Note that in order for the button
;;; action to be taken, the mouse must remain within the button while the
;;; mouse button is pressed.
;;;
;;; Button information can be accessed via the following attributes.
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	ACTION
;;;	TEXT
;;;	FONT
;;;	FOREGROUND
;;;	BACKGROUND
;;;
;;;	ATTRIBUTES
;;;	DELETE-OBJECT

(define (MAKE-PUSH-BUTTON button-name x y width height text-list action
	    opt-foreground opt-background font)
    
    (define FOREGROUND (or opt-foreground 'black))
    (define BACKGROUND (or opt-background 'white))
    (define DRAWING-NAME (current-drawing-name))
    
    (define BOLD #f)
    (define INVERT #f)
    
    (define (ENTER) (if (not *mouse-button1*) (draw-button #t #f)))
    
    (define (EXIT) (draw-button #f #f))
    
    (define (BUTTON-DOWN) (draw-button #t #t))
    
    (define (BUTTON-UP)
	    (when invert
		  (set! *user-event-misc*
			(list (car text-list)
			      (if (pair? (cdr text-list))
				  (cadr text-list)
				  (car text-list))))
		  (ezd '(draw-now))
		  (if (procedure? action) (action) (eval action))
		  (set! text-list
			(append (cdr text-list) (list (car text-list)))))
	    (draw-button #t #f))
    
    (define (DRAW-BUTTON b i)
	    (set! bold b)
	    (set! invert i)
	    (ezd '(save-drawing)
		 `(set-drawing ,drawing-name)
		 `(object ,button-name
			  (fill-rectangle ,x ,y ,width ,height
			      ,(if invert foreground background))
			  (rectangle ,x ,y ,width ,height ,(if bold 3 0)
			      ,foreground)
			  (text ,x ,y ,width ,height center center
				,(car text-list)
				,(if invert background foreground)
				,@(if font (list font) '())))
		 '(restore-drawing)))
    
    (define (STRING-LIST? x)
	    (if (null? x)
		#f
		(let loop ((x x))
		     (if (pair? x)
			 (loop (cdr x))
			 (null? x)))))
    
    (define (GET-ATTRIBUTES)
	    (map (lambda (a)
			 (case a
			       ((X) x)
			       ((Y) y)
			       ((WIDTH) width)
			       ((HEIGHT) height)
			       ((TEXT) text-list)
			       ((ACTION) action)
			       ((FONT) font)
			       ((FOREGROUND) foreground)
			       ((BACKGROUND) background)
			       ((ATTRIBUTES) '(x y width height text action
						 font foreground background
						 attributes delete-object))
			       (else (ezd-error 'push-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    (define (SET-ATTRIBUTES)
	    
	    (define (TEXT-LIST? x)
		    (or (null? x)
			(and (string? (car x)) (text-list? (cdr x)))))
	    
	    (define DELETE #f)
	    
	    (for-each
		(lambda (a)
			(cond ((match? (X number?) a)
			       (set! x (cadr a)))
			      ((match? (Y number?) a)
			       (set! y (cadr a)))
			      ((match? (WIDTH positive-number?) a)
			       (set! width (cadr a)))
			      ((match? (HEIGHT positive-number?) a)
			       (set! height (cadr a)))
			      ((and (eq? (car a) 'TEXT) (text-list? (cdr a))
				    (> (length a) 1))
			       (set! text-list (cdr a)))
			      ((match? (ACTION any?) a)
			       (set! action (cadr a)))
			      ((match? (FONT string?) a)
			       (set! font (cadr a)))
			      ((match? (FOREGROUND color?) a)
			       (set! foreground (cadr a)))
			      ((match? (BACKGROUND color?) a)
			       (set! background (cadr a)))
			      ((equal? '(DELETE-OBJECT) a)
			       (set! delete #t))
			      (else (ezd-error 'push-button
					"Illegal attribute: ~s" a))))
		*user-event-misc*)
	    (if delete
		(ezd `(object ,button-name) `(when ,button-name * #f))
		(make-push-button button-name x y width height text-list action
		    foreground background font)))
    
    (draw-button #f #f)
    (ezd `(when ,button-name get-attributes ,get-attributes)
	 `(when ,button-name set-attributes ,set-attributes))
    (if action
	(ezd `(when ,button-name enter ,enter)
	     `(when ,button-name exit ,exit)
	     `(when ,button-name button1down ,button-down)
	     `(when ,button-name button1up ,button-up))
	(ezd `(when ,button-name enter #f)
	     `(when ,button-name exit #f)
	     `(when ,button-name button1down #f)
	     `(when ,button-name button1up #f))))

(define-ezd-command
    `(push-button ,symbol? ,number? ,number? ,positive-number?
	 ,positive-number? (repeat ,string?) ,any? (optional, color?)
	 (optional ,color?) (optional ,string?))
    "(push-button name x y width height text ... action [foreground [background]] [\"font\"])"
    make-push-button)
