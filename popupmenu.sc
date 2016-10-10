;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module implement popup menus.

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

(module popupmenu)

(include "struct.sch")
(include "match.sch")
(include "ginfo.sch")
(include "commands.sch")
(include "display.sch")
(include "window.sch")
(include "drawing.sch")
;(include "view.sch")
;(include "graphic.sch")
(include "ezd.sch")
(include "events.sch")
(include "text.sch")
(include "xternal.sch")

;;; The procedure DEFINE-POPUP defines a procedure that implements a popup
;;; menu.  The user supplies a procedure name, a list of menu entries and
;;; actions and optionally a font.  Each menu entry/action is a list of either
;;; a string, a procedure, or an expression that evaluates to a string and an
;;; action procedure or expression to be evaluated.
;;;
;;; The menu is attached to some object by using a WHEN command to associate
;;; the evaluation of the popup procedure with a button press.  When the button
;;; is pressed, the menu will come up under the button.  When the button comes
;;; up, the action associated with the menu entry is executed.  Moving the
;;; mouse outside the menu without releasing the button will cause the menu to
;;; disappear.
;;;
;;; The popup menu object has the following attributes.  Address them in the
;;; drawing "popup-name", object POPUP.
;;;
;;;	ENTRIES
;;;	REPLACE-NAME
;;;	REPLACE-ACTION
;;;	FOREGROUND
;;;	BACKGROUND
;;;	FONT
;;;
;;;	DELETE-OBJECT
;;;	ATTRIBUTES

(define (DEFINE-POPUP popup-name name-action-list colors font)
    
    (define FOREGROUND (if colors (car colors) 'black))
    (define BACKGROUND (if colors (cadr colors) 'white))
    
    (define WINDOW #f)
    (define VISIBLE #f)
    
    (define MENU-X #f)
    (define MENU-Y #f)
    (define MENU-WIDTH 0)
    (define MENU-HEIGHT 0)
    (define MENU-LENGTH (length name-action-list))
    (define POPPED-WINDOW #f)
    (define POPPED-DRAWING #f)
    (define POPPED-OBJECT #f)
    (define POPPED-OBJECT-X #f)
    (define POPPED-OBJECT-Y #f)
    
    (define (ANY-BUTTON?) (or *mouse-button1* *mouse-button2* *mouse-button3*
			      *mouse-button4* *mouse-button5*))
    
    ;;; Compute the size of a menu entry text string.
    
    (define (COMPUTE-MENU-WIDTH-AND-HEIGHT text)
	    (let ((height-width (text->height-width (string-append "  " text
							"  ") font)))
		 (set! menu-height
		       (max menu-height (* 2 (car height-width))))
		 (set! menu-width
		       (max menu-width (cadr height-width)))))
    
    ;;; Service procedure to make the window invisible.
    
    (define (MAKE-INVISIBLE)
	    (when visible
		  (set! visible #f)
		  (ezd `(delete-view ,popup-name ,popup-name))))
    
    ;;; When the window is initially poped up, the following procedure is
    ;;; called to verify that the mouse is still in the window.
    
    (define (DROP-WHEN-NOT-THERE)
	    (if (and visible
		     (or (< *mouse-x* menu-x)
			 (> *mouse-x* (+ menu-x menu-width))
			 (< *mouse-y* menu-y)
			 (> *mouse-y* (* menu-length (+ menu-y menu-height)))
			 (not (any-button?))))
		(make-invisible)))
    
    ;;; When the button event occurs, the following procedure is called to
    ;;; display the popup menu in its own window.
    
    (define (POPITUP)
	    (set! menu-x (max 0 (- *mouse-x* (quotient menu-width 2))))
	    (set! menu-y (max 0 (- *mouse-y* (quotient menu-height 2))))
	    (set! popped-window *user-event-window*)
	    (set! popped-drawing *user-event-drawing*)
	    (set! popped-object *user-event-object*)
	    (set! popped-object-x *user-event-x*)
	    (set! popped-object-y *user-event-y*)
	    (if window
		(xmovewindow *dpy* (window-xwindow window) menu-x menu-y)
		(begin (ezd `(window ,popup-name ,menu-x ,menu-y ,menu-width
				     ,(* menu-length menu-height)))
		       (set! window (name->window popup-name))
		       (xchangewindowattributes *dpy* (window-xwindow window)
			   (bit-or cwsaveunder cwoverrideredirect)
			   (let ((x (make-xsetwindowattributes)))
				(xsetwindowattributes-save_under! x 1)
				(xsetwindowattributes-override_redirect! x 1)
				x))))
	    (set! visible #t)
	    (ezd `(overlay ,popup-name ,popup-name)))
    
    ;;; Procedures to draw a menu entry as either high or low lighted.
    
    (define (HIGHLIGHT object y text)
	    (draw-text object y text background foreground #f))
    
    (define (LOWLIGHT object y text)
	    (draw-text object y text foreground background #f))
    
    (define (DRAW-TEXT object y text forecolor backcolor stipple)
	    (ezd `(set-drawing ,popup-name)
		 `(object ,object (fill-rectangle 0 ,y ,menu-width
				      ,menu-height ,backcolor)
			  (text 0 ,y ,menu-width ,menu-height center center
				,(menu-text text) ,forecolor
				,@(if stipple (list stipple) '())
				,@(if font (list font) '())))))
    
    ;;; Compute the actual text string.
    
    (define (MENU-TEXT menu-name)
	    (cond ((string? menu-name) menu-name)
		  ((procedure? menu-name) (menu-name))
		  (else (eval menu-name))))
    
    ;;; The following procedure "configures" the drawing representing the
    ;;; popup menu.  It is called 
    
    (define  (CONFIGURE-POPUP)
	(if window (ezd `(delete-window ,popup-name)))
	(set! window #f)
	(set! visible #f)
	(set! menu-width 0)
	(set! menu-height 0)
	(set! menu-length (length name-action-list))
	;;; 1. Compute size of the menu.
	(for-each
	    (lambda (name-action) (compute-menu-width-and-height
				      (menu-text (car name-action))))
	    name-action-list)
	;;; 2. Draw the entries in the menu and arm their event handlers.
	(ezd '(save-drawing)
	     `(set-drawing ,popup-name)
	     '(object popup)
	     `(when popup get-attributes ,get-attributes)
	     `(when popup set-attributes ,set-attributes))
	(let loop ((y 0) (args name-action-list))
	     (if (pair? args)
		 (let* ((name-action (car args))
			(object (string->symbol (format "~s-~s" popup-name y)))
			(text (car name-action))
			(action (cadr name-action)))
		       
		       (define (ENTER-OBJ)
			       (if (any-button?)
				   (if action (highlight object y text))
				   (make-invisible)))
		       
		       (define (EXIT-OBJ)
			       (if action (lowlight object y text))
			       (unless (and *mouse-window*
					    (eq? (window-name *mouse-window*)
						 popup-name)
					    *mouse-object*)
				       (make-invisible)))
		       
		       (define (EXECUTE-OBJ)
			       (lowlight object y text)
			       (make-invisible)
			       (set! *user-event-type* 'popup)
			       (set! *user-event-window* popped-window)
			       (set! *user-event-drawing* popped-drawing)
			       (set! *user-event-object* popped-object)
			       (set! *user-event-x* popped-object-x)
			       (set! *user-event-y* popped-object-y)
			       (set! *user-event-misc*
				     (list popup-name (menu-text text)))
			       (ezd `(set-drawing *user-event-drawing*)
				    '(draw-now))
			       (if (procedure? action) (action) (eval action)))
		       
		       (draw-text object y text foreground background
			   (if action #f 's8))
		       (if action
			   (ezd `(when ,object button1up ,execute-obj)
				`(when ,object button2up ,execute-obj)
				`(when ,object button3up ,execute-obj)
				`(when ,object button4up ,execute-obj)
				`(when ,object button5up ,execute-obj))
			   (ezd `(when ,object * #f)))
		       (ezd `(when ,object enter ,enter-obj)
			    `(when ,object exit ,exit-obj))
		       (loop (+ y menu-height) (cdr args)))))
	(ezd `(when * expose ,drop-when-not-there)
	     '(restore-drawing)))
    
    ;;; Attributes are accessed via the following procedure.
    
    (define (GET-ATTRIBUTES)
	    (map (lambda (a)
			 (case a
			       ((ENTRIES) (flatten name-action-list))
			       ((FOREGROUND) foreground)
			       ((BACKGROUND) background)
			       ((FONT) font)
			       ((ATTRIBUTES) '(entries replace-name
						  replace-action foreground
						  background font delete-object
						  attributes))
			       (else (ezd-error 'check-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    ;;; List conversion functions ((a b) (c d)) <--> (a b c d)
    
    (define (FLATTEN x)
	    (if (pair? x) (cons* (caar x) (cadar x) (flatten (cdr x))) '()))
    
    (define (PAIR-UP x)
	    (if (pair? x)
		(cons (list (car x) (cadr x)) (pair-up (cddr x)))
		'()))
    
    ;;; Attribute argument parsers.
    
    (define (NAME-ACTION-LIST? x)
	    (or (null? x)
		(and (pair? x) (popup-entry-name? (car x))
		     (pair? (cdr x)) (popup-entry-action? (cadr x))
		     (name-action-list? (cddr x)))))
    
    (define (ENTRY-INDEX? x)
	    (and (exact? x) (<= 0 x) (< x (length name-action-list))))
    
    ;;; Attributes are changed via the following procedure.
    
    (define (SET-ATTRIBUTES)
	    (let ((delete #f))
		 (for-each
		     (lambda (a)
			     (cond ((and (pair? a) (eq? (car a) 'ENTRIES)
					 (name-action-list? (cdr a)))
				    (set! name-action-list (pair-up (cdr a))))
				   ((match? (REPLACE-NAME entry-index?
						popup-entry-name?) a)
				    (set-car!
					(list-ref name-action-list (cadr a))
					(caddr a)))
				   ((match? (REPLACE-ACTION entry-index?
						popup-entry-action?) a)
				    (set-car!
					(cdr (list-ref name-action-list
						 (cadr a)))
					(caddr a)))
				   ((match? (FOREGROUND color?) a)
				    (set! foreground (cadr a)))
				   ((match? (BACKGROUND color?) a)
				    (set! background (cadr a)))
				   ((match? (FONT string?) a)
				    (set! font (cadr a)))
				   ((equal? '(DELETE-OBJECT) a)
				    (set! delete #t))
				   (else (ezd-error 'slider
					     "Illegal attribute: ~s" a))))
		     *user-event-misc*)
		 (if delete
		     (ezd `(delete-window ,popup-name)
			  '(save-drawing)
			  `(set-drawing ,popup-name)
			  '(clear)
			  '(restore-drawing))
		     (begin (if visible (make-invisible))
			    (configure-popup)))))
    
    ;;; Install POPITUP as the top-level value of the value of POPUP-NAME.
    (configure-popup)
    (set-top-level-value! popup-name popitup))

;;; Booleans for command and attribute parsing.

(define (POPUP-ENTRY-NAME? x) (or (string? x) (procedure? x) (pair? x)))

(define (POPUP-ENTRY-ACTION? x) (or (pair? x) (procedure? x) (eq? x #f)))

(define-ezd-command
    `(define-popup ,symbol? (repeat ,popup-entry-name? ,popup-entry-action?)
	 (optional ,color? ,color?) (optional ,string?))
    "(define-popup popup-name item-action-list [foreground background] [\"font\"])"
    define-popup)
