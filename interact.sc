;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module provide some basic interactive elements
;;; to provide event logging, attribute access, clicks on objects, and bells.

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

(module interact)

(include "struct.sch")
(include "commands.sch")
(include "display.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "ezd.sch")
(include "events.sch")
(include "xternal.sch")

;;; Any event may be logged on stdout by supplying a call to the following
;;; procedure as the action.  This will result in the following string being
;;; printed:
;;;
;;;	(event-type window-name drawing-name object-name user-x user-y
;;;	 window-x window-y [misc-info] [more-misc])\n
;;;
;;; where EVENT-TYPE is the type of event.  WINDOW-NAME is the name of the
;;; window.  DRAWING-NAME is the name of the drawing.  OBJECT-NAME is the name
;;; of the object, #f, or * when no object is specified.  This is followed by
;;; the mouse coordinates in the user and X window coordinate systems.
;;; MISC-INFO is event specific information.  It is followed by any additional
;;; information that the user wishes to add.

(define (LOG-EVENT . more-misc)
    (define (LOG port)
	    (format port "(~a ~a ~a ~a ~a ~a ~a ~a"
		    *user-event-type* *user-event-window* *user-event-drawing*
		    *user-event-object* *user-event-x* *user-event-y*
		    *mouse-window-x* *mouse-window-y*)
	    (for-each (lambda (x) (format port " ~s" x)) *user-event-misc*)
	    (for-each (lambda (x) (format port " ~s" x)) more-misc)
	    (display ")" port)
	    (newline port))

    (when ezd-log
	  (display ";-> " ezd-log)
	  (log ezd-log))
    (log stdout-port))

;;; The following procedure provides the name of the current drawing for
;;; Scheme extensions.  If *current-drawing* is #f then #f is returned.

(define (CURRENT-DRAWING-NAME)
    (if *current-drawing* (drawing-name *current-drawing*) #f))

;;; Attributes of an object may be accessed by the following function.  Either
;;; a list of values or #f (object doesn't respond to this sort of event) is
;;; returned.

(define (GET-ATTRIBUTES drawing object . args)
    (cond ((not (attributes-drawing-exists? drawing))
	   (error 'get-attributes "Arguments is not a drawing name: ~s"
		  drawing))
	  ((not (attributes-object-exists? object))
	   (error 'get-attributes "Argument is not an object name: ~s" object))
	  ((not (get-attribute-list? args))
	   (error 'get-attributes "Argument is not an attribute list: ~s"
		  args))
	  (else (handle-attribute-events drawing object 'get-attributes
		    args))))

(define (GET-ATTRIBUTE drawing object attribute)
    (car (get-attributes drawing object attribute)))

;;; Object attributes are set by the following procedure.  It does not return
;;; any values.

(define (SET-ATTRIBUTES drawing object . args)
    (cond ((not (attributes-drawing-exists? drawing))
	   (error 'set-attributes "Arguments is not a drawing name: ~s"
		  drawing))
	  ((not (attributes-object-exists? object))
	   (error 'set-attributes "Argument is not an object name: ~s" object))
	  ((not (set-attribute-list? args))
	   (error 'set-attributes "Argument is not an attribute list: ~s"
		  args))
	  (else (handle-attribute-events drawing object 'set-attributes args)
		#f)))

;;; Parsing procedure for attribute access commands.

(define ATTRIBUTES-DRAWING-NAME #f)

(define (ATTRIBUTES-DRAWING-EXISTS? x)
    (if (and (symbol? x) (drawing-exists? x))
	(begin (set! attributes-drawing-name x)
	       #t)
	#f))

(define (ATTRIBUTES-OBJECT-EXISTS? x)
    (and (symbol? x) (isa-graphic? (getprop x attributes-drawing-name))))

(define (SET-ATTRIBUTE? x)
    (and (pair? x) (symbol? (car x)) (null? (cdr (last-pair x)))))

(define (SET-ATTRIBUTE-LIST? x)
    (or (null? x)
	(and (pair? x) (set-attribute? (car x))
	     (set-attribute-list? (cdr x)))))

(define (GET-ATTRIBUTE? x)
    (or (symbol? x) (set-attribute? x)))

(define (GET-ATTRIBUTE-LIST? x)
    (or (null? x)
	(and (pair? x) (get-attribute? (car x))
	     (get-attribute-list? (cdr x)))))

;;; GET-ATTRIBUTE returns the attribute values in a log message.  The values
;;; are prefaced with the request's tag.

(define-ezd-command
    `(get-attributes ,attributes-drawing-exists? ,attributes-object-exists?
	 ,any? (repeat ,get-attribute?))
    "(get-attributes drawing object tag attributes...)"
    (lambda (drawing object tag args)
	    (set! *user-event-misc*
		  (cons tag (handle-attribute-events drawing object
				'get-attributes args)))
	    (set! *user-event-window* #f)
	    (set! *user-event-drawing* drawing)
	    (set! *user-event-object* object)
	    (set! *user-event-x* #f)
	    (set! *user-event-y* #f)
	    (set! *user-event-type* 'get-attributes)
	    (set! *user-event-xevent* #f)
	    (log-event)))

;;; SET-ATTRIBUTE sets a number of attribute values.  It does not return a
;;; value.

(define-ezd-command
    `(set-attributes ,attributes-drawing-exists? ,attributes-object-exists?
	 (repeat ,set-attribute?))
    "(set-attributes drawing object (attribute args...) ...)"
    (lambda (drawing object args)
	    (handle-attribute-events drawing object 'set-attributes args)))

;;; An action can be associated with a click on a object by the CLICK command.
;;; A click is defined as button down with appropriate modifiers within the
;;; object followed by button up.  If the mouse is moved out of the object
;;; before the button comes up, then no action will be taken.

(define (CLICK-ON-OBJECT object-name modifiers button action)
    
    (define DOWN #f)
    
    (define (ON-EXIT) (set! down #f))
    
    (define (ON-DOWN) (set! down (list *user-event-x* *user-event-y*
				       *mouse-window-x* *mouse-window-y*)))
    
    (define (ON-UP)
	    (when down
		  (set! *user-event-type* 'click)
		  (set! *user-event-misc* down)
		  (set! down #f)
		  (if (procedure? action) (action) (eval action))))
    
    (ezd `(when ,object-name ,@modifiers
		,(vector-ref '#(#f button1down button2down button3down
				button4down button5down) button)
		,on-down)
	 `(when ,object-name exit ,on-exit)
	 `(when ,object-name
		,(vector-ref '#(#f button1up button2up button3up
				button4up button5up) button)
		,on-up)))

(define-ezd-command
    `(click ,symbol? (repeat ,button-modifier?)
	    ,(lambda (x) (memq x '(1 2 3 4 5))) ,any?)
    "(click object-name [button-modifiers] button-number action)"
    click-on-object)

;;; The BELL command rings the bell on the keyboard.

(define-ezd-command
    `(bell)
    "(bell)"
    (lambda () (xbell *dpy* 0) (xflush *dpy*)))
