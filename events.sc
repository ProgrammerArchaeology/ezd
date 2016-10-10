;;; ezd - easy drawing for X11 displays.
;;;
;;; Event handling.

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

(module events)

(include "struct.sch")
(include "commands.sch")
(include "display.sch")
(include "window.sch")
(include "view.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "xternal.sch")

;;; Pointing device motion and keyboard input are reported to ezd by events.
;;; Events are represented by event records of the following form.  Lists of
;;; events are a part of each drawing and graphic.

(define-structure EVENT
    type		;;; Symbolic event type.
    modifier-mask	;;; Button modifier bitmask.
    modifier-names	;;; Button modifier list of symbols.
    action)		;;; Action procedure.

(define-in-line-structure-access EVENT
    type
    modifier-mask
    modifier-names
    action)

;;; Event handlers are defined by the "when" command that has the following
;;; syntax:
;;;
;;;	(when <object> <event> <action>)
;;;
;;; <object> specifies the object name where the event is to be handled.  The
;;; object name "*" indicates that the event handler is for any object that
;;; does not have an explicit event handler.
;;;
;;; <event> is one of the following event types:
;;;
;;;	ENTER		pointing device entered the object.
;;;	EXIT		pointing device exitied the object.
;;;	MOTION		pointing device moved within the object.
;;;	BUTTONbUP	button "b" up in the object.
;;;	BUTTONbDOWN	button "b" down in the object.  A down button event 
;;;			may be preceeded by any combination of SHIFT, META,
;;;			CTRL, and LOCK indicating that those keys must be down
;;;			for the event to occur.
;;;	KEYPRESS	any keyboard key down in the object.
;;;	KEYRELEASE	any keyboard key up in the object.
;;;
;;;	RESIZE		window containing the drawing was resized.
;;;	EXPOSE		window containing the drawing was exposed.
;;;	OVERLAY		the drawing was overlayed or underlayed into a window.
;;;	VISIBLE		a portion of the drawing is now visible in some window.
;;;
;;;	GET-ATTRIBUTES	message to read attribute values.
;;;	SET-ATTRIBUTES	message to set attribute values.
;;;
;;;	*		only allowed with action equal to #f to delete all
;;;			event handlers.
;;;
;;; <action> is either the procedure handling the event, or a Scheme expression
;;; to be evaluated when the event occurs.  When the event occurs, the action
;;; is invoked, and the following information is available as top-level
;;; bindings:
;;;
;;;	*USER-EVENT-WINDOW*	window name
;;;	*USER-EVENT-DRAWING*	drawing name
;;;	*USER-EVENT-OBJECT*	object name or #f
;;;	*USER-EVENT-X*		mouse position in drawing coordinate system
;;;	*USER-EVENT-Y*
;;;	*USER-EVENT-TYPE*	event type
;;;	*USER-EVENT-XEVENT*	X event structure or #f
;;;	*USER-EVENT-MISC*	event specific items
;;;
;;; When the event type is SEND, the *USER-EVENT-WINDOW*, *USER-EVENT-X*,
;;; and *USER-EVENT-Y* fields are not valid.

(define (WHEN-EVENT name event action)
    (define (ENCODE-MODIFIER)
	    (let loop ((mods (car event)) (mask 0))
		 (if (pair? mods)
		     (loop (cdr mods)
			   (bit-or mask
				   (cadr (assoc (car mods)
						`((shift ,shiftmask)
						  (lock ,lockmask)
						  (capslock ,lockmask)
						  (control ,controlmask)
						  (ctrl ,controlmask)
						  (meta ,mod1mask)
						  (compose ,mod1mask))))))
		     mask)))
    (let ((modifier (if (symbol? event)
			0
			(encode-modifier)))
	  (type (if (symbol? event) event (cadr event)))
	  (action (if (or (procedure? action) (eq? action #f))
		      action
		      (lambda () (eval action)))))
	 
	 (define (DELETE-EVENT evl)
		 (if (pair? evl)
		     (let ((e (car evl)))
			  (if (and (equal? (event-type e) type)
				   (equal? (event-modifier-mask e) modifier))
			      (cdr evl)
			      (cons (car evl) (delete-event (cdr evl)))))
		     '()))
	 
	 (define (ADD-EVENT evl)
		 (if action
		     (append evl
			     (list (make-event type modifier
				       (if (zero? modifier) '() (car event))
				       action)))
		     evl))
	 
	 (cond ((eq? type '*)
		(cond ((not (eq? action #f))
		       (ezd-error 'WHEN-EVENT
			   "Event type * requires action #f"))
		      ((not (eq? name '*))
		       (graphic-events! (name->graphic name) '())
		       (drawing-window-watch! *current-drawing*
			   (remq name
				 (drawing-window-watch *current-drawing*))))
		      (else (drawing-events! *current-drawing* '()))))
	       ((not (eq? name '*))
		(let ((g (name->graphic name)))
		     (if (and (memq type '(expose overlay resize visible))
			      (not (memq name (drawing-window-watch
						  *current-drawing*))))
			 (drawing-window-watch! *current-drawing*
			     (cons name
				   (drawing-window-watch *current-drawing*))))
		     (graphic-events! g
			 (add-event (delete-event (graphic-events g))))))
	       (else (drawing-events! *current-drawing*
			 (add-event (delete-event (drawing-events
						      *current-drawing*))))))))
 
(define (BUTTON-MODIFIER? x)
    (memq x '(shift meta control ctrl lock capslock hyper super)))

(define (BUTTON-DOWN? x)
    (memq x '(button1down button2down button3down button4down button5down)))

(define (WHEN-EVENT? x)
    (memq x '(button1down button2down button3down button4down button5down
		 button1up button2up button3up button4up button5up
		 enter exit motion keypress keyrelease resize expose overlay
		 visible get-attributes set-attributes *)))

(define-ezd-command
    `(when ,symbol?
	   (or ((repeat ,button-modifier?) ,button-down?)
	       (,when-event?))
	   ,any?)
    "(when <object-name> <event> ... <action>)"
    when-event)

;;; A specific event is looked up in an event list by the following procedure.
;;; It returns the event or #f.
	 
(define (FIND-WHEN-EVENT events type modifier)
    (let loop ((events events))
	 (if (pair? events)
	     (let ((event (car events)))
		  (if (and (eq? (event-type event) type)
			   (eq? (event-modifier-mask event) modifier))
		      event
		      (loop (cdr events))))
	     #f)))

;;; When a window is resized or exposed, all drawings expecting the event are
;;; notified by the following procedure.

(define (HANDLE-WINDOW-EVENTS window event-type event args)    
    (for-each
	(lambda (view)
		(handle-view-events view event-type event args)
		(if (eq? event-type 'resize) (handle-visible-events view)))
	(window-views window)))

;;; When a window related event happens to a specific view, the following
;;; procedure is called to invoke the event handlers.  The general event
;;; handler (object = *) is called before any specific object event handlers.

(define (HANDLE-VIEW-EVENTS view event-type event args)
    (let ((save-current-drawing *current-drawing*))
	 (set! *current-drawing* (view-drawing view)) 
	 (set-view view '())
	 (user-action (find-when-event (drawing-events (view-drawing view))
			  event-type 0) view #f event-type event args)
	 (for-each
	     (lambda (graphic-name)
		     (let ((graphic (name-of-graphic? graphic-name)))
			  (if graphic
			      (user-action
				  (find-when-event (graphic-events graphic)
				      event-type 0)
				  view graphic event-type event args))))
	     (drawing-window-watch (view-drawing view)))
	 (set! *current-drawing* save-current-drawing)))

;;; When an attribute message is sent to an object, the following procedure
;;; finds the event handler and calls it.  If the event handler does not exist,
;;; then the message is ignored.

(define (HANDLE-ATTRIBUTE-EVENTS drawing object event-type arguments)
    (let ((user-event (find-when-event
			  (append (drawing-events (name->drawing drawing))
				  (graphic-events (getprop object drawing)))
			  event-type 0)))
	 (if user-event
	     (let ((save-current-drawing *current-drawing*))
		  (set-drawing drawing)
		  (set! *user-event-window* #f)
		  (set! *user-event-drawing* drawing)
		  (set! *user-event-object* object)
		  (set! *user-event-x* #f)
		  (set! *user-event-y* #f)
		  (set! *user-event-type* event-type)
		  (set! *user-event-xevent* #f)
		  (set! *user-event-misc* arguments)
		  (let ((result ((event-action user-event))))
		       (set! *current-drawing* save-current-drawing)
		       result))
	     #f)))

;;; The following global variables maintain the current mouse state
;;; They are automatically updated when each X event is processed, or when
;;; changes are made to drawings displayed in the window containing the
;;; mouse.

(define *MOUSE-X* 0)

(define *MOUSE-Y* 0)

(define *MOUSE-XWINDOW* #f)

(define *MOUSE-WINDOW* #f)

(define *MOUSE-WINDOW-X* 0)

(define *MOUSE-WINDOW-Y* 0)

(define *MOUSE-VIEW* #f)

(define *MOUSE-OBJECT* #f)

(define *MOUSE-BUTTON1* #f)

(define *MOUSE-BUTTON2* #f)

(define *MOUSE-BUTTON3* #f)

(define *MOUSE-BUTTON4* #f)

(define *MOUSE-BUTTON5* #f)

;;; Mouse state is maintained by the following procedure.
	 
(define (UPDATE-MOUSE event)
    (let ((event-type (xevent-type event)))
	 (cond ((or (eq? event-type buttonpress)
		    (eq? event-type buttonrelease))
		(set! *mouse-x* (xevent-xbutton-x_root event))
		(set! *mouse-y* (xevent-xbutton-y_root event))
		(set! *mouse-xwindow* (xevent-xbutton-window event))
		(if (eq? event-type buttonpress)
		    (case (xevent-xbutton-button event)
			  ((1) (set! *mouse-button1* #t))
			  ((2) (set! *mouse-button2* #t))
			  ((3) (set! *mouse-button3* #t))
			  ((4) (set! *mouse-button4* #t))
			  ((5) (set! *mouse-button5* #t)))
		    (case (xevent-xbutton-button event)
			  ((1) (set! *mouse-button1* #f))
			  ((2) (set! *mouse-button2* #f))
			  ((3) (set! *mouse-button3* #f))
			  ((4) (set! *mouse-button4* #f))
			  ((5) (set! *mouse-button5* #f)))))
	       ((eq? event-type enternotify)
		(set! *mouse-x* (xevent-xcrossing-x_root event))
		(set! *mouse-y* (xevent-xcrossing-y_root event))
		(set! *mouse-xwindow* (xevent-xcrossing-window event))
		(set! *mouse-object* ""))
	       ((eq? event-type leavenotify)
		(set! *mouse-x* (xevent-xcrossing-x_root event))
		(set! *mouse-y* (xevent-xcrossing-y_root event))
		(set! *mouse-window* #f)
		(set! *mouse-xwindow* #f)
		(set! *mouse-object* ""))
	       ((eq? event-type motionnotify)
		(set! *mouse-x* (xevent-xmotion-x_root event))
		(set! *mouse-y* (xevent-xmotion-y_root event))
		(set! *mouse-xwindow* (xevent-xmotion-window event))))
	 (if *trace-events*
	     (format *trace-events* "~s (~s,~s) ~s~s~s~s~s ==> "
		     (if *mouse-xwindow*
			 (window-name
			     (xwindow->window *mouse-xwindow*))
			 #f)
		     *mouse-x* *mouse-y* *mouse-button1* *mouse-button2*
		     *mouse-button3* *mouse-button4* *mouse-button5*))))

;;; X event code to event name conversion.

(define (EVENT->NAME code)
    (cond ((eq? code motionnotify) 'motionnotify)
	  ((eq? code buttonpress) 'buttonpress)
	  ((eq? code buttonrelease) 'buttonrelease)
	  ((eq? code enternotify) 'enternotify)
	  ((eq? code leavenotify) 'leavenotify)
	  ((eq? code expose) 'expose)
	  ((eq? code keypress) 'keypress)
	  ((eq? code keyrelease) 'keyrelease)
	  (else code)))

;;; Translate X button events to button symbol.
	 
(define (BUTTON-DOWN-SYMBOL event)
    (case (xevent-xbutton-button event)
	  ((1) 'button1down)
	  ((2) 'button2down)
	  ((3) 'button3down)
	  ((4) 'button4down)
	  ((5) 'button5down)))
	 
(define (BUTTON-UP-SYMBOL event)
    (case (xevent-xbutton-button event)
	  ((1) 'button1up)
	  ((2) 'button2up)
	  ((3) 'button3up)
	  ((4) 'button4up)
	  ((5) 'button5up)))

;;; Object entry, exit, and motion events are generated by the following
;;; procedure.  Each time the mouse has moved, it is called with a window and
;;; position in window.
	 
(define (EXIT-ENTER event window x y)
    (let ((was-in *mouse-object*)
	  (was-in-view *mouse-view*)
	  (was-x *mouse-window-x*)
	  (was-y *mouse-window-y*)
	  (was-in-exit (find-when-event (is-in-events) 'exit 0)))
	 (set! *mouse-window* window)
	 (set! *mouse-window-x* x)
	 (set! *mouse-window-y* y)
	 (let loop ((views (reverse (window-views window))))
	      (if (pair? views)
		  (let* ((view (car views))
			 (object (and (or (not (view-clip-minx view))
					  (and (<= (view-clip-minx view)
						   *mouse-window-x*
						   (view-clip-maxx view))
					       (<= (view-clip-miny view)
						   *mouse-window-y*
						   (view-clip-maxy view))))
				      (bbgraphics-really-intersect view
					  (- x 1) (- y 1) (+ x 1) (+ y 1)))))
			(if object
			    (begin (set! *mouse-view* view)
				   (set! *mouse-object* object))
			    (loop (cdr views))))
		  (begin (set! *mouse-view* #f)
			 (set! *mouse-object* #f))))
	 (if (eq? was-in *mouse-object*)
	     (if (and (isa-graphic? *mouse-object*)
		      (or (not (= was-x *mouse-window-x*))
			  (not (= was-y *mouse-window-y*))))
		 (user-action (find-when-event (is-in-events) 'motion 0)
		     *mouse-view* *mouse-object* 'motion event '()))
	     (begin (user-action was-in-exit was-in-view was-in 'exit event
			(if *mouse-view*
			    `(,(window-name (view-window *mouse-view*))
			      ,(drawing-name (view-drawing *mouse-view*))
			      ,(graphic-name *mouse-object*))
			    '(#f #f #f)))
		    (if (isa-graphic? *mouse-object*)
			(user-action
			    (find-when-event (is-in-events) 'enter 0)
			    *mouse-view* *mouse-object* 'enter event '()))))))

;;; Return a list of possible events for a given window and the current mouse
;;; object.
	 
(define (IS-IN-EVENTS)
    (if (isa-graphic? *mouse-object*)
	(append (graphic-events *mouse-object*)
		(drawing-events (view-drawing *mouse-view*)))
	'()))

;;; Signal a possible user event.  Once the user event has been run, the mouse
;;; position is recomputed if there was anything drawn in the mouse window.

(define *CLEAN-MOUSE-WINDOW* #f)

(define *USER-EVENT-WINDOW* #f)

(define *USER-EVENT-DRAWING* #f)

(define *USER-EVENT-OBJECT* #f)

(define *USER-EVENT-X* #f)

(define *USER-EVENT-Y* #f)

(define *USER-EVENT-TYPE* #f)

(define *USER-EVENT-XEVENT* #f)

(define *USER-EVENT-MISC* #f)

(define (USER-ACTION user-event view object user-event-type event misc)
    (when user-event
	  (if *trace-events* (format *trace-events* "~s " user-event-type))
	  (if object
	      (begin (if (pair? (view-new-transform view))
			 (display-event-handler *display*))
		     (set! *user-event-object* (graphic-name object))
		     (set! *user-event-x*
			   ((view-x->user view) *mouse-window-x*))
		     (set! *user-event-y*
			   ((view-y->user view) *mouse-window-y*)))
	      (begin (set! *user-event-object* '*)
		     (set! *user-event-x* 0)
		     (set! *user-event-y* 0)))
	  (set! *clean-mouse-window* *mouse-window*)
	  (set! *user-event-window* (window-name (view-window view)))
	  (set! *user-event-drawing* (drawing-name (view-drawing view)))
	  (set! *user-event-type* user-event-type)
	  (set! *user-event-xevent* event)
	  (set! *user-event-misc* misc)
	  (set-drawing *user-event-drawing*)
	  ((event-action user-event))
	  (when (or (and (eq? user-event-type 'visible)
			 (eq? *clean-mouse-window* (view-window view)))
		    (and (not *clean-mouse-window*) *mouse-window*))
		(set! *clean-mouse-window* #f)
		(exit-enter #f *mouse-window* *mouse-window-x*
		    *mouse-window-y*))
	  (set! *clean-mouse-window* #f)))

;;; When an event occurs in a window, the window's views are examined from top
;;; to bottom until an object in which the event occurred is found.  Before
;;; that object's event handler is called with the appropriate event, EXIT and
;;; ENTER events are generated as needed.

(define *TRACE-EVENTS* #f)

(define (HANDLE-WHEN-EVENTS window event)
    (let ((event-type (xevent-type event)))
	 
	 (if *trace-events*
	     (format *trace-events* "X: ~s ~s ==> " (window-name window)
		     (event->name event-type)))
	 (cond ((eq? event-type buttonpress)
		(update-mouse event)
		(exit-enter event window
		    (xevent-xbutton-x event) (xevent-xbutton-y event))
		(let ((user-event (find-when-event (is-in-events)
				      (button-down-symbol event)
				      (xevent-xbutton-state event))))
		     (if user-event
			 (user-action user-event *mouse-view* *mouse-object*
			     (button-down-symbol event) event
			     (event-modifier-names user-event)))))
	       ((eq? event-type buttonrelease)
		(update-mouse event)
		(exit-enter event window
		    (xevent-xbutton-x event) (xevent-xbutton-y event))
		(user-action
		    (find-when-event (is-in-events)
			(button-up-symbol event) 0)
		    *mouse-view* *mouse-object* (button-up-symbol event)
		    event '()))
	       ((eq? event-type enternotify)
		(update-mouse event)
		(exit-enter event window (xevent-xcrossing-x event)
		    (xevent-xcrossing-y event)))
	       ((eq? event-type leavenotify)
		(let ((was-in *mouse-object*)
		      (was-in-view *mouse-view*)
		      (was-in-exit (find-when-event (is-in-events) 'exit 0)))
		     (update-mouse event)
		     (user-action was-in-exit was-in-view was-in
			 'exit event '(#f #f #f))))
	       ((eq? event-type keypress)
		(user-action
		    (find-when-event (is-in-events) 'keypress 0)
		    *mouse-view* *mouse-object* 'keypress event
		    (ylookupstring event #t)))
	       ((eq? event-type keyrelease)
		(user-action
		    (find-when-event (is-in-events) 'keyrelease 0)
		    *mouse-view* *mouse-object* 'keyrelease event
		    (ylookupstring event #t)))
	       ((eq? event-type motionnotify)
		(let loop ((event event))
		     (if (and (> (xeventsqueued *dpy* queuedafterreading) 0)
			      (eq? (xevent-type (xpeekevent *dpy*))
				   motionnotify))
			 (loop (xnextevent *dpy*))
			 (when (and (< -1 (xevent-xmotion-x event)
				       (window-width window))
				    (< -1 (xevent-xmotion-y event)
				       (window-height window)))
			       (update-mouse event)
			       (exit-enter event window
				   (xevent-xmotion-x event)
				   (xevent-xmotion-y event))))))
	       ((eq? event-type expose)
		(handle-window-events window 'expose event '())))
	 (if *trace-events* (format *trace-events* "~%"))))

;;; Module reset/initialization

(define (EVENTS-MODULE-INIT)
    (set! *mouse-x* 0)
    (set! *mouse-y* 0)
    (set! *mouse-xwindow* #f)
    (set! *mouse-window* #f)
    (set! *mouse-window-x* 0)
    (set! *mouse-window-y* 0)
    (set! *mouse-view* #f)
    (set! *mouse-object* #f)
    (set! *mouse-button1* #f)
    (set! *mouse-button2* #f)
    (set! *mouse-button3* #f)
    (set! *mouse-button4* #f)
    (set! *mouse-button5* #f)
    (set! *clean-mouse-window* #f)
    (set! *user-event-window* #f)
    (set! *user-event-drawing* #f)
    (set! *user-event-object* #f)
    (set! *user-event-x* #f)
    (set! *user-event-y* #f)
    (set! *user-event-type* #f)
    (set! *user-event-xevent* #f)
    #t)
