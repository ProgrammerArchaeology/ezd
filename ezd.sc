;;; ezd - easy drawing for X11.
;;;
;;; Batch command loop and Scheme command interpreter.

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

(module ezd
    (with xlib
	  struct
	  match
          commands
	  pps
	  ginfo
	  display
	  window
	  drawing
	  view
	  graphic
	  rectangle
	  line
	  text
	  arc
	  quilt
	  psdraw
	  events
	  interact
	  stringinput
	  popupmenu
	  buttons
	  slider
	  textree
	  jtextree
	  textdrawing
	  mouseedit
	  editcolor
	  transpbuttons))

(include "struct.sch")
(include "commands.sch")
(include "display.sch")
(include "window.sch")
(include "view.sch")
(include "drawing.sch")
(include "graphic.sch")
(include "psdraw.sch")
(include "events.sch")
(include "xternal.sch")

(define-c-external (C-SELECT int pointer pointer pointer pointer) int "select")

;;; Version tag

(define *EZD-VERSION* "15mar93jfb")

;;; The program accepts five command line arguments arguments:
;;;
;;;	-i		interactive Scheme interpreter
;;;	-l		log commands to ezd.LOG
;;;	-nopixmap	don't use a pixmap when updating windows
;;;	-p		treat an end-of-file on stdin as a pause command
;;;	-s		allow interrupt signal
;;;
;;; If the environment variable EZDLOG is set, then commands are logged to a
;;; file with EZDLOG's value as its name.  If the environment flag EZDNOPIXMAP
;;; is set, then pixmaps are not used for window updates.

(define EZD-DONE #f)

(define EZD-LOG #f)

(define-c-external (GETENV pointer) pointer "getenv")

(eval-when (eval)
    (define ENV-EZDLOG "")
    (define ENV-EZDNOPIXMAP ""))
    
(eval-when (load)
    (define ENV-EZDLOG (c-string->string (getenv "EZDLOG")))
    (define ENV-EZDNOPIXMAP (c-string->string (getenv "EZDNOPIXMAP"))))

(define NOPIXMAP (if (eq? env-ezdnopixmap "") #f #t))

(define (READ-EVAL-DRAW clargs)
    (define PAUSE (member "-p" clargs))
    
    
    (set! ezd-log (if (or (member "-l" clargs) (not (equal? env-ezdlog "")))
		      (let ((port (open-output-file (if (equal? env-ezdlog "")
							"ezd.LOG"
							env-ezdlog))))
			   (format port ";;; *EZD-VERSION* ~a~%" *ezd-version*)
			   port)
		      #f))
    (set! nopixmap (or (member "-nopixmap" clargs) nopixmap))
    (if (member "-i" clargs)
	(read-eval-print)
	(let ((old-reset reset))
	     (if (and (not (member "-p" clargs)) (not (member "-s" clargs)))
		 (signal 2 1))
	     (set! ezd-done #f)
	     (set! in-read-eval-draw #t)
	     (call-with-current-continuation
		 (lambda (return)
			 (set! reset (lambda () (return #t)))))
	     (let loop ((command (or ezd-done (read))))
		  (if ezd-log
		      (if (or ezd-done (eof-object? command))
			  (close-output-port ezd-log)
			  (begin (write command ezd-log)
				 (newline ezd-log))))
		  (if (or ezd-done (eof-object? command))
		      (begin (if (and (eof-object? command) pause)
				 (ezd '(pause)))
			     (ezd-reset))
		      (begin (ezd command)
			     (loop (or ezd-done (read))))))
	     (set! reset old-reset)
	     (set! ezd-done #f)
	     (set! in-read-eval-draw #f))))

;;; One or more ezd commands are executed by the following function.  Any
;;; graphical objects generated are added to the current drawing.  This
;;; procedure is the command interface for ezd from Scheme user programs.

(define (EZD . commands)
    (for-each
	(lambda (c)
		(let ((value (ezd-one c)))
		     (if (isa-graphic? value)
			 (drawing-add *current-drawing* value))))
	commands)
    #f)

;;; Execute a single ezd command and return its value.  This is not an external
;;; interface.

(define (EZD-ONE command) 
    (if (pair? command)
	(let* ((x (assoc (car command) ezd-commands))
	       (template (if x (cadr x)))
	       (description (if x (caddr x)))
	       (action (if x (cadddr x))))
	      (when (and (not *display*)
			 (not (memq (car command) '(include quit scheme))))
		    (set! *display* (make-display ""))
		    (ezd '(set-drawing ezd))
		    (if (not (memq (car command)
				   '(window save-drawing set-drawing)))
			(ezd '(window ezd 400 400 points "ezd")
			     '(overlay ezd ezd))))
	      (if x
		  (let ((args (arg-parse template command)))
		       (if (eq? args #f)
			   (ezd-error 'ezd
			       "Illegal command: ~s~%          expected - ~a"
			       command description)
			   (apply action args)))
		  (ezd-error 'ezd "Unrecognized command: ~s" command)))
	(ezd-error 'ezd "Command is not a list: ~s" command)))

;;; The following procedure resets the entire drawing system.  All modules
;;; needing initialization in order to be rerun must define a reset procedure
;;; and have it called from here.

(define (EZD-RESET)
    (ezd-module-init)
    (commands-module-init)
    (display-module-init)
    (window-module-init)
    (view-module-init)
    (drawing-module-init)
    (graphic-module-init)
    (events-module-init))

;;; The SCHEME command allows Scheme expressions to be evaluated within ezd.

(define-ezd-command
    `(scheme (repeat ,any?))
    "(scheme <scheme expression> ...)"
    (lambda args (for-each eval (car args))))

;;; The INCLUDE command loads ezd commands or Scheme expressions (files with
;;; a suffix of .sc) from a file.

(define (EZD-INCLUDE file)
    (if (and (>= (string-length file) 3)
	     (equal? (substring file (- (string-length file) 3)
			 (string-length file))
		     ".sc"))
	(loadq file)
	(with-input-from-file
	    file
	    (lambda ()
		    (let loop ((exp (read)))
			 (unless (eof-object? exp)
				 (ezd exp)
				 (loop (read))))))))

(define-ezd-command
    `(include ,string?)
    "(include \"file-name\")"
    ezd-include)

;;; The QUIT command terminates ezd processing.

(define-ezd-command
    `(quit)
    "(quit)"
    (lambda ()
	    (let ((handling-events (and (isa-display? *display*)
					(display-handling-events *display*)))
		  (command-stream in-read-eval-draw))
		 (ezd-reset)
		 (if command-stream (set! ezd-done #t))
		 (if handling-events
		     (if (procedure? (top-level-value 'top-level))
			 (reset)
			 (exit))))))

;;; Command stepping.  Each step identifies itself by a STEP command with some
;;; expression.  The expression is assigned to *STEP* and then the value of
;;; *STEPPER* is evaluated.  If it is true, then the program pauses until some
;;; event calls NEXT-STEP or *STEPPER* is set to #f.

(define *STEP* #f)	;;; Value associated with this step.

(define *STEPPER* #f)	;;; Expression to evaluate at each step to decide
			;;; whether or not to continue.

(define *NEXT-STEP* #f)	;;; Stepper advances when this is true.

;;; Define the stepper.

(define (STEPPER exp)
    (set! *stepper* exp))

(define-ezd-command
    `(stepper ,any?)
    "(stepper expression)"
    stepper)

;;; Test to see if the stepper requires a stop at this step.

(define (STEP exp)
    (set! *step* exp)
    (set! *next-step* #f)
    (when (eval *stepper*)
	  (display-event-handler *display*)
	  (let loop ()
	       (yselect *dpy* 1000000 0)
	       (display-event-handler *display*)
	       (if (and *stepper* (not *next-step*)) (loop)))))

(define-ezd-command
    `(step ,any?)
    "(step expression)"
    step)

;;; Call this from an event handler to allow further processing.

(define (NEXT-STEP)
    (set! *next-step* #t))

;;; The PAUSE command waits for some number of milliseconds, or until ezd
;;; completes, before returning.

(define-ezd-command
    `(pause (optional ,positive-number?))
    "(pause [ millisecond pause time ])"
    (lambda (ms-pause)
	    (if ms-pause
		(let ((timeval (make-string 8)))
		     (ezd '(draw-now))
		     (c-int-set! timeval 0 (quotient ms-pause 1000))
		     (c-int-set! timeval 4 (* (remainder ms-pause 1000) 1000))
		     (c-select 0 0 0 0 timeval))
		(wait-system-file #f))))

;;; Module reset/initialization procedure.

(define (EZD-MODULE-INIT)
    (set! ezd-done #f))
