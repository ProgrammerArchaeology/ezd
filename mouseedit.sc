;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module implement a mouse based editor for use inside
;;; a TEXT-DRAWING.  In order to test the utility of attributes, all
;;; communication with the text drawing is via attributes.

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

(module mouseedit (with ezd))

(include "struct.sch")
(include "display.sch")
(include "ezd.sch")
(include "events.sch")
(include "interact.sch")
(include "xternal.sch")

;;; When a TEXT-DRAWING is created, the following procedure is called to
;;; install mouse based editting.

(define (MOUSE-EDIT-INIT drawing object options)
    (let* ((read-only (memq 'read-only options))
	   (current-window #f)
	   (first-line #f)
	   (last-line #f)
	   (slider #f)
	   (button1 #f)
	   (button1down-line #f)
	   (button1down-char #f)
	   (cursor-line 0)
	   (cursor-char 0)
	   (selection #f)
	   (begin-line 0)
	   (begin-char 0)
	   (end-line 0)
	   (end-char 0)
	   (undo-cursor #f)
	   (undo-text #f))
	  
	  ;;; Change cursor on drawing entry and exit.  Find out viewed lines
	  ;;; and slider name on entry.
	  
	  (define (ENTER)
		  (when (not current-window)
			(set! current-window *user-event-window*)
			(let ((view (get-attribute drawing object
					`(view ,current-window))))
			     (set! first-line (car view))
			     (set! last-line (cadr view))
			     (set! slider (caddr view)))
			(set! button1 #f)
			(ezd `(save-cursor ,current-window)
			     `(set-cursor ,current-window xc_xterm))))
	  
	  (define (EXIT)
		  (when (and current-window
			     (or (not (eq? (car *user-event-misc*)
					   current-window))
				 (not (eq? (cadr *user-event-misc*) drawing))))
			(ezd `(restore-cursor ,current-window))
			(set! current-window #f)))
	  
	  ;;; Covert the mouse position in the current event to the cursor
	  ;;; position stored here.  Note that the screen cursor is not
	  ;;; updated at this time.
	  
	  (define (MOUSE->CURSOR)
		  (let* ((line-char-text (get-attribute drawing object
					     `(xy->line-char-text
						  ,*user-event-x*
						  ,*user-event-y*)))
			 (line (car line-char-text))
			 (char (cadr line-char-text))
			 (text (caddr line-char-text)))
			(set! cursor-line line)
			(set! cursor-char
			      (if (and (positive? char)
				       (eq? char (string-length text))
				       (eq? (string-ref text (- char 1))
					    #\newline))
				  (- char 1)
				  char))))
	  
	  ;;; Query the drawing for the current cursor position.  If the
	  ;;; cursor is not visibile in the current window, then scroll it to
	  ;;; make it visible.
	  
	  (define (READ-CURSOR)
		  (let ((line-char (get-attribute drawing object 'cursor)))
		       (set! cursor-line (car line-char))
		       (set! cursor-char (cadr line-char))
		       (if (not (<= first-line cursor-line last-line))
			   (let ((newfirst (if (< cursor-line first-line)
					       cursor-line
					       (+ first-line
						  (- cursor-line last-line)))))
				(set! last-line
				      (+ last-line (- newfirst first-line)))
				(set! first-line newfirst)
				(if slider
				    (set-attributes slider 'slider
					`(value ,newfirst)))
				(set-attributes drawing object
				    '(mouse-edit)
				    `(scroll ,current-window ,newfirst))))))
	  
	  ;;; Mouse button 1 going down sets the cursor and clears any
	  ;;; current selection.
	  
	  (define (BUTTON1DOWN)
		  (mouse->cursor)
		  (set! button1down-line cursor-line)
		  (set! button1down-char cursor-char)
		  (set! selection #f)
		  (set! button1 #t)
		  (set-attributes drawing object
		      '(mouse-edit)
		      `(cursor ,cursor-line ,cursor-char) '(highlight)))
	  
	  ;;; Motion with mouse button 1 down causes the cursor to move and
	  ;;; starts/extends the current selection.  Completion of the
	  ;;; selection causes the selection to be copied to the X cut buffer
	  ;;; when in READ-ONLY mode.
	  
	  (define (MOTION-BUTTON1UP)
		  (if button1
		      (let ((event *user-event-xevent*))
			   (mouse->cursor)
			   (set! button1 *mouse-button1*)
			   (extend-selection cursor-line cursor-char)
			   (if (and read-only (not button1))
			       (cut/copy #f (xevent-xbutton-time event))))))
	  
	  ;;; The current selection is extended by the following function.  The
	  ;;; cursor is placed at the end of the selection.  If the selection
	  ;;; turns out to be null when the button comes up, it disappears.
	  
	  (define (EXTEND-SELECTION line char)
		  (if (lc<? button1down-line button1down-char line char)
		      (let* ((lc (inc-line-char drawing object line char #f
				     -1))
			     (new-line (car lc))
			     (new-char (cadr lc)))
			    (set! begin-line button1down-line)
			    (set! begin-char button1down-char)
			    (set! end-line new-line)
			    (set! end-char new-char))
		      (let* ((lc (inc-line-char drawing object button1down-line
				     button1down-char #f -1))
			     (new-line (car lc))
			     (new-char (cadr lc)))
			    (set! begin-line line)
			    (set! begin-char char)
			    (set! end-line new-line)
			    (set! end-char new-char)))
		  (if (lc=? line char button1down-line button1down-char)
		      (begin (set! selection #f)
			     (set! cursor-line line)
			     (set! cursor-char char)
			     (set-attributes drawing object
				 '(mouse-edit)
				 `(cursor ,line ,char) '(highlight)))
		      (let ((lc (inc-line-char drawing object end-line end-char
				    #f 1)))
			   (set! cursor-line (car lc))
			   (set! cursor-char (cadr lc))
			   (set! selection #t)
			   (set-attributes drawing object
			       '(mouse-edit)
			       `(cursor ,cursor-line ,cursor-char)
			       `(highlight ,begin-line ,begin-char
				    ,end-line ,end-char)))))
	  
	  ;;; Keyboard input is handled here.
	  
	  (define (KEYPRESS)
		  (let* ((key (car *user-event-misc*))
			 (keysym (cadr *user-event-misc*))
			 (ascii-code (if (equal? key "") 0
					 (char->integer (string-ref key 0))))
			 (key-state (xevent-xkey-state *user-event-xevent*))
			 (time (xevent-xkey-time *user-event-xevent*)))
			(cond (read-only (ezd '(bell)))
			      ((or (eq? ascii-code 8)	;;; control-h
				   (eq? ascii-code 127));;; backspace
			       (if selection
				   (delete-selection)
				   (delete-before-cursor)))
			      ((eq? ascii-code 13)	;;; return
			       (delete-selection)
			       (set-attributes drawing object
				   '(mouse-edit)
				   `(insert ,cursor-line ,cursor-char
					    ,(list->string '(#\newline))))
			       (read-cursor)
			       (unless (zero? cursor-char)
				       (set! cursor-line (+ cursor-line 1))
				       (set! cursor-char 0)
				       (set-attributes drawing object
					   '(mouse-edit)
					   `(cursor ,cursor-line ,cursor-char))
				       (read-cursor)))
			      ((not (zero? (bit-and key-state mod1mask)))
			       (cond ((equal? key "z") (undo))
				     ((equal? key "x") (cut/copy #t time))
				     ((equal? key "c") (cut/copy #f time))
				     ((equal? key "v") (paste))
				     (else (ezd '(bell)))))
			      ((or (and (string<=? " " key)
					(string<=? key "~"))
				   (eq? ascii-code 9))	;;; tab
			       (delete-selection)
			       (set-attributes drawing object
				   '(mouse-edit)
				   `(insert ,cursor-line ,cursor-char ,key))
			       (read-cursor))
			      ((<= xk_left keysym xk_down)
			       (cursor-motion keysym))
			      ((and (not (<= xk_shift_l keysym xk_hyper_r))
				    (not (= keysym xk_multi_key)))
			       (ezd '(bell))))))
	  
	  ;;; Delete the currently selected text.
	  
	  (define (DELETE-SELECTION)
		  (when selection
			(set! selection #f)
			(set! undo-cursor (list begin-line begin-char))
			(set! undo-text (selection->string))
			(set-attributes drawing object
			    '(mouse-edit)
			    '(highlight)
			    `(delete ,begin-line ,begin-char
				     ,end-line ,end-char))
			(read-cursor)))
	  
	  ;;; Delete the character behind the cursor.
	  
	  (define (DELETE-BEFORE-CURSOR)
		  (when (lc>? cursor-line cursor-char 0 0)
			(if (zero? cursor-char) (cursor-motion xk_left))
			(cursor-motion xk_left)
			(set-attributes drawing object
			    '(mouse-edit)
			    `(delete ,cursor-line ,cursor-char
				     ,cursor-line ,cursor-char))
			(read-cursor)
			(if (zero? cursor-char) (cursor-motion xk_left))))
	  
	  ;;; Undo the last edit command.
	  
	  (define (UNDO)
		  (when undo-cursor
			(set-attributes drawing object
			    '(mouse-edit)
			    `(cursor ,@undo-cursor)
			    `(insert ,@undo-cursor ,undo-text)
			    '(highlight))
			(set! undo-cursor #f)
			(set! undo-text #f)))
	  
	  ;;; Return a string containing the current selection.
	  
	  (define (SELECTION->STRING)
		  (define CUT-BUFFER #f)
		  
		  (let loop ((i begin-line) (len 0))
		       (if (<= i end-line)
			   (let* ((whole-line (get-attribute drawing object
						  `(text-line ,i)))
				  (line (if (or (< begin-line i end-line)
						(eq? whole-line ""))
					    whole-line
					    (substring whole-line
						(if (eq? i begin-line)
						    begin-char
						    0)
						(if (eq? i end-line)
						    (min (+ end-char 1)
							 (string-length
							     whole-line))
						    (string-length
							whole-line)))))
				  (line-len (string-length line)))
				 (case (and (< i end-line)
					    (positive? line-len)
					    (string-ref line (- line-len 1)))
				       ((#f #\tab #\space #\newline)
					(loop (+ i 1) (+ line-len len)))
				       ((#\.)
					(loop (+ i 1) (+ line-len len 2)))
				       (else
					    (loop (+ i 1) (+ line-len len 1))))
				 (do ((j 0 (+ j 1)))
				     ((= j line-len))
				     (string-set! cut-buffer (+ j len)
					 (string-ref line j))))
			   (set! cut-buffer (make-string len #\space))))
		  cut-buffer)
	  
	  ;;; Cut or copy the current selection to the X selection.
	  
	  (define (CUT/COPY cut time)
		  (let ((cut-buffer (selection->string)))
		       (xsetselectionowner *dpy* xa_primary none time)
		       (xstorebytes *dpy*
			   (type/value->pointer 'charap cut-buffer)
			   (string-length cut-buffer))
		       (if cut (delete-selection))))
	  
	  ;;; Paste the current X selection into the document.
	  
	  (define (PASTE)
		  (let* ((ptr-cnt (xfetchbytes *dpy*))
			 (ptr (pointer-value (car ptr-cnt)))
			 (cnt (cadr ptr-cnt))
			 (buffer (make-string cnt)))
			(delete-selection)
			(do ((i 0 (+ i 1)))
			    ((= i cnt))
			    (string-set! buffer i
				(integer->char (c-byte-ref ptr i))))
			(if (not (zero? ptr)) (xfree ptr))
			(set-attributes drawing object
			    '(mouse-edit)
			    `(insert ,cursor-line ,cursor-char ,buffer))
			(read-cursor)))
	  
	  ;;; Handle a cursor character.
	  
	  (define (CURSOR-MOTION keysym)
		  (let ((line-char (inc-line-char drawing object
				       cursor-line cursor-char
				       (cond ((eq? keysym xk_up) -1)
					     ((eq? keysym xk_down) 1)
					     (else #f))
				       (cond ((eq? keysym xk_left) -1)
					     ((eq? keysym xk_right) 1)
					     (else #f)))))
		       (set! cursor-line (car line-char))
		       (set! cursor-char (cadr line-char))
		       (set! selection #f)
		       (set-attributes drawing object
			   '(mouse-edit)
			   `(cursor ,cursor-line ,cursor-char)
			   '(highlight))
		       (read-cursor)))

	  ;;; Get current cursor position and highlight information from
	  ;;; the drawing.
	  (let ((cursor-line-char (get-attribute drawing object 'cursor))
		(l-c-l-c (get-attribute drawing object 'highlight)))
	       (set! cursor-line (car cursor-line-char))
	       (set! cursor-char (cadr cursor-line-char))
	       (when (not (negative? (cadr l-c-l-c)))
		     (set! selection #t)
		     (set! begin-line (car l-c-l-c))
		     (set! begin-char (cadr l-c-l-c))
		     (set! end-line (caddr l-c-l-c))
		     (set! end-char (cadddr l-c-l-c))))
	  
	  ;;; Install event handlers.
	  (ezd '(save-drawing)
	       `(set-drawing ,drawing)
	       `(when * enter ,enter)
	       `(when * exit ,exit)
	       `(when * button1down ,button1down)
	       `(when * motion ,motion-button1up)
	       `(when * button1up ,motion-button1up)
	       `(when * keypress ,keypress)
	       '(restore-drawing))))

;;; Booleans for comparing line/character positions.

(define (LC=? l0 c0 l1 c1) (and (eq? l0 l1) (eq? c0 c1)))

(define (LC<? l0 c0 l1 c1) (or (< l0 l1) (and (eq? l0 l1) (< c0 c1))))

(define (LC>? l0 c0 l1 c1) (or (> l0 l1) (and (eq? l0 l1) (> c0 c1))))

(define (LC<=? l0 c0 l1 c1) (not (lc>? l0 c0 l1 c1)))

(define (LC>=? l0 c0 l1 c1) (not (lc<? l0 c0 l1 c1)))

;;; Procedure to move a line-character position either a number of lines or a
;;; number of characters.  A list consisting of the new line and character
;;; positions is returned.

(define (INC-LINE-CHAR drawing object line char delta-line delta-char)
    (if delta-line
	(let* ((line (min (get-attribute drawing object 'lines)
			  (max 0 (+ line delta-line))))
	       (text (get-attribute drawing object `(text-line ,line)))
	       (text-len (string-length text))
	       (char (min char text-len)))
	      (list line char))
	(let* ((text (get-attribute drawing object `(text-line ,line)))
	       (text-len (string-length text))
	       (char (+ delta-char char)))
	      (cond ((negative? char)
		     (inc-line-char drawing object line 1000000 -1 #f))
		    ((<= char text-len)
		     (list line char))
		    (else (inc-line-char drawing object line 0 1 #f))))))
