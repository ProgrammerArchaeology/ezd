;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module implement TEXT-DRAWINGs.  A TEXT-DRAWING is
;;; a drawing that displays a document.  Like any other drawing, it may be
;;; displayed in multiple windows.

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

(module textdrawing (with jtextree mouseedit ezd))

(include "struct.sch")
(include "match.sch")
(include "jtextree.sch")
(include "mouseedit.sch")
(include "commands.sch")
(include "ginfo.sch")
(include "ezd.sch")
(include "display.sch")
(include "drawing.sch")
(include "window.sch")
(include "view.sch")
(include "events.sch")
(include "interact.sch")
(include "xternal.sch")

;;; The basic data structure used is a TEXT-DRAWING containing the following
;;; fields:
;;;
;;;	NAME			name of the drawing
;;;	WIDTH			width of a text line in pixels
;;;	TEXT-COLOR		color to draw text in
;;;	TEXT-STIPPLE		stipple to draw text with
;;;	FONT			text font to use
;;;	CUSOR-COLOR		color to draw the cursor in
;;;	HIGHLIGHT-COLOR 	color to draw the highlighted area in
;;;	HIGHLIGHT-STIPPLE	stipple to draw the highlight in
;;;	OPTIONS			list of any of UNJUSTIFIED or READ-ONLY.
;;;	BEGIN-HIGHLIGHT		marker for the begining of the highlight
;;;	END-HIGHLIGHT		marker for the end of the highlight
;;;	DXFS			font's xfontstruct
;;;	ROW-HEIGHT		height of each row of text
;;;	CURSOR-FONT		font to display the "^" cursor in
;;;	CURSOR			marker for the cursor
;;;	TEXT-DELTA-X		delta x from origin position to draw text
;;;	CURSOR-DELTA-Y		delta y from character position to draw cursor
;;;	VIEWS			list of triples of window, first and last lines
;;;	VISIBLE-LINES		ordered list of visible line ranges, where
;;;				each range is a list of first and last lines
;;;	JTEXTREE		JTEXTREE that holds the text

(define-structure TEXT-DRAWING
    name
    width
    text-color
    text-stipple
    font
    cursor-color
    highlight-color
    highlight-stipple
    options
    (begin-highlight (make-marker 'begin 0 -1))
    (end-highlight (make-marker 'end 0 -1))
    (dxfs (display-font->xfontstruct *display* font))
    (row-height (+ (xfontstruct-ascent (text-drawing-dxfs self))
		   (xfontstruct-descent (text-drawing-dxfs self))))
    (cursor-font "8x13bold")
    (cursor (make-marker 'cursor 0 -1))
    (text-delta-x (let* ((fs (display-font->xfontstruct *display*
				 (text-drawing-cursor-font self)))
			 (cs (cadddr (xtextextents fs "^" 1))))
			(quotient (xcharstruct-width cs) 2)))
    (cursor-delta-y (xfontstruct-ascent (text-drawing-dxfs self)))
    (views '())
    (visible-lines '())
    (jtextree (let ((jtextree (make-jtextree)))
		   (text-drawing-width! self width)
		   (jtextree-width! jtextree
		       (and (not (memq 'UNJUSTIFIED options))
			    (- width (* (text-drawing-text-delta-x self) 2))))
		   (jtextree-font! jtextree (text-drawing-dxfs self))
		   (if (not text-color)
		       (text-drawing-text-color! self 'black))
		   (if (not highlight-color)
		       (text-drawing-highlight-color! self 'gray95))
		   (jtextree-markers! jtextree
		       (list (text-drawing-begin-highlight self)
			     (text-drawing-end-highlight self)
			     (text-drawing-cursor self)))
		   (ezd '(save-drawing)
			`(set-drawing ,name)
			'(object text-drawing
				 (fill-rectangle 0 0 1000000 1000000 clear))
			'(object highlight)
			'(object cursor)
			`(when * visible
			       ,(lambda () (text-drawing-visible self)))
			`(when * set-attributes
			       ,(lambda () (text-drawing-set-attributes self)))
			`(when * get-attributes
			       ,(lambda () (text-drawing-get-attributes self)))
			'(restore-drawing))
		   (mouse-edit-init name 'text-drawing options)
		   jtextree)))

;;; Each view of a TEXT-DRAWING is represented by a TEXT-VIEW record with the
;;; following fields.
;;;
;;;	WINDOW		window-name
;;;	X		overlay position in pixels in the window
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	FIRST		first line visible
;;;	LAST		last line visible
;;;	SLIDER		name of the slider drawing associated with the view

(define-structure TEXT-VIEW
    window
    x
    y
    width
    height
    first
    last
    (slider #f))

(define-in-line-structure-access TEXT-VIEW window x y width height first last
    slider)

;;; A TEXT-DRAWING is created by the following ezd command.

(define (TEXT-DRAWING-OPTION? x) (memq x '(read-only unjustified)))

(define-ezd-command
    `(text-drawing ,symbol? ,positive-number? (optional POINTS)
	 (optional ,color?) (optional ,stipple?) (optional ,string?)
	 (optional ,color?) (optional ,color?) (optional ,stipple?)
	 (repeat ,text-drawing-option?))
    "(text-drawing name width [points] [color] [stipple] [\"font\"] [cursor-color [highlight-color] [highlight-stipple]] [READ-ONLY] [UNJUSTIFIED])"
    (lambda (name width points color stipple font cursor highlight
		  highlight-stipple options)
	    (make-text-drawing name (if points (points->pixels width) width)
		color stipple font cursor highlight highlight-stipple
		options)))
		
;;; When a portion of the TEXT-DRAWING is displayed in a view, ezd notifies
;;; the drawing by sending it a VISIBLE event.  Origin changes are also visible
;;; here and will result in changes to any sliders as needed.

(define (TEXT-DRAWING-VISIBLE self)
    (let ((row-height (text-drawing-row-height self))
	  (jt (text-drawing-jtextree self)))
	 
	 ;;; Turn the current event into a text-view iff it's not null.
	 (define (MAKE-VIEW old)
		 (let ((x (list-ref *user-event-misc* 0))
		       (y (list-ref *user-event-misc* 1))
		       (width (list-ref *user-event-misc* 2))
		       (height (list-ref *user-event-misc* 3)))
		      (if y
			  (let ((first (quotient (+ y (- row-height 1))
					   row-height))
				(last (quotient
					  (- (+ y height) (- row-height 1))
					  row-height)))
			       (if (>= last first)
				   (if old
				       (let ((slider (text-view-slider old))
					     (was-range (- (text-view-last
							       old)
							   (text-view-first
							       old)
							   -1))
					     (is-range (- last first -1)))
					    (text-view-width! old width)
					    (text-view-height! old height)
					    (text-view-first! old first)
					    (text-view-last! old last)
					    (if (and slider
						     (not (eq? was-range
							       is-range)))
						(ezd `(set-attributes
							  ,slider
							  slider
							  (value ,first)
							  (indicator-size
							      ,is-range))))
					    old)
				       (make-text-view *user-event-window*
					   x y width height first last))
				   #f))
			  (begin (if (and old (text-view-slider old))
				     (ezd `(delete-view ,*user-event-window*
					       ,(text-view-slider old))))
				 
				 #f))))
	 
	 ;;; Correct the views list.
	 (text-drawing-views! self
	     (let loop ((views (text-drawing-views self)))
		  (if (pair? views)
		      (if (eq? *user-event-window*
			       (text-view-window (car views)))
			  (let ((view (make-view (car views))))
			       (if view
				   (cons view (cdr views))
				   (loop (cdr views))))
			  (cons (car views) (loop (cdr views))))
		      (let ((view (make-view #f)))
			   (if view (list view) '())))))
	 (text-drawing-compute-visible-lines self)))

;;; When a view is added or deleted, or a view is scrolled, the following
;;; procedure is called to recompute (and redraw) the lines visible in the
;;; drawing.

(define (TEXT-DRAWING-COMPUTE-VISIBLE-LINES self)
    (let ((was-visible (text-drawing-visible-lines self))
	  (row-height (text-drawing-row-height self)))    
	 
	 ;;; Deleted lines from the drawing.
	 (define (DELETED-LINES f l)
		 (ezd '(save-drawing)
		      `(set-drawing ,(text-drawing-name self)))
		 (do ((i f (+ i 1)))
		     ((> i l))
		     (ezd `(object ,(string->symbol (format "T~s" i)))))
		 (ezd '(restore-drawing)))
	 
	 ;;; 1. Recompute the visible lines.
	 (text-drawing-visible-lines! self
	     (let loop ((views (text-drawing-views self)) (lines '()))
		  (if (pair? views)
		      (loop (cdr views)
			    (let loop ((first (text-view-first (car views)))
				       (last (text-view-last (car views)))
				       (lines lines))
				 (if (pair? lines)
				     (let ((fl (caar lines))
					   (ll (cadar lines)))
					  (cond ((< last fl)
						 (cons `(,first ,last) lines))
						((> first ll)
						 (cons (car lines)
						       (loop first last
							     (cdr lines))))
						(else (loop (min first fl)
							    (max last ll)
							    (cdr lines)))))
				     `((,first ,last)))))
		      lines)))
	 
	 ;;; 2. Display newly visible lines, delete no longer visible lines.
	 (let loop ((was was-visible) (is (text-drawing-visible-lines self)))
	      (cond ((and (pair? was) (pair? is))
		     (let ((was-f (caar was))
			   (was-l (cadar was))
			   (is-f (caar is))
			   (is-l (cadar is)))
			  (cond ((eq? was-f is-f)
				 (cond ((< was-l is-l)
					(loop (cdr was)
					      (cons `(,(+ was-l 1) ,is-l)
						    (cdr is))))
				       ((> was-l is-l)
					(loop (cons `(,(+ is-l 1) ,was-l)
						    (cdr was))
					      (cdr is)))
				       (else (loop (cdr was) (cdr is)))))
				((< was-l is-f)
				 (deleted-lines was-f was-l)
				 (loop (cdr was) is))
				((< is-l was-f)
				 (text-drawing-draw-lines self is-f is-l)
				 (loop was (cdr is)))
				((< was-f is-f)
				 (loop `((,was-f ,(- is-f 1))
					 (,is-f ,was-l)
					 ,@(cdr was))
				       is))
				(else (loop was
					    `((,is-f ,(- was-f 1))
					      (,was-f ,is-l)
					      ,@(cdr is)))))))
		    ((pair? is)
		     (text-drawing-draw-lines self (caar is) (cadar is))
		     (loop was (cdr is)))
		    ((pair? was)
		     (deleted-lines (caar was) (cadar was))
		     (loop (cdr was) is))))))

;;; Lines of text existing in the document and visible in some view are drawn
;;; by the following procedure.

(define (TEXT-DRAWING-DRAW-LINES self first last)
    (let ((row-height (text-drawing-row-height self))
	  (xpad (text-drawing-text-delta-x self))
	  (color (text-drawing-text-color self))
	  (stipple (if (text-drawing-text-stipple self)
		       (list (text-drawing-text-stipple self))
		       '()))
	  (font (if (text-drawing-font self)
		    (list (text-drawing-font self))
		    '()))
	  (jt (text-drawing-jtextree self)))
	 (ezd '(save-drawing)
	      `(set-drawing ,(text-drawing-name self)))
	 (let loop ((i first) (visible (text-drawing-visible-lines self)))
	      (if (and (pair? visible) (<= i last))
		  (let ((f (caar visible))
			(l (cadar visible)))
		       (cond ((< i f) (loop f visible))
			     ((> i l) (loop i (cdr visible)))
			     (else (ezd `(object ,(string->symbol
						      (format "T~s" i))
						 (text ,xpad
						       ,(* row-height i)
						       ,(jtextree-expanded-text
							    jt i)
						       ,color
						       ,@stipple
						       ,@font)))
				   (loop (+ i 1) visible))))))
	 (ezd '(restore-drawing))))

;;; Information can be extracted from the TEXT-DRAWING object via ezd's
;;; attribute mechanism.  The following attributes may be read:
;;;
;;;	WIDTH		width in pixels
;;;	TEXT-COLOR
;;;	TEXT-STIPPLE
;;;	FONT
;;;	CURSOR-COLOR
;;;	HIGHLIGHT-COLOR
;;;	HIGHLIGHT-STIPPLE
;;;	OPTIONS
;;;	ROW-HEIGHT	height in pixels of each row
;;;	CURSOR		list of cursor line and char
;;;	HIGHLIGHT	line/character position or #f
;;;	LINES		# of lines in the document
;;;	(TEXT-LINE x)	contents of text line x
;;;	(VIEW window)	first, last & slider for the view or #f.
;;;	(XY->LINE-CHAR-TEXT x y) convert drawing coordinate to line/character
;;;				 position and contents of line.
;;;
;;;	ATTRIBUTES	list of all attributes that can be either read or set.

(define (TEXT-DRAWING-GET-ATTRIBUTES self)
    (map (lambda (a)
		 (cond ((eq? a 'WIDTH)
			(text-drawing-width self))
		       ((eq? a 'TEXT-COLOR)
			(text-drawing-text-color self))
		       ((eq? a 'TEXT-STIPPLE)
			(text-drawing-text-stipple self))
		       ((eq? a 'FONT)
			(text-drawing-font self))
		       ((eq? a 'CURSOR-COLOR)
			(text-drawing-cursor-color self))
		       ((eq? a 'HIGHLIGHT-COLOR)
			(text-drawing-highlight-color self))
		       ((eq? a 'HIGHLIGHT-STIPPLE)
			(text-drawing-highlight-stipple self))
		       ((eq? a 'OPTIONS)
			(text-drawing-options self))
		       ((eq? a 'ROW-HEIGHT)
			(text-drawing-row-height self))
		       ((eq? a 'CURSOR)
			(if (marker-line (text-drawing-cursor self))
			    (list (marker-line (text-drawing-cursor self))
				  (marker-char (text-drawing-cursor self)))
			    #f))
		       ((eq? a 'HIGHLIGHT)
			(if (marker-line (text-drawing-begin-highlight self))
			    (list (marker-line
				      (text-drawing-begin-highlight self))
				  (marker-char
				      (text-drawing-begin-highlight self))
				  (marker-line
				      (text-drawing-end-highlight self))
				  (marker-char
				      (text-drawing-end-highlight self)))
			    #f))
		       ((eq? a 'LINES)
			(jtextree-lines (text-drawing-jtextree self)))
		       ((match? (TEXT-LINE non-negative?) a)
			(jtextree-text (text-drawing-jtextree self) (cadr a)))
		       ((match? (VIEW symbol?) a)
			(let loop ((tvl (text-drawing-views self)))
			     (if (pair? tvl)
				 (let ((tv (car tvl)))
				      (if (eq? (cadr a) (text-view-window tv))
					  (list (text-view-first tv)
						(text-view-last tv)
						(text-view-slider tv))
					  (loop (cdr tvl))))
				 #f)))
		       ((match? (XY->LINE-CHAR-TEXT non-negative?
				    non-negative?) a)
			(let* ((jt (text-drawing-jtextree self))
			       (line (min (quotient (caddr a)
					      (text-drawing-row-height self))
					  (jtextree-lines jt)))
			       (char (pixel->texti-jtextree jt line
					 (- (cadr a)
					    (text-drawing-text-delta-x self))))
			       (text (jtextree-text jt line)))
			      (list line char text)))
		       ((eq? a 'ATTRIBUTES)
			'(width text-color text-stipple font cursor-color
				highlight-color highlight-stipple options
				row-height cursor highlight lines text-line
				xy->line-char-text insert delete view
				scroll delete-view delete-object attributes))
		       (else (ezd-error 'TEXT-DRAWING "Invalid attribute: ~s"
				 a))))
	 *user-event-misc*))

;;; A TEXT-DRAWING is changed by setting its attributes.  The following
;;; attributes may be set:
;;;
;;;	(INSERT "string")	insert text at the end of the document.
;;;	(INSERT line char "string")
;;;				insert text before the specified line and
;;;				character positions.  Note that line and
;;;				character indices begin at 0.
;;;
;;;	(DELETE line0 char0 line1 char1)
;;;				deletes a range of text, including the end
;;;				points.
;;;	(DELETE line char END)  delete from starting position through the
;;;				end of the document.
;;;
;;;	(CURSOR)		turns off cursor display
;;;	(CURSOR line char)	sets the cursor position
;;;
;;;	(HIGHLIGHT)		turns off the highlight
;;;	(HIGHLIGHT line0 char0 line0 char1)
;;;				highlights a range of text including the end
;;;				points.
;;;
;;;	(VIEW window x y width height slider-width)
;;;				create a view in that window of the designated
;;;				size.  If slider-width is non-zero, then that
;;;				much area of the view will be allocated for a
;;;				slider.
;;;
;;;	(SCROLL window line)	scroll the view in the designated window so
;;;				that the designated line is the first line
;;;				visible.
;;;
;;;	(DELETE-VIEW window)	delete a view
;;;
;;;	(DELETE-OBJECT)		delete the drawing
;;;
;;;	(MOUSE-EDIT)		indicates changes are coming from the mouse
;;;				based editor so it need not be initialized.

(define (TEXT-DRAWING-SET-ATTRIBUTES self)
    (let* ((jt (text-drawing-jtextree self))
	   (was-lines (jtextree-lines jt))
	   (mouse-edit #f))
	  
	  (DEFINE (SET-CURSOR l c)
		  (let ((cursor (text-drawing-cursor self)))
		       (marker-line! cursor l)
		       (marker-char! cursor c)
		       (marker-changed! cursor #t)))
	  
	  (define (SET-HIGHLIGHT line0 char0 line1 char1)
		  (let ((begin-highlight (text-drawing-begin-highlight self))
			(end-highlight (text-drawing-end-highlight self)))
		       (marker-line! begin-highlight line0)
		       (marker-char! begin-highlight char0)
		       (marker-changed! begin-highlight #t)
		       (marker-line! end-highlight line1)
		       (marker-char! end-highlight char1)
		       (marker-changed! end-highlight #t)))
	  
	  (for-each
	      (lambda (a)
		      (cond ((match? (INSERT string?) a)
			     (insert-jtextree jt (jtextree-lines jt) 0
				 (cadr a) #t))
			    ((match? (INSERT non-negative? non-negative?
					     string?) a)
			     (insert-jtextree jt (cadr a) (caddr a) (cadddr a)
				 #t))
			    ((match? (DELETE non-negative? non-negative?
					     non-negative? non-negative?)
				     a)
			     (delete-jtextree jt (list-ref a 1) (list-ref a 2)
				 (list-ref a 3) (list-ref a 4) #t))
			    ((match? (DELETE non-negative? non-negative?
					     (lambda (x) (eq? x 'END)))
				     a)
			     (delete-jtextree jt (list-ref a 1) (list-ref a 2)
				 (jtextree-lines jt) 0 #t))
			    ((match? (CURSOR) a)
			     (set-cursor 0 -1))
			    ((match? (CURSOR non-negative? non-negative?) a)
			     (set-cursor (cadr a) (caddr a)))
			    ((match? (HIGHLIGHT) a)
			     (set-highlight 0 -1 0 -1))
			    ((match? (HIGHLIGHT non-negative? non-negative?
					 non-negative? non-negative?) a)
			     (set-highlight (list-ref a 1) (list-ref a 2)
				 (list-ref a 3) (list-ref a 4)))
			    ((match? (VIEW window-exists? non-negative?
					   non-negative? non-negative?
					   non-negative? non-negative?) a)
			     (text-drawing-new-view self (list-ref a 1)
				 (list-ref a 2) (list-ref a 3) (list-ref a 4)
				 (list-ref a 5) (list-ref a 6)))
			    ((match? (SCROLL window-exists? non-negative?) a)
			     (text-view-scroll self (cadr a) (caddr a)))
			    ((match? (DELETE-VIEW symbol?) a)
			     (ezd `(delete-view ,(cadr a)
				       ,(text-drawing-name self))))
			    ((equal? '(DELETE-OBJECT) a)
			     (for-each
				 (lambda (tv)
					 (ezd `(delete-view
						   ,(text-view-window tv)
						   ,(text-drawing-name self))))
				 (text-drawing-views self))
			     (ezd `(save-drawing)
				  `(set-drawing ,(text-drawing-name self))
				  '(clear)
				  '(restore-drawing)))
			    ((equal? '(MOUSE-EDIT) a)
			     (set! mouse-edit #t))
			    (else (ezd-error 'TEXT-DRAWING
				      "Invalid attribute: ~s" a))))
	      *user-event-misc*)
	  (if (not mouse-edit)
	      (mouse-edit-init (text-drawing-name self) 'text-drawing
		  (text-drawing-options self)))
	  (text-drawing-update-display self was-lines)))

;;; After changes have been made to the display by changing attributes, the
;;; following procedure is called to update the display.

(define (TEXT-DRAWING-UPDATE-DISPLAY self was-lines)
    (let* ((jt (text-drawing-jtextree self)) 
	   (first (jtextree-first-changed jt))
	   (last (jtextree-last-changed jt))
	   (is-lines (jtextree-lines jt))
	   (row-height (text-drawing-row-height self))
	   (text-delta-x (text-drawing-text-delta-x self))
	   (highlight-color (text-drawing-highlight-color self))
	   (highlight-stipple (if (text-drawing-highlight-stipple self)
				  `(,(text-drawing-highlight-stipple
					 self))
				  '()))
	   (cursor (text-drawing-cursor self))
	   (begin-highlight (text-drawing-begin-highlight self))
	   (end-highlight (text-drawing-end-highlight self)))
	  
	  (ezd '(save-drawing)
	       `(set-drawing ,(text-drawing-name self)))
	  ;;; 1. Redraw changed text lines.
	  (if first (text-drawing-draw-lines self first
			(if (eq? is-lines was-lines)
			    last
			    (max last was-lines))))
	  ;;; 2. Change maximum and value on sliders on text size change.
	  (if (not (eq? is-lines was-lines))
	      (for-each
		  (lambda (tv)
			  (if (text-view-slider tv)
			      (let* ((slider (text-view-slider tv))
				     (value (car (get-attributes slider 'slider
						     'value)))
				     (max-value (max 0
						     (- is-lines
							(- (text-view-last tv)
							   (text-view-first
							       tv))
							1))))
				    (if (< max-value value)
					(text-view-scroll self
					    (text-view-window tv)
					    max-value))
				    (set-attributes slider 'slider
					`(value ,(min value max-value))
					`(max-value ,max-value)))))
		  (text-drawing-views self)))
	  ;;; 3. Redraw the cursor.
	  (if (marker-changed cursor)
	      (if (>= (marker-char cursor) 0)
		  (ezd `(object cursor
				(text ,(texti->pixel-jtextree jt
					   (marker-line cursor)
					   (marker-char cursor))
				      ,(+ (* row-height (marker-line cursor))
					  (text-drawing-cursor-delta-y self))
				      "^"
				      ,(or (text-drawing-cursor-color self)
					   (text-drawing-text-color self))
				      ,(text-drawing-cursor-font self))))
		  (ezd `(object cursor))))
	  ;;; 4. Redraw the highlighted area.
	  (if (or (marker-changed begin-highlight)
		  (marker-changed end-highlight))
	      (if (and (>= (marker-char begin-highlight) 0)
		       (>= (marker-char end-highlight) 0))
		  (let* ((line0 (marker-line begin-highlight))
			 (char0 (marker-char begin-highlight))
			 (xchar0 (texti->pixel-jtextree jt line0 char0))
			 (line1 (marker-line end-highlight))
			 (char1 (marker-char end-highlight))
			 (xchar1 (texti->pixel-jtextree jt line1
				     (+ 1 char1)))
			 (width1 (texti->pixel-jtextree jt line1
				     1000000)))
			
			(define (DRAW i)
				`(fill-rectangle
				     ,(+ text-delta-x
					 (if (eq? i line0) xchar0 0))
				     ,(* row-height i)
				     ,(- (texti->pixel-jtextree jt i
					     1000000)
					 (if (eq? i line0) xchar0 0)
					 (if (eq? i line1)
					     (- width1 xchar1)
					     0))
				     ,row-height
				     ,highlight-color
				     ,@highlight-stipple))
			
			(ezd `(object highlight
				      ,@(let loop ((i line0))
					     (if (<= i line1)
						 (cons (draw i) (loop (+ i 1)))
						 '())))))
		  (ezd '(object highlight))))
	  (ezd '(restore-drawing))
	  (clear-changes-jtextree jt)))
	   
;;; A new text view is created when the following procedure is called by
;;; TEXT-DRAWING-SET-ATTRIBUTES.

(define (TEXT-DRAWING-NEW-VIEW self window x y width height slider)
    (let* ((drawing (text-drawing-name self))
	   (slider-name (string->symbol (string-append (symbol->string window)
					    "-" (symbol->string drawing)
					    "-SLIDER")))
	   (lines (quotient height
		      (text-drawing-row-height self)))
	   (document-lines (jtextree-lines (text-drawing-jtextree self))))
	  (ezd `(overlay ,window ,drawing ,(+ x slider) ,y ,(- width slider)
		    ,height))
	  (for-each
	      (lambda (tv)
		      (when (eq? (text-view-window tv) window)
			    (text-view-x! tv (+ x slider))
			    (text-view-y! tv y)))
	      (text-drawing-views self))
	  (when (positive? slider)
		(ezd '(save-drawing)
		     `(set-drawing ,slider-name)
		     `(overlay ,window ,slider-name ,x ,y ,slider ,height)
		     `(origin ,window ,slider-name ,x ,y)
		     `(slider slider 0 0 ,slider ,height ,lines 0
			      ,(max 0 (- document-lines lines)) 0 ,(- lines 1)
			      (ezd `(set-attributes
					,,(list 'quote drawing) text-drawing
					(scroll ,,(list 'quote window)
						,(car *user-event-misc*))))
			      ,(text-drawing-text-color self) s8))
		(for-each
		    (lambda (tv)
			    (if (eq? (text-view-window tv) window)
				(text-view-slider! tv slider-name)))
		    (text-drawing-views self)))
	  (ezd `(origin ,window ,drawing ,(+ x slider) ,y))))

;;; A TEXT-VIEW is scrolled by the following procedure that is called from
;;; TEXT-DRAWING-SET-ATTRIBUTES.

(define (TEXT-VIEW-SCROLL self window line)
    (let* ((new-first (inexact->exact (round line)))
	   (row-height (text-drawing-row-height self)))
	  (for-each
	      (lambda (tv)
		      (when (and (eq? (text-view-window tv) window)
				 (not (eq? (text-view-first tv) new-first)))
			    (ezd `(origin ,window ,(text-drawing-name self)
					  ,(text-view-x tv)
					  ,(+ (text-view-y tv)
					      (* (- row-height) new-first))))))
	      (text-drawing-views self))))
