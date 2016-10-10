;;; ezd - easy drawing for X11 displays.
;;;
;;; Procedures in this module are responsible for managing text stored in
;;; a justified text tree.

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

(module jtextree (with xlib))

(include "struct.sch")
(include "textree.sch")
(include "display.sch")
(include "xternal.sch")

;;; A justified text tree is represented by an instance of the JTEXTREE
;;; structure which contains the following fields.
;;;
;;;	TEXTREE		textree representing the text.  Besides visible
;;;			characters, a line of text may also contain spaces,
;;;			tabs, newline characters and markers (char code = #x1-)
;;;	LINES		# of lines in the textree
;;;	FIRST-CHANGED	index of first changed line
;;;	LAST-CHANGED	index of last changed line
;;;	MARKERS		list of markers describing points in the text.
;;;	WIDTH		width to justify the text (in pixels).  If this is
;;;			false, then no justification is done.
;;;	FONT		X font structure for the font

(define-structure JTEXTREE
    (textree '())
    (lines 0)
    (first-changed #f)
    (last-changed #f)
    (markers '())
    (width #f)
    (font #f))

(define-in-line-structure-access JTEXTREE textree lines first-changed
    last-changed markers width font)

;;; A marker in the text is defined by a MARKER entry of the following
;;; form:

(define-structure MARKER
    name
    line
    char
    (changed #f))

(define-in-line-structure-access MARKER name line char changed)

;;; The change record in a JTEXTREE is cleared by calling the following
;;; procedure.

(define (CLEAR-CHANGES-JTEXTREE jt)
    (jtextree-first-changed! jt #f)
    (jtextree-last-changed! jt #f)
    (for-each
	(lambda (marker) (marker-changed! marker #f))
	(jtextree-markers jt)))

;;; Changes are logged in the JTEXTREE structure by calling the following
;;; procedure.

(define (CHANGED-LINE-JTEXTREE jt line)
    (cond ((not (jtextree-first-changed jt))
	   (jtextree-first-changed! jt line)
	   (jtextree-last-changed! jt line))
	  ((< line (jtextree-first-changed jt))
	   (jtextree-first-changed! jt line))
	  ((> line (jtextree-last-changed jt))
	   (jtextree-last-changed! jt line))))

;;; A portion of a JTEXTREE can be justified by calling the following
;;; procedure.  Lines changed will be reflected in the JTEXTREE structure.

(define (JUSTIFY-JTEXTREE jt line line-count)
    (if (and (positive? line-count) (< line (jtextree-lines jt))
	     (jtextree-width jt))
	(case (justify-jtextree-line jt line)
	      ((-1) (justify-jtextree jt line line-count))
	      ((0) (justify-jtextree jt (+ line 1) (- line-count 1)))
	      ((1) (justify-jtextree jt (+ line 1) line-count)))))

;;; Justify a single line in a JTEXTREE.  It will return with the number of
;;; lines that it changed the JTEXTREE structure by.  Changed lines will be
;;; noted in the structure as needed.

(define (JUSTIFY-JTEXTREE-LINE jt line)
    (let* ((width (jtextree-width jt))
	   (font (jtextree-font jt))
	   (old-text (jtextree-text jt line))
	   (old-text-len (string-length old-text))
	   (space-width (xtextwidth font " " 1)))
	  
	  ;;; Delete the next line and append it to the existing line when at
	  ;;; least one token will fit.  One or two spaces of padding will be
	  ;;; added as needed.
	  
	  (define (JOIN-LINE left)
		  (let* ((next-text (jtextree-text jt (+ line 1)))
			 (next-len (string-length next-text))
			 (next-width (car (token-size next-text next-len width
					      0 0)))
			 (pad (if (or (eq? old-text "") (eq? next-text ""))
				  0
				  (let ((last-char (string-ref old-text
						       (- old-text-len 1))))
				       (cond ((or (char-whitespace? last-char)
						  (char-whitespace?
						      (string-ref next-text
							  0)))
					      0)
					     ((eq? last-char #\.)
					      2)
					     (else 1))))))
			
			(if (> (+ next-width (* pad space-width)) left)
			    0
			    (let loop ((markers (jtextree-markers jt)))
				 (if (pair? markers)
				     (let* ((m (car markers))
					    (ml (marker-line m))
					    (mc (marker-char m)))
					   (loop (cdr markers))
					   (cond ((eq? ml line)
						  (marker-char! m mc))
						 ((eq? ml (+ line 1))
						  (marker-line! m line)
						  (marker-char! m
						      (+ old-text-len mc pad))
						  (marker-changed! m #t)))
					   -1)
				     (begin (delete-jtextree jt (+ line 1) 0
						(+ line 1)
						(string-length next-text) #f)
					    (insert-jtextree jt line
						old-text-len
						(case pad
						      ((0) next-text)
						      ((1) (string-append " "
							       next-text))
						      ((2) (string-append "  "
							       next-text)))
						#f)
					    -1))))))
	  
	  ;;; Break the existing line at the character index and insert the
	  ;;; rest of the text in a new line that follows.  Trailing and
	  ;;; leading spaces in the middle of a line are deleted.
	  
	  (define (BREAK-LINE charx)
		  (let ((last (let loop ((x (- charx 1)))
				   (if (and (positive? x)
					    (eq? (string-ref old-text x)
						 #\space))
				       (loop (- x 1))
				       x)))
			(first (let loop ((x charx))
				    (if (and (< x old-text-len)
					     (eq? (string-ref old-text x)
						  #\space))
					(loop (+ x 1))
					x))))
		       
		       (define (FIX-MARKER m)
			       (cond ((and (eq? (marker-line m) line)
					   (>= (marker-char m) first))
				      (marker-changed! m #t)
				      (marker-line! m (+ line 1))
				      (marker-char! m
					  (- (marker-char m) first)))
				     ((and (eq? (marker-line m) line)
					   (> (marker-char m) last))
				      (marker-changed! m #t)
				      (marker-char! m (+ last 1)))
				     ((> (marker-line m) line)
				      (marker-changed! m #t)
				      (marker-line! m (+ (marker-line m) 1)))))
		       
		       (delete-jtextree jt line (+ last 1)
			   line old-text-len #f)
		       (jtextree-textree! jt
			   (insert-textree (jtextree-textree jt) (+ line 1)
			       (if (eq? old-text-len first)
				   ""
				   (substring old-text first old-text-len))))
		       (changed-line-jtextree jt (+ line 1))
		       (jtextree-lines! jt
			   (+ (jtextree-lines jt) 1))
		       (for-each fix-marker (jtextree-markers jt))
		       1))
	  
	  ;;; Compute the size of a token and return the width in pixels, and
	  ;;; the incrments to charx and tabx.
	  
	  (define (TOKEN-SIZE text text-len line-width charx tabx)
		  (case (and (< charx text-len) (string-ref text charx))
			((#\space)
			 (list space-width 1 1))
			((#\tab)
			 (list (xtextwidth font "        "
				   (- 8 (remainder tabx 8)))
			       1 (- 8 (remainder tabx 8))))
			((#\newline)
			 (list (- width line-width) 1 1))
			((#f)
			 (list 0 0 0))
			(else (let loop ((x charx))
				   (if (and (< x text-len)
					    (not (char-whitespace?
						     (string-ref text x))))
				       (loop (+ x 1))
				       (list (xtextwidth font
						 (substring text charx x)
						 (- x charx))
					     (- x charx) (- x charx)))))))
	  
	  ;;; Step across the line looking to break or join as needed.
	  (let loop
	       ((line-width
		    (if (eq? line "")
			0
			(let ((dim (cadddr (xtextextents font old-text 1))))
			     (- (xcharstruct-lbearing dim)))))
		(charx 0)
		(tabx 0))
	       (let* ((cwidth-charx-tabx (token-size old-text old-text-len
					     line-width charx tabx))
		      (cwidth (car cwidth-charx-tabx))
		      (delta-charx (cadr cwidth-charx-tabx))
		      (delta-tabx (caddr cwidth-charx-tabx)))
		     (cond ((and (< cwidth (- width line-width))
				 (< charx old-text-len))
			    ;;; Step to the next token
			    (loop (+ line-width cwidth) (+ charx delta-charx)
				  (+ tabx delta-tabx)))
			   ((eq? charx old-text-len)
			    ;;; Out of text...
			    (if (or (eq? cwidth (- width line-width))
				    (eq? line (- (jtextree-lines jt) 1)))
				;;; Exact match or end of text buffer.
				0
				;;; Try to join with the next line.
				(join-line (- width line-width))))
			   ((zero? charx)
			    ;;; First token too large, break after it.
			    (break-line delta-charx))
			   ((eq? cwidth (- width line-width))
			    ;;; Exact fit if we take this one.
			    (if (eq? (+ charx delta-charx) old-text-len)
				0
				(break-line (+ charx delta-charx))))
			   ;;; Next token won't fit.
			   (else (break-line charx)))))))

;;; Before a line can be printed, all tabs must be expanded and newline and
;;; mark characters deleted.  This is done by the following procedure.

(define (JTEXTREE-EXPANDED-TEXT jt linex)
    (let* ((line (textree-text (jtextree-textree jt) linex))
	   (buffer #f)
	   (len (string-length line)))
	  (let loop ((i 0) (j 0))
	       (if (eq? i len)
		   (set! buffer (make-string j #\space))
		   (let ((char (string-ref line i)))
			(cond ((char>=? char #\space)
			       (loop (+ i 1) (+ j 1))
			       (string-set! buffer j char))
			      ((eq? char #\tab)
			       (loop (+ i 1) (+ j (- 8 (remainder j 8)))))
			      (else (loop (+ i 1) j))))))
	  buffer))

;;; A character index in the JTEXTREE text is converted to an X pixel offset
;;; in the displayed text by the following procedure.

(define (TEXTI->PIXEL-JTEXTREE jt linex charx)
    (let* ((line (textree-text (jtextree-textree jt) linex))
	   (font (jtextree-font jt))
	   (len (string-length line))
	   (buffer (make-string 1)))
	  (let loop ((i 0) (j 0) (pixels 0)
		     (lb (if (eq? line "")
			     0
			     (let ((dim (cadddr (xtextextents font line 1))))
				  (- (xcharstruct-lbearing dim))))))
	       (if (or (eq? i charx) (eq? i len))
		   pixels
		   (let ((char (string-ref line i)))
			(cond ((char>=? char #\space)
			       (string-set! buffer 0 char)
			       (loop (+ i 1) (+ j 1)
				     (+ pixels lb (xtextwidth font buffer 1))
				     0))
			      ((eq? char #\tab)
			       (loop (+ i 1)
				     (+ j (- 8 (remainder j 8)))
				     (+ pixels lb (xtextwidth font "        "
						      (- 8 (remainder j 8))))
				     0))
			      (else (loop (+ i 1) j pixels lb))))))))
	
;;; A pixel index into a displayed line is converted to a character index in
;;; the JTEXTREE text line by the following procedure.

(define (PIXEL->TEXTI-JTEXTREE jt linex pixel)
    (let* ((line (textree-text (jtextree-textree jt) linex))
	   (font (jtextree-font jt))
	   (len (string-length line))
	   (buffer (make-string 1)))
	  (let loop ((i 0) (j 0)
		     (width (+ pixel
			       (if (eq? line "")
				   0
				   (let ((dim (cadddr (xtextextents font line
							  1))))
					(xcharstruct-lbearing dim))))))
	       (if (eq? i len)
		   i
		   (let ((char (string-ref line i))
			 (cwidth 0)
			 (j j))
			(cond ((char>=? char #\space)
			       (string-set! buffer 0 char)
			       (set! j (+ j 1))
			       (set! cwidth (xtextwidth font buffer 1)))
			      ((eq? char #\tab)
			       (set! j (+ j (- 8 (remainder j 8))))
			       (set! cwidth (xtextwidth font "        "
						(- 8 (remainder j 8))))))
			(if (< width (quotient cwidth 2))
			    i
			    (loop (+ i 1) j (- width cwidth))))))))

;;; Text is inserted into a JTEXTREE before a given line and character by
;;; calling the following procedure.  Markers will be corrected as required.
;;; Justification is an option as this procedure is called from inside the
;;; justifier.

(define (INSERT-JTEXTREE jt line char text justify)
    (let ((lines (jtextree-lines jt))
	  (textree (jtextree-textree jt)))      
	 (if (>= line lines)
	     (begin (jtextree-lines! jt (+ 1 lines))
		    (jtextree-textree! jt (insert-textree textree line text)))
	     (let* ((old-text (textree-text textree line))
		    (old-len (string-length old-text)))
		   (jtextree-textree! jt
		       (textree-text! textree line
			   (string-append
			       (substring old-text 0 char)
			       text
			       (if (>= char old-len)
				   ""
				   (substring old-text char old-len)))))))
	 (for-each
	     (lambda (marker)
		     (when (and (eq? (marker-line marker)
				     line)
				(>= (marker-char marker)
				    char))
			   (marker-char! marker
			       (+ (marker-char marker)
				  (string-length text)))
			   (marker-changed! marker #t)))
	     (jtextree-markers jt))
	 (changed-line-jtextree jt line)
	 (when justify
	       (if (positive? line)
		   (justify-jtextree jt (- line 1) 2)
		   (justify-jtextree jt line 1))
	       (if (> (jtextree-lines jt) lines)
		   (jtextree-last-changed! jt (- (jtextree-lines jt) 1))))))

;;; All text from one character position through another is deleted from a
;;; JTEXTREE by the following procedure.  Justification is an option as this
;;; procedure is called from inside the justifier.  Markers outside the range
;;; are corrected.  Markers inside the range are left unchanged, but the change
;;; flag is set.

(define (DELETE-JTEXTREE jt line0 char0 line1 char1 justify)
    (let* ((textree (jtextree-textree jt))
	   (lines (jtextree-lines jt))
	   (deleted-lines 0)
	   (text0 (textree-text textree line0))
	   (text1 (textree-text textree line1))
	   (len1 (string-length text1))
	   (new-text (string-append (substring text0 0 char0)
			 (if (>= char1 (- len1 1))
			     ""
			     (substring text1 (+ 1 char1) len1)))))
	  
	  (define (DELETE-LINE line)
		  (when (< line (- lines deleted-lines))
			(set! textree (delete-textree textree line))
			(set! deleted-lines (+ 1 deleted-lines))))
	  
	  (do ((i (+ line0 1) (+ i 1)))
	      ((> i line1))
	      (delete-line (+ 1 line0)))
	  (if (eq? new-text "")
	      (delete-line line0)
	      (set! textree (textree-text! textree line0 new-text)))
	  (for-each
	      (lambda (marker)
		      (let ((mline (marker-line marker))
			    (mchar (marker-char marker)))
			   (cond ((or (< mline line0)
				      (and (eq? mline line0) (< mchar char0)))
				  #t)
				 ((> mline line1)
				  (marker-line! marker (- mline deleted-lines))
				  (marker-changed! marker #t))
				 ((and (eq? mline line1) (> mchar char1))
				  (if (and (eq? new-text "") (> line0 0))
				      (let ((prev-text (textree-text textree
							   (- line0 1))))
					   (marker-line! marker (- line0 1))
					   (marker-char! marker
					       (string-length prev-text)))
				      (begin (marker-char! marker
						 (- mchar char1 (- char0) 1))
					     (marker-line! marker
						 (max 0 (- mline
							   deleted-lines)))))
				  (marker-changed! marker #t))
				 (else (marker-changed! marker #t)))))
	      (jtextree-markers jt))
	  (jtextree-lines! jt (- lines deleted-lines))
	  (jtextree-textree! jt textree)
	  (changed-line-jtextree jt line0)
	  (when justify
		(if (positive? line0)
		    (justify-jtextree jt (- line0 1) 2)
		    (justify-jtextree jt line0 1))
		(if (< (jtextree-lines jt) lines)
		    (jtextree-last-changed! jt (- (jtextree-lines jt) 1))))))

;;; Lines of text in a JTEXTREE may be accessed by the following two
;;; procedures.  Changed lines will be noted in the JTEXTREE structure.

(define (JTEXTREE-TEXT jt line)
    (textree-text (jtextree-textree jt) line))

(define (JTEXTREE-TEXT! jt line text)
    (jtextree-textree! jt (textree-text! (jtextree-textree jt) line text))
    (changed-line-jtextree jt line))
