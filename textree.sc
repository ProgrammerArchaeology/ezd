;;; ezd - easy drawing for X11 displays.
;;;
;;; Text in text areas is stored in a balanced tree composed of strings and
;;; TEXTREE entries.

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

(module textree)

(include "struct.sch")

;;; A TEXTREE entry consists of a structure with the following fields.
;;;
;;;	LEFT-CNT	# of lines in the left subtree.
;;;	HEIGHT		tree height of this node.
;;;	LEFT		left subtree.
;;;	RIGHT		right subtree.

(define-structure TEXTREE
    left-cnt
    height
    left
    right)

(define-in-line-structure-access TEXTREE left-cnt height left right)

;;; The n'th line of text in a balanced tree composed of TEXTREE entries and
;;; strings is returned by the following procedure.

(define (TEXTREE-TEXT tree n)
    (cond ((null? tree) "")
	  ((string? tree) (if (eq? n 0) tree ""))
	  (else (let ((left-cnt (textree-left-cnt tree)))
		     (if (< n left-cnt)
			 (textree-text (textree-left tree) n)
			 (textree-text (textree-right tree)
			     (- n left-cnt)))))))

;;; The n'th line of text in a balanced tree composed of TEXTREE entries and
;;; strings is replaced by the following procedure.  It returns the new tree
;;; in order to cover the degenerate case of a tree consisting of a single
;;; string.

(define (TEXTREE-TEXT! tree n text)
    (if (string? tree)
	text
	(begin (let ((left-cnt (textree-left-cnt tree)))
		    (if (< n left-cnt)
			(let ((left (textree-left tree)))
			     (if (string? left)
				 (textree-left! tree text)
				 (textree-text! left n text)))
			(let ((right (textree-right tree)))
			     (if (string? right)
				 (textree-right! tree text)
				 (textree-text! right (- n left-cnt) text)))))
	       tree)))

;;; A new line is inserted before the n'th line of text in a balanced tree
;;; composed of TEXTREE entries and strings by the following procedure.

(define (INSERT-TEXTREE tree n text)
    (cond ((null? tree) text)
	  ((string? tree)
	   (if (zero? n)
	       (make-textree 1 1 text tree)
	       (make-textree 1 1 tree text)))
	  ((<= n (textree-left-cnt tree))
	   (textree-left-cnt! tree (+ (textree-left-cnt tree) 1))
	   (textree-left! tree (insert-textree (textree-left tree) n text))
	   (adjust-textree tree))
	  (else
	   (textree-right! tree (insert-textree (textree-right tree)
				    (- n (textree-left-cnt tree)) text))
	   (adjust-textree tree))))


;;; The n'th line of text in a balanced tree composed of TEXTREE entries and
;;; strings is deleted by the following procedure.

(define (DELETE-TEXTREE tree n)
    (cond ((string? tree) '())
	  ((< n (textree-left-cnt tree))
	   (let ((left (textree-left tree)))
		(if (string? left)
		    (textree-right tree)
		    (begin (textree-left-cnt! tree
			       (- (textree-left-cnt tree) 1))
			   (textree-left! tree (delete-textree left n))
			   (adjust-textree tree)))))
	  (else
	   (let ((right (textree-right tree)))
		(if (string? right)
		    (textree-left tree)
		    (begin (textree-right! tree
			       (delete-textree right
				   (- n (textree-left-cnt tree))))
			   (adjust-textree tree)))))))

;;; Nodes in a balanced tree composed of TEXTREE entries and strings are
;;; abjusted by the following procedure.  The adjusted tree is the return
;;; value.
    
(define (ADJUST-TEXTREE tree)
    
    (define (COUNT tree)
	    (if (string? tree)
		1
		(+ (textree-left-cnt tree) (count (textree-right tree)))))
    
    (if (string? tree)
	tree
	(let* ((tx-height
		   (lambda (x) (if (string? x) 0 (textree-height x))))
	       (left (textree-left tree))
	       (left-height (tx-height left))
	       (right (textree-right tree))
	       (right-height (tx-height right))
	       (delta (- left-height right-height)))
	      (cond ((>= delta 2)
		     (textree-left! tree (textree-right left))
		     (textree-left-cnt! tree (count (textree-right left)))
		     (textree-height! tree
			 (+ 1 (max (tx-height (textree-left tree))
				   (tx-height (textree-right tree)))))
		     (textree-right! left tree)
		     (textree-height! left
			 (+ 1 (max (tx-height (textree-left left))
				   (tx-height tree))))
		     left)
		    ((<= delta -2)
		     (textree-right! tree (textree-left right))
		     (textree-height! tree
			 (+ 1 (max (tx-height (textree-left tree))
				   (tx-height (textree-right tree)))))
		     (textree-left! right tree)
		     (textree-left-cnt! right (count tree))
		     (textree-height! right
			 (+ 1 (max (tx-height (textree-right right))
				   (tx-height tree))))
		     right)
		    (else (textree-height! tree
			      (+ 1 (max left-height right-height)))
			  tree)))))
