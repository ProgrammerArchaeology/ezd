;;; ezd - easy drawing for X11 displays.
;;;
;;; Structure pretty-printer.

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

(module pps)

;;; (PPS form [ output ]) pretty-prints the form on the current output port,
;;; another port, or to a file depending upon the value of "output".

(define (PPS form . output)
    (let ((form (if (number? form) (vector-ref object-cache form) form)))
	 (cond ((null? output)
		(pp1 form (current-output-port) #f))
	       ((output-port? (car output))
		(pp1 form (car output) #f))
	       (else
		    (let ((port (open-output-file (car output))))
			 (pp1 form port #f)
			 (close-output-port port))))
	 #t))

(define (PP1 form port cache-structs)
    (let* ((indent (write-count port))
	   (left (print-in form (- (write-width port) indent))))
	  (cond ((negative? left)
		 (cond ((pair? form)
			(display "(" port)
			(pp1 (car form) port #t)
			(do ((tab (make-string (+ indent 2) #\space))
			     (x (cdr form) (cdr x)))
			    ((not (pair? x))
			     (when x
				   (newline port)
				   (display tab port)
				   (display ". " port)
				   (pp1 x port #t))
			     (display ")" port))
			    (newline port)
			    (display tab port)
			    (pp1 (car x) port #t)))
		       ((vector? form)
			(display "#" port)
			(if (not cache-structs)
			    (pp1 (vector->list form) port #t)
			    (let ((x object-cache-index))
				 (vector-set! object-cache x form)
				 (set! object-cache-index
				       (remainder (+ object-cache-index 1)
					   (vector-length object-cache)))
				 (pp1 (cons (vector-ref form 0) x) port #t))))
		       (else (write form port))))
		(else (write form port)))))

;;; Objects are cached in OBJECT-CACHE and OBJECT-CACHE-INDEX is the index of
;;; the next free entry.

(define OBJECT-CACHE (make-vector 40))

(define OBJECT-CACHE-INDEX 0)

;;; PRINT-IN is used to decide if a form can be printed in line-length 
;;; characters.  If it can, then it will return:
;;;    line-length - # characters needed
;;; otherwise it will return a negative number.

(define (PRINT-IN form line-length)
    (cond ((negative? line-length) line-length)
	  ((pair? form)
	   (cond ((null? (cdr form))	;;; End of list
		  (- (print-in (car form) (- line-length 1)) 1))
		 ((pair? (cdr form))	;;; Continued list
		  (print-in (cdr form) (print-in (car form)
					   (- line-length 1))))
		 (else			;;; Dotted pair
		     (print-in (cdr form)
			 (print-in (car form) (- line-length 5))))))
	  ((vector? form)		;;; Vector is 1 longer than its list
	   (print-in (vector->list form) (- line-length 1)))
	  (else				;;; Print to a string port and measure
	      (let ((port (open-output-string)))
		   (write form port)
		   (- line-length (string-length (get-output-string port)))))))
