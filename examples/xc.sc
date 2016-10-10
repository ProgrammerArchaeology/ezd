;;; A simple postfix calculator based on ezd.  The keyboard looks something
;;; like the following:
;;;
;;;     [                       ]
;;;	[			]
;;;	[			]
;;;	+	D	E	F
;;;	-	A	B	C
;;;	*	7	8	9
;;;	/	4	5	6
;;;	+/-	1	2	3
;;;	ODH	0	ENTER	CLR
;;;	DUP	XCH	ROT	QUIT

;*              Copyright 1990 Digital Equipment Corporation
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

;;; To run this program:
;;;
;;;	csh >ezd -i
;;;	Scheme->C -- 28sep90jfb -- Copyright 1989 Digital ...
;;;	> (load "xc.sc")
;;;	MODULE form ignored
;;;	(DEFINE-EXTERNAL EZD TOP-LEVEL)
;;;	(DEFINE-EXTERNAL *MOUSE-BUTTON1* TOP-LEVEL)
;;;		.
;;;		.
;;;		.
;;;	XCH
;;;	ROT
;;;	QUIT
;;;	"xc.sc"
;;;	> (xc)
;;;	#T
;;;	> > ^D
;;;	csh >

(module xc (main start-xc) (with ezd))

(define-external ezd top-level)
(define-external *mouse-button1* top-level)

(define (START-XC clargs)
    (case (length clargs)
	  ((1) (xc)
	       (ezd '(pause)))
	  ((2) (xc (read (open-input-string (cadr clargs))))
	       (ezd '(pause)))
	  (else (format stderr-port "xc [key-color]~%"))))

(define X-SCALE 50)
(define Y-SCALE 30)
(define XY-PAD 2)
(define KEY-COLOR 'lightblue)
(define TEXT-COLOR 'black)
(define REGISTER-HEIGHT 18)

(define (XC . x)
    (if x (set! key-color (car x)))
    (ezd `(window xc 100 100 ,(* x-scale 4) ,(* y-scale 9) fixed-size))
    (ezd '(set-drawing xc))
    (ezd '(overlay xc xc))

    (make-key 0 2 "+" plus)
    (make-key 1 2 "D" (lambda () (digit-in 13)))
    (make-key 2 2 "E" (lambda () (digit-in 14)))
    (make-key 3 2 "F" (lambda () (digit-in 15)))

    (make-key 0 3 "-" minus)
    (make-key 1 3 "A" (lambda () (digit-in 10)))
    (make-key 2 3 "B" (lambda () (digit-in 11)))
    (make-key 3 3 "C" (lambda () (digit-in 12)))

    (make-key 0 4 "*" times)
    (make-key 1 4 "7" (lambda () (digit-in 7)))
    (make-key 2 4 "8" (lambda () (digit-in 8)))
    (make-key 3 4 "9" (lambda () (digit-in 9)))

    (make-key 0 5 "/" divide)
    (make-key 1 5 "4" (lambda () (digit-in 4)))
    (make-key 2 5 "5" (lambda () (digit-in 5)))
    (make-key 3 5 "6" (lambda () (digit-in 6)))

    (make-key 0 6 "+/-" complement)
    (make-key 1 6 "1" (lambda () (digit-in 1)))
    (make-key 2 6 "2" (lambda () (digit-in 2)))
    (make-key 3 6 "3" (lambda () (digit-in 3)))

    (make-key 0 7 (lambda ()
			  (case number-base
				((8) "OCT")
				((10) "DEC")
				((16) "HEX")))
	 base)
    (make-key 1 7 "0" (lambda () (digit-in 0)))
    (make-key 2 7 "ENTER" enter)
    (make-key 3 7 "POP" pop)

    (make-key 0 8 "DUP" dup)
    (make-key 1 8 "XCH" xch)
    (make-key 2 8 "ROT" rot)
    (make-key 3 8 "QUIT" quit)

    (set-xc-value #f #f #f '()))

;;; Key construction.
		  
(define (MAKE-KEY x y key action)
    
    (define XX (+ (* x x-scale) xy-pad))
    (define YY (+ (* y y-scale) xy-pad))
    (define WIDTH (- x-scale xy-pad xy-pad))
    (define HEIGHT (- y-scale xy-pad xy-pad))
    
    (define KEYNAME (string->symbol (format "KEY~s~s" x y)))
    
    (define BOLD #f)
    (define HIGHLIGHT #f)
    
    (define (ENTER) (if (not *mouse-button1*) (draw-key #t #f)))
    
    (define (EXIT) (draw-key #f #f))
    
    (define (BUTTON-DOWN) (draw-key #t #t))
    
    (define (BUTTON-UP)
	    (if highlight (action))
	    (draw-key #t #f))
    
    (define (DRAW-KEY B H)
	    (set! bold b)
	    (set! highlight h)
	    (ezd '(set-drawing xc))
	    (ezd `(object ,keyname
			  (fill-rectangle ,xx ,yy ,width ,height
			      ,(if h text-color key-color))
			  (rectangle ,xx ,yy ,width ,height)
			  (text ,xx ,yy ,width ,height
				center center ,(if (string? key) key (key))
				,(if h key-color text-color)
				,(if b "8x13bold" "8x13")))))
    
    (draw-key #f #f)
    (ezd `(when ,keyname enter ,enter))
    (ezd `(when ,keyname exit ,exit))
    (ezd `(when ,keyname button1down ,button-down))
    (ezd `(when ,keyname button1up ,button-up)))

;;; Memory.

(define REG-A #f)
(define REG-B #f)
(define REG-C #f)
(define REG-REST '())
(define PUSH-DIGIT #t)

(define (SET-XC-VALUE a b c rest)
    (define (REGISTER reg reg-value)
	    `(text ,(+ xy-pad xy-pad)
		   ,(+ xy-pad xy-pad (* reg register-height))
		   ,(- (* 4 x-scale) (* 8 xy-pad))
		   ,(- register-height xy-pad) right center
		   ,(if reg-value
			(number->string reg-value number-base)
			"")
		   ,text-color "8x13"))
    (if (eq? a 'pop)
	(begin (set! reg-a b)
	       (set! reg-b c)
	       (if (null? reg-rest)
		   (set! reg-c #f)
		   (begin (set! reg-c (car reg-rest))
			  (set! reg-rest (cdr reg-rest)))))
	(begin (set! reg-a a)
	       (set! reg-b b)
	       (set! reg-c c)
	       (set! reg-rest rest)))
    (ezd '(set-drawing xc))
    (ezd `(object xcvalue
		  (rectangle ,xy-pad ,xy-pad
		      ,(- (* 4 x-scale) xy-pad xy-pad)
		      ,(- (* 2 y-scale) xy-pad xy-pad)
		      ,key-color)
		  ,(register 2 reg-a)
		  ,(register 1 reg-b)
		  ,(register 0 reg-c)))
    (set! push-digit #t))

;;; Key definitions.

(define (DIGIT-IN v)
    (when (< v number-base)
	  (if push-digit
	      (set-xc-value v reg-a reg-b (cons reg-c reg-rest))
	      (if (number? reg-a)
		  (if (positive? reg-a)
		      (set-xc-value (+ v (* reg-a number-base)) reg-b reg-c
			  reg-rest)
		      (set-xc-value (- v (* reg-a number-base)) reg-b reg-c
			  reg-rest))
		  (set-xc-value v reg-b reg-c reg-rest)))
	  (set! push-digit #f)))
 
(define (PLUS)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (+ reg-a reg-b) reg-c reg-rest)))

(define (MINUS)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (- reg-b reg-a) reg-c reg-rest)))

(define (TIMES)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (* reg-a reg-b) reg-c reg-rest)))

(define (DIVIDE)
    (if (and (number? reg-a) (number? reg-b) (not (zero? reg-a)))
	(set-xc-value 'pop (quotient reg-b reg-a) reg-c reg-rest)))

(define (COMPLEMENT)
    (if (and (number? reg-a)) (set-xc-value (- reg-a) reg-b reg-c reg-rest)))

(define NUMBER-BASE 10)

(define (BASE)
    (set! number-base (cdr (assq number-base '((10 . 16) (16 . 8) (8 . 10)))))
    (set-xc-value reg-a reg-b reg-c reg-rest)
    (set! push-digit #f))

(define (POP) (set-xc-value 'pop reg-b reg-c reg-rest))

(define (ENTER) (set! push-digit #t))

(define (DUP)
    (if (number? reg-a)
	(set-xc-value reg-a reg-a reg-b (cons reg-c reg-rest))))

(define (XCH)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value reg-b reg-a reg-c reg-rest)))

(define (ROT)
    (if (and (number? reg-a) (number? reg-b) (number? reg-c))
	(set-xc-value reg-c reg-a reg-b reg-rest)))

(define (QUIT) (ezd '(quit)))
