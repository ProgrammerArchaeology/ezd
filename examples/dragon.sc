;;; DRAGON - a solitaire game played with mah-jongg tiles.

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

;;; To interpret this program:
;;;
;;;	csh >ezd -i
;;;	Scheme->C -- 28sep90jfb -- Copyright 1989 Digital ...
;;;	> (load "dragon.sc")
;;;	MODULE form ignored
;;;	(DEFINE-EXTERNAL EZD TOP-LEVEL)
;;;	(DEFINE-EXTERNAL NOPIXMAP TOP-LEVEL)
;;;		.
;;;		.
;;;		.
;;;	SHOW-ALL-MOVES
;;;	DRAGON-MAIN
;;;	DRAGON
;;;	HELP-TEXT
;;;	"dragon.sc"
;;;	> (dragon)
;;;	> ^D
;;;	csh >
;;;
;;; See the makefile to compile this program.

(module dragon
    (with ezd)
    (heap 5)
    (main dragon-main))

;;; Externals

(define-external ezd top-level)

(define-external nopixmap top-level)

(define-external *user-event-xevent* top-level)

(define-external xevent-xbutton-time top-level)

;;; The following globals control how tiles are drawn.

(define *TILE-ORIGIN-X* 10)
(define *TILE-ORIGIN-Y* 10)
(define *TILE-WIDTH* 30)
(define *TILE-HEIGHT* 40)
(define *TILE-SHADOW* 5)

(define *TILE-ROWS* 8)
(define *TILE-COLUMNS* 15)
(define *TILE-ELEVATION* 5)

(define *WINDOW-WIDTH*  (+ (* (+ *tile-width* 1) *tile-columns*)
			   (+ (* 2 *tile-origin-x*) *tile-shadow*)))
(define *WINDOW-HEIGHT* (+ (* *tile-rows* (+ *tile-height* 1))
			   (+ (* 2 *tile-origin-y*) *tile-shadow*)))

;;; The tiles are stacked in a 3-D "dragon" defined by *TILE-PLAN*.  The
;;; tile plan is organized in levels from bottom to top.  A non-zero value
;;; indicates a tile in that position.  Tiles numbered 1 are in a normal
;;; position.  Tiles numbered 2 are to be shifted 0.5 in the Y direction.
;;; Tiles numbered 3 are to be shifted -1.5 in the Y direction.
;;; Tiles numbered 4 are to be shifted in 0.5 in both the X and Y direction.

(define *TILE-PLAN*
    '#( #(#(0 1 1 1 1 1 1 1 1 1 1 1 1 0 0)
	  #(0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
	  #(0 0 1 1 1 1 1 1 1 1 1 1 0 0 0)
	  #(0 1 1 1 1 1 1 1 1 1 1 1 1 2 2)
	  #(0 1 1 1 1 1 1 1 1 1 1 1 1 0 0)
	  #(3 0 1 1 1 1 1 1 1 1 1 1 0 0 0)
	  #(0 0 0 1 1 1 1 1 1 1 1 0 0 0 0)
	  #(0 1 1 1 1 1 1 1 1 1 1 1 1 0 0))

	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 1 1 1 1 1 1 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 1 1 1 1 0 0 0 0 0 0)
	  #(0 0 0 0 0 1 1 1 1 0 0 0 0 0 0)
	  #(0 0 0 0 0 1 1 1 1 0 0 0 0 0 0)
	  #(0 0 0 0 0 1 1 1 1 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 1 1 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 1 1 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

	#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 4 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ))
  
;;; There are four tiles of each type.  The types are encoded in the form of
;;; a list of lists of color and text symbol.

(define *TILE-TYPES*
    '((red "") (white "") (green "")				;;; Dragons
      (tan "N") (tan "S") (tan "E") (tan "W")   		;;; Winds
      (magenta "")						;;; Flowers
      (goldenrod "")						;;; Seasons
      (yellow "1") (yellow "2") (yellow "3")			;;; Bams
      (yellow "4") (yellow "5") (yellow "6")
      (yellow "7") (yellow "8") (yellow "9")
      (plum "1") (plum "2") (plum "3")				;;; Dots
      (plum "4") (plum "5") (plum "6")
      (plum "7") (plum "8") (plum "9")
      (cyan "1") (cyan "2") (cyan "3")				;;; Craks
      (cyan "4") (cyan "5") (cyan "6")
      (cyan "7") (cyan "8") (cyan "9")))

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Procedures to draw tiles and handle events ;;;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Each tile is represented by an instance of the following object.  The
;;; arguments are:
;;;
;;;	POSITION	logical position in the dragon.
;;;	X		X coordinate for drawing the tile.
;;;	Y		Y coordinate for drawing the tile.
;;;	COLOR		color of the tile face.
;;;	TEXT		text written on the tile.

(define (MAKE-TILE position x y color text)
    (let ((name (string->symbol (format "TILE-~s-~s" x y)))
	  (visible #f)
	  (type (list color text)))
	 
	 (define (BUTTON1DOWN)
		 (if (tile-playable? position)
		     (if *selected-tile*
			 (if (eq? self *selected-tile*)
			     (begin (set! *selected-tile* #f)
				    (draw #f))
			     (if (equal? type (*selected-tile* 'type))
				 (begin (set! *deleted-tiles*
					      (cons self
						    (cons *selected-tile*
							  *deleted-tiles*)))
					(clear)
					(*selected-tile* 'clear)
					(set! *selected-tile* #f)
					(if (null? *visible-tiles*)
					    (for-each
						(lambda (tile)
							(ezd '(draw-now))
							(tile 'draw))
						*deleted-tiles*)))))
			 (begin (set! *selected-tile* self)
				(draw #t)))))
	 
	 (define (BUTTON2DOWN)
		 (let ((tiles '()))
		      
		      (define (LOWLIGHT)
			      (for-each
				  (lambda (tile)
					  (or (eq? tile *selected-tile*)
					      (tile 'lowlight)))
				  tiles)
			      (ezd `(when ,name button2up #f)
				   `(when ,name exit #f)))
		      
		      (for-each
			  (lambda (tile)
				  (if (and (not (eq? tile self))
					   (equal? (tile 'type) type))
				      (let* ((p (tile 'position))
					     (a (tile-above p)))
					   (when (or (not a)
						     (not (a 'visible))
						     (eq? (caddr p)
							  (- *tile-elevation*
							     2)))
						 (set! tiles (cons tile tiles))
						 (tile 'highlight)))))
			  *visible-tiles*)
		      (ezd `(when ,name button2up ,lowlight)
			   `(when ,name exit ,lowlight))))
	 
	 (define (DRAW highlight)
		 (if (not visible)
		     (set! *visible-tiles* (cons self *visible-tiles*)))
		 (set! visible #t)
		 (draw-tile name x y *tile-width* *tile-height* *tile-shadow*
		     color text highlight)
		 (ezd `(when ,name button1down ,button1down)
		      `(when ,name button2down ,button2down)))
	 
	 (define (CLEAR)
		 (if visible
		     (set! *visible-tiles* (remq! self *visible-tiles*)))
		 (set! visible #f)
		 (ezd '(set-drawing dragon)
		      `(object ,name)))
	 
	 (define (SELF x)
		 (case x
		       ((highlight) (draw #t))
		       ((lowlight) (draw #f))
		       ((draw) (draw #f))
		       ((clear) (clear))
		       ((visible) visible)
		       ((type) type)
		       ((position) position)))
	 
	 (draw #f)
	 self))

;;; When tiles are deleted from the dragon, they are placed on the following
;;; list.

(define *DELETED-TILES* '())

;;; All tiles that are visible are on the following list.

(define *VISIBLE-TILES* '())

;;; Event handling for tiles is done by the following procedure.

;;; Tiles are drawn by the following procedure.
;;;
;;;	     XY	A-------D
;;;	       /|	|
;;;	     SA |	|
;;;	      | |	|
;;;	      |	|	|
;;;	      |	B-------C
;;;	      |/       /
;;;	     SB------SC

(define (DRAW-TILE name x y w h shadow color text highlight)
    (let ((sa-x x)
	  (sa-y (+ y shadow))
	  (sb-x x)
	  (sb-y (+ y shadow h))
	  (sc-x (+ x w))
	  (sc-y (+ y shadow h))
	  (a-x (+ x shadow))
	  (a-y y)
	  (b-x (+ x shadow))
	  (b-y (+ y h))
	  (c-x (+ x shadow w))
	  (c-y (+ y h))
	  (d-x (+ x shadow w))
	  (d-y y))
	 (ezd `(set-drawing dragon)
	      `(object ,name
		       (fill-rectangle ,a-x ,a-y ,w ,h ,color)
		       (fill-polygon ,sa-x ,sa-y ,sb-x ,sb-y ,sc-x ,sc-y
			   ,c-x ,c-y ,b-x ,b-y ,a-x ,a-y wheat)
		       (line ,sa-x ,sa-y ,a-x ,a-y)
		       (line ,sb-x ,sb-y ,b-x ,b-y)
		       (line ,sc-x ,sc-y ,c-x ,c-y 2)
		       (line ,sa-x ,sa-y ,sb-x ,sb-y)
		       (line ,sb-x ,sb-y ,sc-x ,sc-y 2)
		       (rectangle ,a-x ,a-y ,w ,h)
		       ,@(if highlight
			     `((rectangle ,(+ a-x 2) ,(+ a-y 2)
				   ,(- w 4) ,(- h 4) 3))
			     '())
		       (text ,(+ a-x 4) ,(+ a-y 4) ,(- w 8) ,(- h 8)
			     right up ,text "8x13")))))

;;; In order to play the game, the program must have an understanding of the
;;; dragon's geometry.  The data structures and query procedures for this
;;; information are maintained by the following procedures.

(define *TILE-GEOMETRY* #f)

(define *SELECTED-TILE* #f)

(define (INIT-GEOMETRY)
    (set! *selected-tile* #f)
    (set! *tile-geometry*
	  (let loop ((v *tile-plan*))
	       (if (vector? v)
		   (let ((vv (make-vector (vector-length v) #f)))
			(do ((i (- (vector-length v) 1) (- i 1)))
			    ((= i -1) vv)
			    (vector-set! vv i (loop (vector-ref v i)))))
		   #f))))
    
;;; Return the tile to the left of a tile position.

(define (TILE-LEFT position)
    (let ((x (- (car position) 1))
	  (y (cadr position))
	  (z (caddr position)))
	 (if (= x -1)
	     #f
	     (vector-ref (vector-ref (vector-ref *tile-geometry* z) y) x))))

;;; Return the tile to the right of a tile position.

(define (TILE-RIGHT position)
    (let ((x (+ (car position) 1))
	  (y (cadr position))
	  (z (caddr position)))
	 (if (= x *tile-columns*)
	     #f
	     (vector-ref (vector-ref (vector-ref *tile-geometry* z) y) x))))

;;; Return the tile above a tile position.

(define (TILE-ABOVE position)
    (let ((x (car position))
	  (y (cadr position))
	  (z (+ 1 (caddr position))))
	 (if (= z *tile-elevation*)
	     #f
	     (vector-ref (vector-ref (vector-ref *tile-geometry* z) y) x))))

;;; Boolean to determine that a tile is playable.

(define (TILE-PLAYABLE? position)
    (let ((left (tile-left position))
	  (right (tile-right position))
	  (above (tile-above position)))
	 (if (or (and above (above 'visible))
		 (and left right (left 'visible) (right 'visible)))
	     #f
	     #t)))

;;; Set an entry in *TILE-GEOMETRY*.

(define (TILE-GEOMETRY! x y z v)
    (vector-set! (vector-ref (vector-ref *tile-geometry* z) y) x v))

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Procedures to draw the dragon ;;;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The tiles are shuffled by calling the following procedure with a random
;;; integer.  It returns a list of tiles.

(define RAND0 0)

(define (SHUFFLE-TILES)
    (let ((input (list->vector (append *tile-types* *tile-types* *tile-types*
				       *tile-types*)))
	  (output '())
	  (m 144000)
	  (a 3021)
	  (c 713))
	 
	 (define (SELECT-ONE)
		 (let ((x (modulo (+ (* a rand0) c) m)))
		      (set! rand0 x)
		      (let loop ((x (quotient rand0 1000)))
			   (if (vector-ref input x)
			       (let ((tile (vector-ref input x)))
				    (vector-set! input x #f)
				    tile)
			       (loop (modulo (+ x 1) 144))))))

	 (set! rand0 (inexact->exact (modulo (abs rand0) m)))
	 (do ((i 0 (+ i 1)))
	     ((= i (vector-length input)) output)
	     (set! output (cons (select-one) output)))))

;;; The tiles are drawn by the following procedure.

(define (DRAW-TILES origin-x origin-y w h s)
    (init-geometry)
    (do ((tiles (shuffle-tiles))
	 (z 0 (+ z 1)))
	((= z *tile-elevation*))
	(do ((y 0 (+ y 1)))
	    ((= y *tile-rows*))
	    (do ((x (- *tile-columns* 1) (- x 1)))
		((= x -1))
		(let* ((type (vector-ref (vector-ref (vector-ref *tile-plan* z)
					     y) x))
		       (color-text (if (not (zero? type))
				       (let ((x (car tiles)))
					    (set! tiles (cdr tiles))
					    x)
				       #f)))
		      (case type
			    ((1) (let ((tile (make-tile (list x y z)
						 (+ origin-x (* z s)
						    (* x (+ w 1)))
						 (+ origin-y (- (* z s))
						    (* y (+ h 1)))
						 (car color-text)
						 (cadr color-text))))
				      (tile-geometry! x y z tile)))
			    ((2) (let ((tile (make-tile (list x y z)
						 (+ origin-x (* z s)
						    (* x (+ w 1)))
						 (+ origin-y (- (* z s))
						    (/ h 2)
						    (* y (+ h 1)))
						 (car color-text)
						 (cadr color-text))))
				      (tile-geometry! x y z tile)
				      (tile-geometry! x (+ y 1) z tile)))
			    ((3) (let ((tile (make-tile (list x y z)
						 (+ origin-x (* z s)
						    (* x (+ w 1)))
						 (+ origin-y (- (* z s)) (- h)
						    (- (/ h 2)) (* y (+ h 1)))
						 (car color-text)
						 (cadr color-text))))
				      (tile-geometry! x (- y 2) z tile)
				      (tile-geometry! x (- y 1) z tile)))
			    ((4) (let ((tile (make-tile (list x y z)
						 (+ origin-x (* z s) (/ w 2)
						    (* x (+ w 1)))
						 (+ origin-y (- (* z s))
						    (/ h 2)
						    (* y (+ h 1)))
						 (car color-text)
						 (cadr color-text))))
				      (tile-geometry! x y z tile)
				      (tile-geometry! x (+ y 1) z tile)
				      (tile-geometry! (+ x 1) y z tile)
				      (tile-geometry! (+ x 1) (+ y 1) z
					  tile)))))))))

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Procedures to start the game ;;;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Start a new game.

(define (NEW-GAME)
    (ezd '(save-cursor dragon)
	 '(set-cursor dragon xc_watch)
	 '(set-drawing dragon)
	 '(clear)
	 `(object background (fill-rectangle 0 0 ,*window-width*
				 ,*window-height* clear))
	 `(object start (text 0 0 ,*window-width* ,*window-height*
			      center center
			      "...drawing tiles..." "9x15"))
	 '(draw-now))
    (set! *deleted-tiles* '())
    (set! *visible-tiles* '())
    (draw-tiles
	*tile-origin-x* *tile-origin-y* *tile-width* *tile-height*
	*tile-shadow*)
    (ezd '(object start)
	 '(when background button1down (dragon-menu))
	 '(when background button2down (show-all-moves))
	 '(restore-cursor dragon)))

;;; Undo the previous move.

(define (UNDO)
    (when *deleted-tiles*
	  (when *selected-tile*
		(*selected-tile* 'lowlight)
		(set! *selected-tile* #f))
	  ((car *deleted-tiles*) 'draw)
	  ((cadr *deleted-tiles*) 'draw)
	  (set! *deleted-tiles* (cddr *deleted-tiles*))))

;;; Show all moves until the button comes up.

(define (SHOW-ALL-MOVES)
    (let ((tiles '())
	  (high-tiles '()))
	 
	 (define (LOWLIGHT)
		 (ezd '(save-cursor dragon)
		      '(set-cursor dragon xc_watch))
		 (for-each (lambda (tile) (tile 'lowlight)) high-tiles)
		 (if *selected-tile* (*selected-tile* 'highlight))
		 (ezd '(set-drawing dragon)
		      '(when background button2up #f)
		      '(when background exit #f)
		      '(restore-cursor dragon)))
	 
	 (define (ANOTHER-TILE? tile)
		 (let ((type (tile 'type)))
		      (let loop ((tiles tiles))
			   (if (pair? tiles)
			       (let ((x (car tiles)))
				    (if (and (equal? (x 'type) type)
					     (not (eq? x tile)))
					#t
					(loop (cdr tiles))))
			       #f))))
	 
	 (ezd '(save-cursor dragon)
	      '(set-cursor dragon xc_watch))
	 (if *selected-tile* (*selected-tile* 'lowlight))
	 (for-each
	     (lambda (tile)
		     (if (tile-playable? (tile 'position))
			 (set! tiles (cons tile tiles))))
	     *visible-tiles*)
	 (for-each
	     (lambda (tile)
		     (when (another-tile? tile)
			   (tile 'highlight)
			   (set! high-tiles (cons tile high-tiles))))
	     tiles)
	 (ezd '(set-drawing dragon)
	      `(when background button2up ,lowlight)
	      `(when background exit ,lowlight)
	      '(restore-cursor dragon))))

;;; Main.
	
(define (DRAGON-MAIN clargs)
    (if (member "-nopixmap" clargs) (set! nopixmap #t))
    (dragon))

(define (DRAGON)
    
    (define (START-UP)
	    (set! rand0 (xevent-xbutton-time *user-event-xevent*))
	    (new-game))
    
    (ezd '(quit)
	 `(window dragon ,*window-width* ,*window-height* fixed-size)
	 '(set-drawing dragon)
	 '(overlay dragon dragon)
	 `(object start
		  (fill-rectangle 0 0 ,*window-width* ,*window-height* clear)
		  (text 0 0 ,*window-width* 60
			center center "DRAGON" "vxms-37")
		  ,@(help-text))
	 `(when start button1down ,start-up)
	 '(define-popup dragon-menu
	      "UNDO" (undo) "NEW GAME" (new-game) "QUIT" (ezd '(quit)) "8x13")
	 '(pause)))

(define (HELP-TEXT)
    (let loop ((x 10)
	       (y 60)
	       (text '(
"The \"dragon\" is a stack of 144 stylized mah-jongg tiles."
"The object of the game is to remove all tiles from the"
"stack, a matching pair at a time.  The only tiles that"
"can be removed are those at the left or right ends of a"
"row.  Use the mouse buttons as follows to play the game:"
""
"Click button 1 to select and highlight the first tile of a"
"pair to remove.  Click button 1 on an identical tile to"
"remove them.  The highlight can be removed from the initial"
"tile by clicking button 1 on it."
""
"Press button 1 on the background to pop up an options menu."
""
"Hold down button 2 on a tile to highlight all identical"
"tiles that are visible.  Hold down button 2 on the"
"background to show all playable tiles."
""
"               CLICK BUTTON 1 TO START GAME"
		      )))
	 (if (pair? text)
	     (cons `(text ,x ,y ,(car text) "8x13")
		   (loop x (+ y 16) (cdr text)))
	     '())))
