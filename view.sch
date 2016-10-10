;;; A VIEW object maps a DRAWING onto a WINDOW object.

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

(define-external VIEW-MODULE-INIT top-level)

(define-external MAKE-VIEW top-level)

(define-in-line-structure-access VIEW
    drawing-name
    window-name
    clip-minx
    clip-miny
    clip-maxx
    clip-maxy
    #f
    #f
    user->x
    user->y
    user->lw
    x->user
    y->user
    user->width
    user->height
    width->user
    height->user
    originx
    originy
    scalex
    scaley
    scalelw
    new
    bb-head
    bb-tail
    stipple-x
    stipple-y
    new-transform)

(define-external VIEW-DRAWING top-level)

(define-external VIEW-WINDOW top-level)

(define-external *CURRENT-VIEW* top-level)

(define-external *WINDOW* top-level)

(define-external *WIDTH* top-level)

(define-external *HEIGHT* top-level)

(define-external *NAME* top-level)

(define-external *FOREGROUND-NAME* top-level)

(define-external *BACKGROUND-NAME* top-level)

(define-external *FOREGROUND* top-level)

(define-external *BACKGROUND* top-level)

(define-external *XWINDOW* top-level)

(define-external USER->X top-level)

(define-external USER->Y top-level)

(define-external USER->LW top-level)

(define-external X->USER top-level)

(define-external Y->USER top-level)

(define-external USER->WIDTH top-level)

(define-external USER->HEIGHT top-level)

(define-external WIDTH->USER top-level)

(define-external HEIGHT->USER top-level)

(define-external SET-VIEW top-level)

(define-external CLIP-BBL-TO-VIEW top-level)

(define-external PARTITION-VIEWS top-level)

(define-external CV-GC top-level)

(define-external POINTS->PIXELS top-level)

(define-external WINDOW-DRAWING->VIEW top-level)

(define-external DELETE-VIEW top-level)

(define-external HANDLE-VISIBLE-EVENTS top-level)

(define-external TRANSFORM-VIEWS top-level)

(define-external REDRAW-A-VIEW top-level)
