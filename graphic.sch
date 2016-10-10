;;; A DRAWING is an ordered list of GRAPHIC objects.  A DRAWING mapped by a
;;; VIEW is composed of a list of BBGRAPHIC objects.

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

(define-external MAKE-GRAPHIC top-level)

(define-in-line-structure-access GRAPHIC
    name
    compute-bb
    xdraw
    psdraw
    intersect?
    events
    redraw-seq)

(define-external SET-GRAPHIC! top-level)

(define-external DRAW-CLEAR top-level)

(define-external NULL-GRAPHIC top-level)

(define-external MAKE-BBGRAPHIC top-level)

(define-in-line-structure-access BBGRAPHIC
    graphic
    minx
    miny
    maxx
    maxy)

(define-external BBGRAPHIC-BOUNDING-BOX top-level)

(define-external UPDATE-BBGRAPHIC top-level)

(define-external XDRAW-BBGRAPHIC-LIST top-level)

(define-external XDRAW-BBGRAPHIC top-level)

(define-external PSDRAW-BBGRAPHIC-LIST top-level)

(define-external PSDRAW-BBGRAPHIC top-level)

(define-external BBMIN top-level)

(define-external BBMAX top-level)

(define-external BBGRAPHICS-INTERSECT top-level)

(define-external BBGRAPHICS-NOT-INTERSECT top-level)

(define-external BBGRAPHICS-REALLY-INTERSECT top-level)

(define-external GRAPHIC-MODULE-INIT top-level)
