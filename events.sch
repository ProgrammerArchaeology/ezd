;;; User specified event handling.

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

(define-external BUTTON-MODIFIER? top-level)

(define-external BUTTON-DOWN? top-level)

(define-external HANDLE-WINDOW-EVENTS top-level)

(define-external HANDLE-VIEW-EVENTS top-level)

(define-external HANDLE-ATTRIBUTE-EVENTS top-level)

(define-external *MOUSE-X* top-level)

(define-external *MOUSE-Y* top-level)

(define-external *MOUSE-XWINDOW* top-level)

(define-external *MOUSE-WINDOW* top-level)

(define-external *MOUSE-WINDOW-X* top-level)

(define-external *MOUSE-WINDOW-Y* top-level)

(define-external *MOUSE-OBJECT* top-level)

(define-external *MOUSE-BUTTON1* top-level)

(define-external *MOUSE-BUTTON2* top-level)

(define-external *MOUSE-BUTTON3* top-level)

(define-external *MOUSE-BUTTON4* top-level)

(define-external *MOUSE-BUTTON5* top-level)

(define-external *CLEAN-MOUSE-WINDOW* top-level)

(define-external *USER-EVENT-WINDOW* top-level)

(define-external *USER-EVENT-DRAWING* top-level)

(define-external *USER-EVENT-OBJECT* top-level)

(define-external *USER-EVENT-X* top-level)

(define-external *USER-EVENT-Y* top-level)

(define-external *USER-EVENT-TYPE* top-level)

(define-external *USER-EVENT-XEVENT* top-level)

(define-external *USER-EVENT-MISC* top-level)

(define-external HANDLE-WHEN-EVENTS top-level)

(define-external EVENTS-MODULE-INIT top-level)
