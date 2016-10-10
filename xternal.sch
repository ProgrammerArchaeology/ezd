;;; X11 Xlib declarations.

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

(define-external ALLOCNONE top-level)

(define-external ARCCHORD top-level)

(define-external ARCPIESLICE top-level)

(define-external BUTTONPRESS top-level)

(define-external BUTTONPRESSMASK top-level)

(define-external BUTTONRELEASE top-level)

(define-external BUTTONRELEASEMASK top-level)

(define-external CAPNOTLAST top-level)

(define-external COMPLEX top-level)

(define-external CONFIGURENOTIFY top-level)

(define-external CONTROLMASK top-level)

(define-external COORDMODEORIGIN top-level)

(define-external CWBACKPIXEL top-level)

(define-external CWBORDERPIXEL top-level)

(define-external CWCOLORMAP top-level)

(define-external CWOVERRIDEREDIRECT top-level)

(define-external CWSAVEUNDER top-level)

(define-external DOBLUE top-level)

(define-external DOGREEN top-level)

(define-external DORED top-level)

(define-external ENTERNOTIFY top-level)

(define-external ENTERWINDOWMASK top-level)

(define-external EVENODDRULE top-level)

(define-external EXPOSE top-level)

(define-external EXPOSUREMASK top-level)

(define-external FILLSTIPPLED top-level)

(define-external GRAPHICSEXPOSE top-level)

(define-external INPUTOUTPUT top-level)

(define-external JOINMITER top-level)

(define-external KEYPRESS top-level)

(define-external KEYPRESSMASK top-level)

(define-external KEYRELEASE top-level)

(define-external KEYRELEASEMASK top-level)

(define-external LEAVENOTIFY top-level)

(define-external LEAVEWINDOWMASK top-level)

(define-external LINEONOFFDASH top-level)

(define-external LINESOLID top-level)

(define-external LOCKMASK top-level)

(define-external MAKE-XCOLOR top-level)

(define-external MAKE-XEVENT top-level)

(define-external MAKE-XGCVALUES top-level)

(define-external MAKE-XPOINT top-level)

(define-external MAKE-XRECTANGLE top-level)

(define-external MAKE-XSETWINDOWATTRIBUTES top-level)

(define-external MAKE-XSIZEHINTS top-level)

(define-external MAKE-XWMHINTS top-level)

(define-external MAPPINGNOTIFY top-level)

(define-external MOD1MASK top-level)

(define-external MOTIONNOTIFY top-level)

(define-external NOEVENTMASK top-level)

(define-external NOEXPOSE top-level)

(define-external NONE top-level)

(define-external NULL-POINTER? top-level)

(define-external OWNERGRABBUTTONMASK top-level)

(define-external POINTERMOTIONMASK top-level)

(define-external PMAXSIZE top-level)

(define-external PMINSIZE top-level)

(define-external POINTER-VALUE top-level)

(define-external PPOSITION top-level)

(define-external PSEUDOCOLOR top-level)

(define-external PSIZE top-level)

(define-external QUEUEDAFTERREADING top-level)

(define-external RESIZEREDIRECTMASK top-level)

(define-external SHIFTMASK top-level)

(define-external STRUCTURENOTIFYMASK top-level)

(define-external TYPE/VALUE->POINTER top-level)

(define-external UNSORTED top-level)

(define-external USPOSITION top-level)

(define-external USSIZE top-level)

(define-external VISUAL-CLASS top-level)

(define-external XA_PRIMARY top-level)

(define-external XALLOCCOLOR top-level)

(define-external XALLOCCOLORCELLS top-level)

(define-external XALLOCNAMEDCOLOR top-level)

(define-external XBELL top-level)

(define-external XBLACKPIXEL top-level)

(define-external XC_LEFT_PTR top-level)

(define-external XCHANGEWINDOWATTRIBUTES top-level)

(define-external XCHARSTRUCT-ASCENT top-level)

(define-external XCHARSTRUCT-DESCENT top-level)

(define-external XCHARSTRUCT-LBEARING top-level)

(define-external XCHARSTRUCT-RBEARING top-level)

(define-external XCHARSTRUCT-WIDTH top-level)

(define-external XCLEARAREA top-level)

(define-external XCLOSEDISPLAY top-level)

(define-external XCOLOR-BLUE top-level)

(define-external XCOLOR-BLUE! top-level)

(define-external XCOLOR-FLAGS! top-level)

(define-external XCOLOR-GREEN top-level)

(define-external XCOLOR-GREEN! top-level)

(define-external XCOLOR-PIXEL top-level)

(define-external XCOLOR-PIXEL! top-level)

(define-external XCOLOR-RED top-level)

(define-external XCOLOR-RED! top-level)

(define-external XCONNECTIONNUMBER top-level)

(define-external XCOPYAREA top-level)

(define-external XCREATEBITMAPFROMDATA top-level)

(define-external XCREATECOLORMAP top-level)

(define-external XCREATEFONTCURSOR top-level)

(define-external XCREATEGC top-level)

(define-external XCREATEPIXMAP top-level)

(define-external XCREATESIMPLEWINDOW top-level)

(define-external XCREATEWINDOW top-level)

(define-external XDEFAULTCOLORMAP top-level)

(define-external XDEFAULTDEPTH top-level)

(define-external XDEFAULTROOTWINDOW top-level)

(define-external XDEFAULTSCREEN top-level)

(define-external XDEFAULTVISUAL top-level)

(define-external XDEFINECURSOR top-level)

(define-external XDESTROYREGION top-level)

(define-external XDESTROYWINDOW top-level)

(define-external XDISPLAYHEIGHT top-level)

(define-external XDISPLAYWIDTH top-level)

(define-external XDISPLAYWIDTHMM top-level)

(define-external XDRAWARC top-level)

(define-external XDRAWLINE top-level)

(define-external XDRAWPOINT top-level)

(define-external XDRAWPOINTS top-level)

(define-external XDRAWRECTANGLE top-level)

(define-external XDRAWSTRING top-level)

(define-external XEVENT-TYPE top-level)

(define-external XEVENT-XANY-WINDOW top-level)

(define-external XEVENT-XBUTTON-BUTTON top-level)

(define-external XEVENT-XBUTTON-STATE top-level)

(define-external XEVENT-XBUTTON-TIME top-level)

(define-external XEVENT-XBUTTON-X top-level)

(define-external XEVENT-XBUTTON-X_ROOT top-level)

(define-external XEVENT-XBUTTON-Y top-level)

(define-external XEVENT-XBUTTON-Y_ROOT top-level)

(define-external XEVENT-XBUTTON-WINDOW top-level)

(define-external XEVENT-XCONFIGURE-WIDTH top-level)

(define-external XEVENT-XCONFIGURE-HEIGHT top-level)

(define-external XEVENT-XCROSSING-X top-level)

(define-external XEVENT-XCROSSING-X_ROOT top-level)

(define-external XEVENT-XCROSSING-Y top-level)

(define-external XEVENT-XCROSSING-Y_ROOT top-level)

(define-external XEVENT-XCROSSING-WINDOW top-level)

(define-external XEVENT-XEXPOSE-COUNT top-level)

(define-external XEVENT-XEXPOSE-HEIGHT top-level)

(define-external XEVENT-XEXPOSE-WIDTH top-level)

(define-external XEVENT-XEXPOSE-X top-level)

(define-external XEVENT-XEXPOSE-Y top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-COUNT top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-DRAWABLE top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-HEIGHT top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-WIDTH top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-X top-level)

(define-external XEVENT-XGRAPHICSEXPOSE-Y top-level)

(define-external XEVENT-XKEY-STATE top-level)

(define-external XEVENT-XKEY-TIME top-level)

(define-external XEVENT-XKEY-X top-level)

(define-external XEVENT-XKEY-Y top-level)

(define-external XEVENT-XMOTION-WINDOW top-level)

(define-external XEVENT-XMOTION-X top-level)

(define-external XEVENT-XMOTION-X_ROOT top-level)

(define-external XEVENT-XMOTION-Y top-level)

(define-external XEVENT-XMOTION-Y_ROOT top-level)

(define-external XEVENT-XNOEXPOSE-DRAWABLE top-level)

(define-external XEVENTSQUEUED top-level)

(define-external XFETCHBYTES top-level)

(define-external XFILLARC top-level)

(define-external XFILLPOLYGON top-level)

(define-external XFILLRECTANGLE top-level)

(define-external XFILLRECTANGLES top-level)

(define-external XFONTSTRUCT-ASCENT top-level)

(define-external XFONTSTRUCT-DESCENT top-level)

(define-external XFONTSTRUCT-FID top-level)

(define-external XFONTSTRUCT-MAX_BOUNDS-ASCENT top-level)

(define-external XFONTSTRUCT-MAX_BOUNDS-DESCENT top-level)

(define-external XFLUSH top-level)

(define-external XFREE top-level)

(define-external XFREEPIXMAP top-level)

(define-external XGETWINDOWATTRIBUTES top-level)

(define-external XIFEVENT top-level)

(define-external XK_DOWN top-level)

(define-external XK_HYPER_R top-level)

(define-external XK_LEFT top-level)

(define-external XK_MULTI_KEY top-level)

(define-external XK_RIGHT top-level)

(define-external XK_SHIFT_L top-level)

(define-external XK_UP top-level)

(define-external XLOADQUERYFONT top-level)

(define-external XMAPRAISED top-level)

(define-external XMAPWINDOW top-level)

(define-external XMATCHVISUALINFO top-level)

(define-external XNEXTEVENT top-level)

(define-external XMOVERESIZEWINDOW top-level)

(define-external XMOVEWINDOW top-level)

(define-external XOPENDISPLAY top-level)

(define-external XPARSECOLOR top-level)

(define-external XPEEKEVENT top-level)

(define-external XPOINT-LIST->XPOINTA top-level)

(define-external XPOINT-X! top-level)

(define-external XPOINT-Y! top-level)

(define-external XPOINTINREGION top-level)

(define-external XPOLYGONREGION top-level)

(define-external XRECTANGLE-HEIGHT! top-level)

(define-external XRECTANGLE-LIST->XRECTANGLEA top-level)

(define-external XRECTANGLE-WIDTH! top-level)

(define-external XRECTANGLE-X! top-level)

(define-external XRECTANGLE-Y! top-level)

(define-external XREFRESHKEYBOARDMAPPING top-level)

(define-external XROOTWINDOW top-level)

(define-external XSELECTINPUT top-level)

(define-external XSETARCMODE top-level)

(define-external XSETBACKGROUND top-level)

(define-external XSETCLIPMASK top-level)

(define-external XSETCLIPRECTANGLES top-level)

(define-external XSETFILLSTYLE top-level)

(define-external XSETFONT top-level)

(define-external XSETFOREGROUND top-level)

(define-external XSETGRAPHICSEXPOSURES top-level)

(define-external XSETICONNAME top-level)

(define-external XSETLINEATTRIBUTES top-level)

(define-external XSETNORMALHINTS top-level)

(define-external XSETSELECTIONOWNER top-level)

(define-external XSETSTIPPLE top-level)

(define-external XSETTSORIGIN top-level)

(define-external XSETWINDOWATTRIBUTES-BACKGROUND_PIXEL! top-level)

(define-external XSETWINDOWATTRIBUTES-BORDER_PIXEL! top-level)

(define-external XSETWINDOWATTRIBUTES-COLORMAP! top-level)

(define-external XSETWINDOWATTRIBUTES-OVERRIDE_REDIRECT! top-level)

(define-external XSETWINDOWATTRIBUTES-SAVE_UNDER! top-level)

(define-external XSETWMHINTS top-level)

(define-external XSIZEHINTS-FLAGS top-level)

(define-external XSIZEHINTS-FLAGS! top-level)

(define-external XSIZEHINTS-HEIGHT! top-level)

(define-external XSIZEHINTS-MIN_HEIGHT! top-level)

(define-external XSIZEHINTS-MIN_WIDTH! top-level)

(define-external XSIZEHINTS-MAX_HEIGHT! top-level)

(define-external XSIZEHINTS-MAX_WIDTH! top-level)

(define-external XSIZEHINTS-WIDTH! top-level)

(define-external XSIZEHINTS-X! top-level)

(define-external XSIZEHINTS-Y! top-level)

(define-external XSTOREBYTES top-level)

(define-external XSTORECOLOR top-level)

(define-external XSTORENAME top-level)

(define-external XTEXTEXTENTS top-level)

(define-external XTEXTWIDTH top-level)

(define-external XUNMAPSUBWINDOWS top-level)

(define-external XUNMAPWINDOW top-level)

(define-external XVISUALINFO-VISUAL top-level)

(define-external XWHITEPIXEL top-level)

(define-external XWINDOWATTRIBUTES-HEIGHT top-level)

(define-external XWINDOWATTRIBUTES-WIDTH top-level)

(define-external XWINDOWEVENT top-level)

(define-external XWMHINTS-FLAGS! top-level)

(define-external XWMHINTS-INPUT! top-level)

(define-external YLOOKUPSTRING top-level)

(define-external YNEXTEVENT top-level)

(define-external YSELECT top-level)
