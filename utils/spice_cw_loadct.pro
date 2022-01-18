;+
; Project     : SOHO - CDS     
;                   
; Name        : SPICE_CW_LOADCT()
;               
; Purpose     : Color table picker w. gamma/top/bottom control.
;               
; Explanation : Inspired by (and ripping off) the color table bar in
;               IMAGE_TOOL, this compound widget provides an easy access to
;               color table selection and adjustment without calling XLOADCT.
;
;               This is an autonomous compound widget, it returns absolutely
;               NO EVENTS.
;
;               The user may control:
;
;               bottom: Click and drag left mouse button
;               gamma : Click and drag middle mouse button
;               top   : Click and drag right mouse button
;               
; Use         : ID = SPICE_CW_LOADCT(BASE)
;    
; Inputs      : BASE : The widget base to put the menu/color bar on.
;               
; Opt. Inputs : None.
;               
; Outputs     : Returns ID of the compound widget.
;               
; Opt. Outputs: None.
;               
; Keywords    : XSIZE, YSIZE : The size of the color bar (widget_draw).
;                              Default is 256 x 20.
;
;               FRAME : The frame keyword used in creating the widget_base
;                       surrounding the compound widget.
;
;               FONT  : The font to use for the pulldown menu with the
;                       color table names.
;
;               MENU : Controls the appearance of the top level pulldown
;                      menu with color table names (see WIDGET_BUTTON).
;
;               FILE : The file to examine for color tables. If supplied,
;                      the file should exist. See LOADCT for details.
;
; Calls       : CLEANPLOT, LOADCT
;
; Common      : COLORS -- Shared with xloadct and others
;               CTABLE4ITOOL -- Shared with image_tool
;               
; Restrictions: Uses 256 colors.
;               
; Side effects: Changes color table.
;               
; Category    : Utility, Image
;               
; Prev. Hist. : See itool_adj_ctable
;
; Written     : s.v.h.haugan@astro.uio.no, 29 July 1996
;               
; Modified    : Version 1, SVHH, 31 July 1996
;                       Copied the contents of itool_adj_ctable into
;                       cw_loadct_ctable, and added FILE keyword to
;                       enable extended color tables.
;               Version 2, SVHH, 22 August 1996
;                       Started seeing a strange error, a mismatch between the
;                       size of r_orig and r_curr (etc..) with the r_curr ones
;                       being smaller -- changed the initializing to catch
;                       those.
;               Version 3, SVHH, 27 August 1996
;                       Added REVERSE_COLORS option.
;               Version 4, SVHH, 15 September 1997
;                       Shrunk the size for IDL v 4.0+ (xpad=1 etc..)
;		Version 5, William Thompson, GSFC, 8 April 1998
;			Changed !D.N_COLORS to !D.TABLE_SIZE for 24-bit displays
;               Version 6, Martin Wiesmann, 26 August 2021
;                       Copied to SPICE rep. and renamed to spice_cw_loadct
;                       An event is now sent to parent widget, if color table changes, this happens in
;                       spice_cw_loadct_event, spice_cw_loadct_newstart and spice_cw_loadct_reverse
;
; Version     :
; $Id: 2021-09-10 10:20 CEST $
;-            


;; This procedure was snatched from itool_adj_ctable
;;
PRO spice_cw_loadct_adj_ctable, event, initialize=initialize
  
  ON_ERROR, 2
  COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
  COMMON ctable4itool, mevent, x00, wxsize, pstatus, vbot, vtop, gamma, $
     vbot0, vtop0, lgamma0, lgamma

  IF KEYWORD_SET(initialize) THEN BEGIN
     tvlct,r,g,b,/get
     IF N_ELEMENTS(r) NE N_ELEMENTS(r_curr) THEN begin
        r_orig = r
        g_orig = g
        b_orig = b
        r_curr = r_orig
        g_curr = g_orig
        b_curr = b_orig
        IF N_ELEMENTS(r_curr) EQ 0 THEN crash = 2*crash
     ENDIF
     vbot = 0
     vtop = 100
     lgamma = 50
     gamma = 1.0
     RETURN
  ENDIF

  IF TAG_NAMES(event, /structure_name) NE 'WIDGET_DRAW' THEN RETURN
  
  CASE (event.type) OF
  0:BEGIN
;---------------------------------------------------------------------------
;        Button pressed; record some basic info
;---------------------------------------------------------------------------
     IF N_ELEMENTS(gamma) EQ 0 THEN BEGIN
        vbot = 0
        vtop = 100
        lgamma = 50
        gamma = 1.0
     ENDIF
     vtop0 = vtop
     vbot0 = vbot
     lgamma0 = lgamma
     x00 = event.x
     pstatus = event.press
     mevent = WIDGET_INFO(event.id, /draw_motion)
;---------------------------------------------------------------------------
;        Set draw motion event on, and get xsize of the widget window
;---------------------------------------------------------------------------
     WIDGET_CONTROL, event.id, draw_motion=1, get_value=cur_wid
     win_saved = !d.window
     wset, cur_wid
     wxsize = FLOAT(!d.x_size)
     wset, win_saved
     ENDCASE 
     
  1:BEGIN 
;---------------------------------------------------------------------------
;        Button released; set the motion event attribute back
;---------------------------------------------------------------------------
     WIDGET_CONTROL, event.id, draw_motion=mevent
     ENDCASE
     
  ELSE:BEGIN & END
     
  END
  
;---------------------------------------------------------------------------
;  Return if it is not a motion event
;---------------------------------------------------------------------------
  IF event.type NE 2 THEN RETURN
     
  dx = 100*FLOAT(event.x-x00)/wxsize
  
  CASE (pstatus) OF
  1:BEGIN
;---------------------------------------------------------------------------
;        Dragging left button, streching bottom
;---------------------------------------------------------------------------
     vbot = (vbot0+dx) > 0 < 100
     ENDCASE
     
  2:BEGIN
;---------------------------------------------------------------------------
;        Dragging middle button, adjust gamma
;---------------------------------------------------------------------------
     lgamma = (lgamma0+dx) > 0 < 100
     gamma = 10^((lgamma/50.) - 1)
     ENDCASE
     
  4:BEGIN 
;---------------------------------------------------------------------------
;        Dragging right button, streching top
;---------------------------------------------------------------------------
     vtop = (vtop0+dx) > 0 < 100
     ENDCASE
     
  ELSE:BEGIN & END
     
  END
  
  nc = !d.table_size
  s = (nc-1)/100.
  cbot = 0
  
  x0 = vbot * s
  x1 = vtop * s
  IF x0 NE x1 THEN s = (nc-1.0)/(x1 - x0) ELSE s = 1.0
  int = -s * x0
  IF gamma EQ 1.0 THEN $
     s = ROUND(FINDGEN(nc) * s + int > 0.0) $
  ELSE $
     s = ((FINDGEN(nc) * (s/nc) + (int/nc) > 0.0) ^ gamma) * nc
  s = s + cbot
  ON_ERROR,0
  r_curr(cbot) = (r=r_orig(s))
  g_curr(cbot) = (g=g_orig(s))
  b_curr(cbot) = (b=b_orig(s))
  TVLCT, r, g, b, cbot
  
  RETURN
END
     
     
PRO spice_cw_loadct_event,ev
  ; Only events from the widget_draw come here
  spice_cw_loadct_adj_ctable,ev

  ; send an event out to the top widget to notify it that color table changed
  widget_control, ev.top, send_event = {SPICE_CW_LOADCT_NEW_CT, ID:ev.id, TOP:ev.top, HANDLER:0L}
END


;; Make sure we're displaying a color bar with text on top.

PRO spice_cw_loadct_realize,id
  WIDGET_CONTROL,id,get_value=tv_id
  wset,tv_id
  bar = congrid(BINDGEN(256),!D.X_SIZE)
  bar = rebin(REFORM(bar,!D.X_SIZE,1),!D.X_SIZE,!D.Y_SIZE)
  cleanplot
  Text = 'Click and drag mouse buttons to modify'
  xyouts,0.5,0.2,align=0.5,width=width,text,/normal
  tvscl,bar
  xyouts,0.5,0.2,align=0.5,text,/normal,charsize=.9/width
END

PRO spice_cw_loadct_newstart,ev
  WIDGET_CONTROL,ev.id,get_uvalue=table_no
  WIDGET_CONTROL,WIDGET_INFO(ev.handler,/child),get_uvalue=file
  
  IF file NE '' THEN loadct,table_no,file=file,/silent $
  ELSE               loadct,table_no,/silent
  
  spice_cw_loadct_adj_ctable,/initialize

  ; send an event out to the top widget to notify it that color table changed
  widget_control, ev.top, send_event = {SPICE_CW_LOADCT_NEW_CT, ID:ev.id, TOP:ev.top, HANDLER:0L}
END

PRO spice_cw_loadct_reverse,ev
  
  reverse_colors
  
  ; send an event out to the top widget to notify it that color table changed
  widget_control, ev.top, send_event = {SPICE_CW_LOADCT_NEW_CT, ID:ev.id, TOP:ev.top, HANDLER:0L}
END


FUNCTION spice_cw_loadct,base,xsize=xsize,ysize=ysize,frame=frame,file=file,$
                   menu=menu,font=font
  
  default,xsize,256
  default,ysize,20
  default,frame,0
  default,file,''
  default,font,''
  default,menu,2
  
  menu = menu > 1
  names = 0
  
  IF KEYWORD_SET(file) THEN $
     IF NOT file_exist(file) THEN file = ''
  
  f = concat_dir(GETENV('SSW_SETUP_DATA'), 'color_table.eit')
  
  IF file NE '' THEN loadct,get_names=names,file=file $
  ELSE               loadct,get_names=names
  
  N = N_ELEMENTS(names)
  
  ibase=WIDGET_BASE(base,/column,event_pro='spice_cw_loadct_newstart',frame=frame,$
                    space=1,xpad=1,ypad=1)
  
  irow = WIDGET_BASE(ibase,/row,space=1,xpad=1,ypad=1)
  
  pulldown = WIDGET_BUTTON(irow,value='Select color table',menu=menu,$
                           font=font)
  reverse_b = WIDGET_BUTTON(irow,value='Reverse colors',font=font,$
                           event_pro='spice_cw_loadct_reverse')
  
  pd_part_one = WIDGET_BUTTON(pulldown,/menu,font=font,$
                              value='Tables 0 - '+trim(N/2))
  pd_part_two = WIDGET_BUTTON(pulldown,/menu,font=font,$
                              value='Tables '+trim(N/2+1)+' - '+trim(N-1))
  FOR i = 0,N/2 DO $
     dummy = WIDGET_BUTTON(pd_part_one,value=names(i),uvalue=i,font=font)
  FOR i = N/2,N-1 DO $
     dummy = WIDGET_BUTTON(pd_part_two,value=names(i),uvalue=i,font=font)
  
  
  bar = WIDGET_DRAW(ibase,xsize=xsize,ysize=ysize,/button_events,$
                    event_pro='spice_cw_loadct_event',$
                    notify_realize='spice_cw_loadct_realize')
  
  WIDGET_CONTROL,irow,set_uvalue=file
  spice_cw_loadct_adj_ctable,/initialize
END
