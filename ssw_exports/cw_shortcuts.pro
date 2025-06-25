;+
; Project     : XCFIT_BLOCK     
;                   
; Name        : CW_SHORTCUTS
;               
; Purpose     : Implements a pusbutton status switch (e.g., On/Off)
;               
; Explanation : This compound widget is designed to produce a button for
;               switching between e.g., modes of operation, switching
;               something on/off etc.
;
;               The VALUE is an array of texts representing different modes,
;               and the UVALUE should be an array of texts with the same
;               number of elements.
;
;               EVENTS
;               
;               When the user pushes a flipswitch button, the state (and the
;               text displayed) of the flipswitch changes. Likewise, the
;               uvalue of the flipswitch widget changes to the one
;               corresponding to the new status text. Then the button event is
;               sent on to the caller, with the ID set to the ID of the
;               flipswitch widget.
;
;               The user program determines the current state by simply
;               retrieving the UVALUE of the EVENT.ID.
;
;               READING/SETTING THE STATE
;
;               The state of the flipswitch may be read by either checking the
;               UVALUE of the widget ID, or through the WIDGET_CONTROL
;               GET_VALUE mechanism (this actually returns the UVALUE, not the
;               displayed text).
;
;               To set the state of the flipswitch, use the WIDGET_CONTROL
;               SET_VALUE=<uvalue of desired state> mechanism.
;
;               It is also possible to use this routine to implement a "small"
;               button (since the ysize is fixed).
;               
; Use         : ID=CW_SHORTCUTS(BASE,VALUE=<string_arr>,uvalue=<string_arr>)
;    
; Inputs      : BASE : The base to put the flipswitch on.
;               
; Opt. Inputs : 
;               
; Outputs     : 
;               
; Opt. Outputs: 
;               
; Keywords    : VALUE, UVALUE : Text arrays.
;
; Calls       : default, since_version(), xupdate
;
; Common      : None.
;               
; Restrictions: Arrays VALUE/UVALUE must have same number of elements > 1
;               
; Side effects: None known.
;               
; Category    : Compound widget
;               
; Prev. Hist. : None.
;
; Written     : S. V. H. Haugan, UiO, 4 January 1997
;               
; Modified    : Version 2, SVHH, 15 September 1997
;                       Added support for call mode with only one
;                       value/uvalue (faking a normal button).
;               Version 3, SVHH, 15 December 1997
;                       Added update on/off to avoid growing parent.
;                       
; Version     : 3, 15 December 1997
;-            


FUNCTION cw_shortcuts_getv,id
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  return,info.uvalue(info.i)
END


PRO cw_shortcuts_setv,id,uval
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  
  i = where(info.uvalue EQ uval(0))
  IF i(0) GT -1 THEN BEGIN
     info.i = i(0)
     widget_control,id,set_uvalue=info.uvalue(i(0))
     widget_control,info.txt_id,set_value=info.value(info.i)
  END ELSE print,"Cannot find uvalue:"+uval
  widget_control,storage,set_uvalue=info
END


FUNCTION cw_shortcuts_event,ev
  
  ;; Storage == id here..
  
  widget_control,ev.id,get_uvalue = info
  
  info.i = (info.i+1) MOD n_elements(info.value)
  xupdate,info.txt_id,0
  widget_control,info.txt_id,set_value=info.value(info.i)
  xupdate,info.txt_id,1
  widget_control,ev.handler,set_uvalue=info.uvalue(info.i)
  widget_control,ev.id,set_uvalue=info
  
  ;; Prepare event
  ev.id = ev.handler
  ev.handler = 0L
  
  return,ev
  
END


FUNCTION cw_shortcuts,on_base,value=value,uvalue=uvalue
  
  default,value,''
  default,uvalue,'CW_SHORTCUTS'
  default,instruct,'Enter value'
  
  IF since_version('4.0') THEN sml = 1 ELSE sml = 0
  small = {xpad:sml,ypad:sml,space:sml}
  
  IF n_elements(value) EQ 1 THEN value = replicate(value(0),2)
  
  IF n_elements(uvalue) EQ 1 THEN uvalue = replicate(uvalue(0),2)
  
  IF n_elements(value) NE n_elements(uvalue) THEN $
     message,"Value and uvalue must have same number of elements"
  
  base = widget_base(on_base,uvalue=uvalue,$
                     event_func='cw_shortcuts_event',$
                     pro_set_value='cw_shortcuts_setv',$
                     func_get_value='cw_shortcuts_getv')
  
  txt_id = widget_button(base,value=value(0),xoffset=0,yoffset=0,$
                         uvalue=uvalue(0))
  
  IF since_version('4.0') THEN widget_control,txt_id,scr_ysize = 25
  
  storage = txt_id
  
  i = 0L
  
  info = {txt_id:txt_id,value:value,uvalue:uvalue,i:i}
  
  IF since_version('4.0.1') THEN widget_control,txt_id,/dynamic_resize
  
  widget_control,storage,set_uvalue=info,/no_copy
  
  return,base
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'cw_shortcuts.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
