;+
; Project     : XCFIT_BLOCK     
;                   
; Name        : CW_SHORTCUTS
;               
; Purpose     : Implements a pusbutton status switch (e.g., On/Off)
;               
; Explanation : Compound widget to generate keyboard shortcut events
;               
; Use         : ID=CW_SHORTCUTS(BASE,uvalue=<...>)
;    
; Inputs      : BASE : The base to put the widget on.
;               
; Opt. Inputs : 
;               
; Outputs     : Creates events with ev.key information
;               
; Opt. Outputs: 
;               
; Keywords    : UVALUE, the usual value
;
; Calls       : 
;
; Common      : None.
;               
; Restrictions: 
;               
; Side effects: None known.
;               
; Category    : Compound widget
;               
; Prev. Hist. : None.
;
; Written     : S. V. H. Haugan, UiO, 27 June 2025
;               
; Modified    : 
;                       
; Version     : 1, 27 June 2025
;-            


FUNCTION cw_shortcuts_getv,id
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  return,0
END


PRO cw_shortcuts_setv,id,uval
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  
  widget_control, info.text_id, set_value=['0','1x3','4']
  widget_control, info.text_id, set_text_select=[3,1]
  widget_control, info.text_id, /input_focus
  
  widget_control,storage,set_uvalue=info
END


FUNCTION cw_shortcuts_event,ev
  storage = widget_info(ev.handler, /child)
  widget_control,storage,get_uvalue = info
  
  offset = ev.offset
  dir = '? ' + trim(offset) + ' ?'
  if offset eq 1 OR offset EQ 0 then dir = "UP"
  if offset eq 7 OR offset EQ 6 then dir = "DOWN"
  if offset eq 3 then dir = "LEFT"
  if offset eq 5 then dir = "RIGHT"
  
  event = {cw_shortcuts, $
           id:ev.handler, $
           top:ev.top, $
           handler:0L, $
           key: dir $
          }
  
  widget_control, info.text_id, set_value=['0','1x3','4']
  widget_control, info.text_id, set_text_select=[3,1]
  widget_control, info.text_id, /input_focus

  return,event
END


FUNCTION cw_shortcuts,on_base,uvalue=uvalue
  
  default,uvalue,'CW_SHORTCUTS'
  default,instruct,'Enter value'
  
  small = {xpad:1,ypad:1,space:1}
  
  my_base = widget_base(on_base,uvalue=uvalue,$
                     event_func='cw_shortcuts_event',$
                     pro_set_value='cw_shortcuts_setv',$
                     func_get_value='cw_shortcuts_getv')
  
  text_id = widget_text(my_base, value=['0','1x3','4'], /editable, /all_events, $
                        xsize=5, ysize=5, scr_xsize=1, scr_ysize=1, uvalue='TEXT_FIELD')


  storage = text_id
  
  info = {text_id:text_id, uvalue:uvalue}
  
  widget_control,storage,set_uvalue=info,/no_copy
  return,my_base
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   xcfit_block_test2
END

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'cw_shortcuts.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
