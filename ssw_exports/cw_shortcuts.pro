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
  return,info
END


PRO cw_shortcuts_setv,id,message
  storage = widget_info(id,/child)
  widget_control,storage,get_uvalue=info
  
  widget_control, info.text_id, set_value=['U','X','D']
  widget_control, info.text_id, set_text_select=[2,1]
  widget_control, info.text_id, /input_focus
  widget_control, id, timer = 0.25
END


FUNCTION cw_shortcuts_event,ev
  ; Reset text widget first thing:
  cw_shortcuts_setv, ev.handler

  type = tag_names(ev,/structure_name)

  if type eq 'WIDGET_TIMER' then begin
    cw_shortcuts_setv, ev.handler
    return, 0
  endif

  ; Ignore anything but arrow keys for now
  if type ne 'WIDGET_TEXT_SEL' then return,0

  CASE ev.offset OF 
     0: dir = 'UP   '
     1: dir = 'UP   '
     2: dir = 'LEFT '
     3: dir = 'RIGHT'
     4: dir = 'RIGHT'
     5: dir = 'DOWN '
  END
  print, dir
  event = {cw_shortcuts, $
           id:ev.handler, $
           top:ev.top, $
           handler:0L, $
           key: dir $
          }
  ; 
  return,event
END


FUNCTION cw_shortcuts,on_base,uvalue=uvalue
  
  default,uvalue,'CW_SHORTCUTS'
  default,instruct,'Enter value'
  
  small = {xpad:1,ypad:1,space:1}
  
  my_base = widget_base(on_base,uvalue=uvalue,$
                        frame=0, xpad=0, ypad=0, scr_xsize=1, scr_ysize=1, xsize=1, ysize=1, $
                        event_func='cw_shortcuts_event',$
                        pro_set_value='cw_shortcuts_setv',$
                        func_get_value='cw_shortcuts_getv', $
                        notify_realize='cw_shortcuts_setv')
  
  text_id = widget_text(my_base, value=['U','X','D'], /all_events, /editable, $
                        xsize=2, ysize=3, uvalue='TEXT_FIELD')


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
