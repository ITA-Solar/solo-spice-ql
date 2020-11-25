;; TODO: documentation & get better fonts
; $Id: 2020-11-25 21:19 CET $

PRO spice_modal_message__event_handler, event
  widget_control,event.top,/destroy
END


PRO spice_modal_message,parent_base, messages, timer=timer
  base = widget_base(group_leader=parent_base,/floating, /modal, /row)
  left = widget_base(base,xsize=10)
  center = widget_base(base,/column, frame=0)
  right = widget_base(base,xsize=10)
  
  message_base = widget_base(center,/row,frame=5)
  
  left = widget_base(message_base,xsize=5)
  middle = widget_base(message_base,/column)
  right = widget_base(message_base,xsize=5)
  
  label = widget_label(middle,value=' ')
  label = widget_label(middle,value=' ')
  aligned_left = widget_base(middle,/column,/base_align_left)
  foreach message, messages DO label = widget_label(aligned_left,value='   '+message+'   ')
  label = widget_label(middle,value=' ')
  text = widget_text(center,value="    Hit any key or click to dismiss    ",/all_events)
  label = widget_label(middle,value=' ')
  widget_control,base,/realize
  spice_center_overlay_window, base, parent_base
  IF keyword_set(timer) THEN widget_control,base,timer=timer
  widget_control,text, /input_focus
  xmanager,"spice_modal_message",base,event_handler="spice_modal_message__event_handler"
END

