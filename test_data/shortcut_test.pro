; Test program for keyboard shortcuts in IDL widgets
; This program creates a simple widget with:
; - A base widget
; - An editable text field
; - Keyboard event handling and focus management

PRO shortcut_test_event, event

  ; Get the widget information
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=uvalue
  CASE uvalue OF
     'TEXT_FIELD':BEGIN
        dir = "? " + trim(event.offset) + " ?"
        CASE event.offset OF 
           0: dir = '0 = UP'
           1: dir = '1 = UP'
           2: dir = '2 = LEFT'
           3: dir = '3 = RIGHT'
           4: dir = '4 = RIGHT'
           5: dir = '5 = DOWN'
        END
        print, dir
        widget_control, info.message_id, set_value=dir
        widget_control, event.id, set_text_select=[2,1]
        END

     'TEXT_FIELD2':BEGIN
        print,"TEXT_FIELD2 event"
        END
        
     'FOCUS_BUTTON':BEGIN
        print, 'Setting focus to text field'
        widget_control, info.text_id, /input_focus
        ENDCASE
        
     'CLEAR_BUTTON':BEGIN
        print, 'Clearing text field'
        widget_control, info.text_id, set_value=['U','X','D']
        widget_control, info.text_id, /input_focus
     ENDCASE
     
     'QUIT_BUTTON': BEGIN
        print, 'Quit button pressed'
        widget_control, event.top, /destroy
     END
     
     ELSE: ; Do nothing for other events
     ENDCASE

  END

PRO shortcut_test

  ; Create the main base widget
  base = widget_base(title='IDL Keyboard Shortcut Test', $
                     column=1, $
                     /align_center, $
                     xsize=400, $
                     ysize=300)
  
  ; Create a label with instructions
  instructions = 'Keyboard Shortcut Test Application'
  !null = widget_label(base, value=instructions, /align_left)
  
  ; Create the editable text field
  ; Use /all_events to capture keyboard events
  tfbase = widget_base(base, frame=0, xpad=0, ypad=0, scr_xsize=1, scr_ysize=1, xsize=1, ysize=1)
  text_field = widget_text(tfbase, $
                          value=['U','X','D'], $
                          /all_events, $
                          xsize=3, $
                           ysize=4, $
                           frame=0, $
                          uvalue='TEXT_FIELD')
  
  ; Create a button row (+message)
  button_base = widget_base(base, row=1, /align_center)
  
  message_id = widget_label(button_base, value='Use arrow keys')
  
  ; Create buttons
  !null = widget_button(button_base, $
                       value='Set Focus to Text', $
                       uvalue='FOCUS_BUTTON')
  
  !null = widget_button(button_base, $
                       value='Clear Text', $
                       uvalue='CLEAR_BUTTON')
  
  !null = widget_button(button_base, $
                       value='Quit', $
                       uvalue='QUIT_BUTTON')
  
  ; Create info structure to pass widget IDs
  info = {text_id: text_field, $
          message_id: message_id, $
          base_id: base}
  
  ; Store the info structure in the base widget
  widget_control, base, set_uvalue=info
  
  ; Realize the widget
  widget_control, base, /realize
  widget_control, text_field, /input_focus
  widget_control, text_field, set_text_select=[2,1]
  
  ; Set initial focus to the text field
  widget_control, text_field, /input_focus
  
  ; Print startup message
  print, 'Keyboard shortcut test application started'
  print, 'Widget ID of text field: ', text_field
  print, 'Focus should be on the text field'
  
  ; Start the event loop
  xmanager, 'shortcut_test', base, /no_block
  
END

shortcut_test
end
