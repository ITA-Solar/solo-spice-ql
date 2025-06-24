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
    'TEXT_FIELD': BEGIN
      help,event
      if event.offset eq 1 then print,"UP!"
      if event.offset eq 7 then print,"DOWN!"
      if event.offset eq 3 then print,"LEFT!"
      if event.offset eq 5 then print,"RIGHT!"
      widget_control, event.id, set_text_select=[3,1]
    END

    'TEXT_FIELD2': BEGIN
      print,"TEXT_FIELD2 event"
    END
    
    'FOCUS_BUTTON': BEGIN
      ; Set focus to the text field
      print, 'Setting focus to text field'
      widget_control, info.text_id, /input_focus
    END
    
    'CLEAR_BUTTON': BEGIN
      ; Clear the text field
      print, 'Clearing text field'
      widget_control, info.text_id, set_value=''
      widget_control, info.text_id, /input_focus
    END
    
    'QUIT_BUTTON': BEGIN
      ; Quit the application
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
  text_field = widget_text(base, $
                          value=['0','1x3','4'], $
                          /editable, $
                          /all_events, $
                          xsize=50, $
                          ysize=5, $
                          uvalue='TEXT_FIELD')
  !null = widget_text(base, $
                         value=['1','2x3','4'], $
                          /editable, $
                          /all_events, $
                          xsize=50, $
                          ysize=5, $
                          uvalue='TEXT_FIELD2')
                          
  ; Create a button row
  button_base = widget_base(base, row=1, /align_center)
  
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
          base_id: base}
  
  ; Store the info structure in the base widget
  widget_control, base, set_uvalue=info
  
  ; Realize the widget
  widget_control, base, /realize
  widget_control, text_field, /input_focus
  widget_control, text_field, set_text_select=[3,1]
  
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