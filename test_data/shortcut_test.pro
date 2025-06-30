; Test program for cw_shortcuts

PRO shortcut_test_event, ev
  ; Get the widget information
  widget_control, ev.top, get_uvalue = info
  widget_control, ev.id, get_uvalue = uvalue
  CASE uvalue OF
    'SHORTCUTS': BEGIN
      widget_control, info.message_id, set_value = ev.key
    END
    'QUIT_BUTTON': BEGIN
      print, 'Quit button pressed'
      widget_control, ev.top, /destroy
    END

    ELSE: ; Do nothing for other events
  ENDCASE
END

PRO shortcut_test
  ; Create the main base widget
  base = widget_base(title = 'IDL Keyboard Shortcut Test', /column, xsize = 400, ysize = 400)

  !null = widget_label(base, value = "CW_KEYBOARD_SHORTCUTS test")
  shortcuts_id = cw_keyboard_shortcuts(base, uvalue = "SHORTCUTS")

  message_id = widget_label(base, value = 'Use arrow keys')
  !null = widget_button(base, value = 'Quit', uvalue = 'QUIT_BUTTON')

  ; Create info structure to pass widget IDs
  info = {shortcuts_id: shortcuts_id, $
    message_id: message_id}

  ; Store the info structure in the base widget
  widget_control, base, set_uvalue = info
  widget_control, base, /realize
  xmanager, 'shortcut_test', base, /no_block
END

shortcut_test
END
