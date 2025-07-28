; Simple utility to display a message with a dismiss button

FUNCTION xcfit_announce_text
  txt = inline_text("  ; -")
  ; ***********************************************************************
  ; XCFIT_BLOCK has gotten a facelift!
  ; ***********************************************************************
  ;
  ; First: if you experience problems with XCFIT_BLOCK, send an email to
  ; prits-group@astro.uio.no. This message is meant to be shown only
  ; once, but can be displayed again with "xcfit_block,/help"
  ;
  ; For new users (existing users look below for news)
  ;
  ; 1. Please read the documentation. It's a powerful program but has a
  ; steep learning curve.
  ;
  ; 2. Navigating inside the data cube is done by mouse clicks and arrow
  ; keys:
  ;
  ; Left click  : zoom out
  ; Middle click: set focus
  ; Right click : zoom in
  ;
  ; 3. On laptops with no mouse, right-click is usually a tap with two
  ; fingers. Middle-click is more difficult, you'll have to find out what
  ; is right for your machine. On MacOS, XQuartz has a setting to emulate
  ; a third button with Alt or Cmd. I can also recommend BetterTouchTool which
  ; allows you to do a middle-click in many different ways.
  ;
  ; 4. Warning: This line fitting tool forces you to look at residuals. Please
  ; don't ignore them. Also, you can choose to show chi^2 from the
  ; result (pulldown menu at bottom of buttons area). Please have
  ; a look, it can tell you if your data is systematically wrong.
  ;
  ; NEWS:
  ;
  ; 1. You can now navigate from pixel to pixel using arrow keys! The
  ; movement direction inside the data cubes is given by the dimensions
  ; displayed in the "currently focused data cube". The currently focused
  ; data cube is indicated with a yellow header, and is the one with the
  ; image that was clicked last. I.e., you switch by clicking on the image.
  ;
  ; 2. NOTE: XCFIT_BLOCK WILL SOMETIMES ERRONEOUSLY INTERFERE WITH KEYBOARD
  ; SELECTION IN OTHER WINDOWS. It can be very confusing, and the only way to
  ; stop the interference is to quit XCFIT_BLOCK. Sorry, no idea why this happens!
  ;
  ; 3. THE KEYBOARD FOCUS SOMETIMES GOES AWAY from the hidden text widget
  ; that captures the arrow key strokes. This is fixed by clicking a data
  ; image again.
  ;
  ; 4. It has gotten A LOT FASTER - in fact about twice as fast ASSUMING
  ; YOU HAVE ALSO INSTALLED THE CFIT DLM. If it is not installed, an automatic
  ; installation will be attempted (LOAD_GEN_DLMS), see also
  ; $SSW/gen/idl/dlm/sources/AAA-README.txt
  ;
  ; 5. XCFIT_BLOCK will now adjust the size of the plot and image windows based
  ; on your screen size.
  ;
  ; -
  txt = strmid(txt, 3, 1000)
  return, txt
END

PRO xcfit_announce_event, ev
  widget_control, ev.top, /destroy
END

FUNCTION xcfit_announce_show_it, text, once_key = once_key
  COMMON xcfit_announce, once_texts
  ; catch, err
  ; IF err NE 0 THEN return, 1

  IF n_elements(once_texts) EQ 0 THEN once_texts = !null

  IF NOT keyword_set(once_key) THEN return, 1

  already_seen = "Text " + once_key + " already seen, not showing"

  user_preferences, once_key, value, /get
  IF value EQ "seen" THEN BEGIN
    print, "User preferences: " + already_seen
    return, 0
  END

  FOR i = 0, n_elements(once_texts) - 1 DO BEGIN
    IF array_equal(*once_texts[i], text) THEN BEGIN
      print, already_seen
      return, 0
    END
  END

  once_texts = [once_texts, ptr_new(text)]
  user_preferences, once_key, "seen", /set
  return, 1
END

PRO xcfit_announce_widget, text, once_key = once_key
  IF NOT xcfit_announce_show_it(text, once_key = once_key) THEN return
  ; Build widget
  base = widget_base(/column, xoffset = 150, yoffset = 150)
  !null = widget_button(base, value = 'DISMISS', uvalue = "QUIT")
  ysize = min([45, n_elements(text)])
  !null = widget_text(base, value = text, ysize = ysize, /scroll)
  widget_control, base, /realize
  xmanager, 'XCFIT_ANNOUNCE', base
END

PRO xcfit_announce, help = help
  ; Main announcement:
  ;
  once_key = keyword_set(help) ? !null : "XCFIT_BLOCK V2 announcement"
  text = xcfit_announce_text()
  xcfit_announce_widget, text, once_key = once_key

  load_gen_dlms, success = success
  IF success THEN return
  text = ["NOTE: You do not have the CFIT DLM installed, and", $
    "an automated attempt to install it seems to have failed.", $
    "", $
    "Without the CFIT DLM, line fitting is 30% slower.", $
    "", $
    "See $SSW/gen/idl/dlm/sources/AAA-README.txt for instructions on how to", $
    "install it manually. Contact prits-group@astro.uio.no if you", $
    "have problems with the installation" $
    ]
  xcfit_announce_widget, text, once_key = "CFIT DLM announcement"
END
