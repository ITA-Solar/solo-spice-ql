;+
;  FILE: prits_tools__center_window
;
;  PURPOSE:
;       Center a top-level base on the screen
;
;  CATEGORY:
;       Widgets
;
;  CONTENTS:
;       prits_tools::center_window
;
;  NAMED STRUCTURES:
;       none.
;
;  COMMON BLOCKS:
;       none.
;
;  MODIFICATION HISTORY:
;       2020-08-24  SVHH Extracted from spice_cat
;
;-
; -----------------------------------------------------------------------------
;
; Purpose: Given a top-level base, move it to the center of the screen
;
; Keywords: None
;
; $Id: 2024-02-13 13:59 CET $
;-

PRO prits_tools::center_window, top_base
  compile_opt idl2, static
  screen_size = spice_get_screen_size()
  
  ;; Left edge offset from left edge of screen is...
  ;; middle of screen minus half our size.
  ;; Ditto for top edge.
  
  widget_control,top_base, tlb_get_size=tlb_size
  offsets = screen_size/2 - tlb_size/2
  widget_control,top_base, xoffset=offsets[0], yoffset=offsets[1]
END
