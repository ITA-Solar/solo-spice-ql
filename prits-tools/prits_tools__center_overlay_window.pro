;+
;  FILE: prits_tools__center_overlay_window
;
;  PURPOSE:
;       Center a top-level base relative to another one
;
;  CATEGORY:
;       Widgets
;
;  MODIFICATION HISTORY:
;       2020-08-28  SVHH Adapted from prits_tools.center_window
;
;-
; -----------------------------------------------------------------------------
;
; Purpose: Given top level bases A and B, move A to the center of B
;
; Keywords: None
;
; $Id: 2024-02-13 13:59 CET $
;-

PRO prits_tools::center_overlay_window, new_window, old_window
  compile_opt idl2, static
  widget_control, new_window, tlb_get_size=new_window_tlb_size
  widget_control, old_window, tlb_get_size=old_window_tlb_size
  widget_control, old_window, tlb_get_offset=old_window_offset
  
  old_window_center = old_window_offset + old_window_tlb_size/2
  new_window_offset = old_window_center - new_window_tlb_size/2
  
  widget_control,new_window, xoffset=new_window_offset[0], yoffset=new_window_offset[1]
END
