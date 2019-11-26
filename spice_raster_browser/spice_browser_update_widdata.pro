;+
; NAME:
;     spice_browser_update_widdata
;
; PURPOSE:
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_update_widdata, state, meta
;
; INPUTS:
;     input:  XXX
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     None.
;
; OUTPUT:
;     XXX
;
; EXAMPLE:
;
; INTERNAL ROUTINES:
;
; PROGRAMMING NOTES:
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


PRO spice_browser_update_widdata, state, meta
  ;
  ; This takes the metadata structure (see spice_browser_get_metadata)
  ; and modifies entries in wid_data. (This is needed when a new object
  ; is loaded.)
  ;
  wid_data=state.wid_data

  wid_data.xpos=meta.xpos
  wid_data.ypos=meta.ypos
  wid_data.midtime=meta.midtime
  wid_data.tmid=meta.midtime
  wid_data.utc=meta.utc
  wid_data.sit_stare=meta.sit_stare
  wid_data.origin=meta.origin
  wid_data.scale=meta.scale
  wid_data.tmid_min=meta.tmid_min
  wid_data.rast_direct=meta.rast_direct
  wid_data.l1p5_ver=meta.l1p5_ver

  state.wid_data=wid_data
  widget_control,state.spice_browser_base,set_uvalue=state

END
