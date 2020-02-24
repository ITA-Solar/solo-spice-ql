;+
; NAME:
;     spice_browser_update_info
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_update_info, state
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
; $Id: 24.02.2020 20:49 CET $


PRO spice_browser_update_info, state
  ;
  ; This updates the metadata displayed on the left-side of the
  ; widget. This is needed when browsing multiple files.
  ;

  xpix=state.wid_data.xpix
  tmid=state.wid_data.midtime
  widget_control,state.xtext,set_val='X-pixel: '+trim(xpix)
  n=state.wid_data.nx
  IF xpix GE n THEN tstr='Time: N/A' ELSE tstr='Time: '+tmid[xpix]
  widget_control,state.ttext,set_val=tstr

  date_obs=state.data->get_header_info('DATE-BEG', 0)
  ex=anytim(/ex,date_obs)
  val='TIME: '+strpad(trim(ex[0]),2,fill='0')+':'+strpad(trim(ex[1]),2,fill='0')
  widget_control,state.text3,set_val=val

END
