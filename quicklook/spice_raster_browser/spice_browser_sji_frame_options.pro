;+
; NAME:
;     spice_browser_sji_frame_options
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_sji_frame_options, state, default_option=default_option
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


FUNCTION spice_browser_sji_frame_options, state, default_option=default_option
  ;
  ; This works out the options for the droplist that shows the choices
  ; for the number of SJI frames to display in the movie.
  ;
  ; DEFAULT_OPTION:  an index within the output specifying the default
  ;                  option (one closest to 10 mins).
  ; OUTPUT:  A string array containing the frame options.
  ;

  ;
  ; Get number of exposures for the SJI channel
  ;
  sji_nexp=state.sji_d->getnexp(0)

  sji_cadence=state.wid_data.sji_cadence

  ;
  ; These are the options for the number of frames to display. Note
  ; that the max no. of frames is appended (although minus 1).
  ;
  value=[11,21,31,51,101,201,301,501,1001,2001,3001,5001]
  k=where(value LT sji_nexp)
  value=[value[k],sji_nexp]
  value_string=string(value)

  ;
  ; Choose default value of no. of frames by choosing closest to 10mins
  ;
  getmin=min(abs(float(value)*sji_cadence-600.),imin)
  default_option=imin

  return,value_string

END
