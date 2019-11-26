;+
; NAME:
;     spice_browser_coltable
;
; PURPOSE:
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_coltable, desc=desc, state=state, set_value=set_value
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


PRO spice_browser_coltable, desc=desc, state=state, set_value=set_value
  ;
  ; This provides the list of color tables for the color pull-down menu
  ; (output 'desc'). It also sets the color table if 'state' and
  ; 'set_value' are specified.
  ;
  ; **Be careful when adding new color tables. Need to update 'desc' and
  ; **the if statements.

  ;
  ; This determines if the user has the aia_lct routine.
  ;
  swtch=have_proc('aia_lct')

  desc=['1\COLOR', $
    '0\B+W','0\Blue']

  IF have_proc('aia_lct') THEN BEGIN
    desc=[desc,'0\Red','2\AIA 193']
  ENDIF ELSE BEGIN
    desc=[desc,'2\Red']
  ENDELSE

  n_ct=n_elements(desc)

  IF n_tags(state) NE 0 AND n_elements(set_value) NE 0 THEN BEGIN
    state.wid_data.coltable=set_value
    widget_control,state.spice_browser_base,set_uvalue=state
    ;
    IF set_value EQ 0 THEN loadct,0
    IF set_value EQ 1 THEN loadct,1
    IF set_value EQ 2 THEN loadct,3
    IF set_value EQ 3 THEN aia_lct,r,g,b,wavelnth=193,/load
  ENDIF


END
