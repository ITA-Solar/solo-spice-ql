;+
; NAME:
;     spice_browser_wvl_list
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_wvl_list, data, wid_data
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


FUNCTION spice_browser_wvl_list, data, wid_data
  ;
  ; This creates the pull-down list of wavelength windows.
  ;
  id=data->get_window_id()
  n_id=n_elements(id)
  choices='1\Choose a wavelength window'
  FOR i=0,n_id-1 DO BEGIN
    IF i NE n_id-1 THEN choices=[choices,'0\'+id[i]] ELSE choices=[choices,'2\'+id[i]]
  ENDFOR
  ;
  ; PRY, 16-Mar-2017
  ; The option below introduces an extra button that allows the Whisker
  ; plot to be made. However, this requires a change to be made to the
  ; whisker object, which hasn't been made yet. I've changed the check
  ; below to 2 instead of 1 (no data-set will have a value of 2) so that
  ; the button never appears. If the object does get fixed, then I can
  ; change the check below back to 1.
  ;
  IF wid_data.sit_stare EQ 2 THEN choices=[choices,'0\Whisker (t-Y) plot']
  return,choices

END
