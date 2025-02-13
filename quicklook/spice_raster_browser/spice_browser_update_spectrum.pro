;+
; NAME:
;     spice_browser_update_spectrum
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_update_spectrum, state, pwin
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
; $Id: 2024-11-28 14:38 CET $

PRO spice_browser_update_spectrum, state, pwin
  ;
  ; This routine updates the state.spectra (1D spectra) and
  ; state.expimages (lambda-Y plots) tags for the window with index
  ; pwin.
  ;
  ; MODIFIES: state.spectra, state.expimages, state.wid_data.exptime
  ;
  iwin = state.wid_data.iwin[pwin]
  xpix = state.wid_data.xpix
  ypix = state.wid_data.ypix
  nl = state.data.get_header_keyword('NAXIS3', iwin)

  exptime = replicate(state.data.get_exposure_time(iwin), state.data.get_number_exposures(iwin))

  nx = state.wid_data.nx
  ny = state.data.get_header_keyword('NAXIS2', iwin)

  state.spectra[*, pwin] = 0.
  state.expimages[*, pwin] = 0.

  ;
  ; The following loads up the expimages and spectra tags with the new
  ; data. Note that the exposure image is divided by the exposure time
  ; to be consistent with the raster image.
  ;
  IF xpix LT nx THEN BEGIN
    widget_control, state.mask_butt, get_value = masking
    no_masking = masking[0] EQ 0
    expimg = state.data.get_one_image(iwin, xpix, no_masking = no_masking)
    IF exptime[xpix] NE 0. THEN expimg = expimg / exptime[xpix]
    state.expimages[0 : nl - 1, 0 : ny - 1, pwin] = expimg
    state.spectra[0 : nl - 1, pwin] = expimg[*, ypix]
  ENDIF ELSE BEGIN
    state.expimages[*, *, pwin] = 0
    state.spectra[*, pwin] = 0
  ENDELSE

  ;
  ; Get exposure time for window.
  ;
  state.wid_data.exptime[pwin] = exptime[xpix]

  widget_control, state.spice_browser_base, set_uvalue = state
END
