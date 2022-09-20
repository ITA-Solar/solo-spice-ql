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
; $Id: 2022-09-20 14:35 CEST $


PRO spice_browser_update_spectrum, state, pwin
  ;
  ; This routine updates the state.spectra (1D spectra) and
  ; state.expimages (lambda-Y plots) tags for the window with index
  ; pwin.
  ;
  ; MODIFIES: state.spectra, state.expimages, state.wid_data.exptime
  ;
  iwin=state.wid_data.iwin[pwin]
  xpix=state.wid_data.xpix
  ypix=state.wid_data.ypix
  nl=state.data->get_header_keyword('NAXIS3', iwin)

  exptime=replicate(state.data->get_exposure_time(iwin), state.data->get_number_exposures(iwin))

  ;
  ; This is the X-offset (index number) used for "chunked" sit-and-stare data.
  ;
  xoff=state.wid_data.ichunk*state.wid_data.nxpos

  nx=state.wid_data.nx
  ny=state.data->get_header_keyword('NAXIS2', iwin)
  scale=state.wid_data.scale

  state.spectra[*,pwin]=0.
  state.expimages[*,pwin]=0.

  ;
  ; Work out Y-offsets between the 3 channels.
  ;
  ;  IF state.wid_data.yoffsets EQ 1 THEN BEGIN
  ;    reg=state.data->getregion(iwin,/full)
  ;    ;
  ;    CASE reg OF
  ;      'FUV1': yoff=round(6.0/scale[1])
  ;      'FUV2': yoff=round(2.0/scale[1])
  ;      'NUV': yoff=0
  ;    ENDCASE
  ;  ENDIF ELSE BEGIN
  yoff=0
  ;  ENDELSE

  ;
  ; The following loads up the expimages and spectra tags with the new
  ; data. Note that the exposure image is divided by the exposure time
  ; to be consistent with the raster image.
  ;
  IF xpix LT nx THEN BEGIN
    expimg=state.data->get_one_image(iwin,xpix)
    IF exptime[xpix] NE 0. THEN expimg=expimg/exptime[xpix]
    state.expimages[0:nl-1,yoff:yoff+ny-1,pwin]=expimg
    state.spectra[0:nl-1,pwin]=expimg[*,ypix-yoff]
  ENDIF ELSE BEGIN
    state.expimages[*,*,pwin]=0
    state.spectra[*,pwin]=0
  ENDELSE

  ;
  ; Get exposure time for window.
  ;
  state.wid_data.exptime[pwin]=exptime[xpix]

  widget_control,state.spice_browser_base,set_uvalue=state

END
