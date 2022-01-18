;+
; NAME:
;     spice_browser_calc_zoom_params
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_calc_zoom_params, state, pwin
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


PRO spice_browser_calc_zoom_params, state, pwin
  ;
  ; Calculates the pixel ranges to be displayed given the zoom
  ; parameters and selected pixels. The ranges are stored in
  ; wid_data.xrange, .yrange, .lrange.
  ;
  iwin=state.wid_data.iwin[pwin]
  lam=state.data->get_lambda_vector(iwin)

  nx=state.wid_data.nxpos
  ny=state.wid_data.ny
  nl=n_elements(lam)

  xpix=state.wid_data.xpix - state.wid_data.ichunk*state.wid_data.nxpos
  ypix=state.wid_data.ypix
  lpix=state.wid_data.ilambda[pwin]

  origin=state.wid_data.origin
  scale=state.wid_data.scale


  ;
  ; Get x-range and y-range for images
  ; ----------------------------------
  zoom=state.wid_data.im_zoom
  ;
  ; The parameter zoom ranges from 0 to 7, and the zoom
  ; factor is then 2^zoom. From the value of zoom we can work out the
  ; pixel range of 'im' that is to be plotted. The pixel ranges are
  ; [xlim1:xlim2,ylim1:ylim2].
  ;
  xlim1=0
  ylim1=0
  ;
  IF zoom NE 0 THEN BEGIN
    ix=nx*abs(scale[0])/2/2^zoom
    iy=ny*abs(scale[1])/2/2^zoom
    ixy=max([ix,iy])
    ;
    xlim1=xpix-fix(ixy/abs(scale[0]))
    xlim2=xpix+fix(ixy/abs(scale[0]))
    IF xlim1 LT 0 THEN BEGIN
      xlim2=min([xlim2-xlim1,nx-1])
      xlim1=0
    ENDIF
    ;
    IF xlim2 GE nx THEN BEGIN
      xlim1=max([xlim1-(xlim2-nx),0])
      xlim2=nx-1
    ENDIF
    ;
    ylim1=ypix-fix(ixy/abs(scale[1]))
    ylim2=ypix+fix(ixy/abs(scale[1]))
    IF ylim1 LT 0 THEN BEGIN
      ylim2=min([ylim2-ylim1,ny-1])
      ylim1=0
    ENDIF
    ;
    IF ylim2 GE ny THEN BEGIN
      ylim1=max([ylim1-(ylim2-ny),0])
      ylim2=ny-1
    ENDIF
    ;
    state.wid_data.xrange=[xlim1,xlim2]
    state.wid_data.yrange=[ylim1,ylim2]
  ENDIF ELSE BEGIN
    state.wid_data.xrange=[0,nx-1]
    state.wid_data.yrange=[0,ny-1]
  ENDELSE


  ;
  ; Get range for wavelength window
  ; -------------------------------
  spec_zoom=state.wid_data.spec_zoom[pwin]
  ;
  IF spec_zoom NE 0 THEN BEGIN
    nlz=nl/2/2^(spec_zoom)
    ;
    i0=max([lpix-nlz,0])
    i1=min([lpix+nlz-1,nl-1])
    ;
    state.wid_data.lrange[*,pwin]=[max([i0,0]),min([i1,nl-1])]
  ENDIF ELSE BEGIN
    state.wid_data.lrange[*,pwin]=[0,nl-1]
  ENDELSE

  widget_control,state.spice_browser_base,set_uvalue=state


END
