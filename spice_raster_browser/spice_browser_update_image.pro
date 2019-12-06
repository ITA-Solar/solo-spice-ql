;+
; NAME:
;     spice_browser_update_image
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_update_image, state, pwin
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


PRO spice_browser_update_image, state, pwin
  ;
  ; Updates the state.images tag with the image for the window with
  ; index pwin.
  ;
  wwidth=state.wid_data.lbin

  t1=systime(1)

  missing=[-200.,-199.]
  nmiss=n_elements(missing)

  iwin=state.wid_data.iwin[pwin]
  nx=state.wid_data.nx
  nxpos=state.wid_data.nxpos
  ny=state.wid_data.ny
  nl=state.data->get_header_info('NAXIS1', iwin)

  exptime=replicate(state.data->get_exposure_time(iwin), state.data->get_number_exposures(iwin))

  ;
  ; The following is used for breaking long sit-and-stare sequences into
  ; more manageable chunks. X0 and X1 define the X-indices of the
  ; start/end of the chunk.
  ;
  x0=state.wid_data.ixpos
  x1=min([state.wid_data.jxpos,state.wid_data.nx-1])
  nx=x1-x0+1

  scale=state.wid_data.scale

  lam=state.data->get_lambda_vector(iwin)
  getmin=min(abs(lam-state.wid_data.lambda[pwin]),imin)
  wpix=imin

  j0=max([0,wpix-wwidth/2])
  j1=min([nl-1,wpix+wwidth/2])

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

  wd=fltarr(wwidth,nx,ny)
  img=fltarr(nx,ny+40)
  ;
  IF state.wid_data.rast_direct EQ 0 THEN BEGIN
    i0=x1
    i1=x0
    dx=-1
  ENDIF ELSE BEGIN
    i0=x0
    i1=x1
    dx=1
  ENDELSE
  ;
  exptime=exptime[i0:i1]
  ;
  FOR i=i0,i1,dx DO BEGIN
    expimg=state.data->get_one_image(iwin,i)
    expt=exptime[i-i0]
    IF expt NE 0. THEN wd[*,i-i0,*]=expimg[j0:j1,*]/expt ELSE wd[*,i-i0,*]=expimg[j0:j1,*]
  ENDFOR
  ;
  FOR j=0,nmiss-1 DO BEGIN
    k=where(wd EQ missing[j],nk)
    IF nk GT 0 THEN wd[k]=0.
  ENDFOR
  ;
  IF wwidth GT 1 THEN BEGIN
    img[x0-i0:x1-i0,yoff:yoff+ny-1]=average(wd[0:wwidth-1,*,*],1,missing=0.)
  ENDIF ELSE BEGIN
    img[x0-i0:x1-i0,yoff:yoff+ny-1]=reform(wd[wpix-j0,*,*])
  ENDELSE

  wd=0

  state.images[*,*,pwin]=0
  state.images[0:nx-1,*,pwin]=img

  widget_control,state.spice_browser_base,set_uvalue=state

  t2=systime(1)
  ;print,'Update image '+trim(pwin)+' takes '+trim(string(format='(f5.1)',t2-t1))+' s'

END
