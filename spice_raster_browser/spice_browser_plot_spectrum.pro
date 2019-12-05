;+
; NAME:
;     spice_browser_plot_spectrum
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     XXX
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_plot_spectrum, state, pwin
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


PRO spice_browser_plot_spectrum, state, pwin
  ;
  ; Plots the spectrum in the lower window.
  ;
  iwin=state.wid_data.iwin[pwin]

  spec=state.spectra[*,pwin]

  nl=state.data->getxw(iwin)
  spec=spec[0:nl-1]


  ;
  ; Create WVL array, giving wavelengths as a function of exposure number.
  ;
  ll=state.data->getlam(iwin)
  wvl=ll
  nw=n_elements(wvl)

  xpix=state.wid_data.xpix
  ypix=state.wid_data.ypix

  lambda=state.wid_data.lambda[pwin]
  lpix=state.wid_data.ilambda[pwin]

  irange=state.wid_data.lrange[*,pwin]
  wrange=wvl[irange]


  k=where(spec NE -100 AND wvl GE wrange[0] AND wvl LE wrange[1],nk)

  IF nk NE 0 THEN yrange=[max([0,min(spec[k])*0.90]),max(spec[k])*1.10]

  title='lpix='+trim(lpix)+', t_exp='+trim(string(format='(f6.2)',state.wid_data.exptime[pwin]))+' s'

  ytitle='Intensity / DN s!u-1!n'

  wset,state.wid_data.spec_plot_id[pwin]
  IF state.wid_data.velocity EQ 1 THEN BEGIN
    v=lamb2v(wvl-lambda,lambda)
    wrange=lamb2v(wrange-lambda,lambda)
    xtitle='Velocity / km s!u-1!n'
    plot,v,spec,psym=10,/xsty,xrange=wrange, $
      tit=title,ysty=1,yrange=yrange, $
      xtitle=xtitle,ytitle=ytitle
    usersym,[-1,1,0,-1,1,0],[-1,1,0,1,-1,0],th=2
    plots,v[lpix],spec[lpix],psym=8,symsiz=2
  ENDIF ELSE BEGIN
    xtitle='Wavelength / angstroms'
    plot,wvl,spec,psym=10,/xsty,xrange=wrange, $
      tit=title,ysty=1,yrange=yrange, $
      xtitle=xtitle,ytitle=ytitle
    usersym,[-1,1,0,-1,1,0],[-1,1,0,1,-1,0],th=2
    plots,wvl[lpix],spec[lpix],psym=8,symsiz=2
  ENDELSE


  IF state.wid_data.line_ids EQ 1 THEN result=spice_browser_oplot_line_ids(wrange,!y.crange,state.wid_data.idstr,velocity=state.wid_data.velocity,refwvl=lambda)

END
