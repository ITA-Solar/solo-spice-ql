FUNCTION spice_plot_mask_auto_fit, spec, mask_fit, _extra = extra, $
  FUNCTION_name = FUNCTION_name
  ;+
  ; NAME:
  ;     SPICE_PLOT_MASK_AUTO_FIT
  ;
  ; PURPOSE:
  ;     Plots the Gaussian fits produced by spice_mask_auto_fit.
  ;
  ; CATEGORY:
  ;     SPICE; fitting; display.
  ;
  ; CALLING SEQUENCE:
  ;     Result = SPICE_PLOT_MASK_AUTO_FIT( Spec, Mask_Fit )
  ;
  ; INPUTS:
  ;     Spec:  An IDL structure containing the spectrum that was fitted.
  ;            Should be the same spectrum that was given to
  ;            spice_mask_auto_fit.
  ;     Mask_Fit:  The output from spice_mask_auto_fit (an IDL structure).
  ;
  ; OUTPUTS:
  ;     An IDL plot object showing the spectrum that was fit (thick,
  ;     stairstep curve), the fit function (thick, blue) and the
  ;     individual Gaussians (thin, black).
  ;
  ; EXAMPLE:
  ;     IDL> mask_fit=spice_mask_auto_fit(wd,spec,template)
  ;     IDL> w=spice_plot_mask_auto_fit(spec,mask_fit)
  ;
  ; MODIFICATION HISTORY:
  ;     Ver.1, 08-Nov-2024, Peter Young
  ;       Adapted from spice_plot_mask_auto_fit.pro.
  ;-

  IF n_elements(FUNCTION_name) EQ 0 THEN FUNCTION_name = 'gauss_sg'

  w = window(_extra = extra)

  xra = [mask_fit[0].x0, mask_fit[0].x1]

  th = 2
  fs = 12
  xtl = 0.015
  ytl = 0.015

  p = plot(spec.wvl, spec.int, /stairstep, xra = xra, /xsty, /current, $
    th = th, xth = th, yth = th, font_size = fs, $
    xticklen = xtl, yticklen = ytl, $
    xtitle = 'Wavelength / ' + string(197b), $
    ytitle = 'Intensity', $
    _extra = extra)

  n = n_elements(mask_fit)

  nw = 101
  x = findgen(nw) / float(nw - 1) * (xra[1] - xra[0]) + xra[0]
  func = fltarr(nw)

  IF mask_fit[0].y0 NE mask_fit[0].y1 THEN BEGIN
    c = linfit([mask_fit[0].x0, mask_fit[0].x1], [mask_fit[0].y0, mask_fit[0].y1])
    bg = c[0] + c[1] * x
  ENDIF ELSE BEGIN
    bg = mask_fit[0].y0 + fltarr(nw)
  ENDELSE

  sig_fwhm = 2.35482

  FOR i = 0, n - 1 DO BEGIN
    add_func = call_function(FUNCTION_name, x, [mask_fit[i].peak, mask_fit[i].wvl, mask_fit[i].width / sig_fwhm])
    ; add_func=mask_fit[i].peak*exp( -(x-mask_fit[i].wvl)^2/2./(mask_fit[i].width/sig_fwhm)^2)
    func = func + add_func
    !NULL = plot(/overplot, x, add_func + bg)
  ENDFOR

  !NULL = plot(x, func + bg, th = th, /overplot, color = 'blue')

  p.yrange = [0, max(func + bg) * 1.1]

  return, w
END
