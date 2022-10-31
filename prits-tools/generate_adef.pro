;+
; NAME:
;      GENERATE_ADEF
;
; PURPOSE:
;      This function finds peaks and widths of lines in a given spectrum.
;      It returns those values in form of fit components, defined in mk_comp_gauss().
;
; CATEGORY:
;      Fitting -- utility
;
; CALLING SEQUENCE:
;      adef = generate_adef(data, lam, widmin=widmin)
;
; INPUTS:
;      data: The data cube to be analysed. The first dimension must be the wavelength.
;      lambda: A cube of same size as data. Contains the wavelength of each pixel in 'data'.
;
; OPTIONAL INPUTS:
;      widmin: Minimum width of a gaussian fit
;      VELOCITY : Set this equal to the initial velocity if you want
;                 the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength
;                 is taken from the supplied POSITION, i.e., INT_POS_FWHM(1).
;                 This input is ignored if /POSITION is set.
;                 Default is zero.
;
; KEYWORDS:
;      position: If set, then the line position is NOT represented by the velocity
;              relative to a lab wavelength, but as the wavelength.
;
; OUTPUTS:
;      Structure containing a list of found fit components, including background component.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      spice_gt_peaks, mk_comp_gauss, mk_comp_poly, box_message
;
; HISTORY:
;      Ver. 1, 18-Oct-2021, Martin Wiesmann
;      Ver. 1.1, 17-Jan-2022, Terje Fredvik: minimum line width is determined
;                                            by the instrument optics and
;                                            should be the same for all lines.
;      Ver. 1.2, 13-Jun-2022, Martin Wiesmann: position is now by default represented as
;                                            velocity, added keywords velocity and position.
;-
; $Id: 2022-10-31 10:45 CET $


FUNCTION generate_adef, data, lam, widmin=widmin, position=position, velocity=velocity
  ;; Automatically generate cfit analysis definitions based on input intensity and
  ;; wavelength arrays
  blue_means_negative_velocity = 1
  sz = size(data)

  badix = where(data ne data, n_bad, complement=goodix)
  IF n_bad GT 0 THEN data[badix] = min(data[goodix]) > 0

  meanprofile = rebin(data,sz[1],1,1)

  IF n_bad GT 0 THEN data[badix] = (typename(data) EQ 'FLOAT') ? !values.f_nan : !values.d_nan
  peakinds = spice_gt_peaks(meanprofile, fwhm=fwhm, minmedian=4.5,/sort,/plot)
  npeaks = n_elements(peakinds)

  gaussians = replicate(mk_comp_gauss([0,0,0]), npeaks)

  int0 = meanprofile[peakinds]
  lampeak = lam[peakinds,*,*]
  lamfwhm = lam[peakinds-fwhm,*,*]

  lam0 = fltarr(npeaks)
  wid0 = fltarr(npeaks)

  FOR i=0,npeaks-1 DO lam0[i] = median(lampeak[i,*,*])
  FOR i=0,npeaks-1 DO wid0[i] = lam0[i] - median(lamfwhm[i,*,*])

  v = 150.                       ; Max shift in km/s
  dlam = v*lam0/3.e5            ; Max shift in Aangstrom

  intmin = fltarr(npeaks)          ; minimum intensity is 0
  lammin = (lam0 - dlam) > min(lam); v0 - v
  IF NOT keyword_set(widmin) THEN widmin = min((wid0 - 0.04) >  0.02)  ; random guess...

  intmax = int0*100;30000                 ; More random guessing
  lammax = (lam0 + dlam) < max(lam) ; v0 + v
  widmax = wid0 + 0.04              ; A final shot in the dark

  ; taken from xcfit:
  widmax = 10*wid0
  widmin = 0.1*wid0

  IF ~keyword_set(position) THEN BEGIN
    IF N_ELEMENTS(velocity) EQ 0 THEN vel=0.0 $
    ELSE vel = velocity
  ENDIF

  FOR i=0,n_elements(peakinds)-1 DO BEGIN
    ;    gauss = spice_mk_comp_gauss([int0[i],lam0[i],wid0[i]], $
    ;      max_arr=[intmax[i],lammax[i],widmax[i]], $
    ;      min_arr=[intmin[i],lammin[i],widmin], $
    ;      ;trans_a=[1,1,0.424661], trans_b=[0,0,0], $
    ;      ;const=[0b,0b,0b], $
    ;      velocity=vel)
    gauss = spice_mk_comp_gauss([int0[i],lam0[i],wid0[i]], $
      max_lam=lammax[i], min_lam=lammin[i],$
      min_fwhm = widmin[i], max_fwhm = widmax[i],$
      min_intens=intmin[i], $
      velocity=vel)
    IF ~keyword_set(position) AND keyword_set(blue_means_negative_velocity) THEN BEGIN
      gauss.param(1).trans_a = -gauss.param(1).trans_a
    ENDIF
    lam0txt = trim(lam0[i],'(F6.2)')
    gauss.name = 'AutoGauss'+lam0txt
    gaussians[i] = gauss
  ENDFOR


  bg = mk_comp_poly([0.5*median(meanprofile)], max_arr=[30000],min_arr=[0],trans_a=[1],$
    trans_b=[0],const=[0b])
  bg.name = 'Background'

  IF npeaks EQ 1 THEN adef = {igauss2:gaussians[0], bg:bg}
  IF npeaks EQ 2 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], bg:bg}
  IF npeaks EQ 3 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], bg:bg}
  IF npeaks EQ 4 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], bg:bg}
  IF npeaks EQ 5 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], $
    igauss6:gaussians[4], bg:bg}
  IF npeaks EQ 6 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], $
    igauss6:gaussians[4], igauss7:gaussians[5], bg:bg}
  IF npeaks EQ 7 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], $
    igauss6:gaussians[4], igauss7:gaussians[5], $
    igauss8:gaussians[6], bg:bg}
  IF npeaks EQ 8 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], $
    igauss6:gaussians[4], igauss7:gaussians[5], $
    igauss8:gaussians[6], igauss9:gaussians[7], $
    bg:bg}
  IF npeaks GE 9 THEN adef = {igauss2:gaussians[0], igauss3:gaussians[1], $
    igauss4:gaussians[2], igauss5:gaussians[3], $
    igauss6:gaussians[4], igauss7:gaussians[5], $
    igauss8:gaussians[6], igauss9:gaussians[7], $
    igauss10:gaussians[8], bg:bg}


  IF npeaks GE 9 THEN box_message,'Found '+trim(npeaks)+', fitting the 9 highest...'

  return, adef
END
