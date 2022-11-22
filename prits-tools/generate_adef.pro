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
;      adef = generate_adef(data, lam, widmin=widmin, /position, velocity=velocity, line_list=line_list)
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
;      line_list: A hash containing a predefined line list. The keys are the wavelengths of the lines
;                 to be fitted, while the value are the names of the lines.
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
;      Ver. 1.3, Nov-2022, Martin Wiesmann: Uses now spice_line_list() to get a list of possible
;                                            peaks to be included.
;-
; $Id: 2022-11-22 15:06 CET $


FUNCTION generate_adef, data, lam, widmin=widmin, position=position, velocity=velocity, $
  line_list=line_list
  ;; Automatically generate cfit analysis definitions based on input intensity and
  ;; wavelength arrays

  prits_tools.parcheck, data, 1, "data", 'NUMERIC', [2,3,4]
  prits_tools.parcheck, lam, 2, "lam", 'NUMERIC', [2,3,4]
  prits_tools.parcheck, widmin, 0, "widmin", 'NUMERIC', 0, minval=0, /optional
  prits_tools.parcheck, velocity, 0, "velocity", 'NUMERIC', 0, /optional
  prits_tools.parcheck, line_list, 0, "line_list", 'OBJREF', 0, object_name='hash', /optional

  use_list = keyword_set(line_list)
  blue_means_negative_velocity = 1

  meanprofile = data
  sz = size(meanprofile)
  while sz[0] gt 1 do begin
    meanprofile = mean(meanprofile, dimension=2, /nan)
    sz = size(meanprofile)
  endwhile

  meanlambda = lam
  sz = size(meanlambda)
  while sz[0] gt 1 do begin
    meanlambda = mean(meanlambda, dimension=2, /nan)
    sz = size(meanlambda)
  endwhile

  IF use_list THEN BEGIN
    lines = line_list.keys()
    lines = lines.toArray()
    min_lambda = min(lam, max=max_lambda)
    ind_lines = where(lines GT min_lambda AND lines LT max_lambda, npeaks)

    IF npeaks GT 0 THEN BEGIN

      peakinds = intarr(npeaks)
      for iline=0,npeaks-1 do begin
        lambda_diff = abs(meanlambda-lines[ind_lines[iline]])
        min_diff = min(lambda_diff, lambda_ind)
        IF lambda_ind LT 3 || lambda_ind GE N_ELEMENTS(meanlambda)-3 THEN BEGIN
          peakinds[iline] = 0
        ENDIF ELSE BEGIN
          peakinds[iline] = lambda_ind
        ENDELSE
        ind = where(peakinds gt 0, npeaks)
        if npeaks gt 0 then peakinds = peakinds[ind]
      endfor

      fwhm = intarr(npeaks) ; TODO: Estimate FWHM in pixels for each peak
      fwhm[*] = 3 ; for now
    ENDIF ; npeaks GT 0

  ENDIF ELSE BEGIN ; use_list
    peakinds = spice_gt_peaks(meanprofile, fwhm=fwhm, minmedian=4.5, /sort, /plot)
    npeaks = n_elements(peakinds)
  ENDELSE ; use_list


  IF npeaks GT 0 THEN BEGIN

    gaussians = replicate(mk_comp_gauss([0,0,0]), npeaks)

    int0 = meanprofile[peakinds]
    IF use_list THEN lam0 = lines[ind_lines] $
    ELSE lam0 = meanlambda[peakinds]
    wid0 = lam0 - meanlambda[peakinds-fwhm]

    v = 150.                       ; Max shift in km/s
    dlam = v*lam0/3.e5            ; Max shift in Aangstrom

    intmin = fltarr(npeaks)          ; minimum intensity is 0
    lammin = (lam0 - dlam) > min(lam); v0 - v
    IF NOT keyword_set(widmin) THEN widmin = min((wid0 - 0.04) >  0.02)  ; random guess...

    intmax = int0*100;30000                 ; More random guessing
    lammax = (lam0 + dlam) < max(lam) ; v0 + v
    widmax = wid0 + 0.04              ; A final shot in the dark

    IF ~keyword_set(position) THEN BEGIN
      IF N_ELEMENTS(velocity) EQ 0 THEN vel=0.0 $
      ELSE vel = velocity
    ENDIF

    FOR i=0,n_elements(peakinds)-1 DO BEGIN
      gauss = spice_mk_comp_gauss([int0[i],lam0[i],wid0[i]], $
        max_lam=lammax[i], min_lam=lammin[i],$
        min_fwhm = widmin[i], max_fwhm = widmax[i],$
        min_intens=intmin[i], $
        velocity=vel)
      IF ~keyword_set(position) AND keyword_set(blue_means_negative_velocity) THEN BEGIN
        gauss.param(1).trans_a = -gauss.param(1).trans_a
      ENDIF
      lam0txt = trim(lam0[i],'(F6.2)')
      gauss.name = 'AutoGauss ' + lam0txt + 'nm, ' + line_list[lam0[i]]
      gaussians[i] = gauss
    ENDFOR

  ENDIF ; npeaks GT 0

  bg = mk_comp_poly([0.5*median(meanprofile)], max_arr=[30000],min_arr=[0],trans_a=[1],$
    trans_b=[0],const=[0b])
  bg.name = 'Background'

  IF npeaks EQ 0 THEN adef = {bg:bg}
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

  result_message = 'Found '+trim(npeaks)+' peaks.'
  IF npeaks GT 9 THEN result_message += ' Fitting only the 9 highest.'
  box_message, result_message

  return, adef
END
