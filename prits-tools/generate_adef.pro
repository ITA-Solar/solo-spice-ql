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
;     version :   Returns the version number of this software.
;     gt_peaks_version : Returns the version number of spice_gt_peaks.
;
; CALLS:
;      spice_gt_peaks, mk_comp_gauss, mk_comp_poly, box_message
;
; HISTORY:
;      Ver. 1, 18-Oct-2021, Martin Wiesmann (prits-group@astro.uio.no)
;      Ver. 1.1, 17-Jan-2022, Terje Fredvik: minimum line width is determined
;                                            by the instrument optics and
;                                            should be the same for all lines.
;      Ver. 1.2, 13-Jun-2022, Martin Wiesmann: position is now by default represented as
;                                            velocity, added keywords velocity and position.
;      Ver. 1.3, Nov-2022, Martin Wiesmann: Uses now spice_line_list() to get a list of possible
;                                            peaks to be included.
;      Ver. 1.4, 26-Apr-2023, Terje Fredvik: When blue_means_negative_velocity
;                                            the min and max values of
;                                            parameters corresponding to
;                                            velocities must be switched and
;                                            change sign.
;      Ver. 1.5, 22-Aug-2025, Terje Fredvik: support up to 24 lines. Sort lines
;                                            in decreasing order of intensity
;                                            also when using a line list. Use
;                                            linear background for full detector.
;-
; $Id: 2026-01-16 09:08 CET $

FUNCTION generate_adef, data, lam, widmin = widmin, position = position, velocity = velocity, $
  line_list = line_list, plot = plot, version = version, gt_peaks_version = version_gt_peaks
  ; ; Automatically generate cfit analysis definitions based on input intensity and
  ; ; wavelength arrays

  version = 2 ; PLEASE increase this number when editing the code

  ptools.parcheck, data, 1, "data", 'NUMERIC', [2, 3, 4]
  ptools.parcheck, lam, 2, "lam", 'NUMERIC', [2, 3, 4]
  ptools.parcheck, widmin, 0, "widmin", 'NUMERIC', 0, minval = 0, /optional
  ptools.parcheck, velocity, 0, "velocity", 'NUMERIC', 0, /optional
  ptools.parcheck, line_list, 0, "line_list", 'OBJREF', 1, object_name = 'hash', /optional

  use_list = keyword_set(line_list)
  blue_means_negative_velocity = 1

  meanprofile = data
  sz = size(meanprofile)
  WHILE sz[0] GT 1 DO BEGIN
    meanprofile = mean(meanprofile, dimension = 2, /nan)
    sz = size(meanprofile)
  ENDWHILE

  meanlambda = lam
  sz = size(meanlambda)
  WHILE sz[0] GT 1 DO BEGIN
    meanlambda = mean(meanlambda, dimension = 2, /nan)
    sz = size(meanlambda)
  ENDWHILE

  IF use_list THEN BEGIN
     lines = line_list.keys()
     lines = lines.toArray()
     min_lambda = min(lam, max = max_lambda)
     ind_lines = where(lines GT min_lambda AND lines LT max_lambda, npeaks)
     lam0 = lines[ind_lines]
     
     IF npeaks GT 0 THEN BEGIN
        peakinds = intarr(npeaks)
        lam0_peaks = fltarr(npeaks) ;xxx
        FOR iline = 0, npeaks - 1 DO BEGIN
           lambda_diff = abs(meanlambda - lam0[iline]) ;xxx
           !NULL = min(lambda_diff, lambda_ind)
           IF lambda_ind LT 3 || lambda_ind GE n_elements(meanlambda) - 3 THEN BEGIN
              peakinds[iline] = 0
              lam0_peaks[iline] = lam0[0]
           ENDIF ELSE BEGIN
              peakinds[iline] = lambda_ind
              lam0_peaks[iline] = lam0[iline] ;xxx
           ENDELSE
        ENDFOR
        ind = where(peakinds GT 0, npeaks)
        IF npeaks GT 0 THEN BEGIN
           peakinds = peakinds[ind]
           lam0_peaks = lam0_peaks[ind] 
           fwhm = intarr(npeaks)        ; TODO: Estimate FWHM in pixels for each peak
           fwhm[*] = 3                  ; for now
        ENDIF
        sorted_by_decreasing_intensity_ix = reverse(sort(meanprofile[peakinds]))
        peakinds = peakinds[sorted_by_decreasing_intensity_ix]
        lam0_peaks = lam0_peaks[sorted_by_decreasing_intensity_ix] ;xxx
     ENDIF                                                         ; npeaks GT 0
  ENDIF ELSE BEGIN                                                 ; use_list
     peakinds = spice_gt_peaks(meanprofile, fwhm = fwhm, minmedian = 4.5, /sort, plot = plot, version = version_gt_peaks)
     npeaks = n_elements(peakinds)
  ENDELSE                       ; use_list
  
  plot_spectrum_for_debugging_purposes = 0
  IF plot_spectrum_for_debugging_purposes THEN BEGIN 
     plot,meanlambda,meanprofile,yst=3,/xst
     lam0_sorted=lam0[sorted_by_decreasing_intensity_ix]
     FOR i=0,n_elements(peakinds)-1 DO xyouts,meanlambda[peakinds[i]]+0.02,(meanprofile[peakinds[i]]+0.04),line_list[lam0_sorted[i]]+' '+trim(lam0_sorted[i]),$
                                              color=200,charsize=1.5,orientation=90
     FOR i=0,n_elements(peakinds)-1 DO xyouts,meanlambda[peakinds[i]]-0.02,min(meanprofile),trim(i),color=180,charsize=1.5
     FOR i=0,n_elements(peakinds)-1 DO plots,[meanlambda[peakinds[i]],meanlambda[peakinds[i]]],[0,meanprofile[peakinds[i]]],color=150,line=2
  ENDIF
  
  IF npeaks GT 0 THEN BEGIN
    gaussians = replicate(spice_mk_comp_gauss([0, 0, 0]), npeaks)

    int0 = meanprofile[peakinds]
    IF use_list THEN lam0 = lam0_peaks $; lam0 = lines[ind_lines] $
    ELSE lam0 = meanlambda[peakinds]
 
    wid0 = lam0 - meanlambda[peakinds - fwhm] > widmin

    v = 150. ; Max shift in km/s
    dlam = v * lam0 / 3.e5 ; Max shift in Aangstrom

    min_intens = fltarr(npeaks) ; minimum intensity is 0
    negative_int0_ix = where(int0 LT 0, /NULL) ; SPICE L2 HDUs may have negative values
    IF negative_int0_ix NE !NULL THEN min_intens[*] = min(int0[negative_int0_ix]) * 5.

    min_lam = (lam0 - dlam) > min(lam) ; v0 - v
    min_fwhm = (keyword_set(widmin)) ? widmin : min((wid0 - 0.04) > 0.02) ; random guess...

    max_intens = abs(int0) * 100 ; 30000    ; Ensure that max value is greater than min value also for negative values
    max_lam = (lam0 + dlam) < max(lam) ; v0 + v
    max_fwhm = wid0 + 0.1; 0.07 ;

    IF ~keyword_set(position) THEN BEGIN
      IF n_elements(velocity) EQ 0 THEN vel = 0.0 $
      ELSE vel = velocity
    ENDIF

    FOR i = 0, n_elements(peakinds) - 1 DO BEGIN
      gauss = spice_mk_comp_gauss([int0[i], lam0[i], wid0[i]], $
        max_intens = max_intens[i], min_intens = min_intens[i], $
        max_lam = max_lam[i], min_lam = min_lam[i], $
        min_fwhm = min_fwhm, max_fwhm = max_fwhm[i], $
        velocity = vel, use_list = use_list)
      IF ~keyword_set(position) AND keyword_set(blue_means_negative_velocity) THEN BEGIN
        gauss.param[1].trans_a = -gauss.param[1].trans_a
        max_vel = -gauss.param[1].min_val
        gauss.param[1].min_val = -gauss.param[1].max_val
        gauss.param[1].max_val = max_vel
      ENDIF
      iontxt = (use_list) ? line_list[lam0[i]] : 'AutoGauss'
      lam0txt = trim(lam0[i], '(F6.2)') + ' nm'
      gauss.name = iontxt + ' ' + lam0txt
      gaussians[i] = gauss
    ENDFOR
  ENDIF ; npeaks GT 0
  
  linear_background_when_full_detector = n_elements(meanlambda) EQ 1024
  IF linear_background_when_full_detector THEN $
     bg = mk_comp_poly(1, max_arr = [10,2], min_arr = [-10,-2], trans_a = [1,1], $
                       trans_b = [0,0], const = [0b,0b])$
  ELSE $
     bg = mk_comp_poly([0.5 * median(meanprofile)], max_arr = [30000], min_arr = [-100], trans_a = [1], $
                       trans_b = [0], const = [0b])                    
  
 ; bg = mk_comp_poly(2, max_arr = [60, 1, 0.02], min_arr = [-10, -2, -0.05], trans_a = [1,1,1], $
 ;                   trans_b = [0,0,0], const = [0b,0b,0b])
   
  bg.name = 'Background'

  IF npeaks EQ 0 THEN adef = {bg: bg}
  IF npeaks EQ 1 THEN adef = {igauss2: gaussians[0], bg: bg}
  IF npeaks EQ 2 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], bg: bg}
  IF npeaks EQ 3 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], bg: bg}
  IF npeaks EQ 4 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], bg: bg}
  IF npeaks EQ 5 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], $
    igauss6: gaussians[4], bg: bg}
  IF npeaks EQ 6 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], $
    igauss6: gaussians[4], igauss7: gaussians[5], bg: bg}
  IF npeaks EQ 7 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], $
    igauss6: gaussians[4], igauss7: gaussians[5], $
    igauss8: gaussians[6], bg: bg}
  IF npeaks EQ 8 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], $
    igauss6: gaussians[4], igauss7: gaussians[5], $
    igauss8: gaussians[6], igauss9: gaussians[7], $
    bg: bg}
  IF npeaks GE 9 THEN adef = {igauss2: gaussians[0], igauss3: gaussians[1], $
    igauss4: gaussians[2], igauss5: gaussians[3], $
    igauss6: gaussians[4], igauss7: gaussians[5], $
    igauss8: gaussians[6], igauss9: gaussians[7], $
    igauss10: gaussians[8], bg: bg}
  
   IF npeaks GE 10 THEN adef = {igauss2:  gaussians[0], igauss3:  gaussians[1], $
                                igauss4:  gaussians[2], igauss5:  gaussians[3], $
                                igauss6:  gaussians[4], igauss7:  gaussians[5], $
                                igauss8:  gaussians[6], igauss9:  gaussians[7], $
                                igauss10: gaussians[8], igauss11: gaussians[9], $
                                bg: bg}
   
   IF npeaks GE 11 THEN adef = {igauss2:  gaussians[0], igauss3:  gaussians[1], $
                                igauss4:  gaussians[2], igauss5:  gaussians[3], $
                                igauss6:  gaussians[4], igauss7:  gaussians[5], $
                                igauss8:  gaussians[6], igauss9:  gaussians[7], $
                                igauss10: gaussians[8], igauss11: gaussians[9], $
                                igauss12: gaussians[10], $
                                bg: bg}
   
   IF npeaks GE 12 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11], $
                                bg: bg}
   
   IF npeaks GE 14 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], $
                                bg: bg}
   
   IF npeaks GE 15 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                bg: bg}
   
   IF npeaks GE 16 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], $
                                bg: bg}
   
   IF npeaks GE 17 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                bg: bg}
   
   IF npeaks GE 18 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], $
                                bg: bg}
   
   IF npeaks GE 19 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                bg: bg}
   
   IF npeaks GE 20 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                igauss20: gaussians[18], $
                                bg: bg}
   
   IF npeaks GE 21 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                igauss20: gaussians[18], igauss21: gaussians[19],$
                                bg: bg}
   
   IF npeaks GE 22 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                igauss20: gaussians[18], igauss21: gaussians[19],$
                                igauss22: gaussians[20], $
                                bg: bg}
   
   IF npeaks GE 23 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                igauss20: gaussians[18], igauss21: gaussians[19],$
                                igauss22: gaussians[20], igauss23: gaussians[21],$
                                bg: bg}
   
   IF npeaks GE 24 THEN adef = {igauss2:  gaussians[0],  igauss3:  gaussians[1], $
                                igauss4:  gaussians[2],  igauss5:  gaussians[3], $
                                igauss6:  gaussians[4],  igauss7:  gaussians[5], $
                                igauss8:  gaussians[6],  igauss9:  gaussians[7], $
                                igauss10: gaussians[8],  igauss11: gaussians[9], $
                                igauss12: gaussians[10], igauss13: gaussians[11],$
                                igauss14: gaussians[12], igauss15: gaussians[13],$
                                igauss16: gaussians[14], igauss17: gaussians[15],$
                                igauss18: gaussians[16], igauss19: gaussians[17],$
                                igauss20: gaussians[18], igauss21: gaussians[19],$
                                igauss22: gaussians[20], igauss23: gaussians[21],$
                                igauss24: gaussians[22], $
                                bg: bg}
   
  result_message = 'Found ' + trim(npeaks) + ' peaks '
  IF npeaks GT 24 THEN result_message += ' Fitting only the 24 highest.'
  box_message, result_message

  return, adef
END
