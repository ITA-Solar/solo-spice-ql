;+
; NAME:
;       SPICE_CALC_SIGMA
;
; PURPOSE:
;       Calculate the sigma (error) for a given SPICE window.
;
; CALLING SEQUENCE:
;       d = spice_calc_sigma(file, iwin)
;
; INPUTS:
;       FILE: input SPICE fits file or data object. See restrictions.
;
; OPT. INPUT:
;       IWIN: scalar with the index of the desired window. This can
;             also be a wavelength or a string that
;             matches one of the window ids.
;
; KEYWORDS:
;
; OUTPUTS:
;
; EXAMPLES:
;       Get window containing Si IV 1393 line:
;
;       IDL> wd = spice_getwindata(file,1393)
;
; PROGRAMMING NOTES:
;
; CALLS:
;       spice_object
;
; MODIFICATION HISTORY:
;       Ver.1, 3-Feb-2020, Martin Wiesmann
;-
; $Id: 2025-06-25 15:11 CEST $

FUNCTION spice_calc_sigma, file, window_index ; , $
  ; iwin = 0, no_masking = 0, approximated_slit = 0, $
  ; sig_read = 6.9, err = err, ind_good = ind_good, ind_miss = ind_miss
  obj = spice_object(file, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return, !NULL

  ; Set various calibration parameters:
  calibration_factor = obj.get_calibration_factor(window_index, variable_values = calibration_factor_var)
  nbin = obj.get_total_binning(window_index)
  xposure = obj.get_exposure_time(window_index)
  noise_factors = obj.get_noise_factors(window_index)
  noise_factor = noise_factors.noise_factor
  gain = noise_factors.gain
  read_noise = noise_factors.read_noise
  i_dark = noise_factors.i_dark
  dark_subtraction_factor = noise_factors.dark_subtraction_factor

  data = obj.get_window_data(window_index, no_masking = no_masking, approximated_slit = approximated_slit)

  sigma = sqrt( $
    noise_factor ^ 2 * calibration_factor * (data > 0) * gain $ ; signal noise
    + dark_subtraction_factor * nbin * $
    (read_noise ^ 2 $ ; read noise
      + i_dark * xposure)) $ ; dark current noise
    / calibration_factor

  IF object_created THEN obj_destroy, obj
  return, sigma

  ; data = [W m-2 sr-1 nm-1]
  ; noise_factor = []
  ; calibration_factor = [DN/(W m-2 sr-1 nm-1)]
  ; gain = [dn/photon]
  ; read_noise = [DN]
  ; i_dark = [DN/s]
  ; dark_subtraction_factor = []
  ; xposure = [s]
  ; nbin = []
  ; 
  ; sigma = sqrt( $
  ;     DN^2 / photon $ ; signal noise
  ;   + DN^2          $ ; read noise
  ;   + DN            $ ; dark current noise
  ;   ) / [DN/(W m-2 sr-1 nm-1)]
  ;   = W m-2 sr-1 nm-1 = [data]
  ;    IF we ignore [photon] and that the dark current noise is DN and not DN^2
  ;    
  ;    from python code:
  ;    read_noise = [DN/pixel]
  ;    gain = [DN/photon]
  ;    i_dark = [DN/s/pixel]
  ;    xposure = [s]
  ;    noise_factor = []

  code = 'python' ; default code
  missing_val = -100.
  k = where(~finite(data) OR data LE 0., nk)
  IF nk NE 0 THEN data[k] = missing_val
  ind_good = where(data NE missing_val, n_good, complement = ind_miss, ncomplement = n_miss)
  ; IF n_good GT 0 THEN err[ind_good] = sqrt(noise_factor ^ 2 * alpha * data[ind_good] * gain + nbin_total * sig_read ^ 2 + nbin_total * i_dark * t) / alpha
  ; IF n_miss GT 0 THEN err[ind_miss] = missing_val
  ; TODO: What to do with negative values?
  IF code EQ 'IDL' THEN BEGIN
    sigma = sqrt( $
      noise_factor ^ 2 * calibration_factor * data * gain $ ; signal noise
      + read_noise ^ 2 * nbin $ ; read noise
      + i_dark * xposure * nbin) $ ; dark current noise
      / calibration_factor
  END ELSE IF code EQ 'Python' THEN BEGIN
    sigma = sqrt( $
      noise_factor ^ 2 * calibration_factor * data * gain $ ; signal noise
      + read_noise ^ 2 * nbin * 2 $ ; read noise
      + i_dark * xposure * nbin * 2) $ ; dark current noise
      / calibration_factor
  END ELSE BEGIN
    print, 'Unknown code: ', code
    return, -1
  END
END
