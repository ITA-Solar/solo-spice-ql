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
;       IRIS_GET_CALIB, SPICE_OBJ, NEW_SPIKE
;
; MODIFICATION HISTORY:
;       Ver.1, 3-Feb-2020, Martin Wiesmann
;-
; $Id: 2025-06-04 15:09 CEST $

FUNCTION spice_calc_sigma, file, window_index, $
  iwin = 0, no_masking = 0, approximated_slit = 0, $
  sig_read = 6.9, err = err, ind_good = ind_good, ind_miss = ind_miss
  obj = spice_object(file, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return, !NULL

  code = 'python' ; default code

  ; Set various calibration parameters:
  calibration_factor = obj.get_calibration_factor(window_index, variable_values = calibration_factor_var)
  nbin = obj.get_binning(window_index)
  xposure = obj.get_exposure_time(window_index)
  noise_factor = obj.get_noise_factor(window_index)
  gain = obj.get_gain(window_index)
  read_noise = obj.get_read_noise(window_index)
  i_dark = obj.get_dark_current_noise(window_index)

  lambda = obj.get_lambda_vector(window_index)
  IF mean(lambda) GT 900. THEN BEGIN
    noise_factor = 1.6 ; noise factor
    gain = 0.57 ; gain
    read_noise = 6.9 ; read noise
    i_dark = 0.54 ; dark current noise
    quantum_efficiency = 0.25
  ENDIF ELSE BEGIN
    noise_factor = 1.0 ; noise factor
    gain = 3.58 ; gain
    read_noise = 6.9 ; read noise
    i_dark = 0.89 ; dark current noise
    IF mean(lambda) LT 740. THEN quantum_efficiency = 0.12 ELSE quantum_efficiency = 0.1 ; not perfectly consistent with Python code
  ENDELSE

  data = obj.get_window_data(window_index, no_masking = no_masking, approximated_slit = approximated_slit)
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

  IF object_created THEN obj_destroy, obj
  return, sigma
END
