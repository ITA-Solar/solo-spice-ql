;+
; NAME:
;       SPICE_CALC_SIGMA
;
; PURPOSE:
;       Calculate the sigma (error) for a given SPICE window.
;
; CALLING SEQUENCE:
;       d = spice_calc_sigma(input, iwin)
;
; INPUTS:
;       INPUT: Input SPICE fits file or data object. Or the data cube of a SPICE window.
;       WINDOW_INDEX: Scalar with the index of the desired window.
;             This can also be a wavelength or a string that
;             matches one of the window ids.
;
; OPT. INPUT:
;       HDR_RESULT: FITS header of the level P SPICE result window. Required if INPUT is a data cube.
;       HDR_DATA: FITS header of the level P SPICE data window. Required if INPUT is a data cube.
;       SIGMADAT: String containing a function that returns the sigma (error) for a given data cube.
;              Required if INPUT is a data cube.
;
; KEYWORDS:
;
; OUTPUTS:
;
; EXAMPLES:
;
; PROGRAMMING NOTES:
;
; CALLS:
;       spice_object
;
; MODIFICATION HISTORY:
;       Ver.1, 3-Feb-2020, Martin Wiesmann (prits-group@astro.uio.no)
;-
; $Id: 2025-08-12 15:53 CEST $

FUNCTION spice_calc_sigma, input, window_index, SIGMADAT = SIGMADAT, hdr_result = hdr_result, hdr_data = hdr_data, $
  no_masking = no_masking, approximated_slit = approximated_slit
  COMPILE_OPT IDL2

  IF keyword_set(SIGMADAT) THEN BEGIN
    ; Calculate sigma for a level P SPICE data cube.
    IF NOT keyword_set(hdr_result) THEN message, 'SIGMADAT is set but no header is provided'
    data = input

    ; Set various calibration parameters:
    file_l2 = fxpar(hdr_data, 'PARENT', missing = '')
    obj = spice_object(file_l2, is_spice = is_spice, object_created = object_created)
    IF is_spice THEN BEGIN
      window_index = fxpar(hdr_data, 'WINNO', missing = -1)
      calibration_factor = obj.get_calibration_factor(window_index, variable_values = calibration_factor_var)
      nbin = obj.get_total_binning(window_index)
      xposure = obj.get_exposure_time(window_index)
    ENDIF ELSE BEGIN
      nbin = fxpar(hdr_data, 'NBIN', missing = 1)
      xposure = fxpar(hdr_data, 'XPOSURE', missing = 0.0)
    ENDELSE
    calibration_factor = fxpar(hdr_result, 'RADCAL', missing = 0.0)
    noise_factor = fxpar(hdr_result, 'NOISEFAC', missing = 0.0)
    gain = fxpar(hdr_result, 'GAIN', missing = 0.0)
    read_noise = fxpar(hdr_result, 'READNOIS', missing = 0.0)
    i_dark = fxpar(hdr_result, 'DARKCURR', missing = 0.0)
    dark_subtraction_factor = fxpar(hdr_result, 'DARKSUBF', missing = 0.0)
  ENDIF ELSE BEGIN
    ; Calculate sigma for a level 2 SPICE fits file or data object.
    obj = spice_object(input, is_spice = is_spice, object_created = object_created)
    IF ~is_spice THEN return, !NULL
    data = obj.get_window_data(window_index, no_masking = no_masking, approximated_slit = approximated_slit)

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
  ENDELSE

  sdata = size(data)
  full_calibration_factor = data
  FOR iwave = 0, sdata[3] - 1 DO full_calibration_factor[*, *, iwave, *] = calibration_factor_var[0, 0, iwave]

  sigma = sqrt( $
    noise_factor ^ 2 * full_calibration_factor * (data > 0) * gain $ ; signal noise
    + dark_subtraction_factor * nbin * $
    (read_noise ^ 2 $ ; read noise
      + i_dark * xposure)) $ ; dark current noise
    / full_calibration_factor

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
  ; DN^2 / photon $ ; signal noise
  ; + DN^2          $ ; read noise
  ; + DN            $ ; dark current noise
  ; ) / [DN/(W m-2 sr-1 nm-1)]
  ; = W m-2 sr-1 nm-1 = [data]
  ; IF we ignore [photon] and that the dark current noise is DN and not DN^2
  ;
  ; from python code:
  ; read_noise = [DN/pixel]
  ; gain = [DN/photon]
  ; i_dark = [DN/s/pixel]
  ; xposure = [s]
  ; noise_factor = []
END
