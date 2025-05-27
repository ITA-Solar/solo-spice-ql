FUNCTION test_spice_error_python, file, window_index
  obj = spice_data(file)
  ; Set various calibration parameters:
  alpha = obj.get_header_keyword('radcal', window_index)
  np = obj.get_header_keyword('nbin', window_index)
  npx = obj.get_header_keyword('nbin1', window_index)
  npy = obj.get_header_keyword('nbin2', window_index)
  t = obj.get_header_keyword('xposure', window_index)

  lam = obj.get_lambda_vector(window_index)
  IF mean(lam) GT 900. THEN BEGIN
    f = 1.6
    g = 0.57 ; gain
    sig_read = 6.9
    i_dark = 0.54
    quantum_efficiency = 0.25
  ENDIF ELSE BEGIN
    f = 1.0
    g = 3.58 ; gain
    sig_read = 6.9
    i_dark = 0.89
    IF mean(lam) LT 740. THEN quantum_efficiency = 0.12 ELSE quantum_efficiency = 0.1 ; not perfectly consistent with Python code
  ENDELSE
  ;
  wd = obj.get_window_data(window_index, no_masking = no_masking, approximated_slit = approximated_slit)

  sigma_dark = sqrt(i_dark * t * npx * npy) * sqrt(2)

  background_instr = 0
  sigma_Background = sqrt(background_instr * quantum_efficiency * t * npx * npy * g * g)

  sigma_read = sig_read * sqrt(npx * npy) * sqrt(2)

  sigma_signal = sqrt(wd * alpha * g) * f

  constant_noise = sqrt(sigma_dark * sigma_dark + sigma_Background * sigma_Background + sigma_read * sigma_read)
  sigma_total = sqrt(sigma_signal * sigma_signal + constant_noise * constant_noise)

  return, sigma_total
END
