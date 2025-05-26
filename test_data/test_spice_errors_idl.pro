PRO test_spice_errors_idl
  file = 'solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits'
  file = spice_find_file(file)
  file = file[0]
  print, file

  window_index = 0
  no_masking = 0
  approximated_slit = 0
  debug_plot = 0

  obj = spice_data(file)

  obj.transform_data_for_ana, window_index, no_masking = no_masking, approximated_slit = approximated_slit, $
    debug_plot = debug_plot, $
    DATA = DATA, LAMBDA = LAMBDA, WEIGHTS = WEIGHTS, MISSING = MISSING, version = version_add

  help, WEIGHTS
  maxweights = max(WEIGHTS, min = minweights)
  errors = 1.0 / sqrt(WEIGHTS)
  ind = where(~finite(errors), nbad)
  IF nbad GT 0 THEN errors[ind] = 0.0001

  maxerrors = max(errors, min = minerrors)
  print, 'ERRORS      min: ', minerrors, ' max: ', maxerrors
  print, 'DATA        min: ', min(DATA), ' max: ', max(DATA)
  print, 'WEIGHTS     min: ', minweights, ' max: ', maxweights
  rel_err = errors / DATA * 100
  maxrel_err = max(rel_err, min = minrel_err)
  print, 'RELATIVE ERRORS min: ', minrel_err, ' max: ', maxrel_err

  ind = where(WEIGHTS EQ WEIGHTS, countw)
  ind = where(DATA EQ DATA, countd)
  print, 'WEIGHTS == WEIGHTS: ', countw
  print, 'DATA == DATA: ', countd
  print, 'DIFFERENCE: ', countw - countd

  ; a = obj.xcfit_block(0)
END
