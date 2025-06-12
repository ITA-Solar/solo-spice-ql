FUNCTION spice_remove_hot_pix, data, date_beg, detector, xposure, res_earlier = res_earlier
  COMMON spice_remove_hot_pix, hotpix_obj

  Limit_Median_Neighbor = 10.0
  Limit_Fraction_To_Signal = 50.0

  data = fix(data, type = 4)

  IF n_elements(hotpix_obj) EQ 0 THEN hotpix_obj = obj_new('hotpix')
  hotpix_obj.set, date_beg
  print, '--- hotpix_obj set ---'
  ; [, days_window=n, catalog_max_age_hours=n, /reset_catalog]
  ;
  hotpix_obj.darks, lw_map, sw_map ; Get "dark maps" for LW/SW detector
  help, lw_map, sw_map

  IF detector EQ 'SW' THEN BEGIN
    hotmap = lw_map - fmedian(lw_map, 3, 3)
  ENDIF ELSE IF detector EQ 'LW' THEN BEGIN
    hotmap = sw_map - fmedian(sw_map, 3, 3)
  ENDIF ELSE BEGIN
    message, 'Unknown detector: ' + detector
    return, data
  ENDELSE
  data_norm = data / (xposure / 10.0) / hotmap

  ind = where(hotmap GT Limit_Median_Neighbor AND $
    data_norm LT Limit_Fraction_To_Signal, count)
  IF count GT 0 THEN data[ind] = !values.f_nan

  IF arg_present(res_earlier) THEN BEGIN
    file = spice_find_file("solo_L1_spice-n-ras_20250331T160031_V02_318767282-000.fits", /user, level = 1)
    file = file[0]
    res_earlier = readfits(file, h)
  ENDIF

  return, data
END
