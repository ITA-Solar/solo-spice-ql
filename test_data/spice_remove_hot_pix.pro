FUNCTION spice_remove_hot_pix, data, object, window_index, res_earlier = res_earlier
  COMMON spice_remove_hot_pix, hotpix_obj

  Limit_Median_Neighbor = 6.0
  Limit_Fraction_To_Signal = 150.0

  data = fix(data, type = 4)

  date_beg = object.get_start_time()
  detector = object.get_header_keyword('DETECTOR', window_index)
  xposure = object.get_exposure_time(window_index)
  window_pos = object.get_window_position_level_1(window_index, /idl_coord)
  window_pos[0 : 1] = window_pos[0 : 1] MOD 1024
  naxis1 = object.get_header_keyword('naxis1', window_index)
  naxis2 = object.get_header_keyword('naxis2', window_index)
  naxis3 = object.get_header_keyword('naxis3', window_index)
  naxis4 = object.get_header_keyword('naxis4', window_index)
  nbin2 = object.get_spatial_binning(window_index)
  nbin3 = object.get_spectral_binning(window_index)

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
  help, hotmap
  hotmap = hotmap[window_pos[0] : window_pos[1], window_pos[2] : window_pos[3]]
  help, hotmap
  xsize = (window_pos[1] - window_pos[0] + 1) / nbin3
  ysize = (window_pos[3] - window_pos[2] + 1) / nbin2
  IF nbin2 GT 1 || nbin3 GT 1 THEN hotmap = rebin(hotmap, xsize, ysize)
  help, hotmap
  ; stop
  hotmap = rebin(reform(hotmap, 1, xsize, ysize, 1), naxis1, naxis2, naxis3, naxis4)
  help, hotmap
  data_norm = data / (xposure / 10.0) / hotmap
  help, data_norm
  stop

  window, 0
  pih, data[*, *, 5], 0.01

  ind = where(hotmap GT Limit_Median_Neighbor AND $
    data_norm LT Limit_Fraction_To_Signal, count)
  IF count GT 0 THEN data[ind] = !values.f_nan
  print, 'Number of hot pixels removed: ', count
  help, data
  ; stop
  ; print, data[ind]
  window, 1
  pih, data[*, *, 5], 0.01

  stop
  IF arg_present(res_earlier) THEN BEGIN
    file = spice_find_file("solo_L1_spice-n-ras_20250331T160031_V02_318767282-000.fits", /user, level = 1)
    file = file[0]
    res_earlier = readfits(file, h)
  ENDIF

  return, data
END
