FUNCTION spice_remove_hot_pix, data, object, window_index, res_earlier = res_earlier
  COMMON spice_remove_hot_pix, hotpix_obj
  IF n_elements(hotpix_obj) EQ 0 THEN hotpix_obj = obj_new('hotpix')

  Limit_Median_Neighbor = 0.5
  Limit_Fraction_To_Signal = 50.0

  detector = object.get_header_keyword('DETECTOR', window_index)
  IF detector NE 'SW' AND detector NE 'LW' THEN message, "Unknown detector"
  
  data = fix(data, type = 4)

  date_beg = object.get_start_time()
  xposure = object.get_exposure_time(window_index)
  window_pos = object.get_window_position_level_1(window_index, /idl_coord)
  window_pos[0 : 1] = window_pos[0 : 1] MOD 1024
  naxis1 = object.get_header_keyword('naxis1', window_index)
  naxis2 = object.get_header_keyword('naxis2', window_index)
  naxis3 = object.get_header_keyword('naxis3', window_index)
  naxis4 = object.get_header_keyword('naxis4', window_index)
  nbin2 = object.get_spatial_binning(window_index)
  nbin3 = object.get_spectral_binning(window_index)

  hotpix_obj.set, date_beg
  print, '--- hotpix_obj set ---'
  ; [, days_window=n, catalog_max_age_hours=n, /reset_catalog]
  ;
  hotpix_obj.darks, lw_map, sw_map ; Get "dark maps" for LW/SW detector
  help, lw_map, sw_map

  window, 2
  pih, data[*, 0:200, 5], 0.01
  
  map = detector EQ 'SW' ? sw_map : lw_map
  hotmap = map - fmedian(map, 3, 3)
  help, hotmap
  hotmap_extract = hotmap[window_pos[0] : window_pos[1], window_pos[2] : window_pos[3]]
  help, hotmap_extract
  xsize = (window_pos[1] - window_pos[0] + 1) / nbin3
  ysize = (window_pos[3] - window_pos[2] + 1) / nbin2
  
  IF nbin2 GT 1 || nbin3 GT 1 THEN hotmap_extract = rebin(hotmap_extract, xsize, ysize) * nbin2 * nbin3
  help, hotmap_extract

  do_it_the_complicated_way = 1
  hotmap_mask = hotmap_extract GE limit_median_neighbor
  data_punched = data
  IF do_it_the_complicated_way THEN BEGIN
     data_norm = data / (xposure / 10.0)
     IF object.get_sit_and_stare() THEN BEGIN
      ; TODO ?
     ENDIF ELSE BEGIN
        FOR i = 0, naxis3 - 1 DO BEGIN
           data_norm_temp = data_norm[*, *, i] / hotmap_extract
           ind = where(hotmap_extract GT Limit_Median_Neighbor AND $
                       data_norm_temp LT Limit_Fraction_To_Signal, count)
           IF count GT 0 THEN BEGIN
              data_temp = data[*, *, i]
              data_temp[ind] = !values.f_nan
              data_punched[*, *, i] = data_temp
              print, 'Number of hot pixels removed in slice ', i, ': ', count
           ENDIF
        ENDFOR
     ENDELSE
  ENDIF
  
  IF 1 THEN BEGIN
     hotmap_extract2 = rebin(reform(hotmap_extract, 1, xsize, ysize, 1), naxis1, naxis2, naxis3, naxis4)
     help, hotmap_extract2
     data_norm2 = data / (xposure / 10.0) / hotmap_extract2
     help, data_norm2

     ind = where(hotmap_extract2 GT Limit_Median_Neighbor AND $
                 data_norm2 LT Limit_Fraction_To_Signal, count)
     data_punched2 = data
     IF count GT 0 THEN data_punched2[ind] = !values.f_nan
     print, 'Number of hot pixels removed: ', count
     help, data_punched
  ENDIF


  window, 3
  pih, data_punched[*, 0:200, 5], 0.01

  stop
  IF arg_present(res_earlier) THEN BEGIN
    file = spice_find_file("solo_L1_spice-n-ras_20250331T160031_V02_318767282-000.fits", /user, level = 1)
    file = file[0]
    res_earlier = readfits(file, h)
  ENDIF

  return, data
END

test_new_sigma
END
