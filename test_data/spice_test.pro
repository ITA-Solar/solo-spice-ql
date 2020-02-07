PRO spice_test, file_number
  COMPILE_OPT IDL2

  IF N_ELEMENTS(file_number) NE 1 then file_number=1
  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  CASE file_number of
    1: file = path+'solo_L1_spice-n-ras_20210314T135349751_V01.fits'
    2: file = path+'solo_L1_spice-n-sit-db_20210623T132924744_V01.fits'
    3: file = path+'solo_L1_spice-n-ras-int_20210314T135349751_V01.fits'
    else: file = ''
   ENDCASE
   
  obj = spice_object(file, /verbose)
  ;stop
  
  window_index = 0
  
  ;spice_raster_browser, obj, /no_goes
  spice_xwhisker, obj, window_index
  ;d = spice_getwindata(obj, window_index)
  ;help,d
  obj->show_lines
  print, 'NAXIS1 : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2)
  print, 'NAXIS2 : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2)
  print, 'NAXIS3 : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2)
  print, 'NAXIS4 : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
END
