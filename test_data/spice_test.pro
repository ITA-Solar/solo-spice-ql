PRO spice_test, file_number
  COMPILE_OPT IDL2

  IF N_ELEMENTS(file_number) NE 1 then file_number=1
  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  CASE file_number of
    1: file = path+'solo_L1_spice-n-ras_20210314T135349751_V01.fits'
    2: file = path+'solo_L1_spice-n-sit-db_20210623T132924744_V01.fits'
    3: file = path+'solo_L1_spice-n-ras-int_20210314T135349751_V01.fits'
    4: file = path+'solo_LL01_spice-n-exp_0542902641_V202001130934I.fits'
    5: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083015_V202003091733C_12583153-000.fits'
    6: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083203_V202003091733C_12583154-000.fits'
    7: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083431_V202003091733C_12583155-000.fits'
    8: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083611_V202003091733C_12583156-000.fits'
    9: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637082835_V202003091733C_12583152-000.fits'
    10: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637083841_V202003091734C_12583157-000.fits'
    11: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637084153_V202003091734C_12583158-000.fits'
    12: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0640792768_V202004211913C_12583408-000.fits'
    else: file = ''
   ENDCASE
   
  obj = spice_object(file, /verbose)
  ;stop
  
  window_index = 0
  
  ;spice_raster_browser, obj, /no_goes
  
  ;spice_xwhisker, obj, window_index
  
  ;d = spice_getwindata(obj, window_index)
  ;help,d
  
  obj->show_lines
  ;print, 'NAXIS1 : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2)
  ;print, 'NAXIS2 : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2)
  ;print, 'NAXIS3 : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2)
  ;print, 'NAXIS4 : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
  
  for i=0,obj->get_number_windows()-1 do begin
    ;print,obj->get_window_position(i)
    print, strcompress(string(i)) + '  : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
  endfor
  ;print,obj->get_spatial_binning()
  ;print,obj->get_spectral_binning()
  ;stop
  ;spice_xdetector, obj, indgen(obj->get_number_windows())
  
  
  spice_xraster, obj, [0]
END
