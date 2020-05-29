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
    else: file = ''
   ENDCASE
   
  obj = spice_object(file, /verbose)
  obj->show_lines
  ;stop
  
  window_index = 0
  
  ;spice_raster_browser, obj, /no_goes
  
  ;spice_xwhisker, obj, window_index
  
  ;d = spice_getwindata(obj, window_index)
  ;help,d
  
  ;print, 'NAXIS1 : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2)
  ;print, 'NAXIS2 : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2)
  ;print, 'NAXIS3 : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2)
  ;print, 'NAXIS4 : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
  
  for i=0,obj->get_number_windows()-1 do begin
    ;print,obj->get_window_position(i)
    ;print, strcompress(string(i)) + '  : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2) + $
    ;  '  : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2) + $
    ;  '  : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2) + $
    ;  '  : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
  endfor

  ;spice_xdetector, obj, indgen(obj->get_number_windows())

  spice_xraster, obj, [0]
END
