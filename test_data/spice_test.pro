PRO spice_test, file_number
  COMPILE_OPT IDL2

  IF N_ELEMENTS(file_number) NE 1 then file_number=1
  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  CASE file_number of
    1: file = path+'solo_L1_spice-n-ras_20210314T135349751_V01.fits'
    2: file = path+'solo_L1_spice-n-sit-db_20210623T132924744_V01.fits'
    else: file = ''
   ENDCASE
   
  obj = spice_object(file, /verbose)
  ;stop
  
  spice_raster_browser, obj
END
