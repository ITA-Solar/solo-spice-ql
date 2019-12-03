PRO spice_test, file_number
  COMPILE_OPT IDL2

  IF N_ELEMENTS(file_number) NE 1 then file_number=1
  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  CASE file_number of
    1: file = path+'solo_LL01_spice-n-ras_0542899978_V201911131500I.fits'
    2: file = path+'solo_LL01_spice-n-sit_0481295390_V201911131507C.fits'
    3: file = path+'solo_LL01_spice-n-sit-db_0551626020_V201911131503I.fits'
    else: file = ''
   ENDCASE
   
  obj = spice_object(file, /verbose)
  stop
END
