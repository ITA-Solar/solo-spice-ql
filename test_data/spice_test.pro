PRO spice_test
  COMPILE_OPT IDL2

  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  file1 = path+'solo_LL01_spice-n-ras_0542899978_V201911131500I.fits'
  file2 = path+'solo_LL01_spice-n-sit_0481295390_V201911131507C.fits'
  file3 = path+'solo_LL01_spice-n-sit-db_0551626020_V201911131503I.fits'
  
  obj = spice_object(file1, /verbose)
  stop
END
