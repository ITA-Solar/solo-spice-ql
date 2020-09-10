FUNCTION spice_read_fitslist, listfilename
  start_time = systime(1)
  openr, lun, listfilename, /get_lun
  t = ''
  readf, lun, t
  keywords = strsplit(/extract, t, ",")
  tags = keywords.replace("-","$")
  fitslist = list()
  current_spiobsid = 0
  WHILE NOT eof(lun) DO BEGIN
     readf, lun, t
     keyword_values = strsplit(/extract, t, string(9b))
     entry = {}
     foreach tag, tags, ix DO entry = create_struct(entry, tag, keyword_values[ix])
     entry.first_raster = ""
     IF entry.spiobsid NE current_spiobsid THEN BEGIN
        entry.first_raster = "x"
        current_spiobsid = entry.spiobsid
     END
     fitslist.add, entry
  END
  free_lun, lun
  fits_array = fitslist.toArray()
  print, "SPICE_READ_FITSLIST used ", systime(1)-start_time, " seconds "
  return, fits_array
END 
