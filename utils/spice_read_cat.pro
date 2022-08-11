; $Id: 2022-08-11 11:18 CEST $
FUNCTION spice_read_cat, catalog_file
  start_time = systime(1)
  openr, lun, catalog_file, /get_lun
  t = ''
  readf, lun, t
  keywords = strsplit(/extract, t, ",")
  tags = keywords.replace("-","$")
  catalog = list()
  current_spiobsid = 0
  WHILE NOT eof(lun) DO BEGIN
     readf, lun, t
     ; strsplit can't be used here, becaues it does not return empty strings.
     keyword_values = t.split(string(9b))
     entry = {}
     foreach tag, tags, ix DO entry = create_struct(entry, tag, keyword_values[ix])
     entry.first_raster = ""
     IF entry.spiobsid NE current_spiobsid THEN BEGIN
        entry.first_raster = "x"
        current_spiobsid = entry.spiobsid
     END
     catalog.add, entry
  END
  free_lun, lun
  fits_array = catalog.toArray()
  return, fits_array
END 
