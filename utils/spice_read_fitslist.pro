FUNCTION spice_read_fitslist, listfilename
  openr, lun, listfilename, /get_lun
  t = ''
  readf, lun, t
  keywords = strsplit(/extract, t, ",")
  tags = keywords.replace("-","$")
  fitslist = list()
  WHILE NOT eof(lun) DO BEGIN
     readf, lun, t
     keyword_values = strsplit(/extract, t, string(9b))
     entry = {}
     foreach tag, tags, ix DO entry = create_struct(entry, tag, keyword_values[ix])
     fitslist.add, entry
  END
  free_lun, lun
  return, fitslist.toArray()
END 
