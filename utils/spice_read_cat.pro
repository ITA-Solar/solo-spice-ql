; $Id: 2022-08-11 15:06 CEST $
FUNCTION spice_read_cat_txt, catalog_file
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


FUNCTION spice_read_cat_csv, catalog_file
  keyword_values = read_csv(catalog_file, count=count, header=tags)
  ntags = N_ELEMENTS(tags)
  tags = tags.replace("-","$")
  catalog = list()
  entry = {}
  foreach tag, tags, ix DO entry = create_struct(entry, tag, (keyword_values.(ix))[0] )
  fits_array = make_array(count, value=entry)
  FOR itag=0,ntags-1 DO BEGIN
    fits_array.(itag) = keyword_values.(itag)
  ENDFOR
  current_spiobsid = 0
  FOR ientry=0,count-1 DO BEGIN
    IF fits_array[ientry].spiobsid NE current_spiobsid THEN BEGIN
      fits_array[ientry].first_raster = "x"
      current_spiobsid = fits_array[ientry].spiobsid
    END ELSE fits_array[ientry].first_raster = ""
  ENDFOR
  return, fits_array
END


FUNCTION spice_read_cat, catalog_file
  IF ~file_exist(catalog_file) THEN BEGIN
    print, 'This catalog file does not exist. ' + catalog_file
    print, 'Please generate it by running spice_gen_cat'
    return, !NULL
  ENDIF
  dirname = file_dirname(catalog_file)
  basename = file_basename(catalog_file)
  IF basename.endswith('.csv') || basename.endswith('.txt') THEN BEGIN
    ;check if IDL-save file already exists
    file_idl = basename.split('\.')
    file_idl = strjoin([file_idl[0:N_ELEMENTS(file_idl)-2], 'sav'], '.')
    file_idl =concat_dir(dirname, file_idl)
    IF file_modtime(catalog_file) LT file_modtime(file_idl) THEN BEGIN
      restore, file_idl
      return, fits_array
    ENDIF

    ;IDL-save file does not exist, we need to read the txt or csv file
    IF basename.endswith('.csv') THEN BEGIN
      fits_array = spice_read_cat_csv(catalog_file)
    END
    IF basename.endswith('.txt') THEN BEGIN
      fits_array = spice_read_cat_txt(catalog_file)
    ENDIF

    ;and then create a IDL-save file
    save, fits_array, filename=file_idl
    return, fits_array
  ENDIF

  IF basename.endswith('.sav') THEN BEGIN
    restore, catalog_file
    return, fits_array
  ENDIF

  print, 'This is not a catalog file, it must end with .sav, .txt or .csv'
  return, !NULL
END
