; PRELIMINARY documentation
;
; Use: 
;
;     o = obj_new('hotpix')
;
;     o.set,'<ISO center date/time>' $
;          [, days_window=n, catalog_max_age_hours=n, /reset_catalog]
;
;     o.darks, lw_map, sw_map  ;  Get "dark maps" for LW/SW detector
;
;  The "ISO center date/time" is the DATE-BEG of the file to be treated
;
;  To get "hot map" (ditto for SW):
;
;     lw_hot = lw_map - fmedian(lw_map,3,3)
;
;  These hot maps represents "how much a pixel's min value sticks up from the
;  local median". The higher the more likely the pixel is to be bullshit.
;
;  Note - the maps are normalised to 10sec XPOSURE, in longer exposures these
;  pixels will have more "dark signal", less for shorter exposures.
;
;  A pixel should be NaN-ed out if it's BOTH:
;
;     a) Above a certain threshold (LIMIT)
;
;  AND
;
;     b) Above a certain fraction DARK_FRAC_LIMIT of the observation pixel's
;        value. NOTE: Multiply with exposure time divided by 10 before doing
;        comparison in this step!
;
;  So exposures will have to be handled individually (because observation
;  pixels' values will vary from one exposure to the next).
;  

FUNCTION hotpix::files
  compile_opt strictarrsubs
  ix = self.d.files_to_use_ix
  filenames = self.d.filenames[[ix]]
  dates = self.d.dates[ix]
  
  nfiles = n_elements(filenames)
  
  filepaths = strarr(nfiles)
  
  FOR i=0, n_elements(filenames)-1 DO BEGIN
     date = dates[i].extract('[0-9]{4}-[0-9]{2}-[0-9]{2}')
     date_path = date.replace('-', '/')
     filepaths[i] = "/$SPICE_DATA/level1/" + date_path + '/' + filenames[i]
  END
  return, file_expand_path(filepaths)
END

FUNCTION hotpix::darks_process_extension, f, lw_map, sw_map, extension
  compile_opt strictarr
  lw_copy = lw_map * 0 + 2L ^ 30
  sw_copy = sw_map * 0 + 2L ^ 30
  
  catch, error
  IF error NE 0 THEN BEGIN
     catch, /cancel
     print, "Error reading extension ", extension, " in file ", f
     return, 0
  END
  data = readfits(f, hdr, exten = extension)
  IF n_elements(data) EQ 1 THEN message, "Fits reading failed"
  catch, /cancel
  IF fxpar(hdr, "OBS_HDU") NE 1 THEN BEGIN
     print, "Skipping extension ", extension, " in file ", f, " as it is not an observation HDU."
     return, 0
  END ELSE BEGIN
     print, "Processing extension ", extension, " in file ", f
  END
  
  detector = trim(fxpar(hdr, 'DETECTOR')) 
  xposure = fxpar(hdr, 'XPOSURE')
  
  data = float(data)/xposure*10
  
  missing = fxpar(hdr, "BLANK")
  ix = where(data EQ missing, count)
  IF count GT 0 THEN data[ix] = !values.f_nan

  min_data = min(data, dimension = 1)
  IF n_dimensions(min_data) EQ 3 THEN BEGIN
     print, "Time dimension seems to be present, taking min/max"
     min_data = min(min_data, dimension=3)
  END
  
  ; Put data into detector image, debin first:
  min_data = transpose(min_data)
  nbin_dispersion = fxpar(hdr, 'NBIN3')
  nbin_y = fxpar(hdr, 'NBIN2')
  dims = size(min_data, /dimensions)
  min_data = rebin(min_data, dims[0]*nbin_dispersion, dims[1]*nbin_y)
  min_data = min_data/fxpar(hdr, 'NBIN')
  
  ; Slot it in:
  xmin = (fxpar(hdr, 'PXBEG3')-1) MOD 1024
  xmax = (fxpar(hdr, 'PXEND3')-1) MOD 1024
  ymin = fxpar(hdr, 'PXBEG2')-1
  ymax = fxpar(hdr, 'PXEND2')-1

  IF detector EQ 'SW' THEN sw_copy[xmin:xmax, ymin:ymax] = min_data
  IF detector EQ 'LW' THEN lw_copy[xmin:xmax, ymin:ymax] = min_data
  
  lw_map = lw_map < lw_copy
  sw_map = sw_map < sw_copy
  return, 1
  compile_opt static
END

PRO hotpix::darks_process_file, f, lw_map, sw_map
  ; Easier to debug when we can look at the results from a single file:
  lw_map_copy = lw_map * 0 + 2L ^ 30
  sw_map_copy = lw_map * 0 + 2L ^ 30
  print, "Processing file: ", f
  extension = 0
  WHILE 1 DO BEGIN
    catch, err
    catch, /cancel
    IF err NE 0 THEN BEGIN
      catch, /cancel
      print, "Error reading extension ", extension, " in file ", f
      BREAK
    ENDIF
    success = self.darks_process_extension(f, lw_map_copy, sw_map_copy, extension)
    IF NOT success THEN return
    lw_map = lw_map < lw_map_copy
    sw_map = sw_map < sw_map_copy
    extension = extension + 1
  END
END

PRO hotpix::darks, lw_map, sw_map
  ; self.d.lw_map/sw_map will be removed if 
  ; new files have to be read
  IF self.d.haskey('lw_map') THEN BEGIN
     lw_map = self.d.lw_map
     sw_map = self.d.sw_map
     return
  END
  f = self.files()
  sw_map = fltarr(1024, 1024) + 2L ^ 30
  lw_map = sw_map
  FOR i = 0, n_elements(f) - 1 DO BEGIN
     self.darks_process_file, f[i], lw_map, sw_map
  END
  self.d.sw_map = sw_map
  self.d.lw_map = lw_map
END

PRO hotpix::find_files
  center_date_tai = anytim2tai(self.d.center_time)
  min_date = tai2utc(/c, center_date_tai - 24. * 3600. * self.d.days_window / 2.0)
  max_date = tai2utc(/c, center_date_tai + 24. * 3600. * self.d.days_window / 2.0)
  print, "Min date: " + min_date
  print, "Max date: " + max_date
  
  ix = where(self.d.dates GE min_date AND self.d.dates LE max_date, count)
  self.d.files_to_use_ix = ix
  print, "Files to use: ", n_elements(ix)
  IF self.d.haskey('lw_map') THEN self.d.remove, 'lw_map'
  IF self.d.haskey('sw_map') THEN self.d.remove, 'sw_map'
END


PRO hotpix::process_catalog
  compile_opt strictarrsubs
  print, "Processing catalog"
  keyword_info = spice_keyword_info()
  keywords = (keyword_info.keys()).toarray()
  
  file_keys = (self.d.catalog.keys()).toarray()
  nfiles = n_elements(file_keys)
  filenames = strarr(nfiles)
  dates = strarr(nfiles)
  levels = intarr(nfiles)
  
  ;; Get keyword index
  filename_kwix = (where(keywords EQ 'FILENAME'))[0]
  date_beg_kwix = (where(keywords EQ 'DATE-BEG'))[0]
  level_kwix = (where(keywords EQ 'LEVEL'))[0]
  
  foreach file_key, file_keys, ix DO BEGIN
     file_entry = self.d.catalog[file_key]
     values = str_sep(file_entry, string(9b))
     data_level = strmid(values[[level_kwix]], 1, 1)
     levels[ix] = fix(data_level)
     IF levels[ix] NE 1 THEN CONTINUE
     filenames[ix] = values[[filename_kwix]]
     dates[ix] = values[[date_beg_kwix]]
  END
  level1_ix = where(levels EQ 1)
  self.d.file_keys = file_keys[level1_ix]
  self.d.filenames = filenames
  self.d.dates = dates[level1_ix]
END

PRO hotpix::read_catalog
  print, "Reading catalog"
  catalog_filename = 'spice_catalog2_hash.save'
  restore, getenv("SPICE_DATA") + '/' + catalog_filename
  self.d.catalog = temporary(file_hash)
  self.d.catalog_timestamp = systime(1)
END


PRO hotpix::set, center_time, days_window=days_window, reset_catalog=reset_catalog, $
                 catalog_max_age_hours=catalog_max_age_hours
  IF n_elements(center_time) EQ 0 THEN message, 'You must specify the center date'
  
  ptools.default, center_time, '2025-04-15'
  ptools.default, days_window, 2.0
  ptools.default, catalog_max_age_hours, 10*60.
  
  self.d.catalog_max_age_hours = catalog_max_age_hours
  self.d.days_window = days_window
  
  catalog_ok = self.d.haskey('catalog') AND NOT keyword_set(reset_catalog)
  catalog_ok AND= self.d.haskey('catalog_timestamp')
  
  IF catalog_ok THEN BEGIN
     catalog_age = systime(1) - self.d.catalog_timestamp
     catalog_ok = catalog_age LT catalog_max_age_hours*3600
  END
  
  IF NOT catalog_ok THEN BEGIN
     self.read_catalog
     self.process_catalog
  END
  
  IF NOT self.d.haskey('center_time') THEN self.d.center_time = '1900-01-01'
  
  filelist_ok = catalog_ok AND self.d.center_time EQ center_time
  
  IF NOT filelist_ok OR self.d.center_time NE center_time THEN BEGIN 
     self.d.center_time = center_time  
     self.find_files
  END
END

FUNCTION hotpix::d
  return, self.d
END

FUNCTION hotpix::init
  self.d = dictionary()
  return, 1
END

PRO hotpix::churn, sw=sw, lw=lw, limit=limit, date=date, days_window=days_window, $
                   catalog_max_age_hours=catalog_max_age_hours
  
  IF keyword_set(sw) AND keyword_set(lw) THEN message, "Won't show both detectors"
  IF NOT keyword_set(sw) AND NOT keyword_set(lw) THEN message, "Set either /lw or /sw"
  
  default, limit, 15
  default, date, '2025-04-15'
  default, days_window, 2
  default, catalog_max_age_hours, 0.5
  
  self.set, date, days_window = days_window, catalog_max_age_hours = catalog_max_age_hours
  self.darks, lw_map, sw_map
  
  IF keyword_set(sw) THEN showmap = sw_map
  IF keyword_set(lw) THEN showmap = lw_map
  
  nodata_mask = showmap EQ max(showmap, data_mask)

  median_size = 3


  diff = showmap - fmedian(showmap, median_size, median_size)

  nodataix = where(nodata_mask, nnodata, complement=dataix, ncomplement=ndata)

  diff[nodataix] = 0.0
  showmap[nodataix] =  0.0

  print, "Number of good pixels " + trim(ndata)
  print, "Number of nodata pixels " + trim(nnodata)

  ncut_pixels = total(diff[dataix] GE limit, /double)

  
  window, 2, xsize=1000, ysize=1000
  plot_image, diff GE limit

  punched_showmap = showmap
  punched_showmap[where(diff GT limit)] = !values.f_nan

  window, 3, xsize=1000, ysize=1000
  plot_image, punched_showmap

  window, 6, xsize=1000, ysize=1000
  
  thresholded_data = diff[dataix] < limit
  thresholded_data >= -10
  h = histogram(thresholded_data, omin=omin, omax=omax)
  nhist = n_elements(h)
  plot, (x = omin + findgen(nhist)/(nhist-1) * (omax-omin)), $
         h, /ylog, /ynozero, xstyle=2 OR 8, ystyle=8
  oplot, x, h, psym=2

  print, "Data pixels:     " + trim(ndata)
  print, "Cut pixels:      " + trim(ncut_pixels)
  print, "Fraction cut:    " + trim(ncut_pixels/ndata)
  print, "Pixels/line fit: " + trim(ncut_pixels/ndata*16)
END


pro hotpix__define
  !null = {hotpix, d:hash()}
END



IF n_elements(hp) EQ 0 THEN hp = obj_new('hotpix')
hp.churn, /lw

END
