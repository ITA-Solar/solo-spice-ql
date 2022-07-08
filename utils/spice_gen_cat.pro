;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_GEN_CAT
;               
; Purpose     : Create/update the spice_catalog.txt file.
;               
; Explanation : This program creates a file called spice_catalog.txt in the
;               $SPICE_DATA/ directory (but other paths can be specified),
;               with various information on the content of the files found in
;               the directory hierarchy below that path.
;
;               This file is used by SPICE_CAT in order to search/filter
;               the list of files for those files that the user wants.
;
; Use         : SPICE_GEN_CAT [,FITSDIR [,CATALOG_DIR]]
;    
; Inputs      : None required.
;               
; Opt. Inputs : FITSDIR : The top of the directory tree containing the fits
;                         files to be included in the list. Default is taken
;                         from $SPICE_DATA
;
;               LISTDIR : The directory to place the list in. The default is
;                         to put the file at the top level of the scanned
;                         directory tree
;               
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : 
;
; Category    : SPICE_UTILITY
;               
; Prev. Hist. : 
;
; Written     : Stein Vidar H. Haugan, UiO, 9 August 2020
;               
; Modified    : Version 1, SVHH, 9 August 2020
;                          Initial version based on sfitslist.pro
;               Version 2, SVHH, 11 September 2020
;                          Rewritten from scratch
;               Version 3, TF, 8 February 2022
;                          When keyword RESET is set: do not delete old
;                          catalog file before new catalog file is generated
;               Version 4, SVHH, 31 May 2022
;                          Populate FILE_PATH and ICON_PATH with file path
;                          relative to SPICE_DATA
;
; Version     : Version 4, SVHH, 31 May 2022
;
; $Id: 2022-07-08 20:54 CEST $
;-            

FUNCTION spice_gen_cat::extract_basename,line
  foreach level, [3, 2, 1, 0] DO BEGIN
     pattern = "solo_L" + trim(level) + "_spice[^.]+" 
     match = stregex(line, pattern, /extract)
     IF match NE "" THEN return, match
  END
  message, "NO KEY!!"
END


FUNCTION spice_gen_cat::get_header,filename
  openr,lun,filename,/get_lun
  fxhread,lun,header
  free_lun,lun
  return,header
END


;;
;; WRITING:
;;
PRO spice_gen_cat::write_keyword_info_file, filename
  IF self.d.dry_run THEN BEGIN
     print, "DRY RUN - not writing " + filename
     return
  END 
  
  print
  print, "Converting keyword info to json"
  json = json_serialize(self.d.keyword_info, /lowercase)
  print
  print, "Writing " + filename
  openw, lun, filename + '.tmp', /get_lun
  printf, lun, json
  free_lun, lun
  file_move, filename + '.tmp', filename, /overwrite
END


PRO spice_gen_cat::write_plaintext, filename
  print
  IF self.d.dry_run THEN BEGIN
     print, "DRY RUN - not writing " + filename
     return
  END
  
  print, "Writing " + filename
  tmp_filename = filename + '.tmp'
  OPENW,lun, tmp_filename, /get_lun
  
  comma_separated_keywords = self.d.keyword_array.join(",")
  printf,lun,comma_separated_keywords
  keys = self.d.file_hash_keys
  foreach key,keys, index DO BEGIN
     printf,lun,self.d.file_hash[key],format="(a)"
     IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  FREE_LUN,lun
  file_move, tmp_filename, filename,/overwrite  
END


PRO spice_gen_cat::write_csv, filename
  lines = list()
  keys = self.d.file_hash_keys
  
  print
  print, "Splitting plaintext lines into arrays"
  foreach key, keys, index DO BEGIN
     line = self.d.file_hash[key]
     ;; NOTE: strsplit can't be used!
     ;; It treats two consecutive split patterns as a single one!
     elements = line.split(string(9b))
     lines.add, elements
     IF (index + 1) MOD 100 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  print
  print, "Converting list of arrays into 2d array"
  lines = lines.toarray()
  lines = transpose(lines)
  
  IF self.d.dry_run THEN BEGIN
     print
     print, "DRY RUN - not writing " + filename
     return
  END
  print
  print, "Writing " + filename
  write_csv, filename + '.tmp', lines, header=self.d.keyword_array
  file_move, filename + '.tmp', filename, /overwrite
END


;;
;; Generating catalog
;;
FUNCTION spice_gen_cat::line_from_header, header, relative_path
  value_array = []
  foreach keyword,self.d.keyword_array DO BEGIN
     keyword_type = self.d.keyword_info[keyword].type
     missing = keyword_type EQ 't' ? 'MISSING' : 999999
     value = trim(fxpar(header,keyword, missing=missing))
     IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
        value = relative_path
     END
     value_array =  [value_array, value]
  END
  RETURN,strjoin(value_array,string(9b))
END


FUNCTION spice_gen_cat::add_file, fits_filename, message=message
  key = self.extract_basename(fits_filename)
  IF self.d.file_hash.haskey(key) THEN BEGIN
     print
     print, "Skipping DUPLICATE?? File: "+key
     return, !null
  END
  
  IF self.d.old_hash.haskey(key) THEN BEGIN
     message = '(REUSE) '
     self.d.file_hash[key] = self.d.old_hash[key]
     self.d.file_hash_keys = [self.d.file_hash_keys, key]
     return, key
  END ELSE BEGIN
     message = "(NEW  ) "
  END
  
  header = self.get_header(fits_filename)
  relative_filename = fits_filename.replace(self.d.spice_datadir + "/", "")
  relative_path = file_dirname(relative_filename)
  self.d.file_hash[key] = self.line_from_header(header, relative_path)
  self.d.file_hash_keys = [self.d.file_hash_keys, key]
  return, key
END


PRO spice_gen_cat::populate_hash
  print
  print, "Populating list"
  FOREACH fits_filename, self.d.filelist, index DO BEGIN
     key = self.add_file(fits_filename, message = message)
     IF (index + 1) MOD 100 EQ 0 THEN BEGIN 
        IF NOT self.d.quiet THEN PRINT, message + "Files done : " + (index+1).toString("(i6)") + " "+key
     END
  END
END


PRO spice_gen_cat::read_old_cat, filename
  tx = rd_ascii(filename)
;;  profiler, /system
;;  profiler
  FOR i=1, n_elements(tx)-1 DO BEGIN
     key = self.extract_basename(tx[i])
     self.d.old_hash[key] = tx[i]
     self.d.old_hash_keys = [self.d.old_hash_keys, key]
     IF i MOD 1000 EQ 0 THEN BEGIN
        print, "Done " + trim(i) + "  " + key
;;        profiler, /report, /code_coverage, data=data
;;        sortix = reverse(sort(data.time))
;;        sorted = data[sortix]
;;        print, sorted[0:10].name + "    " + trim(sorted[0:10].time)
     END
  END
;;  profiler, data=data, /code_coverage
END


PRO spice_gen_cat::compare_hashes
  keys_old = self.d.old_hash_keys
  keys_new = self.d.file_hash_keys
  print, where(keys_new NE keys_old)
  foreach key, keys_new DO BEGIN
     IF self.d.file_hash[key] NE self.d.file_hash[key] THEN stop
  END
END

PRO spice_gen_cat::execute
  print
  print, "Finding list of files... ", format='(A,$)'
  self.d.filelist = file_search(self.d.spice_datadir,"*.fits")
  
  IF self.d.filelist[0] EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(self.d.filelist)).toString() + " files"
  END
  
  print
  
  IF NOT self.d.reset THEN BEGIN
     print, "Reading old catalog"
     self.read_old_cat, self.d.catalog_basename + '.txt'
  END
  
  IF NOT self.d.csv_test THEN BEGIN
     print
     print, "Generating new catalog"
     self.populate_hash
  END ELSE BEGIN
     self.d.file_hash_keys = self.d.old_hash_keys
     self.d.file_hash = self.d.old_hash
  END
  
  ;self.compare_hashes
  
  self.write_keyword_info_file, self.d.keyword_info_filename
  
  self.write_plaintext, self.d.catalog_basename + '.txt'
  self.write_csv, self.d.catalog_basename + '2.csv'
END

FUNCTION spice_gen_cat::init, catalog_dir, quiet=quiet, reset=reset, dry_run=dry_run, csv_test=csv_test
  self.d = dictionary()
  
  self.d.quiet = keyword_set(quiet)
  self.d.reset = keyword_set(reset)
  self.d.dry_run = keyword_set(dry_run)
  self.d.csv_test = keyword_set(csv_test)
    
  spice_default,spice_datadir,getenv("SPICE_DATA")
  spice_default,catalog_dir,spice_datadir
  
  self.d.spice_datadir = expand_path(spice_datadir) ; Must have explicit path to find relative paths
  self.d.catalog_basename = concat_dir(catalog_dir,'spice_catalog')
  self.d.keyword_info_filename = concat_dir(catalog_dir, 'keyword_info.json')
  self.d.keyword_info = spice_keyword_info(/all)
  self.d.keyword_array = (self.d.keyword_info.keys()).toarray()
  
  self.d.old_hash = orderedhash()
  self.d.old_hash_keys = []
  self.d.file_hash = orderedhash()
  self.d.file_hash_keys = []

  return, 1
END

PRO spice_gen_cat::stop
  stop
END

PRO spice_gen_cat__define
  spice_gen_cat = {spice_gen_cat, d:dictionary()}
END

PRO spice_gen_cat,spice_datadir,catalog_dir, _extra=extra
  ON_ERROR,0
  o = obj_new('spice_gen_cat', catalog_dir, _extra=extra)
  o.execute
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   spice_gen_cat ;, /dry_run
END

END
