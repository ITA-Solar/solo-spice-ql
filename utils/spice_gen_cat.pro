;+
; Project     : SOLAR ORBITER - SPICE     
;                   
; Name        : SPICE_GEN_CAT
;               
; Purpose     : Create/update the spice_catalog.csv file.
;               
; Explanation : This program creates a file called spice_catalog.csv in the
;               $SPICE_DATA/ directory (but other paths can be specified),
;               with various information on the content of the files found in
;               the directory hierarchy below that path.
;
;               This file is used by SPICE_CAT in order to search/filter
;               the list of files for those files that the user wants.
;
; Use         : SPICE_GEN_CAT [,SPICE_DATA_DIR]
;    
; Inputs      : None required.
;               
; Opt. Inputs : SPICE_DATA_DIR : The top of the directory tree containing the fits
;                         files to be included in the list. Default is taken
;                         from $SPICE_DATA
;
; Outputs     : None.
;               
; Opt. Outputs: None.
;               
; Keywords    : REGENERATE: Set to zero to reuse existing catalog
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
;               Version 5, SVHH, 15 July 2022
;                          Major overhaul => objectified
;                          Eliminated many super-slow hash operations
;                          Reinstated reuse of old catalog for speed purposes
;                          Made REGENERATE=1 by default, with warning about slowness
;               Version 6, Martin Wiesmann, 10 August 2022
;                          Reads now also *fits.gz files
;               Version 7, Terje Fredvik, 23 November 2023
;                          Read old catalog is default. Input parameter
;                          NEW_FILES is a string array of existing files that
;                          are to be re-ingested.
;               Version 8, Terje Fredvik, 28 November 2023
;                          Restoring IDL save file containing catalog hashes
;                          is default. 
;               Version 9, Terje Fredvik, 28 November 2023
;                          Do not build up file list hash based on file
;                          structure on disk, instead use saved catalog
;                          hashes. Set use_old_catalog=0 to use disk contents
;                          instead.  
;              Version 10, Terje Fredvik, 28 November 2023
;                          Do not get file list from disk when use_old_catalog
;                          is set
;              Version 11, Terje Fredvik, 29 November 2023
;                          Check that the names of saved files match the
;                          filenames in the hash
;                   
;
; Version     : Version 11, TF, 29 November 2023
;
; $Id: 2023-11-30 09:29 CET $
;-      

FUNCTION spice_gen_cat::extract_filename, line
  pattern = "solo_L._spice[^.]+" 
  filename = stregex(line, pattern, /extract)
  return, filename
END

FUNCTION spice_gen_cat::extract_key,line 
  filename = self.extract_filename(line)
  IF filename NE "" THEN BEGIN 
     key = filename.extract('L.')+'_'+filename.extract('[0-9]+-[0-9]{3}')
     return, key
  ENDIF
  
  message, "NO KEY!!"
END




FUNCTION spice_gen_cat::get_header,filename
  header = headfits(filename) 
  return,header
END


;;
;; WRITING:
;;
PRO spice_gen_cat::write_keyword_info_file, filename
  IF self.d.dry_run THEN BEGIN
     IF ~self.d.quiet THEN print, "DRY RUN - not writing " + filename
     return
  END 
  
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Converting keyword info to json"
  json = json_serialize(self.d.keyword_info, /lowercase)
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Writing " + filename
  openw, lun, filename + '.tmp', /get_lun
  printf, lun, json
  free_lun, lun
  file_move, filename + '.tmp', filename, /overwrite
END


PRO spice_gen_cat::write_plaintext, filename
  print
  IF self.d.dry_run THEN BEGIN
     IF ~self.d.quiet THEN print, "DRY RUN - not writing " + filename
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
     IF NOT self.d.quiet THEN IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  FREE_LUN,lun
  file_move, tmp_filename, filename,/overwrite  
END


PRO spice_gen_cat::write_csv, filename
  lines = list()
  keys = self.d.file_hash_keys
  
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Splitting plaintext lines into arrays"
  foreach key, keys, index DO BEGIN
     line = self.d.file_hash[key]
     ;; NOTE: strsplit can't be used!
     ;; It treats two consecutive split patterns as a single one!
     elements = line.split(string(9b))
     lines.add, elements
     IF NOT self.d.quiet THEN IF (index + 1) MOD 100 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Converting list of arrays into 2d array"
  lines = lines.toarray()
  lines = transpose(lines)
  
  IF self.d.dry_run THEN BEGIN
     IF ~self.d.quiet THEN print
     IF ~self.d.quiet THEN print, "DRY RUN - not writing " + filename
     return
  END
  IF ~self.d.quiet THEN print
  print, "Writing " + filename
  write_csv, filename + '.tmp', lines, header=self.d.keyword_array
  file_move, filename + '.tmp', filename, /overwrite
END


PRO spice_gen_cat::write_hashes_save_file
  print,'Writing '+self.d.catalog_hashes_save_file
  old_hash      = self.d.file_hash
  old_hash_keys = self.d.file_hash_keys
  save, file=self.d.catalog_hashes_save_file, old_hash, old_hash_keys
END

;;
;; Generating catalog
;;
PRO spice_gen_cat::create_catalog_from_scratch
  print,'Generating catalog from scratch. This will take a very long time.'
  self.d.use_old_catalog = 0
  self.d.file_hash = orderedhash()
  self.d.file_hash_keys = []
  self.execute
END

FUNCTION spice_gen_cat::line_from_header, header, relative_path
  value_array = []
  foreach keyword,self.d.keyword_array DO BEGIN
     keyword_type = self.d.keyword_info[keyword].type
     missing = keyword_type EQ 't' ? 'MISSING' : 999999
     
     value = trim(fxpar(header,keyword, missing=missing,/multivalue))
     
     IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
        value = relative_path
     END
     value_array =  [value_array, value[0]]
  END
  RETURN,strjoin(value_array,string(9b))
END


FUNCTION spice_gen_cat::add_file, fits_filename, message=message
  key = self.extract_key(fits_filename)
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
  stop
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
  
  IF self.d.use_old_catalog THEN BEGIN 
     print,'Do not get FITS file names from disk, assuming paths to all modified files are given in input parameter NEW_FILES'
     self.d.file_hash_keys = self.d.old_hash_keys
     self.d.file_hash = self.d.old_hash
     filelist = self.d.new_files
  ENDIF ELSE filelist = self.d.filelist
  
  FOREACH fits_filename, filelist, index DO BEGIN
     stop
     key = self.add_file(fits_filename,  message = message)
     IF (index + 1) MOD 100 EQ 0 THEN BEGIN 
        IF NOT self.d.quiet THEN  PRINT, message + "Files done : " + (index+1).toString("(i6)") + " "+key
     END
  ENDFOREACH
END


PRO spice_gen_cat::set_filelist
  print
  print, "Finding list of files... ", format='(A,$)'
  
  self.d.filelist = file_search(self.d.spice_datadir,"*.{fits,fits.gz}")
  
  IF self.d.filelist[0] EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(self.d.filelist)).toString() + " files"
  END
  
  print
END



PRO spice_gen_cat::remove_files_to_be_updated
  FOREACH file, self.d.new_files, index DO BEGIN  
     this_key = self.extract_key(file_basename(file))
     IF self.d.old_hash.hasKey(this_key) THEN self.d.old_hash.remove, this_key
  ENDFOREACH
END



PRO spice_gen_cat::read_old_cat, catalog_filename
  IF file_exist(self.d.catalog_hashes_save_file) THEN BEGIN 
     print,'Restoring saved catalog hashes'
     restore,self.d.catalog_hashes_save_file
     self.d.old_hash = old_hash
     self.d.old_hash_keys = old_hash_keys
     print,'Done restoring hashes with '+trim(n_elements(old_hash_keys))+' keys'
     return
  ENDIF
  
  print, "Catalog hashes file not found - reading old .txt catalog instead"
  catalog_lines = rd_ascii(catalog_filename)
  FOR i=1, n_elements(catalog_lines)-1 DO BEGIN
     key = self.extract_key(catalog_lines[i])  
     self.d.old_hash[key] = catalog_lines[i]
     self.d.old_hash_keys = [self.d.old_hash_keys, key]
     IF i MOD 1000 EQ 0 THEN BEGIN
        IF NOT self.d.quiet THEN print, "Done " + trim(i) + "  " + key
     END
  END
  
  IF self.d.restore_catalog_hashes_save_file THEN self.write_hashes_save_file

END



PRO spice_gen_cat::compare_hashes
  keys_old = self.d.old_hash_keys
  keys_new = self.d.file_hash_keys
  print, where(keys_new NE keys_old)
  foreach key, keys_new DO BEGIN
     IF self.d.file_hash[key] NE self.d.file_hash[key] THEN stop
  END
END



FUNCTION spice_gen_cat::filenames_match
  self.set_filelist
  keys = self.d.file_hash.keys()
  n_keys = n_elements(keys)
  IF n_keys NE n_elements(self.d.filelist) THEN BEGIN 
     print,trim(n_elements(self.d.filelist))+' files found on disk, '+trim(n_keys)+' files in restored hash.'
     return, 0
  ENDIF
  
  filenames_match = 1
  keyct = n_keys-1
  print,'Checking that all filenames match...'
  tic
  WHILE keyct GE 0 AND filenames_match DO BEGIN 
     line = self.d.file_hash[keys[keyct]]
     filename_hash = self.extract_filename(line)+'.fits'
     filename_disk = file_basename(self.d.filelist[keyct])
     IF filename_hash NE filename_disk THEN filenames_match = 0
     IF ~ self.d.quiet THEN IF keyct MOD 1000 EQ 0 THEN print,'file number '+trim(keyct)
     keyct--
  ENDWHILE 
  
  print, (filenames_match) ? 'All filenames match.' : filename_hash + ' does not match '+filename_disk
  toc
  return,filenames_match
END



PRO spice_gen_cat::execute
  IF self.d.use_old_catalog THEN BEGIN
     self.read_old_cat, self.d.catalog_basename + '.txt'
     self.remove_files_to_be_updated
  END ELSE self.set_filelist
  
  
  IF NOT self.d.csv_test THEN BEGIN
     print, "Generating new catalog"
     self.populate_hash
  END ELSE BEGIN
     self.d.file_hash_keys = self.d.old_hash_keys
     self.d.file_hash = self.d.old_hash
  END
  
                                ;self.compare_hashes
  IF self.d.use_old_catalog THEN BEGIN 
     filenames_in_hash_and_on_disk_match = self.filenames_match()
     IF ~filenames_in_hash_and_on_disk_match THEN self.create_catalog_from_scratch
  ENDIF
  
  self.write_keyword_info_file, self.d.keyword_info_filename
  
  self.write_plaintext, self.d.catalog_basename + '.txt'
  self.write_csv, self.d.catalog_basename + '.csv'
  self.write_hashes_save_file
END



FUNCTION spice_gen_cat::get_catalog_hashes_save_file
  level = self.d.spice_datadir.extract('level[0-9]')
  level_dir = (level EQ '') ? '' : '/l'+level.extract('[0-9]')+'/'
  return, getenv('instr_output')+'/catalog_hashes/'+(level_dir)+'spice_catalog_hashes.save'
END



FUNCTION spice_gen_cat::init, spice_data_dir, quiet=quiet, use_old_catalog=use_old_catalog, $
                              dry_run=dry_run, csv_test=csv_test, new_files=new_files
  self.d = dictionary()
  
  spice_default, spice_data_dir,getenv("SPICE_DATA")
  
  spice_default, use_old_catalog, 1
  
  self.d.quiet = keyword_set(quiet)
  self.d.use_old_catalog = use_old_catalog

  self.d.dry_run = keyword_set(dry_run)
  self.d.csv_test = keyword_set(csv_test)
  
  self.d.spice_datadir = expand_path(spice_data_dir) ; Must have explicit path to find relative paths
  self.d.catalog_basename = concat_dir(spice_data_dir,'spice_catalog')
  self.d.keyword_info_filename = concat_dir(spice_data_dir, 'keyword_info.json')
  self.d.keyword_info = spice_keyword_info(/all)
  self.d.keyword_array = (self.d.keyword_info.keys()).toarray()
  
  self.d.old_hash = orderedhash()
  self.d.old_hash_keys = []
  self.d.file_hash = orderedhash()
  self.d.file_hash_keys = []
  
  self.d.restore_catalog_hashes_save_file = restore_catalog_hashes_save_file
  self.d.catalog_hashes_save_file = self.get_catalog_hashes_save_file()
  
  self.d.new_files = (new_files NE !NULL) ? new_files : !NULL
  self.d.ingest_new_files = self.d.new_files NE !NULL

  IF ~use_old_catalog THEN message, "It takes a very long time to regenerate from scratch - consider setting USE_OLD_CATALOG=1", /info
  
  return, 1
END

PRO spice_gen_cat::stop
  stop
END

PRO spice_gen_cat__define
  spice_gen_cat = {spice_gen_cat, d:dictionary()}
END

PRO spice_gen_cat,spice_data_dir, _extra=extra
  ON_ERROR,0
  tic
  o = obj_new('spice_gen_cat', spice_data_dir, _extra=extra)
  o.execute
  toc
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   spice_gen_cat, use_old_catalog=1 ; /dry_run
END

END
