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
;              Version 12, Terje Fredvik, 1 December 2023
;                          Simplified and cleaned up code, e.g. removed reading
;                          .txt catalog file, removed *_keys arrays and unused
;                          methods and lines of code. Ensure that files are not saved to disk 
;                          more than once when catalog is recreated from
;                          scratch.
;              Version 13, Terje Fredvik, 17 January 2024
;                          New keyword IGNORE_L0. If set, and input
;                          spice_data_dir is the top level of the FITS file
;                          tree, ignore all files in the level0 directory.
;              Version 14, TF, 29.02.2024
;                          ::add_file: Expand fits filename path
;              Version 15, TF, 05.03.2024
;                          New method ::copy_file_to_sdc_fs. Use file_copy to
;                          copy the newly generated astro-sdc-fs keyword_info_file/catalog
;                          file/hash save file to sdc-fs.
;              Version 16, TF, 06.03.2024
;                          Renamed ::copy_file_to_sdc_fs to
;                          rsync_file_to_sdc_fs, use rsync instead of file_copy
;
; Version     : Version 16, TF, 6 March 2024
;
; $Id: 2024-03-06 15:09 CET $
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

PRO spice_gen_cat::rsync_file_to_sdc_fs, filename
  IF ~self.d.running_as_pipeline THEN return
  sdc_fs_filename = (filename).replace('astro-sdc-fs','sdc-fs')
  rsync_command = 'rsync -av '+filename+' '+sdc_fs_filenam
  stop
  spawn, rsync_command, rsync_output
END


PRO spice_gen_cat::write_keyword_info_file, filename
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Converting keyword info to json"
  json = json_serialize(self.d.keyword_info, /lowercase)
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Writing " + filename
  openw, lun, filename + '.tmp', /get_lun
  printf, lun, json
  free_lun, lun
  file_move, filename + '.tmp', filename, /overwrite
  self.rsync_file_to_sdc_fs, filename
END


PRO spice_gen_cat::write_plaintext, filename
  print
  print, "Writing " + filename
  tmp_filename = filename + '.tmp'
  openw,lun, tmp_filename, /get_lun
  
  comma_separated_keywords = self.d.keyword_array.join(",")
  printf,lun,comma_separated_keywords
  keys = self.d.file_hash.Keys()
  foreach key,keys, index DO BEGIN
     printf,lun,self.d.file_hash[key],format="(a)"
     IF NOT self.d.quiet THEN IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  FREE_LUN,lun
  file_move, tmp_filename, filename,/overwrite  
  self.rsync_file_to_sdc_fs, filename
END


PRO spice_gen_cat::write_csv, filename
  lines = list()
  
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Splitting plaintext lines into arrays"
  keys = self.d.file_hash.Keys() ;;TERJE 
  foreach key, keys, index DO BEGIN
     line = self.d.file_hash[key]
     ;; NOTE: strsplit can't be used!
     ;; It treats two consecutive split patterns as a single one!
     elements = line.split(string(9b))
     lines.add, elements
     IF NOT self.d.quiet THEN IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END
  
  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Converting list of arrays into 2d array"
  lines = lines.toarray()
  lines = transpose(lines)
  
  IF ~self.d.quiet THEN print
  print, "Writing " + filename
  write_csv, filename + '.tmp', lines, header=self.d.keyword_array
  file_move, filename + '.tmp', filename, /overwrite  
  self.rsync_file_to_sdc_fs, filename
  END


PRO spice_gen_cat::write_hash_save_file
  print,'Writing '+self.d.catalog_hash_save_file
  old_hash      = self.d.file_hash
  save, file=self.d.catalog_hash_save_file, old_hash
  self.rsync_file_to_sdc_fs, self.d.catalog_hash_save_file
END


PRO spice_gen_cat::write
  self.write_keyword_info_file, self.d.keyword_info_filename
  
  self.write_plaintext, self.d.catalog_basename + '.txt'
  self.write_csv, self.d.catalog_basename + '.csv'
  self.write_hash_save_file
END


;;
;; Generating catalog
;;
PRO spice_gen_cat::create_catalog_from_scratch
  print,'Generating catalog from scratch. This will take a very long time.'
  self.d.use_old_catalog = 0
  self.d.file_hash = orderedhash()
  self.d.creating_catalog_from_scratch = 1
  self.d.n_modified_files = 0

  self.populate_hash
END


FUNCTION spice_gen_cat::line_from_header, header, relative_path
  value_list = list()
  foreach keyword,self.d.keyword_array DO BEGIN
     keyword_type = self.d.keyword_info[keyword].type
     missing = keyword_type EQ 't' ? 'MISSING' : 999999
     value = trim(fxpar(header,keyword, missing=missing,/multivalue))
     IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
        value = relative_path
     END
     value_list.add, value[0]
  END
  value_array = value_list.toArray(/no_copy)
  RETURN,strjoin(value_array,string(9b))
END


FUNCTION spice_gen_cat::add_file, fits_filename
  key = self.extract_key(fits_filename)
  IF self.d.file_hash.haskey(key) THEN BEGIN
     print
     print, "Skipping DUPLICATE?? File: "+key
     return, !null
  END
 
  header = self.get_header(fits_filename)
  fits_filename_expanded = expand_path(fits_filename)
  relative_filename = fits_filename_expanded.replace(self.d.spice_datadir + "/", "")
  relative_path = file_dirname(relative_filename)
  self.d.file_hash[key] = self.line_from_header(header, relative_path)

  return, key
END


PRO spice_gen_cat::populate_hash
  print
  print, "Populating list of files to add or modify using names of files", format='(A,$)'
  
  IF self.d.use_old_catalog THEN BEGIN 
     print,' in saved hash:'
     self.d.file_hash = self.d.old_hash
     filelist = self.d.new_files
  ENDIF ELSE BEGIN
     filelist = self.d.filelist
     print,' on disk:'
  ENDELSE
  n_files = n_elements(filelist)
  n_modified = self.d.n_modified_files
  n_new =  n_files - n_modified
  
  print,'  Adding    '+(n_new).toString('(i6)')+' new files'
  print,'  Modifying '+(n_modified).toString('(i6)')+' existing files'
  print
  modno = n_files/10 - (n_files/10 MOD 10) > 10
  
  FOREACH fits_filename, filelist, index DO BEGIN
     key = self.add_file(fits_filename)
     IF (index + 1) MOD modno EQ 0 THEN BEGIN 
        PRINT, "Files done: " + (index+1).toString("(i6)") + "    (key: "+key+")"
     END
  ENDFOREACH
  
  print
END


PRO spice_gen_cat::set_filelist

  top_level = ~self.d.spice_datadir.contains('level')

  spice_search_dirs = (top_level AND self.d.ignore_L0) ? self.d.spice_datadir+'/level'+['1','2','3']+'/' : self.d.spice_datadir
  ignore_txt        = (top_level AND self.d.ignore_L0) ? ', ignoring /level0'                          : ''
  
  print, "Finding FITS files on disk"+ignore_txt+'... ', format='(A,$)'
   
  self.d.filelist = file_search(spice_search_dirs,"*.{fits,fits.gz}")
 
  IF self.d.filelist[0] EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(self.d.filelist)).toString() + " files"
  END
END


PRO spice_gen_cat::remove_files_to_be_updated
  FOREACH file, self.d.new_files, index DO BEGIN  
     this_key = self.extract_key(file_basename(file))
     IF self.d.old_hash.hasKey(this_key) THEN BEGIN
        self.d.old_hash.remove, this_key
        self.d.n_modified_files++
     ENDIF
  ENDFOREACH
END


PRO spice_gen_cat::restore_old_cat
  IF ~ file_exist(self.d.catalog_hash_save_file) THEN message,'Create hash save file by running spice_gen_cat,dir,use_old_catalog=0'
  
  print,'Restoring '+file_basename(self.d.catalog_hash_save_file)
  restore,self.d.catalog_hash_save_file
  self.d.old_hash = old_hash
  print,'Done restoring hash with '+trim(n_elements(old_hash))+' keys'
END


FUNCTION spice_gen_cat::filenames_match
  print,'Checking that FITS filenames in saved hash match FITS filenames on disk: '
  
  self.set_filelist
  keys = self.d.file_hash.keys()
  n_keys = n_elements(keys)
  
  files_in_hash = strarr(n_keys)
  FOREACH key, keys, ix DO BEGIN 
     line = self.d.file_hash[key]
     files_in_hash[ix] = self.extract_filename(line)+'.fits'
  ENDFOREACH

  files_in_hash = files_in_hash[sort(files_in_hash)]
  
  files_on_disk = file_basename(self.d.filelist)
  files_on_disk = files_on_disk[sort(files_on_disk)]
  
  match = array_equal(files_on_disk, files_in_hash)

  print,  (match) ? 'Filenames match.' : 'Filenames do not match!'
  
  IF ~ match THEN BEGIN
     n_files_on_disk = n_elements(files_on_disk)
     n_files_in_hash = n_elements(files_in_hash)
     equal_txt = ( n_files_in_hash EQ n_files_on_disk) ? 'is the same.' : 'do not match! ('+trim(n_files_in_hash)+' vs '+trim(n_files_on_disk)+')'
     print,'The number of files in hash and on disk '+equal_txt
     FOR i =0,(n_files_on_disk-1) < (n_files_in_hash-1) DO IF files_in_hash[i] NE files_on_disk[i] THEN differs_ix = (differs_ix EQ !NULL) ? i : [differs_ix,i]
     
     IF ~self.d.quiet THEN BEGIN 
        FOREACH diskfile, files_on_disk DO BEGIN
           ix = where(files_in_hash EQ diskfile,/null)
           IF ix EQ !NULL THEN print,file_basename(diskfile)+' not found in hash!'
        ENDFOREACH
        print
        FOREACH hashfile, files_in_hash DO BEGIN
           ix = where(files_on_disk EQ hashfile,/null)
           IF ix EQ !NULL THEN print,file_basename(hashfile)+' not found on disk!'
        ENDFOREACH
     ENDIF 
  
  ENDIF
  
  return,match
END


PRO spice_gen_cat::execute
  IF self.d.use_old_catalog THEN BEGIN
     self.restore_old_cat
     self.remove_files_to_be_updated
  END ELSE self.set_filelist
  
  self.populate_hash
 
  IF self.d.use_old_catalog THEN BEGIN 
     filenames_in_hash_and_on_disk_match = self.filenames_match()
     IF ~filenames_in_hash_and_on_disk_match THEN self.create_catalog_from_scratch
  ENDIF
  
  self.write
END


FUNCTION spice_gen_cat::get_catalog_hash_save_file
  level = self.d.spice_datadir.extract('level[0-9]')
  level_dir = (level EQ '') ? '' : '/l'+level.extract('[0-9]')+'/'
  return, getenv('instr_output')+'/catalog_hashes/'+(level_dir)+'spice_catalog_hash.save'
END


FUNCTION spice_gen_cat::init, spice_data_dir, quiet=quiet, use_old_catalog=use_old_catalog, $
                              new_files=new_files, ignore_L0=ignore_L0
  self.d = dictionary()
  
  prits_tools.default, spice_data_dir,getenv("SPICE_DATA")
  
  prits_tools.default, use_old_catalog, 1
  
  self.d.quiet = keyword_set(quiet)
  self.d.use_old_catalog = use_old_catalog
  self.d.creating_catalog_from_scratch = 0
  
  self.d.spice_datadir = expand_path(spice_data_dir) ; Must have explicit path to find relative paths
  self.d.catalog_basename = concat_dir(spice_data_dir,'spice_catalog')
  self.d.keyword_info_filename = concat_dir(spice_data_dir, 'keyword_info.json')
  self.d.keyword_info = spice_keyword_info(/all)
  self.d.keyword_array = (self.d.keyword_info.keys()).toarray()
  
  self.d.old_hash = orderedhash()
  self.d.file_hash = orderedhash()
  
  self.d.n_modified_files = 0
  
  self.d.catalog_hash_save_file = self.get_catalog_hash_save_file()
  
  self.d.new_files = (new_files NE !NULL) ? new_files : !NULL
  self.d.ingest_new_files = self.d.new_files NE !NULL
  
  self.d.ignore_L0 = (keyword_set(ignore_L0))
  
  self.d.running_as_pipeline = getenv('USER') EQ 'osdcapps'

  IF ~use_old_catalog THEN message, "It takes a very long time to regenerate from scratch - consider setting USE_OLD_CATALOG=1", /info
  
  return, 1
END


PRO spice_gen_cat__define
  spice_gen_cat = {spice_gen_cat, d:dictionary()}
END


;;    ----------------------


PRO spice_gen_cat,spice_data_dir, _extra=extra
  ON_ERROR,0
  o = obj_new('spice_gen_cat', spice_data_dir, _extra=extra)
  o.execute
END

