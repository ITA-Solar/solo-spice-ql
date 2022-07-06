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
; $Id: 2022-07-06 17:47 CEST $
;-            

FUNCTION spice_gen_cat::extract_basename,line
  level_1_to_3 = line.extract("solo_L[1-3]_spice.*_[0-9]{8}T[0-9]{6}.*V[0-9]+[^.]*.fits")
  IF level_1_to_3 NE "" THEN return, level_1_to_3
  level_0 = line.extract("solo_L0_spice[^.]*V[0-9]+[^.]+.fits")
  return, level_0
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
PRO spice_gen_cat::write_keyword_info, filename
  openw, lun, filename + '.tmp', /get_lun
  printf, lun, json_serialize(self.d.keyword_info, /lowercase)
  free_lun, lun
  file_move, filename + '.tmp', filename, /overwrite
END


PRO spice_gen_cat::write_plaintext, filename
  tmp_filename = filename + '.tmp'
  OPENW,lun, tmp_filename, /get_lun
  
  comma_separated_keywords = self.d.keyword_array.join(",")
  printf,lun,comma_separated_keywords
  keys = self.d.file_hash.keys()
  foreach key,keys DO BEGIN
     printf,lun,self.d.file_hash[key],format="(a)"
  END
  
  FREE_LUN,lun
  file_move, tmp_filename, filename,/overwrite  
END


PRO spice_gen_cat::write_csv, filename
  lines = []
  keys = self.d.file_hash.keys()
  foreach key, keys DO lines = [lines, self.d.file_hash[key]]
  file_array = transpose((strsplit(lines, string(9b), /extract)).toarray())
  header = (self.d.keyword_info.keys()).toarray()
  write_csv, filename + '.tmp', file_array, header=header
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


FUNCTION spice_gen_cat::add_file, fits_filename
     key = self.extract_basename(fits_filename)
     IF self.d.file_hash.haskey(key) THEN BEGIN
        print,"Skipping "+key
        return, !null
     END
     
     header = self.get_header(fits_filename)
     relative_filename = fits_filename.replace(self.d.spice_datadir + "/", "")
     relative_path = file_dirname(relative_filename)
     self.d.file_hash[key] = self.line_from_header(header, relative_path)
     return, key
END


FUNCTION spice_gen_cat::read_catalog, filename
  tx = rd_ascii(filename)
  hash = orderedhash()
  foreach line, tx DO BEGIN
     key = self.extract_basename(line)
     hash[key] = line
  END
  return, hash
END

FUNCTION spice_gen_cat::init, catalog_dir, quiet=quiet
  self.d = dictionary()
  
  self.d.quiet = keyword_set(quiet)
  spice_default,spice_datadir,getenv("SPICE_DATA")
  spice_default,catalog_dir,spice_datadir
  
  self.d.spice_datadir = expand_path(spice_datadir) ; Must have explicit path to find relative paths
  self.d.catalog_basename = concat_dir(catalog_dir,'spice_catalog')
  self.d.keyword_info_filename = concat_dir(catalog_dir, 'keyword_info.json')
  self.d.keyword_info = spice_keyword_info(/all)
  self.d.keyword_array = (self.d.keyword_info.keys()).toarray()
  
  self.d.old_cat = self.read_catalog(self.d.catalog_basename + ".txt")
  
  self.d.filelist = file_search(spice_datadir,"*.fits")
  IF self.d.filelist[0] EQ '' THEN BEGIN
     MESSAGE,"No fits files found, exiting"
     RETURN, 0
  END ELSE BEGIN
     PRINT, "Found " + (n_elements(self.d.filelist)).toString() + " files"
  END
  
  PRINT,"About to create new " + self.d.catalog_basename + ".[txt|csv] with "+ $
        (N_ELEMENTS(self.d.filelist)).tostring()+" elements"
  
  self.d.file_hash = orderedhash()
  
  FOREACH fits_filename, self.d.filelist, index DO BEGIN
     key = self.add_file(fits_filename)
     IF NOT quiet THEN PRINT,"Files done :",(index+1).toString("(i6)")," "+key
  END
  
  self.write_keyword_info, self.d.keyword_info_filename
  
  self.write_plaintext, self.d.catalog_basename + '.txt'

  self.write_csv, self.d.catalog_basename + '.csv'
  return, 1
END

PRO spice_gen_cat__define
  spice_gen_cat = {spice_gen_cat, d:dictionary()}
END

PRO spice_gen_cat,spice_datadir,catalog_dir, quiet=quiet
  ON_ERROR,0
  o = obj_new('spice_gen_cat', catalog_dir, quiet=keyword_set(quiet))
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   spice_gen_cat
END

END
