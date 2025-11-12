;+
; Project     : SOLAR ORBITER - SPICE
;
; Name        : SPICE_GEN_CAT
;
; Purpose     : Create/update the spice_catalog.csv file.
;
; Explanation : !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;               ! THIS IS A TEMPORARY VERSION used because Terje is sick
;               !
;               ! The primary reason for this version is to include the
;               ! values of L2 header keywords in the catalog file.
;               !
;               ! In addition, the logic detecting new files and deciding
;               ! whether to build catalog from scratch or not is improved,
;               ! as the previous version always rebuilt the catalog from
;               ! scratch when used independently of the pipeline.
;               !
;               ! The base file name of the catalog is now "spice_catalog",
;               ! to be used by webspice until the regular spice_gen_cat
;               ! can be safely updated. Documentation may be off, and some
;               ! paths are hardcoded. Note, we've changed the location
;               ! of the hash file to be located in the same directory
;               ! as everything else!
;               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
;               This program creates files called spice_catalog.csv and
;               spice_catalog.txt in the $SPICE_DATA/ directory (but other
;               paths can be specified), with various information on the
;               content of the files found in the directory hierarchy below
;               that path.
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
;                          rsync_file_to_sdc_fs, use rsync instead of
;                          file_copy
;              Version 17, TF, 17.04.2024. New method
;                          ::rsync_file_to_other_servers, rsyncs file to any
;                          server returned by spice_get_other_servers()
;              Version 18, SH, 04.09.2024. Change keyword_info.json ->
;                          spice_keyword_info.json
;              Version 19, SH, 10.05.2025
;                          MANY changes, see Explanation above.
;              Version 20, TF, 12.11.2025
;                          Renamed from spice_gen_cat2 to spice_gen_cat
;              Version 21, TF, 12.11.2025
;                          Base file name renamed from spice_catalog2.csv to spice_catalog.csv
;
; Version    : Version 21, TF, 12 November 2025 (prits-group@astro.uio.no)
;
; $Id: 2025-11-12 10:07 CET $
;-

FUNCTION spice_gen_cat::extract_file_basename, line
  ; NOTE: requires the actual file name to be first file name in line
  ; (PARENT file name also occurs)
  pattern = "solo_L._spice[^.]+"
  filename = stregex(line, pattern, /extract)
  return, filename
END

FUNCTION spice_gen_cat::extract_key, line
  filename = self.extract_file_basename(line)
  return, filename
  IF filename NE "" THEN BEGIN
    key = filename.extract('L.') + '_' + filename.extract('[0-9]+-[0-9]{3}')
    return, key
  ENDIF

  message, "NO KEY!!"
END

FUNCTION spice_gen_cat::get_header, filename
  header = headfits(filename)
  IF typename(header) EQ 'LONG' THEN return, []
  IF filename.matches('_L3_') THEN BEGIN
    header2 = headfits(filename, ext = 1)
    ix = where(strmid(header, 0, 4) EQ 'END ')
    header = [header[0 : ix[0] - 1], header2]
  END

  return, header
END

; ;
; ; WRITING:
; ;

PRO spice_gen_cat::rsync_file_to_other_servers, filename
  IF ~self.d.running_as_pipeline THEN return

  FOREACH other_server, self.d.other_servers, ix DO BEGIN
    print, 'rsyncing ' + file_basename(filename) + ' on ' + self.d.host + ' to ' + other_server
    rsync_command = 'rsync -av ' + filename + ' osdcapps@' + other_server + ':' + filename
    spawn, rsync_command, rsync_output
    print, rsync_output
  ENDFOREACH
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
  self.rsync_file_to_other_servers, filename
END

PRO spice_gen_cat::write_plaintext_filtered, filename, filter
  print
  print, "Writing " + filename
  tmp_filename = filename + '.tmp'
  openw, lun, tmp_filename, /get_lun

  comma_separated_keywords = self.d.keyword_array.join(",")
  printf, lun, comma_separated_keywords
  keys = self.d.file_hash.Keys()
  FOREACH key, keys, index DO BEGIN
    IF key.matches(filter) THEN $
      printf, lun, self.d.file_hash[key], format = "(a)"
    IF NOT self.d.quiet THEN IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END

  FREE_LUN, lun
  file_move, tmp_filename, filename, /overwrite
  self.rsync_file_to_other_servers, filename
END

PRO spice_gen_cat::write_plaintext, filename_all
  self.write_plaintext_filtered, filename_all, 'L'
  self.write_plaintext_filtered, filename_all + '.l1', 'L1'
  self.write_plaintext_filtered, filename_all + '.l2', 'L2'
  self.write_plaintext_filtered, filename_all + '.l3', 'L3'
END

PRO spice_gen_cat::write_csv, filename
  lines = list()

  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Splitting plaintext lines into arrays"
  keys = self.d.file_hash.Keys() ; ;TERJE
  FOREACH key, keys, index DO BEGIN
    line = self.d.file_hash[key]
    ; ; NOTE: strsplit can't be used!
    ; ; It treats two consecutive split patterns as a single one!
    elements = line.split(string(9b))
    lines.add, elements
    IF NOT self.d.quiet THEN IF (index + 1) MOD 1000 EQ 0 THEN print, "Done " + trim(index + 1)
  END

  IF ~self.d.quiet THEN print
  IF ~self.d.quiet THEN print, "Converting list of arrays into 2d array"
  lines = lines.toarray()
  lines = transpose(lines)

  IF ~self.d.quiet THEN print
  print, "Writing " + filename + ".tmp"
  write_csv, filename + '.tmp', lines, header = self.d.keyword_array
  print, "Renaming " + filename + ".tmp to " + filename
  file_move, filename + '.tmp', filename, /overwrite
  self.rsync_file_to_other_servers, filename
END

PRO spice_gen_cat::write
  self.write_keyword_info_file, self.d.keyword_info_filename

  self.write_plaintext, self.d.catalog_basename + '.txt'
  self.write_csv, self.d.catalog_basename + '.csv'
  self.write_hash_save_file
END

; ;
; ; Generating catalog
; ;
PRO spice_gen_cat::create_catalog_from_scratch
  print, 'Generating catalog from scratch. This will take a very long time.'
  self.d.use_old_catalog = 0
  self.d.file_hash = orderedhash()
  self.d.n_modified_files = 0
  self.populate_hash
END

FUNCTION spice_gen_cat::line_from_header, header, relative_path
  value_list = list()
  FOREACH keyword, self.d.keyword_array DO BEGIN
    keyword_type = self.d.keyword_info[keyword].type
    missing = keyword_type EQ 't' ? 'MISSING' : 999999
    value = trim(fxpar(header, keyword, missing = missing, /multivalue))
    IF keyword EQ "FILE_PATH" OR keyword EQ "ICON_PATH" THEN BEGIN
      value = relative_path
    END
    value_list.add, value[0]
  END
  value_array = value_list.toArray(/no_copy)
  RETURN, strjoin(value_array, string(9b)) ; Tab
END

FUNCTION spice_gen_cat::add_file, fits_filename
  key = self.extract_key(fits_filename)
  IF self.d.file_hash.haskey(key) THEN BEGIN
    return, !null
  END

  header = self.get_header(fits_filename)
  IF n_elements(header) EQ 0 THEN BEGIN
    print, "Skipping EMPTY?? file: " + fits_filename
    return, !null
  END
  print, "Adding file: " + fits_filename
  fits_filename_expanded = expand_path(fits_filename)
  relative_filename = fits_filename_expanded.replace(self.d.spice_data_dir + "/", "")
  relative_path = file_dirname(relative_filename)
  self.d.file_hash[key] = self.line_from_header(header, relative_path)

  return, key
END

PRO spice_gen_cat::remove_nonexisting_files
  t = systime(1)
  print, "Removing files that are not on disk"
  ondisk_filelist_hash = hash()
  FOREACH file, self.d.filelist, index DO BEGIN
    key = self.extract_key(file_basename(file))
    ondisk_filelist_hash[key] = 1
  END
  print, "Converted filenames to keys", (t2 = systime(1)) - t
  keys = (self.d.file_hash.keys()).toArray()
  FOREACH key, keys DO BEGIN
    is_on_disk = ondisk_filelist_hash.hasKey(key)
    IF ~is_on_disk THEN BEGIN
      line = self.d.file_hash[key]
      filename = self.extract_file_basename(line) + '.fits'
      print, "Removing file: " + filename
      self.d.file_hash.remove, key
    END
  ENDFOREACH
  print, "Removed non-existing files", (t3 = systime(1)) - t2
END

PRO spice_gen_cat::populate_hash
  print
  print, "Populating list of files to add or modify using names of files on disk"

  n_files = n_elements(self.d.filelist)

  n_modified_manual = self.d.n_modified_files
  n_new_manual = n_elements(self.d.new_files_manual) - n_modified_manual

  print, '  Found ' + (n_files).toString('(i6)') + ' files'

  print, '  Adding manually    ' + (n_new_manual).toString('(i6)') + ' new files'
  print, '  Modifying manually ' + (n_modified_manual).toString('(i6)') + ' existing files'
  print
  modno = (n_files / 10 - (n_files / 10 MOD 10) > 10) < 100

  FOREACH file, self.d.filelist, index DO BEGIN
    key = self.add_file(file)
    IF n_elements(key) EQ 0 THEN CONTINUE
    IF (index + 1) MOD modno EQ 0 THEN BEGIN
      PRINT, "Files done: " + (index + 1).toString("(i6)") + "    (key: " + key + ")"
    END
  ENDFOREACH

  print
END

PRO spice_gen_cat::set_filelist
  spice_search_dirs = self.d.spice_data_dir
  data_dir_is_top_level = ~self.d.spice_data_dir.contains('level')
  IF data_dir_is_top_level AND self.d.ignore_L0 THEN BEGIN
    spice_search_dirs = spice_search_dirs + '/level' + ['1', '2', '3'] + '/'
  END
  ignore_txt = (data_dir_is_top_level AND self.d.ignore_L0) ? ', ignoring /level0' : ''

  print, "Finding FITS files on disk" + ignore_txt + '... '

  self.d.filelist = file_search(spice_search_dirs, "*.{fits,fits.gz}")

  IF self.d.filelist[0] EQ '' THEN BEGIN
    MESSAGE, "No fits files found, exiting"
    RETURN
  END ELSE BEGIN
    PRINT, "Found " + (n_elements(self.d.filelist)).toString() + " files"
  END
END

PRO spice_gen_cat::remove_manual_files_to_be_updated
  print, 'Removing manually added files that are already in the catalog'
  FOREACH file, self.d.new_files_manual DO BEGIN
    this_key = self.extract_key(file_basename(file))
    IF self.d.file_hash.hasKey(this_key) THEN BEGIN
      self.d.file_hash.remove, this_key
      self.d.n_modified_files++
    ENDIF
  ENDFOREACH
END

PRO spice_gen_cat::write_hash_save_file
  print, 'Writing ' + self.d.catalog_hash_save_file
  file_hash = self.d.file_hash
  save, file = self.d.catalog_hash_save_file + '.tmp', file_hash
  file_move, self.d.catalog_hash_save_file + '.tmp', self.d.catalog_hash_save_file, /overwrite
  self.rsync_file_to_other_servers, self.d.catalog_hash_save_file
END

PRO spice_gen_cat::restore_hash_save_file
  IF ~file_exist(self.d.catalog_hash_save_file) THEN message, 'Create hash save file by running spice_gen_cat,dir,use_old_catalog=0'
  print, 'Restoring ' + file_basename(self.d.catalog_hash_save_file)
  restore, self.d.catalog_hash_save_file
  ; idl-disable-next-line undefined-var
  IF file_hash EQ !null THEN file_hash = old_hash ; Transition from old to new code
  print, 'Done restoring hash with ' + trim(n_elements(file_hash)) + ' keys'
  self.d.file_hash = file_hash ; idl-disable var-use-before-def
END

PRO spice_gen_cat::execute
  print
  IF self.d.use_old_catalog THEN BEGIN
    self.restore_hash_save_file
    print
    self.remove_manual_files_to_be_updated
  END
  print
  self.set_filelist
  print
  self.remove_nonexisting_files
  print
  self.populate_hash
  print
  self.write
END

FUNCTION spice_gen_cat::init, spice_data_dir, quiet = quiet, use_old_catalog = use_old_catalog, $
  new_files_manual = new_files_manual, ignore_L0 = ignore_L0
  self.d = dictionary()

  ptools.default, spice_data_dir, getenv("SPICE_DATA")
  ptools.default, use_old_catalog, 1
  ptools.default, new_files_manual, !null

  self.d.quiet = keyword_set(quiet)
  self.d.use_old_catalog = use_old_catalog

  self.d.spice_data_dir = expand_path(spice_data_dir) ; Must have explicit path to find relative paths
  self.d.catalog_basename = concat_dir(spice_data_dir, 'spice_catalog')
  self.d.keyword_info_filename = concat_dir(spice_data_dir, 'spice_keyword_info.json')
  self.d.keyword_info = spice_keyword_info()
  self.d.keyword_array = (self.d.keyword_info.keys()).toarray()

  self.d.file_hash = orderedhash()

  self.d.n_modified_files = 0

  self.d.catalog_hash_save_file = self.d.spice_data_dir + "/spice_catalog_hash.save"

  self.d.new_files_manual = new_files_manual

  self.d.ignore_L0 = (keyword_set(ignore_L0))

  self.d.running_as_pipeline = getenv('USER') EQ 'osdcapps'

  self.d.other_servers = spice_get_other_servers(host = host)
  self.d.host = host

  IF ~use_old_catalog THEN message, "It takes a very long time to regenerate from scratch - consider setting USE_OLD_CATALOG=1", /info

  return, 1
END

PRO spice_gen_cat__define
  !null = {spice_gen_cat, d: dictionary()}
END

; ;    ----------------------

PRO spice_gen_cat, spice_data_dir, forever = forever, use_old_catalog = use_old_catalog, ignore_L0 = ignore_L0, quiet=quiet
;  steinhh_paths = getenv("USER") EQ 'steinhh' || getenv("USE_STEINHH_PATHS") NE ''
;  IF NOT steinhh_paths THEN message, 'This program should only be run manually with steinhh paths'
  ptools.default, spice_data_dir, "$HOME/spice_home/fits"
  ptools.default, ignore_L0, 1
  IF ~file_test(spice_data_dir, /directory) THEN message, 'Directory does not exist: ' + spice_data_dir
  ON_ERROR, 0
  REPEAT BEGIN
    o = obj_new('spice_gen_cat', spice_data_dir, use_old_catalog = use_old_catalog, ignore_L0 = ignore_l0, quiet=quiet)
    o.execute
    obj_destroy, o
    use_old_catalog = 1
  END UNTIL ~keyword_set(forever)
END

IF getenv("USER") EQ 'steinhh' THEN BEGIN
  spice_gen_cat, '$HOME/tmp/spice_data', /ignore_l0, /use_old_catalog
ENDIF
END
