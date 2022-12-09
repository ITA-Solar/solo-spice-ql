;+
; NAME:
;      SPICE_CREATE_L3_DRIVER
;
; PURPOSE:
;      This function creates multiple level 3 files and optionally images.
;
; CATEGORY:
;      Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;      spice_create_l3_driver
;
; INPUTS:
;     TIME_START: This can be in any format accepted by the ANYTIM suite of
;               routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;               either the time for which the level 2 file closest to it is returned,
;               or the start of the time window to be searched, in case 'time_end'
;               is also provided.
;
; OPTIONAL INPUT:
;     TIME_END: This can be in any format accepted by the ANYTIM suite of
;               routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;               the end of the time window to be searched.
;     L2_FILES: Instead of providing a time (range), a list of level 2 files can be provided here.
;     TOP_DIR:  Top directory in which the SPICE data lies. If not provided
;               the path given in $SPICE_DATA is searched.
;     PATH_INDEX: If $SPICE_DATA or TOP_DIR contains multiple paths, then this
;               keyword allows you to specify which path should be searched. Default is 0.
;     VELOCITY : Set this equal to the initial velocity if you want
;               the line position represented by the velocity
;               relative to a lab wavelength - the lab wavelength
;               is taken from the supplied POSITION, i.e., INT_POS_FWHM(1), which is
;               calculated/estimated within the procedure 'generate_adef'.
;               This input is ignored if /POSITION is set.
;               Default is zero.
;     IMAGES_TOP_DIR: Top directory in which to save the level 3 images. The data-tree structure
;               will be appended to this directory. If not provided, the path is the same as
;               the level 3 file, except that the 'level3' part of the path will be replaced by 'images'.
;               This input is ignored, if /CREATE_IMAGES is not set.
;
; KEYWORD PARAMETERS:
;     SEQUENCE: If set, then all files of the sequence that the found level 2 files belong to will
;               be returned, i.e. the time window to be searched is expanded to include files
;               outside of the given time window, but only sequences (= Spice observations)
;               that have at least one file in the given time window are returned.
;               If set and 'time_end' is provided, the returned value will be a LIST
;               in which each element is a string or string array with paths to SPICE FITS files
;               that belong to the same sequence.
;     ALL:      If set, then all filenames for the specified day will be returned.
;               Ignored if TIME_END is provided or if NO_TREE_STRUCT or SEQUENCE is set.
;     NO_LEVEL: If set, then the level part of the default path is omitted
;               (e.g. $SPICE_DATA/2020/06/21/ instead of $SPICE_DATA/level2/2020/06/21/).
;               This keyword is ignored for the calculation of the resulting level 3 filepath.
;     NO_TREE_STRUCT: If set, then the tree structure won't be appended to TOP_DIR
;               (e.g. TOP_DIR/level2/ instead of TOP_DIR/level2/2020/06/21/)
;     USER_DIR: If set, the procedure searches and saves in TOP_DIR/user/ instead of TOP_DIR/.
;     SEARCH_SUBDIR: If set then the program looks for spice level 2 files recurrently,
;               i.e. in all subdirectories
;     IGNORE_TIME: If set, TIME_START and TIME_END are ignored, and all files will be
;               returned. Ignored if NO_TREE_STRUCT is not set.
;               This keyword is used by spice_xfiles, it makes it possible to return
;               all files that are stored locally
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;               This procedure masks any y regions in a narrow slit data cube that don't contain
;               slit data, i.e. pixels with contributions from parts of the
;               detector that lie above/below the dumbbells,
;               in the gap between the slit ends and the dumbbells, and the
;               dumbbell regions themselves. The masking procedure is not called for wide-slit
;               observations or if window_index corresponds to a regular
;               dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;               range, i.e. does not estimate the slit length based on the position of the dumbbells.
;     no_fitting: If set, fitting won't be computed. This can still be done manually in xcfit_block.
;     no_widget:  If set, xcfit_block and small window to stopp fitting will not be called.
;     show_xcfit_block: If set, xcfit_block will be called, for each data window before saving.
;               This is ignored if NO_WIDGET has been set.
;     position: If set, then the line position is NOT represented by the velocity
;               relative to a lab wavelength, but as the wavelength.
;     official_l3dir: If set, the file will be moved to the directory $SPICE_DATA/level3, the directory
;               for the official level 3 files, instead of $SPICE_DATA/user/level3.
;     CREATE_IMAGES: If set, then images from the level 3 data will be created.
;               This will call spice_create_l3_images.
;     SEARCH_LEVEL3: If set, the procedure will search level 3 SPICE FITS files, instead of level 2,
;               and not create level 3 file, but only the images, if CREATE_IMAGES has been set.
;     NO_OVERWRITE: If set, then level 3 files won't be regenerated if they already exist.
;
; OUTPUTS:
;     This procedure will create SPICE FITS level 3 files and move them to the correct directory.
;     Optionally, it can also create level 3 images.
;
; OPTIONAL OUTPUTS:
;     COUNT_FILE:An integer containing the number of matching files.
;     COUNT_SEQ: An integer containing the number of matching sequences,
;                only non-zero if SEQUENCE is set.
;     FILES_L3:  A string array containing the full paths and names of the created level 3 files.
;
; CALLS:
;
; HISTORY:
;      Ver. 1, 12-Oct-2022, Martin Wiesmann
;
;-
; $Id: 2022-12-09 10:41 CET $


PRO spice_create_l3_driver, time_start, time_end=time_end, l2_files=l2_files, $
  top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
  all=all, sequence=sequence, no_level=no_level, no_tree_struct=no_tree_struct, user_dir=user_dir, $
  search_subdir=search_subdir, ignore_time=ignore_time, $
  no_masking=no_masking, approximated_slit=approximated_slit, $
  no_fitting=no_fitting, no_widget=no_widget, show_xcfit_block=show_xcfit_block, position=position, velocity=velocity, $
  official_l3dir=official_l3dir, create_images=create_images, images_top_dir=images_top_dir, $
  files_l3=files_l3, search_level3=search_level3, no_overwrite=no_overwrite

  prits_tools.parcheck, time_start, 1, "time_start", 'time', 0
  prits_tools.parcheck, time_end, 0, "time_end", ['time', 'undefined'], 0
  prits_tools.parcheck, l2_files, 0, "l2_files", ['string', 'undefined'], [0, 1]
  prits_tools.parcheck, top_dir, 0, "top_dir", ['string', 'undefined'], [0, 1]
  prits_tools.parcheck, path_index, 0, "path_index", ['integers', 'undefined'], 0
  prits_tools.parcheck, velocity, 0, "velocity", ['NUMERIC', 'undefined'], 0
  prits_tools.parcheck, images_top_dir, 0, "images_top_dir", ['string', 'undefined'], 0

  IF ~keyword_set(l2_files) THEN BEGIN

    IF keyword_set(search_level3) THEN level=3 ELSE level=2
    files = SPICE_FIND_FILE(time_start, time_end=time_end, level=level, $
      top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
      SEQUENCE=SEQUENCE, ALL=ALL, NO_LEVEL=NO_LEVEL, NO_TREE_STRUCT=NO_TREE_STRUCT, USER_DIR=USER_DIR, $
      SEARCH_SUBDIR=SEARCH_SUBDIR, IGNORE_TIME=IGNORE_TIME)

    IF keyword_set(sequence) THEN BEGIN
      files = files.toArray(dimension=1)
    ENDIF

  ENDIF ELSE BEGIN
    count_file = N_ELEMENTS(l2_files)
    files = l2_files
  ENDELSE

  files_l3 = []
  FOR ifile=0,count_file-1 DO BEGIN

    IF ~keyword_set(search_level3) THEN BEGIN

      l2_file = files[ifile]
      print, 'LEVEL 2: '+l2_file

      do_create_l3 = 1
      IF keyword_set(no_overwrite) THEN BEGIN
        filename_l3 = l2_file.replace('_L2_', '_L3_')
        filename_l3 = file_basename(filename_l3)
        spice_ingest, filename_l3, destination=destination, file_moved=file_moved, $
          user_dir=~keyword_set(official_l3dir), top_dir=top_dir, path_index=path_index, /dry_run
        IF ~file_moved[0] THEN BEGIN
          print, 'level 3 file already exists, not doing it again.'
          spice_ingest, filename_l3, destination=destination, /force, $
            user_dir=~keyword_set(official_l3dir), top_dir=top_dir, path_index=path_index, /dry_run
          l3_file = destination[0]
          do_create_l3 = 0
        ENDIF
      ENDIF

      IF do_create_l3 THEN BEGIN
        l2_object = spice_get_object(l2_file, is_spice=is_spice, object_created=object_created)
        IF ~is_spice THEN continue

        l3_file = l2_object->create_l3_file(no_masking=no_masking, approximated_slit=approximated_slit, $
          no_fitting=no_fitting, no_widget=no_widget, no_xcfit_block=~keyword_set(show_xcfit_block), position=position, velocity=velocity, $
          official_l3dir=official_l3dir, top_dir=top_dir, path_index=path_index)
      ENDIF

      files_l3 = [files_l3, l3_file]
    ENDIF ELSE BEGIN
      l3_file = files[ifile]
    ENDELSE

    print, 'LEVEL 3: '+l3_file

    IF keyword_set(create_images) THEN BEGIN
      spice_ingest, l3_file, destination=destination, /force, $
        user_dir=~keyword_set(official_l3dir), top_dir=images_top_dir, path_index=path_index, /dry_run
      out_dir = file_dirname(destination[0], /mark_directory)
      out_dir = out_dir.replace('level3', 'images')
      spice_create_l3_images, l3_file, out_dir, /NO_TREE_STRUCT
    ENDIF

  ENDFOR

END
