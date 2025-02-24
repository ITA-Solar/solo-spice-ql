;+
; NAME:
;      SPICE_CREATE_L2_IMAGES_SINGLE_EXP
;
; PURPOSE:
;      This procedure creates images from level 2 data of single exposure FITS files.
;      It is an interface to spice_create_l3_images_single_exp.pro so that level 2 files
;      can be processed in the same way as level 3 files.
;
; CATEGORY:
;      Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;      spice_create_l2_images_single_exp, l2_files, out_dir [, /show_plot]
;
; INPUTS:
;      l2_files: A SPICE level 2 FITS file. May be an array of files.
;               Must include the full path to the file(s).
;      out_dir: The directory where the images should be saved.
;
; KEYWORDS:
;     SHOW_PLOT: If set, then the image is shown on the screen and not saved into a file.
;
; OUTPUTS:
;      Writes jpeg and png files with images into out_dir.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      prits_tools.parcheck, spice_object, spice_create_l3_images_single_exp
;
; HISTORY:
;      Ver. 1,   10-Feb-2025, Martin Wiesmann
;
;-
; $Id: 2025-02-24 14:40 CET $

PRO spice_create_l2_images_single_exp, l2_files, out_dir, show_plot = show_plot
  prits_tools.parcheck, l2_files, 1, "l2_files", 'STRing', [0, 1]
  prits_tools.parcheck, out_dir, 2, "out_dir", 'STRing', 0

  IF ~file_test(out_dir, /directory) THEN file_mkdir, out_dir

  FOREACH l2_file, l2_files DO BEGIN
    l2_filename = file_basename(l2_file)
    l2_filename = prits_tools.regex_replace(l2_filename, '\..*', '')
    l3ql_filename = prits_tools.regex_replace(l2_filename, 'solo_L2_spice-', 'solo_L3_spice-ql-')
    l3ql_filename = prits_tools.regex_replace(l3ql_filename, 'V[0-9]{2}', 'Vxx')
    filename_base = out_dir + path_sep() + l3ql_filename + '-'
    print, "L3QL filename base: " + filename_base
    l2_object = spice_object(l2_file)
    FOR iwin = 0, l2_object.get_number_windows() - 1 DO BEGIN
      data = l2_object.get_window_data(iwin, /no_masking)
      image_data = transpose(reform(data))
      l2_header = l2_object.get_header(iwin)
      spice_create_l3_images_single_exp, image_data, l2_header, filename_base, show_plot = show_plot
    ENDFOR
  ENDFOREACH
END
