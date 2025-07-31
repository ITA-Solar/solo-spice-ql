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
;      ptools.parcheck, spice_object, spice_create_l3_images_single_exp
;
; HISTORY:
;      Ver. 1,   10-Feb-2025, Martin Wiesmann (prits-group@astro.uio.no)
;
;-
; $Id: 2025-07-31 13:25 CEST $

PRO spcl2im_report_error, l2_file, force_email = force_email
  COMMON spcl2im_report_error, last_report_time
  ptools.default, last_report_time, 0
  error_reports_file = '/tmp/spcl2im_error_reports'
  ; spawn, "echo " + l2_file + " >> " + error_reports_file
  box_message, ['', 'Error reading L2 file!', '', '     ' + l2_file, '', ''], /info
  curr_time = systime(1)
  IF curr_time - last_report_time GT 240 OR keyword_set(force_email) THEN BEGIN
    subject = 'SPICE L2 image creation error: ' + l2_file + ' (' + error_reports_file + ')'
    echo_email_contents = "echo 'See also " + error_reports_file + "'"
    send_mail = "mail -s '" + subject + "' s.v.h.haugan@astro.uio.no < /dev/null"
    cmd = send_mail
    print, cmd
    spawn, cmd
  END
  last_report_time = systime(1)
END

PRO spice_create_l2_images_single_exp, l2_files, out_dir, show_plot = show_plot
  ptools.parcheck, l2_files, 1, "l2_files", 'STRing', [0, 1]
  ptools.parcheck, out_dir, 2, "out_dir", 'STRing', 0

  spawn, "truncate -s 0 /tmp/spcl2im_error_reports"

  IF ~file_test(out_dir, /directory) THEN file_mkdir, out_dir

  FOREACH l2_file, l2_files DO BEGIN
    l2_filename = file_basename(l2_file)
    l2_filename = ptools.regex_replace(l2_filename, '\..*', '')
    l3ql_filename = ptools.regex_replace(l2_filename, 'solo_L2_spice-', 'solo_L3_spice-ql-')
    l3ql_filename = ptools.regex_replace(l3ql_filename, 'V[0-9]{2}', 'Vxx')
    filename_base = out_dir + path_sep() + l3ql_filename + '-'
    print, "L3QL filename base: " + filename_base
    error = 0
    IF getenv("USER") EQ "osdcapps" THEN BEGIN
      catch, error
    END
    IF error NE 0 THEN BEGIN
      catch, /cancel
      spcl2im_report_error, l2_file
      CONTINUE
    ENDIF
    l2_object = spice_object(l2_file)
    FOR iwin = 0, l2_object.get_number_windows() - 1 DO BEGIN
      image_data = l2_object.get_window_data(iwin, /no_masking)
      image_data = transpose(reform(image_data))
      l2_header = l2_object.get_header(iwin)
      spice_create_l3_images_single_exp, image_data, l2_header, filename_base, show_plot = show_plot, oJpg = oJpg_l2
    ENDFOR
    obj_destroy, l2_object
  ENDFOREACH
  IF n_elements(oJpg_l2) NE 0 THEN obj_destroy, oJpg_l2
END
