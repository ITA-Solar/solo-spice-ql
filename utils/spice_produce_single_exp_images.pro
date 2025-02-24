PRO spice_produce_single_exp_images, l2_topdir, level3qljpg_f
  IF ~file_test(l2_topdir, /directory) THEN message, "Input directory does not exist: " + l2_topdir, /continue
  IF ~file_test(level3qljpg_f, /directory) THEN message, "Output directory does not exist: " + level3qljpg_f, /continue
  l2_topdir = prits_tools.physical_path(l2_topdir)
  level3qljpg_f = prits_tools.physical_path(level3qljpg_f)
  l2_files = file_search(l2_topdir, '*exp*.fits', count = nfiles)
  IF nfiles EQ 0 THEN BEGIN
    message, "No SPICE files found in " + l2_topdir, /continue
    return
  ENDIF
  FOREACH l2_file, l2_files DO BEGIN
    fits_relative_location = strmid(l2_file, strlen(l2_topdir) + 1, 1000)
    fits_relative_path = file_dirname(fits_relative_location)
    image_path = level3qljpg_f + path_sep() + fits_relative_path
    IF ~file_test(image_path, /directory) THEN file_mkdir, image_path
    out_dir = level3qljpg_f + path_sep() + fits_relative_path
    IF ~file_test(out_dir, /directory) THEN file_mkdir, out_dir
    spice_create_l2_images_single_exp, [l2_file], out_dir, show_plot = 0
  ENDFOREACH
END

PRO runtest
  IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
    spice_produce_single_exp_images, '$HOME/tmp/spice_data/fits/level2', '$HOME/tmp/spice_data/quicklook/level3qljpg_f'
  END
END

runtest
END
