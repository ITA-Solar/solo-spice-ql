FUNCTION spsei_get_outdir, l2_file, l2_topdir, level3qljpg_f
  fits_path = file_dirname(l2_file)
  fits_relative_path = strmid(fits_path, strlen(l2_topdir) + 1, 1000)
  image_path = level3qljpg_f + path_sep() + fits_relative_path
  return, image_path
END

PRO spsei_rsync_to_other_server, outdir
  ; outdir is an absolute, phuysical path on a file server, i.e.,
  host = (getenv("HOST")).replace('.uio.no', '')
  otherhost = host EQ "sdc-fs2" ? "astro-sdc-fs2" : "sdc-fs2"
  otheroutdir = outdir.replace(host, otherhost)
  mkdir = "ssh " + otherhost + " 'mkdir -p " + otheroutdir + "'"
  print, mkdir
  rsync = "rsync -av " + outdir + "/ " + otherhost + ":" + otheroutdir + "/"
  print, rsync
  IF getenv("USER") NE "osdcapps" THEN BEGIN
    box_message, ['Not osdcapps, not syncing to other server']
  END ELSE BEGIN
    spawn, mkdir
    spawn, rsync
  END
END

FUNCTION spsei_test_for_existing_images, l2_file, l2_topdir, level3qljpg_f
  outdir = spsei_get_outdir(l2_file, l2_topdir, level3qljpg_f)
  ; Match from date up to but not including .fits
  search_pattern = prits_tools.regex_replace(l2_file, ".*([0-9]{8}T[0-9]{6}.*).fits", "*$1*.jpg")
  search_pattern = prits_tools.regex_replace(search_pattern, "_V[0-9]{2}_", "_Vxx_")
  files = file_search(outdir + '/' + search_pattern, count = nfiles)
  IF nfiles EQ 0 THEN return, 0
  info_l2 = file_info(l2_file)
  info_ql = file_info(files[0])
  IF info_ql.ctime GT info_l2.ctime THEN return, 1
  RETURN, 0
END

PRO spsei_process_file, l2_file, l2_topdir, level3qljpg_f, force = force
  outdir = spsei_get_outdir(l2_file, l2_topdir, level3qljpg_f)
  IF spsei_test_for_existing_images(l2_file, l2_topdir, level3qljpg_f) THEN BEGIN
    newer = 'FILE EXISTS and is NEWER'
    IF keyword_set(force) THEN box_message, ['', newer, 'but FORCE keyword is set', ''] $
    ELSE BEGIN
      box_message, ['', newer, 'not processing (but rsyncing)', '']
      box_message, ['', 'Maybe', '']
      COMMON spsei_process_file, last_rsync
      prits_tools.default, last_rsync, ""
      IF last_rsync NE outdir THEN spsei_rsync_to_other_server, outdir $
      ELSE box_message, ['', 'Just kidding, I have rsynced this day before!', '']
      last_rsync = outdir
      return
    END
  END
  IF ~file_test(outdir, /directory) THEN file_mkdir, outdir
  spice_create_l2_images_single_exp, [l2_file], outdir, show_plot = 0
  spsei_rsync_to_other_server, outdir
END

PRO spice_produce_single_exp_images, l2_topdir, level3qljpg_f, date, force = force, forever = forever
  l2_topdir = concat_dir(l2_topdir, date)
  level3qljpg_f = concat_dir(level3qljpg_f, date)
  IF ~file_test(l2_topdir, /directory) THEN message, "Input directory does not exist: " + l2_topdir
  IF ~file_test(level3qljpg_f, /directory) THEN message, "Output directory does not exist: " + level3qljpg_f
  l2_topdir = prits_tools.physical_path(l2_topdir)
  level3qljpg_f = prits_tools.physical_path(level3qljpg_f)
  REPEAT BEGIN
    l2_files = file_search(l2_topdir, '*exp*.fits', count = nfiles)
    l2_files = reverse(l2_files) ; Newest first
    IF nfiles EQ 0 THEN BEGIN
      message, "No SPICE files found in " + l2_topdir, /continue
      return
    ENDIF
    ; l2_files = l2_files[0 : 2]
    FOREACH l2_file, l2_files DO BEGIN
      spsei_process_file, l2_file, l2_topdir, level3qljpg_f, force = force
    ENDFOREACH
  END UNTIL ~keyword_set(forever)
END

PRO runtest
  IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
    spice_produce_single_exp_images, '$HOME/tmp/spice_data/fits/level2', '$HOME/tmp/spice_data/quicklook/level3qljpg_f', /force
  END
END

PRO runtest2
  l2_file = "/mn/sertan/u1/steinhh/tmp/spice_data/fits/level2/2025/01/01/solo_L2_spice-n-exp_20250101T060744_V03_301989908-004.fits"
  search_pattern = prits_tools.regex_replace(l2_file, ".*([0-9]{8}T[0-9]{6}.*).fits", "$1")
  stop
END

PRO spsei_run_forever
  WHILE 1 DO BEGIN
    spice_produce_single_exp_images, '$HOME/spice_home/fits/level2', '$HOME/spice_home/quicklook/level3qljpg_f', /force
  ENDWHILE
END

runtest
END
