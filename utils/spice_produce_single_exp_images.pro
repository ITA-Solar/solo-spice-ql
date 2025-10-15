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
  search_pattern = ptools.regex_replace(l2_file, ".*([0-9]{8}T[0-9]{6}.*).fits", "*$1*.jpg")
  search_pattern = ptools.regex_replace(search_pattern, "_V[0-9]{2}_", "_Vxx_")
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
      box_message, ['', newer, 'not processing (but rsyncing if necessary)', '']
      COMMON spsei_process_file, last_rsync
      ptools.default, last_rsync, ""
      IF last_rsync NE outdir THEN spsei_rsync_to_other_server, outdir
      last_rsync = outdir
      return
    END
  END
  IF ~file_test(outdir, /directory) THEN file_mkdir, outdir
  spice_create_l2_images_single_exp, [l2_file], outdir, show_plot = 0
  spsei_rsync_to_other_server, outdir
END

PRO spsei_set_production_conditions, l2_topdir, level3qljpg_f, date, forever = forever
  IF getenv("USER") NE "osdcapps" THEN $
    message, "This is a production script, only to be run by osdcapps"
  IF getenv("HOST") NE "astro-sdc-fs2.uio.no" THEN $
    message, "This is a production script, only to be run on astro-sdc-fs2"
  IF getenv("USE_STEINHH_PATHS") EQ "" THEN $
    message, "This is a production script, only to be run with steinhh's paths"
  l2_topdir = '$HOME/spice_home/fits/level2'
  level3qljpg_f = '$HOME/spice_home/quicklook/level3qljpg_f'
  ptools.default, date, ''
  forever = 1
END

PRO check_production_memory_usage
  mb_memory = memory(/current) / 2L ^ 20
  print, 'Memory usage: ' + mb_memory.tostring() + ' MB'
  IF mb_memory GT 500 THEN BEGIN
    message, 'Memory usage is too high: ' + mb_memory.tostring() + ' MB', /continue
    exit
  ENDIF
END

PRO spice_produce_single_exp_images, l2_topdir, level3qljpg_f, date, force = force, forever = forever, production = production, pattern = pattern, on_switch = on_switch
  ; Weird: without a .reset, IDL starts off making garbled images - looks like a wrong jpeg encoding,
  ; or like a broken graphics card. So we check here that it has been performed:
  IF getenv("IDL_RESET_DONE") NE "yes" THEN message, "RESET IDL FIRST, or else something does not work (what???)"

  IF keyword_set(production) THEN BEGIN
    spsei_set_production_conditions, l2_topdir, level3qljpg_f, date, forever = forever
  ENDIF

  IF ~keyword_set(pattern) THEN pattern = '.*'
  box_message, "Only processing files matching pattern: " + pattern

  ; Implicitly checking that directories exist:
  l2_topdir = ptools.physical_path(concat_dir(l2_topdir, date))
  level3qljpg_f = ptools.physical_path(concat_dir(level3qljpg_f, date))

  REPEAT BEGIN
    l2_files = file_search(l2_topdir, 'solo_L2*exp*.fits', count = nfiles)
    l2_files = l2_files[sort(l2_files)] ; Oldest first
    l2_files = reverse(l2_files) ; Newest first
    IF nfiles EQ 0 THEN message, "No SPICE files found in " + l2_topdir
    FOREACH l2_file, l2_files DO BEGIN
      IF ~stregex(l2_file, pattern, /boolean) THEN BEGIN
        print, 'Skipping ' + l2_file + " because it does not match pattern " + pattern
        CONTINUE
      END
      IF keyword_set(on_switch) THEN pattern = ".*"
      spsei_process_file, l2_file, l2_topdir, level3qljpg_f, force = force
      IF keyword_set(production) THEN check_production_memory_usage
    ENDFOREACH
    force = 0
  END UNTIL ~keyword_set(forever)
END

PRO runtest
  IF getenv("USER") EQ "steinhh" THEN BEGIN
    setenv,"SPICE_DATA=/home/steinhh/tmp/spice_data/"
    spice_produce_single_exp_images, '$SPICE_DATA/level2', $
      '$SPICE_DATA/quicklook/level3qljpg_f', '2025/03/14', /force
  END
END

runtest
END
