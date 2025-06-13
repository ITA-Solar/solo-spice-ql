PRO test_new_sigma, file = filename
  COMMON test_new_sigma, hotpix_obj

  files = []
  files = [files, "$SPICE_DATA/level2/2025/03/31/solo_L2_spice-n-ras_20250331T160031_V04_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level2/2025/04/01/solo_L2_spice-n-ras_20250401T050032_V03_318767283-000.fits"]
  files = [files, "solo_L3_spice-n-ras_20250331T160031_V03_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level1/2025/03/31/solo_L1_spice-n-ras_20250331T160031_V04_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level1/2025/04/01/solo_L1_spice-n-ras_20250401T050032_V03_318767283-000.fits"]

  files = [files, "solo_L1_spice-n-ras_20250331T160031_V02_318767282-000.fits"] ; level 3 from level 1
  files = [files, "solo_L1_spice-n-ras_20250331T160031_V06_318767282-000.fits"] ; level 3 from level 1 - with hot pixels removed
  files = [files, "solo_L1_spice-n-ras_20250331T160031_V08_318767282-000.fits"] ; level 3 from level 1 - with hot pixels removed
  ; Limit_Median_Neighbor = 6.0
  ; Limit_Fraction_To_Signal = 150.0
  ; Fri 14:38

  out_dir = '/Users/mawiesma/Documents/spice/level3-tests/'

  create_l3_from_l2 = 0
  create_l3_from_l1 = 1

  IF create_l3_from_l2 THEN BEGIN
    file = files[0]
    obj = spice_data(file)
    l3 = obj.create_l3_file([0, 1, 2])
    print, "Created L3 file: ", l3
  ENDIF ELSE IF create_l3_from_l1 THEN BEGIN
    file = spice_find_file(files[3], level = 1)
    file = file[0]
    print, file
    ; file = file.replace('_L1_', '_L2_')
    ; print, file

    obj = spice_data(file)
    l3 = obj.create_l3_file([0], /no_line_list)
    print, "Created L3 file: ", l3
  ENDIF ELSE BEGIN
    file = spice_find_file(files[7], /user, level = 1)
    file = file[0]
    print, file
    spice_create_l3_images, file, out_dir, /no_background_images, /no_tree_struct

    return
    d = readfits(file, h)
    help, d
    ; h = fitshead2struct(h)

    IF n_elements(hotpix_obj) EQ 0 THEN BEGIN
      hotpix_obj = obj_new('hotpix')
      help, hotpix_obj
      ;
      hotpix_obj.set, fxpar(h, 'DATE-BEG')
      print, '--- hotpix_obj set ---'
      ; [, days_window=n, catalog_max_age_hours=n, /reset_catalog]
      ;
    ENDIF
    hotpix_obj.darks, lw_map, sw_map ; Get "dark maps" for LW/SW detector
    help, lw_map, sw_map
    stop

    ana = fits2ana(file)
    help, ana

    spice_xcfit_block, ana = ana[0]
    ; spice_xcfit_block, ana = ana[1]
    ; spice_xcfit_block, ana = ana[2]
  ENDELSE
END
