PRO test_new_sigma, file = filename
  files = []
  files = [files, "$SPICE_DATA/level2/2025/03/31/solo_L2_spice-n-ras_20250331T160031_V04_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level2/2025/04/01/solo_L2_spice-n-ras_20250401T050032_V03_318767283-000.fits"]
  files = [files, "solo_L3_spice-n-ras_20250331T160031_V03_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level1/2025/03/31/solo_L1_spice-n-ras_20250331T160031_V04_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level1/2025/04/01/solo_L1_spice-n-ras_20250401T050032_V03_318767283-000.fits"]

  IF 0 THEN BEGIN
    file = files[0]
    obj = spice_data(file)
    l3 = obj.create_l3_file([0, 1, 2])
    print, "Created L3 file: ", l3
  ENDIF ELSE IF 1 THEN BEGIN
    file = files[3]
    print, file
    ; file = file.replace('_L1_', '_L2_')
    ; print, file

    obj = spice_data(file)
    l3 = obj.create_l3_file([0], /no_line_list)
    print, "Created L3 file: ", l3
  ENDIF ELSE BEGIN
    file = spice_find_file(files[2], /user, level = 3)
    print, file
    ana = fits2ana(file[0])
    spice_xcfit_block, ana = ana[0]
    spice_xcfit_block, ana = ana[1]
    spice_xcfit_block, ana = ana[2]
  ENDELSE
END
