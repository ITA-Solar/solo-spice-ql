PRO test_new_sigma, file = filename
  files = []
  files = [files, "$SPICE_DATA/level2/2025/03/31/solo_L2_spice-n-ras_20250331T160031_V04_318767282-000.fits"]
  files = [files, "$SPICE_DATA/level2/2025/04/01/solo_L2_spice-n-ras_20250401T050032_V03_318767283-000.fits"]
  files = [files, "$SPICE_DATA/level3/2025/04/27/solo_L3_spice-n-sit_20250427T002025_V01_318767457-005.fits"]

  file = files[0]
  obj = spice_data(file)
  l3 = obj.create_l3_file([0, 1, 2])
  print, "Created L3 file: ", l3
END
