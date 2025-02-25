PRO test_get_distorted_coord
  create_l3 = 0 ; create L3 files
  use_l2 = 1 ; use L2 files

  l2_files = ['/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits', $ ; raster
    '/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-sit_20231028T032925_V22_218104192-000.fits', $ ; sit-and-stare
    '/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T151432_V02_167772346-000.fits', $ ; single exposure, small window
    '/Users/mawiesma/data/spice/level2/2024/01/01/solo_L2_spice-n-exp_20240101T180040_V01_234881025-000.fits' $ ; single exposure, whole detector
    ] ; list of L2 files
  file = l2_files[0] ; select one file

  obj = spice_object(file)
  x0 = obj.get_instr_x_vector(0)
  print, x0[0 : 9]

  data = readfits(file, header, ext = 0)
  wcs = fitshead2wcs(header, filename = file)
  corrected_coordinates = wcs_get_coord(wcs)
  help, corrected_coordinates
  sc = size(corrected_coordinates)
  print, corrected_coordinates[0, 0 : 9, floor(sc[2] / 2.), floor(sc[3] / 2.), floor(sc[4] / 2.)]
  stop
END
