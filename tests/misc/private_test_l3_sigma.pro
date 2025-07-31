PRO private_test
  create_l3 = 0 ; create L3 files

  l2_files = ['/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits', $ ; raster
    '/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-sit_20231028T032925_V22_218104192-000.fits', $ ; sit-and-stare
    '/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T151432_V02_167772346-000.fits', $ ; single exposure, small window
    '/Users/mawiesma/data/spice/level2/2024/01/01/solo_L2_spice-n-exp_20240101T180040_V02_234881025-000.fits' $ ; single exposure, whole detector
    ] ; list of L2 files

  IF create_l3 THEN BEGIN
    out_dir = '/Users/mawiesma/Documents/spice/tests/test_l3_files/' ; output directory

    no_xcfit_block = 1
    no_line_list = 0
    no_fitting = 0

    l2_files = l2_files[0]

    window_indices = [0, 1] ; select the first two windows
    FOREACH l2_file, l2_files DO BEGIN
      o = spice_object(l2_file)
      l3_file = o.create_l3_file(window_indices, pipeline_dir = out_dir, no_xcfit_block = no_xcfit_block, no_line_list = no_line_list, no_fitting = no_fitting)
    ENDFOREACH
  ENDIF ELSE BEGIN
    l3_file = '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20231028T001206_V01_218104189-001.fits' ; select the first L3 file
    ana = fits2ana(l3_file)
    stop
  ENDELSE
END
