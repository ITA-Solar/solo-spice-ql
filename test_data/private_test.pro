PRO private_test
  create_l3 = 0 ; create L3 files
  use_l2 = 1 ; use L2 files

  l2_files = ['/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits', $ ; raster
    '/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-sit_20231028T032925_V22_218104192-000.fits', $ ; sit-and-stare
    '/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T151432_V02_167772346-000.fits', $ ; single exposure, small window
    '/Users/mawiesma/data/spice/level2/2024/01/01/solo_L2_spice-n-exp_20240101T180040_V02_234881025-000.fits' $ ; single exposure, whole detector
    ] ; list of L2 files

  IF create_l3 THEN BEGIN
    out_dir = '/Users/mawiesma/Documents/spice/tests/test_l3_files/' ; output directory

    window_indices = [0, 1] ; select the first two windows
    FOREACH l2_file, l2_files DO BEGIN
      o = spice_object(l2_file)
      l3_file = o.create_l3_file(window_indices, pipeline_dir = out_dir, /no_xcfit_block, no_line_list = 0) ; , /no_fitting)
    ENDFOREACH
  ENDIF ELSE BEGIN
    out_dir = '/Users/mawiesma/Documents/spice/tests/images_l4' ; output directory

    IF use_l2 THEN BEGIN
      l2_topdir = '$HOME/tmp/spice_data/fits/level2'
      l2_files = file_search(l2_topdir, '*exp*.fits', count = nfiles)
      l2_file = l2_files[0] ; select the first L3 file

      start_mem = memory(/CURRENT)
      spice_create_l2_images_single_exp, l2_files[0 : 1], out_dir, show_plot = 0, start_mem = start_mem
      print, 'Memory required: ', (memory(/HIGHWATER) - start_mem) / 1024.0 / 1024
    ENDIF ELSE BEGIN
      l3_files = ['/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20231028T001206_V01_218104189-001.fits', $ ; raster
        '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-sit_20231028T032925_V01_218104192-000.fits', $ ; sit-and-stare
        '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-exp_20230117T151432_V01_167772346-000.fits', $ ; single exposure, small window
        '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-exp_20240101T180040_V01_234881025-000.fits' $ ; single exposure, whole detector
        ] ; list of L3 files
      l3_file = l3_files[3] ; select the first L3 file
      ; ana = fits2ana(l3_file)
      ; stop
      ; no_background_images = 1 ; do not create background images
      ; strongest_lines = 1 ; only show the strongest lines
      ; reverse_colortable = 0 ; reverse the colortable
      ; no_tree_struct = 1 ; do not show the tree structure
      ; show_plot = 1 ; show the plot
      ; quiet = 0 ; do not suppress output

      default, force_version, 0
      default, show_plot, 0
      default, image_width_pixel_limit, 5
      default, remove_horizontal_trend, 1
      default, remove_vertical_trend, 1
      default, strongest_lines, 0
      default, reverse_colortable, 0
      ; version = 'xx'

      spice_create_l3_images, l3_file, out_dir, version = version, $
        remove_horizontal_trend = remove_horizontal_trend, remove_vertical_trend = remove_vertical_trend, fit_trend = fit_trend, $
        value_max = value_max, value_min = value_min, smooth = smooth, show_plot = show_plot, $
        /no_background_images, reverse_colortable = reverse_colortable, /no_tree_struct, strongest_lines = strongest_lines, quiet = quiet
    ENDELSE
  ENDELSE
END
