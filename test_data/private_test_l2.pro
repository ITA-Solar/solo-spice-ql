; $Id: 2025-02-18 21:05 CET $
PRO private_test_l2
  files = ['/Users/mawiesma/data/spice/level2/2021/12/26/solo_L2_spice-n-exp_20211226T105125_V02_83886470-000.fits', $
    '/Users/mawiesma/data/spice/level2/2022/06/06/solo_L2_spice-n-exp_20220606T062839_V07_117441013-000.fits']

  l2_files = files
  out_dir = '/Users/mawiesma/spice/tests/single_exp_l2'
  show_plot = 0
  spice_create_l2_images_single_exp, l2_files, out_dir, show_plot = show_plot
END
