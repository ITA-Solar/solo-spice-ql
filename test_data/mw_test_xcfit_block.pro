pro mw_test_xcfit_block

  file_l3 = '/Users/mawiesma/data/spice/level3/2024/01/01/solo_L3_spice-n-ras_20240101T181922_V01_234881026-000.fits'

  obj = spice_data_l3(file_l3)

  ana = obj->xcfit_block(0)
end
