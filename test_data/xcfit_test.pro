PRO xcfit_test
  IF 0 THEN BEGIN
    file = '/Users/mawiesma/data/spice/user/level3/2023/10/28/solo_L3_spice-n-ras_20231028T005506_V22_218104189-003.fits'
    ana = fits2ana(file)
    save_analysis, ana[0]
  ENDIF ELSE BEGIN
    path = routine_dir()
    paths = strsplit(path, path_sep(), /extract)
    filepath = path_sep() + strjoin([paths[0 : -2], 'ancillary', 'xcfit_test_file.ana'], path_sep())
    ana = restore_analysis(filepath)

    handle_value, ana.result_h, result
    handle_value, ana.data_h, data
    handle_value, ana.lambda_h, lambda
    handle_value, ana.weights_h, weights
    handle_value, ana.residual_h, residual
    handle_value, ana.include_h, include
    handle_value, ana.const_h, const
    handle_value, ana.fit_h, fit

    help, ana
    help, result
    help, data
    help, lambda
    help, weights
    help, residual
    help, include
    help, const
    help, fit

    tic

    cfit_block, analysis = ana, /quiet, /double, x_face = 1, smart = 1

    time = toc()
    print, 'Time used in cfit_block : ', time, ' seconds'
    stop

    xcfit_block, analysis = ana
  ENDELSE
END
