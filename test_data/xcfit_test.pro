;
;
FUNCTION get_test_ana
  filepath = ptools.find_nearest_matching_files_in_repo("xcfit_test_file.ana")
  ana = restore_analysis(filepath)
  return, ana
END

PRO make_test_ana
  file = ptools.find_nearest_matching_files_in_repo("solo_L3_spice-n-ras_20231028T005506_V*_218104189-003.fits")
  ana = fits2ana(file)
  save_analysis, ana[0]
END

PRO xcfit_test
  ana = get_test_ana()

  handle_value, ana.result_h, result
  handle_value, ana.data_h, data
  handle_value, ana.lambda_h, lambda
  handle_value, ana.weights_h, weights
  handle_value, ana.residual_h, residual
  handle_value, ana.include_h, include
  handle_value, ana.const_h, const
  handle_value, ana.fit_h, fit
  !except = 0

  box_message, 'Click on stop, This runs just to compile all procedures and functions in the xcfit package.'
  cfit_block, analysis = ana, /quiet, /double, x_face = 1, smart = 1
  
  profiler, /reset
  profiler
  profiler, /system
  tic

  cfit_block, analysis = ana, /quiet, /double, x_face = 1, smart = 1

  time = toc()
  print, 'Time used in cfit_block : ', time, ' seconds'

  profiler, /report, /code_coverage, filename = 'xcfit_test_report.txt'

  xcfit_block, analysis = ana, scale=[1, 4, 1]
  
  ; Check no errors (no lost data cubes):
  ;
  handle_value, ana.result_h, result
  handle_value, ana.data_h, data
  handle_value, ana.lambda_h, lambda
  handle_value, ana.weights_h, weights
  handle_value, ana.residual_h, residual
  handle_value, ana.include_h, include
  handle_value, ana.const_h, const
  handle_value, ana.fit_h, fit  
END

PRO xcfit_block_test
  ana = get_test_ana()
  handle_value, ana.scale_h, [1,4,1],/set
  xcfit_block, ana=ana
END

 xcfit_block_test
xcfit_test
END
