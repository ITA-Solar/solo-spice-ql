FUNCTION get_test_ana
  path = routine_dir()
  paths = strsplit(path, path_sep(), /extract)
  filepath = path_sep() + strjoin([paths[0 : -2], 'ancillary', 'xcfit_test_file.ana'], path_sep())
  ana = restore_analysis(filepath)
  return, ana
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

  !except = 2

  box_message, 'Click on stop, This runs just to compile all procedures and functions in the xcfit package.'
  cfit_block, analysis = ana, /quiet, /double, x_face = 1, smart = 1
  
  profiler, /reset
  profiler
  profiler, /system
  tic

  cfit_block, analysis = ana, /double, x_face = 1, smart = 1

  time = toc()
  print, 'Time used in cfit_block : ', time, ' seconds'

  profiler, /report, /code_coverage, filename = 'xcfit_test_report.txt'
  stop

  xcfit_block, analysis = ana
END

PRO xcfit_block_test
  ana = get_test_ana()
  xcfit_block, ana=ana, phys_scale=[2, 1, 1]
END

xcfit_block_test
END
