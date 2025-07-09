; Tests for xcfit_block

FUNCTION get_test_ana
  test_data_path = concat_dir(routine_dir(), "data")
  analysis_file = concat_dir(test_data_path, "xcfit_test_file.ana")
  ana = restore_analysis(analysis_file)
  return, ana
END

PRO xcfit_block_test
  ana = get_test_ana()
  handle_value, ana.scale_h, [1, 4, 1], /set
  xcfit_block, ana = ana, title = "XCFIT_BLOCK shortcuts", /no_kill_requests, widget_size_scaling = 1.0
END
isolate_ssw_exports
xcfit_block_test
END
