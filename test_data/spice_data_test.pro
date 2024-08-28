; $Id: 2024-08-28 15:22 CEST $
PRO get_window_position_test, spice_object
  result = spice_object->get_window_position('adf')
  IF ~result.equals([-999, -999, -999, -999], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_window_position(0, detector=detector)
  IF ~result.equals([6, 55, 1, 1024], tolerance=0.001) THEN message, 'something is wrong here'
  IF detector NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_window_position(1, detector=detector, /idl_coord)
  IF ~result.equals([1093, 1128, 0, 1023], tolerance=0.001) THEN message, 'something is wrong here'
  IF detector NE 2 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END





;PRO get_xcen_test, spice_object
;  result = spice_object->get_xcen(0)
;  IF ~result.equals(-90.665861, tolerance=0.001) THEN message, 'something is wrong here'
;
;  message, ' passed the tests', /info
;END
;
;
;PRO get_xcen_test, spice_object
;  result = spice_object->get_xcen(0)
;  IF ~result.equals(-90.665861, tolerance=0.001) THEN message, 'something is wrong here'
;
;  message, ' passed the tests', /info
;END
;
;
PRO get_number_windows_test, spice_object
  result = spice_object->get_number_windows()
  IF result NE 2 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_number_extensions_test, spice_object
  result = spice_object->get_number_extensions()
  IF result NE 3 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_title_test, spice_object
  result = spice_object->get_title()
  IF result NE 'SPICE' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_obs_id_test, spice_object
  result = spice_object->get_obs_id()
  IF result NE 234881027 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_start_time_test, spice_object
  result = spice_object->get_start_time()
  IF result NE '2024-01-01T21:33:46.579' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_end_time_test, spice_object
  result = spice_object->get_end_time()
  IF result NE '2024-01-01T21:33:47.079' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_sit_and_stare_test, spice_object
  result = spice_object->get_sit_and_stare()
  IF result NE 0 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_level_test, spice_object
  result = spice_object->get_level()
  IF result NE 2 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_variable_unit_test, spice_object
  result = spice_object->get_variable_unit()
  IF result NE 'W/m2/sr/nm' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_variable_type_test, spice_object
  result = spice_object->get_variable_type()
  IF result NE 'Spectral Radiance' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_satellite_rotation_test, spice_object
  result = spice_object->get_satellite_rotation()
  IF ~result.equals(-7.3168050, tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_missing_value_test, spice_object
  result = spice_object->get_missing_value()
  IF result EQ result THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_ccd_size_test, spice_object
  result = spice_object->get_ccd_size()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF ~result[0:1].equals([1024, 1024], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_window_id_test, spice_object
  result = spice_object->get_window_id()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0] NE 'FLT02_Two Window_OB_ID_253_' || result[1] NE 'FLT02_Two Window_OB_ID_254_' THEN message, 'something is wrong here'

  result = spice_object->get_window_id(1)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF result[0] NE 'FLT02_Two Window_OB_ID_254_' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO show_lines_test, spice_object
  spice_object->show_lines

  message, ' passed the tests', /info
END


PRO get_number_exposures_test, spice_object
  result = spice_object->get_number_exposures()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'

  result = spice_object->get_number_exposures(0)
  IF result NE 1 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_number_y_pixels_test, spice_object
  result = spice_object->get_number_y_pixels()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'

  result = spice_object->get_number_y_pixels(0)
  IF result NE 1024 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_exposure_time_test, spice_object
  result = spice_object->get_exposure_time(0)
  IF result NE 0.5 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_axis_title_test, spice_object
  result = spice_object->get_axis_title()
  IF N_ELEMENTS(result) NE 4 THEN message, 'something is wrong here'

  result = spice_object->get_axis_title(0)
  IF result NE 'Solar X [arcsec]' THEN message, 'something is wrong here'

  result = spice_object->get_axis_title([1,2], /pixels)
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0] NE 'Solar Y [pixels]' THEN message, 'something is wrong here'

  result = spice_object->get_axis_title(3, /no_unit)
  IF result NE 'Time' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_instr_x_vector_test, spice_object
  result = spice_object->get_instr_x_vector(0)
  IF ~result.equals([-90.665861], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_instr_y_vector_test, spice_object
  result = spice_object->get_instr_y_vector(0)
  IF N_ELEMENTS(result) NE 1024 THEN message, 'something is wrong here'
  IF ~result[0:1].equals([-618.38738,  -617.29833], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_instr_y_vector(0, /full_ccd)
  IF N_ELEMENTS(result) NE 1024 THEN message, 'something is wrong here'
  IF ~result[0:1].equals([-618.38738,  -617.29833], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_lambda_vector_test, spice_object
  result = spice_object->get_lambda_vector(0)
  IF N_ELEMENTS(result) NE 50 THEN message, 'something is wrong here'
  IF ~result[0:1].equals([69.576659, 69.586411], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_lambda_vector(0, /full_ccd)
  IF N_ELEMENTS(result) NE 1024 THEN message, 'something is wrong here'
  IF ~result[0:1].equals([69.527900, 69.537652], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_time_vector_test, spice_object
  result = spice_object->get_time_vector(0)
  IF ~result.equals([0.25], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_xcen_test, spice_object
  result = spice_object->get_xcen(0)
  IF ~result.equals(-90.665861, tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_ycen_test, spice_object
  result = spice_object->get_ycen(0)
  IF ~result.equals(-61.333639, tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_fovx_test, spice_object
  result = spice_object->get_fovx(0)
  IF ~result.equals(143.05261, tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_fovy_test, spice_object
  result = spice_object->get_fovy(0)
  IF ~result.equals(1114.1075, tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_wcs_coord_test, spice_object
  result = spice_object->get_wcs_coord('FLT02_TWO WINDOW_OB_ID_253_')
  size_result = size(result)
  IF ~size_result.equals([5, 4, 1, 1024, 50, 1, 5, 204800]) THEN message, 'something is wrong here'

  result = spice_object->get_wcs_coord(1, [0,0,0,0])
  IF N_ELEMENTS(result) NE 4 THEN message, 'something is wrong here'

  result = spice_object->get_wcs_coord(1, [0,0,0,0], /lambda)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_wcs_coord(1, [[0,0,0,0], [3,6,4,7]])
  size_result = size(result)
  IF ~size_result.equals([2, 4, 2, 5, 8]) THEN message, 'something is wrong here'

  result = spice_object->get_wcs_coord(1, [[0,0,0,0], [3,6,4,7]], /y)
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'

  result = spice_object->get_wcs_coord(1, /time)
  size_result = size(result)
  IF ~size_result.equals([4, 1, 1024, 36, 1, 5, 36864]) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_resolution_test, spice_object
  result = spice_object->get_resolution(0)
  IF N_ELEMENTS(result) NE 4 THEN message, 'something is wrong here'
  IF ~result.equals([1.0000000, 1.0980000, 0.0097517000, 1.0000000], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_resolution('FLT02_TWO WINDOW_OB_ID_253_', /x)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF ~result.equals([1.0000000], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_resolution('FLT02_TWO WINDOW_OB_ID_253_', /y)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF ~result.equals([1.0980000], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_resolution('FLT02_TWO WINDOW_OB_ID_253_', /lambda)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF ~result.equals([0.0097517000], tolerance=0.001) THEN message, 'something is wrong here'

  result = spice_object->get_resolution('FLT02_TWO WINDOW_OB_ID_253_', /time)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF ~result.equals([1.0000000], tolerance=0.001) THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_spatial_binning_test, spice_object
  result = spice_object->get_spatial_binning()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0] NE 1 || result[1] NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_spatial_binning(0)
  IF result[0] NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_spatial_binning('FLT02_TWO WINDOW_OB_ID_253_')
  IF result[0] NE 1 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_spectral_binning_test, spice_object
  result = spice_object->get_spectral_binning()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0] NE 1 || result[1] NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_spectral_binning(0)
  IF result[0] NE 1 THEN message, 'something is wrong here'

  result = spice_object->get_spectral_binning('FLT02_TWO WINDOW_OB_ID_253_')
  IF result[0] NE 1 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO check_window_index_test, spice_object
  result = spice_object->check_window_index(0)
  IF ~result THEN message, 'something is wrong here'

  result = spice_object->check_window_index('FLT02_TWO WINDOW_OB_ID_253_')
  IF ~result THEN message, 'something is wrong here'

  result = spice_object->check_window_index('VARIABLE_KEYWORDS')
  IF result THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO check_extension_index_test, spice_object
  result = spice_object->check_extension_index(0)
  IF ~result THEN message, 'something is wrong here'

  result = spice_object->check_extension_index('FLT02_TWO WINDOW_OB_ID_253_')
  IF ~result THEN message, 'something is wrong here'

  result = spice_object->check_extension_index('VARIABLE_KEYWORDS')
  IF ~result THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO return_extension_index_test, spice_object
  result = spice_object->return_extension_index(0)
  IF result NE 0 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index(2)
  IF result NE 2 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index(2, /check_window)
  IF result NE -1 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index(5)
  IF result NE -1 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index('FLT02_TWO WINDOW_OB_ID_253_')
  IF result NE 0 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index('asdfa')
  IF result NE -1 THEN message, 'something is wrong here'

  result = spice_object->return_extension_index('VARIABLE_KEYWORDS')
  IF result NE 2 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO has_dumbbells_test, spice_object
  result = spice_object->has_dumbbells()
  IF result NE 0 THEN message, 'something is wrong here'

  result = spice_object->has_dumbbells(0)
  IF result NE 0 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_dumbbells_index_test, spice_object
  result = spice_object->get_dumbbells_index()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF ~result.equals([-1, -1]) THEN message, 'something is wrong here'

  result = spice_object->get_dumbbells_index(/lower)
  IF result NE -1 THEN message, 'something is wrong here'

  result = spice_object->get_dumbbells_index(/upper)
  IF result NE -1 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_bintable_ttypes_test, spice_object
  result = spice_object->get_bintable_ttypes(column_indices=column_indices)
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF N_ELEMENTS(column_indices) NE 2 THEN message, 'something is wrong here'
  IF column_indices[0] NE 0 || column_indices[1] NE 1 THEN message, 'something is wrong here'
  
  result = spice_object->get_bintable_ttypes(/include_window_tag)
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_253_]' THEN message, 'something is wrong here'

  result = spice_object->get_bintable_ttypes(/include_window_tag, extension=1)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_254_]' THEN message, 'something is wrong here'
  
  result = spice_object->get_bintable_ttypes(/include_window_tag, extension='FLT02_TWO WINDOW_OB_ID_254_')
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_254_]' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO expand_ttypes_test, spice_object
  result = spice_object->expand_ttypes('radcal', column_indices=column_indices)
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_253_]' || result[1] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_254_]' THEN message, 'something is wrong here'
  IF N_ELEMENTS(column_indices) NE 2 THEN message, 'something is wrong here'
  IF column_indices[0] NE 0 || column_indices[1] NE 1 THEN message, 'something is wrong here'

  result = spice_object->expand_ttypes('radcal', extension=1)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_254_]' THEN message, 'something is wrong here'

  result = spice_object->expand_ttypes('radcal', extension='FLT02_TWO WINDOW_OB_ID_254_')
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF result[0] NE 'RADCAL[FLT02_TWO WINDOW_OB_ID_254_]' THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END


PRO get_bintable_data_test, spice_object
  result = spice_object->get_bintable_data()
  IF N_ELEMENTS(result) NE 2 THEN message, 'something is wrong here'
  IF result[0].ttype NE 'RADCAL' THEN message, 'something is wrong here'
  IF (*result[1].data_extension_name)[0] NE 'FLT02_Two Window_OB_ID_254_' THEN message, 'something is wrong here'

  result = spice_object->get_bintable_data(extension=1)
  IF N_ELEMENTS(result) NE 1 THEN message, 'something is wrong here'
  IF (*result[0].data_extension_name)[0] NE 'FLT02_Two Window_OB_ID_254_' THEN message, 'something is wrong here'

  result = spice_object->get_bintable_data(extension=1, /values_only)
  size_result = size(result)
  IF total(size_result EQ [3,1,1,36,5,36]) NE 6 THEN message, 'something is wrong here'

  result = spice_object->get_bintable_data(extension='FLT02_Two Window_OB_ID_254_', /values_only)
  size_result = size(result)
  IF total(size_result EQ [3,1,1,36,5,36]) NE 6 THEN message, 'something is wrong here'

  message, ' passed the tests', /info
END




; MAIN PROGRAM ------------------------------

PRO spice_data_test

  dir = ROUTINE_DIR()
  l2file = filepath('solo_L2_spice-n-exp_20240101T213346_V02_234881027-000.fits', root_dir=dir)
  spice_object = spice_data(l2file)
  
  print, ''
  print, ' --- Start of tests ---'
  print, ''

  get_window_position_test, spice_object
  
  get_number_windows_test, spice_object
  get_number_extensions_test, spice_object
  get_title_test, spice_object
  get_obs_id_test, spice_object
  get_start_time_test, spice_object
  get_end_time_test, spice_object
  get_sit_and_stare_test, spice_object
  get_level_test, spice_object
  get_variable_unit_test, spice_object
  get_variable_type_test, spice_object
  get_satellite_rotation_test, spice_object
  get_missing_value_test, spice_object
  get_ccd_size_test, spice_object
  get_window_id_test, spice_object
  show_lines_test, spice_object
  get_number_exposures_test, spice_object
  get_number_y_pixels_test, spice_object
  get_exposure_time_test, spice_object
  get_axis_title_test, spice_object
  get_instr_x_vector_test, spice_object
  get_instr_y_vector_test, spice_object
  get_lambda_vector_test, spice_object
  get_time_vector_test, spice_object
  get_xcen_test, spice_object
  get_ycen_test, spice_object
  get_fovx_test, spice_object
  get_fovy_test, spice_object
  get_wcs_coord_test, spice_object
  get_resolution_test, spice_object
  get_spatial_binning_test, spice_object
  get_spectral_binning_test, spice_object
  check_window_index_test, spice_object
  check_extension_index_test, spice_object
  return_extension_index_test, spice_object
  has_dumbbells_test, spice_object
  get_dumbbells_index_test, spice_object
  get_bintable_ttypes_test, spice_object
  expand_ttypes_test, spice_object
  get_bintable_data_test, spice_object

  print, ''
  print, ' --- End of tests ---'
  print, ''

  message, 'All tests passed', /info
END
