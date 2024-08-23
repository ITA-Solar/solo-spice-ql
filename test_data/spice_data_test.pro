; $Id: 2024-08-23 14:09 CEST $


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
  message, 'No tests defined yet', /info
END


PRO get_dumbbells_index_test, spice_object
  message, 'No tests defined yet', /info
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
