pro spice_test_l3_driver

  ; single exposure first then raster
  time_start = '2022-01-01 12:17'
  ;time_end = '2022-01-01 12:20'

  ; sit-and-stare
  ;time_start = '2022-03-22 23:34'
  time_end = '2022-03-23 00:20'

  ;top_dir=top_dir
  ;path_index=path_index
  ;all=1
  ;sequence=1
  ;no_level=1
  ;no_tree_struct=1
  user_dir=1
  ;search_subdir=1
  ;ignore_time=1
  ;no_masking=1
  ;approximated_slit=1
  ;no_fitting=1
  ;no_widget=1
  ;show_xcfit_block=1
  ;position=1
  ;velocity=velocity
  ;official_l3dir=1
  create_images=1
  ;images_top_dir=images_top_dir
  ;search_level3=1
  no_overwrite=1


  spice_create_l3_driver, time_start, time_end=time_end, $
    top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
    all=all, sequence=sequence, no_level=no_level, no_tree_struct=no_tree_struct, user_dir=user_dir, $
    search_subdir=search_subdir, ignore_time=ignore_time, $
    no_masking=no_masking, approximated_slit=approximated_slit, $
    no_fitting=no_fitting, no_widget=no_widget, show_xcfit_block=show_xcfit_block, position=position, velocity=velocity, $
    official_l3dir=official_l3dir, create_images=create_images, images_top_dir=images_top_dir, $
    files_l3=files_l3, search_level3=search_level3, no_overwrite=no_overwrite


    help, count_file, count_seq, files_l3
    for i=0,N_ELEMENTS(files_l3)-1 do print,files_l3[i]


end
