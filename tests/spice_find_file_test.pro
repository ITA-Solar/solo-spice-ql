;+
; NAME:
;      SPICE_FIND_FILE_TEST
;
; PURPOSE:
;      This routine tests spice_find_file in various ways
;
; CATEGORY:
;      SPICE -- unit test
;
; CALLING SEQUENCE:
;      SPICE_FIND_FILE_TEST
;
; HISTORY:
;      06-Nov-2020 : Martin Wiesmann : first version
;-
; $Id: 2020-11-09 12:52 CET $

PRO spice_find_file_test

  files_to_create = [ $
    '/tmp/spice/1976/01/17/solo_L2_spice-n-exp_19760117T091832_V02_16777433-000.fits', $
    '/tmp/spice/1976/01/17/solo_L2_spice-n-exp_19760117T101132_V02_16777433-000.fits', $
    '/tmp/spice/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits', $
    '/tmp/spice/1976/01/17/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits', $

    '/tmp/spice/level0/1976/01/17/solo_L0_spice-n-exp_0637083015_V197601170918C_16777433-000.fits', $
    '/tmp/spice/level0/1976/01/17/solo_L0_spice-n-exp_0637083015_V197601171011C_16777433-000.fits', $
    '/tmp/spice/level0/1976/01/17/solo_L0_spice-n-exp_0637083015_V197601171024C_16777433-000.fits', $
    '/tmp/spice/level0/1976/01/17/solo_L0_spice-n-exp_0637083015_V197601171055C_16777433-000.fits', $

    '/tmp/spice/level1/1976/01/17/solo_L1_spice-n-exp_19760117T091832_V02_16777433-000.fits', $
    '/tmp/spice/level1/1976/01/17/solo_L1_spice-n-exp_19760117T101132_V02_16777433-000.fits', $
    '/tmp/spice/level1/1976/01/17/solo_L1_spice-n-exp_19760117T102432_V02_16777433-000.fits', $
    '/tmp/spice/level1/1976/01/17/solo_L1_spice-n-exp_19760117T105532_V02_16777433-000.fits', $

    '/tmp/spice/level2/1976/01/16/solo_L2_spice-n-exp_19760116T231832_V02_16777432-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T061832_V02_16777432-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T062232_V02_16777432-000.fits', $

    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T091832_V02_16777433-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T101132_V02_16777433-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits', $

    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T145532_V02_16777434-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T150532_V02_16777434-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T154532_V02_16777434-000.fits', $

    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T190532_V02_16777435-000.fits', $
    '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T194532_V02_16777435-000.fits', $
    '/tmp/spice/level2/1976/01/18/solo_L2_spice-n-exp_19760118T004532_V02_16777435-000.fits', $

    '/tmp/spice/level2/1976/01/18/solo_L2_spice-n-exp_19760118T150532_V02_16777436-000.fits', $
    '/tmp/spice/level2/1976/01/18/solo_L2_spice-n-exp_19760118T154532_V02_16777436-000.fits', $

    '/tmp/spice/level2/1976/01/18/solo_L2_spice-n-exp_19760118T220532_V02_16777437-000.fits', $
    '/tmp/spice/level2/1976/01/18/solo_L2_spice-n-exp_19760118T224532_V02_16777437-000.fits', $

    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T091832_V02_16777433-000.fits', $
    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T101132_V02_16777433-000.fits', $
    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits', $
    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits', $

    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T190532_V02_16777435-000.fits', $
    '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T194532_V02_16777435-000.fits', $
    '/tmp/spice/notree/solo_L2_spice-n-exp_19760118T004532_V02_16777435-000.fits', $

    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T091832_V02_16777433-000.fits', $
    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T101132_V02_16777433-000.fits', $
    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T102232_V02_16777433-000.fits', $
    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits', $

    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T190532_V02_16777435-000.fits', $
    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T194532_V02_16777435-000.fits', $
    '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760118T004532_V02_16777435-000.fits']


  FOR i=0,N_ELEMENTS(files_to_create)-1 DO BEGIN
    dir = file_dirname(files_to_create[i])
    file_mkdir, dir
    spawn, 'touch ' + files_to_create[i]
  ENDFOR


  top_dir = '/tmp/spice'
  top_dir_level2 = '/tmp/spice/level2'
  top_dir_no_tree_struct = '/tmp/spice/notree'

  search_dates = ['1976/01/17 10:20:00', $ ; 0
    '17-jan-1976 10:20', $ ; 1
    '1976-01-17T10:20:00.000', $ ; 2
    '1976/01/17 10:20:00', $ ; 3
    '1976/01/17 19:20:00', $ ; 4
    '1976/01/18 21:20:00'] ; 5

  ;  1 - search file closest to date
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  1 - pass'

  ;  2 - search file closest to date in level 0
  res = spice_find_file( search_dates[1], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    level=0)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/level0/1976/01/17/solo_L0_spice-n-exp_0637083015_V197601171024C_16777433-000.fits' THEN stop
  print, 'Test  2 - pass'

  ;  3 - search file closest to date in level 1
  res = spice_find_file( search_dates[2], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    level=1)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/level1/1976/01/17/solo_L1_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  3 - pass'

  ;  4 - search file closest to date in level 2
  res = spice_find_file( search_dates[3], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    level=2)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  4 - pass'

  ;  5 - search file closest to date, do not use level part in path
  res = spice_find_file( search_dates[3], top_dir=top_dir_level2, $
    count_file=count_file, count_seq=count_seq, $
    /no_level)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  5 - pass'

  ;  6 - search all files of a specific date
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /all)
  IF count_seq NE 0 THEN stop
  IF count_file NE 11 THEN stop
  IF total(size(res) eq [1,11,7,11]) NE 4 THEN stop
  IF res[4] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  6 - pass'

  ;  7 - search all files of a specific date, but set NO_TREE_STRUCT, so that ALL is ignored
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /all, /no_tree_struct, /no_level)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  7 - pass'

  ;  8 - search all files of a specific date, set SEQUENCE, which will deactivate ALL
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /all, /sequence)
  IF count_seq NE 1 THEN stop
  IF count_file NE 4 THEN stop
  IF total(size(res) eq [1,4,7,4]) NE 4 THEN stop
  IF res[2] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test  8 - pass'

  ;  9 - search all files of a specific date, provide TIME_END, which will deactivate ALL
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /all, time_end=search_dates[4])
  IF count_seq NE 0 THEN stop
  IF count_file NE 6 THEN stop
  IF total(size(res) eq [1,6,7,6]) NE 4 THEN stop
  IF res[5] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T190532_V02_16777435-000.fits' THEN stop
  print, 'Test  9 - pass'

  ; 10 - search file closest to date, do not use level part in path, and do not use a tree structure
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T102432_V02_16777433-000.fits' THEN stop
  print, 'Test 10 - pass'

  ; 11 - search file closest to date, do not use level part in path, and do not use a tree structure, but search also sub directories
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /search_subdir)
  IF count_seq NE 0 THEN stop
  IF count_file NE 1 THEN stop
  IF res NE '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T102232_V02_16777433-000.fits' THEN stop
  print, 'Test 11 - pass'

  ; 12 - search all files no matter the date, do not use level part in path, and do not use a tree structure
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /ignore_time)
  IF count_seq NE 0 THEN stop
  IF count_file NE 7 THEN stop
  IF total(size(res) eq [1,7,7,7]) NE 4 THEN stop
  IF res[4] NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T190532_V02_16777435-000.fits' THEN stop
  print, 'Test 12 - pass'

  ; 13 - search all files no matter the date, do not use level part in path, and do not use a tree structure, search subdirectories
  ; return result as a list of sequences
  res = spice_find_file( search_dates[0], top_dir=top_dir_level2, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /ignore_time, /sequence, /search_subdir)
  IF count_seq NE 6 THEN stop
  IF count_file NE 17 THEN stop
  IF total(size(res) eq [1,6,11,6]) NE 4 THEN stop
  IF total(size(res[2]) eq [1,3,7,3]) NE 4 THEN stop
  IF (res[2])[1] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T150532_V02_16777434-000.fits' THEN stop
  print, 'Test 13 - pass'

  ; 14 - search files within a time window, also in sub-directories
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /search_subdir, time_end=search_dates[4])
  IF count_seq NE 0 THEN stop
  IF count_file NE 6 THEN stop
  IF total(size(res) eq [1,6,7,6]) NE 4 THEN stop
  IF res[4] NE '/tmp/spice/notree/subdir/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits' THEN stop
  print, 'Test 14 - pass'

  ; 15 - search files within a time window (over more than one day)
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, time_end=search_dates[5])
  IF count_seq NE 0 THEN stop
  IF count_file NE 5 THEN stop
  IF total(size(res) eq [1,5,7,5]) NE 4 THEN stop
  IF res[4] NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760118T004532_V02_16777435-000.fits' THEN stop
  print, 'Test 15 - pass'

  ; 16 - search file closest to date, do not use level part in path, and do not use a tree structure
  ; return all files that belong to this sequence
  res = spice_find_file( search_dates[0], top_dir=top_dir_no_tree_struct, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /sequence)
  IF count_seq NE 1 THEN stop
  IF count_file NE 4 THEN stop
  IF total(size(res) eq [1,4,7,4]) NE 4 THEN stop
  IF res[1] NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T101132_V02_16777433-000.fits' THEN stop
  print, 'Test 16 - pass'

  ; 17 - search files within a time window, do not use level part in path, and do not use a tree structure
  ; search all subdirectories
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /no_tree_struct, /no_level, /search_subdir, time_end=search_dates[5])
  IF count_seq NE 0 THEN stop
  IF count_file NE 22 THEN stop
  IF total(size(res) eq [1,22,7,22]) NE 4 THEN stop
  IF res[13] NE '/tmp/spice/notree/solo_L2_spice-n-exp_19760117T105532_V02_16777433-000.fits' THEN stop
  print, 'Test 17 - pass'

  ; 18 - search file closest to date
  ; return all files that belong to this sequence
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /sequence)
  IF count_seq NE 1 THEN stop
  IF count_file NE 4 THEN stop
  IF total(size(res) eq [1,4,7,4]) NE 4 THEN stop
  IF res[0] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T091832_V02_16777433-000.fits' THEN stop
  print, 'Test 18 - pass'

  ; 19 - search files within a time window
  ; return result as a list of sequences
  res = spice_find_file( search_dates[0], top_dir=top_dir, $
    count_file=count_file, count_seq=count_seq, $
    /sequence, time_end=search_dates[4])
  IF count_seq NE 3 THEN stop
  IF count_file NE 10 THEN stop
  IF total(size(res) eq [1,3,11,3]) NE 4 THEN stop
  IF total(size(res[2]) eq [1,3,7,3]) NE 4 THEN stop
  IF (res[2])[1] NE '/tmp/spice/level2/1976/01/17/solo_L2_spice-n-exp_19760117T194532_V02_16777435-000.fits' THEN stop
  print, 'Test 19 - pass'

END
