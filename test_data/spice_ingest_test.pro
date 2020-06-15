;+
; NAME:
;      SPICE_INGEST_TEST
;
; PURPOSE:
;      This routine tests spice_ingest in various ways
;
; CATEGORY:
;      SPICE -- unit test
;
; CALLING SEQUENCE:
;      SPICE_INGEST_TEST
;
; RESTRICTIONS:
;      The environment variable SPICE_DATA must be defined.
;
; HISTORY:
;      15-Jun-2020 : Martin Wiesmann : first version
;-
; $Id: 15.06.2020 14:21 CEST $


function spice_ingest_test_create_fits_file, spiobsid, level, datetime, repetition, startobs=startobs

  spiobsid_string = strtrim(string(spiobsid), 2)
  level_string = strtrim(string(level), 2)
  repetition_string = fns('###', repetition)

  if level eq 0 then begin
    datetime_string = time2fid(datetime, /full_year, /time)
    datetime_string = strjoin(strsplit(datetime_string, '_', /extract))
    file = '/tmp/solo_L' + level_string + '_spice-n-ras_0642754073_V' + datetime_string + 'C_' + spiobsid_string + '-' + repetition_string + '.fits'
  endif else begin
    datetime_string = time2fid(datetime, /full_year, /time, /seconds, /milliseconds)
    datetime_string = strjoin(strsplit(datetime_string, '_', /extract), 'T')
    file = '/tmp/solo_L' + level_string + '_spice-n-ras-db-int_' + datetime_string + '_V01_' + spiobsid_string + '-' + repetition_string + '.fits'
  endelse

  data = intarr(3,3,/nozero)
  mkhdr, mainheader, data
  sxaddpar, mainheader, 'SPIOBSID', spiobsid
  if keyword_set(startobs) then sxaddpar, mainheader, 'STARTOBS', startobs
  writefits, file, data, mainheader

  return, file
end


pro spice_ingest_test

  topdir=getenv('SPICE_DATA')
  IF topdir EQ '' THEN BEGIN
    print,'% SPICE_INGEST:  Please define the environment variable $SPICE_DATA to point to the '
      print,'               top level of your directory structure. Returning...'
    return
  ENDIF
  file_delete, concat_dir(concat_dir(topdir, 'level0'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE
  file_delete, concat_dir(concat_dir(topdir, 'level1'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE
  file_delete, concat_dir(concat_dir(topdir, 'level2'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE

  ; tests with old fits files (no STARTOBS keyword in header)

  message, 'L0 - single file move in new directory', /info
  file = spice_ingest_test_create_fits_file(111333001, 0, '1976-01-17T10:23:33', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L0 - single file move in new directory, but other data exist already', /info
  file = spice_ingest_test_create_fits_file(111333002, 0, '1976-01-17T10:53:11', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L0 - single file move, spiobsid exists already, with newer date', /info
  file = spice_ingest_test_create_fits_file(111333002, 0, '1976-01-17T10:56:11', 5)
  spice_ingest, file, /debug
  ;stop

  message, 'L0 - single file move, spiobsid exists already, with older date', /info
  file = spice_ingest_test_create_fits_file(111333002, 0, '1976-01-17T10:36:11', 2)
  spice_ingest, file, /debug
  ;stop

  message, 'L1 - single file move in new directory', /info
  file = spice_ingest_test_create_fits_file(111333001, 1, '1976-01-17T10:23:33', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L1 - single file move in new directory, but other data exist already', /info
  file = spice_ingest_test_create_fits_file(111333002, 1, '1976-01-17T10:53:11', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L1 - single file move, spiobsid exists already, with newer date', /info
  file = spice_ingest_test_create_fits_file(111333002, 1, '1976-01-17T10:56:11', 5)
  spice_ingest, file, /debug
  ;stop

  message, 'L1 - single file move, spiobsid exists already, with older date', /info
  file = spice_ingest_test_create_fits_file(111333002, 1, '1976-01-17T10:36:11', 2)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move in new directory', /info
  file = spice_ingest_test_create_fits_file(111333001, 2, '1976-01-17T10:23:33', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move in new directory, but other data exist already', /info
  file = spice_ingest_test_create_fits_file(111333002, 2, '1976-01-17T10:53:11', 4)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, spiobsid exists already, with newer date', /info
  file = spice_ingest_test_create_fits_file(111333002, 2, '1976-01-17T10:56:11', 5)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, spiobsid exists already, with older date', /info
  file = spice_ingest_test_create_fits_file(111333002, 2, '1976-01-17T10:36:11', 2)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, spiobsid exists already, with older date, file exists already, no force', /info
  file = spice_ingest_test_create_fits_file(111333002, 2, '1976-01-17T10:36:11', 2)
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, spiobsid exists already, with older date, file exists already, with force', /info
  file = spice_ingest_test_create_fits_file(111333002, 2, '1976-01-17T10:36:11', 2)
  spice_ingest, file, /force, /debug
  ;stop

  message, 'L2 - multiple files move, of different spiobsid', /info
  file1 = spice_ingest_test_create_fits_file(111333003, 2, '1976-01-17T14:00:11', 1)
  file2 = spice_ingest_test_create_fits_file(111333004, 2, '1976-01-17T14:03:11', 2)
  file3 = spice_ingest_test_create_fits_file(111333005, 2, '1976-01-17T14:05:11', 3)
  spice_ingest, [file1, file2, file3], /debug
  ;stop

  message, 'L2 - multiple files move, of same spiobsid', /info
  file1 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:02:19', 2)
  file2 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:04:19', 4)
  file3 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:06:19', 6)
  spice_ingest, [file1, file2, file3], /debug
  ;stop

  message, 'L2 - multiple files move, of same spiobsid, exists already, newer date', /info
  file1 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:01:19', 1)
  file2 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:03:19', 3)
  file3 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:05:19', 5)
  spice_ingest, [file1, file2, file3], /debug
  ;stop

  message, 'L2 - multiple files move, of same spiobsid, exists already, older date', /info
  file1 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:07:19', 7)
  file2 = spice_ingest_test_create_fits_file(111333006, 2, '1976-01-17T15:08:19', 8)
  file3 = spice_ingest_test_create_fits_file(111333007, 2, '1976-01-17T16:05:19', 5)
  spice_ingest, [file1, file2, file3], /debug
  ;stop


  ; tests with new fits files (STARTOBS keyword in header)

  message, 'L2 - single file move, with STARTOBS in header, to new directory', /info
  file = spice_ingest_test_create_fits_file(111333008, 2, '1976-01-17T19:41:11', 2, startobs='1976-01-17T19:40:10')
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, with STARTOBS in header, to existsing directory, newer', /info
  file = spice_ingest_test_create_fits_file(111333008, 2, '1976-01-17T19:42:11', 3, startobs='1976-01-17T19:40:10')
  spice_ingest, file, /debug
  ;stop

  message, 'L2 - single file move, with STARTOBS in header, to existing directory, same', /info
  file = spice_ingest_test_create_fits_file(111333008, 2, '1976-01-17T19:40:11', 1, startobs='1976-01-17T19:40:10')
  spice_ingest, file, /debug
  stop

  file_delete, concat_dir(concat_dir(topdir, 'level0'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE
  file_delete, concat_dir(concat_dir(topdir, 'level1'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE
  file_delete, concat_dir(concat_dir(topdir, 'level2'), '1976'), /ALLOW_NONEXISTENT, /RECURSIVE

end
