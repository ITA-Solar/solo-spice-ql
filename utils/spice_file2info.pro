;+
; NAME:
;      SPICE_FILE2INFO
;
; PURPOSE:
;      This routine takes as input a SPICE filename (or list of files)
;      and extracts information from the filename.
;
; CATEGORY:
;      SPICE -- file management.
;
; CALLING SEQUENCE:
;      info = SPICE_FILE2INFO, file
;
; INPUTS:
;      file: The name of a SPICE file. Can be an array of names.
;
; OUTPUTS:
;      Either a structure or an array of structures. The structure
;      contains the extracted information.
;      info = { $
;         is_spice_file: byte,  1 if the file is a spice file, 0 otherwise
;         filename:      str,   name of the file, without the path
;         level:         int,   data level (0, 1 or 2), -1 if unknown
;         study_type:    str,   type of study
;         sat_time:      str,   internal satellite time of observation, 
;                               only available for level 0
;         datetime:      str,   date and time in CCSDS format of observation, 
;                               for level 0, this is the time the file was downlinked
;         version:       int,   version number (version of the spice data pipelin),
;                               not available for level 0
;         spiobsid:      long,  SPICE OBS ID
;         rasterno:      int,   raster repetition number
;       }
;
; HISTORY:
;      Ver. 1, 17-Jun-2020, Martin Wiesmann
;-
; $Id: 26.10.2020 11:10 CET $


FUNCTION spice_file2info, file

  ; examples
  ; solo_L0_spice-n-sit_0639070018_V202004012012C_12583201-000.fits
  ; solo_L1_spice-n-ras-db-int_20200603T061613824_V01_12583776-000.fits
  ; solo_L2_spice-n-ras-db-int_20200603T061613824_V01_12583776-000.fits

  nfile = N_ELEMENTS(file)

  info_template = {is_spice_file:0B, $
    filename:'', $
    level:-1, $
    study_type:'', $
    sat_time:'', $
    datetime:'', $
    version:-1, $
    spiobsid:-1L, $
    rasterno:-1}

  FOR ifile=0,nfile-1 DO BEGIN
    info_temp = info_template

    fname0 = file_basename(file[ifile])
    fname = strsplit(fname0, '_', /extract)

    ;first check whether this is a spice file
    IF fname[0] EQ 'solo' && strmatch(fname[2], 'spice-*') THEN BEGIN

      info_temp.is_spice_file = 1
      
      info_temp.filename = fname0

      CASE fname[1] OF
        'L0': info_temp.level=0
        'L1': info_temp.level=1
        'L2': info_temp.level=2
        ELSE: BEGIN
          message, 'Cannot determine level of data in file: ' + file[ifile], /info
        END
      ENDCASE

      info_temp.study_type = strmid(fname[2], 6)

      IF info_temp.level EQ 0 THEN BEGIN
        info_temp.sat_time = fname[3]
        obs_date = strmid(fname[4], 1, 8)
        obs_time = strmid(fname[4], 9, 4)
        obs_ms = '0'
      ENDIF ELSE BEGIN ; info_temp.level EQ 0
        info_temp.version = fix(strmid(fname[4], 1))
        obs_date = strmid(fname[3], 0, 8)
        obs_time = strmid(fname[3], 9, 6)
        obs_ms = strmid(fname[3], 15, 3)
      ENDELSE ; info_temp.level EQ 0
      obs_datetime = obs_date+'_'+obs_time
      obs = fid2time('_'+obs_datetime)
      obs = anytim2utc(obs)
      obs.time = obs.time + fix(obs_ms)
      info_temp.datetime = anytim2utc(obs, /CCSDS)

      temp = strsplit(fname[5], '-.', /extract)
      info_temp.spiobsid = long(temp[0])
      info_temp.rasterno = fix(temp[1])

    ENDIF ELSE BEGIN ; fname[0] EQ 'solo' && strmatch(fname[2], 'spice-*')
      message, 'This is not a spice file: ' + file[ifile], /info
    ENDELSE ; fname[0] EQ 'solo' && strmatch(fname[2], 'spice-*')

    IF ifile EQ 0 THEN info = info_temp $
    ELSE info = [info, info_temp]

  ENDFOR ; ifile=0,N_ELEMENTS(file)-1

  return, info
END
