;+
; NAME:
;     get_last_prstep_keyword
;
; PURPOSE:
;     get_last_prstep_keyword searches a FITS keyword header for all processing step keywords defined in the 
;     Solarnet_Metadata_Recommendations document.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     max_version_number = get_last_prstep_keyword(header [, count=count] [, pr_keywords=pr_keywords] [, $
;       pr_keywords_ind=pr_keywords_ind] [, pr_versions=pr_versions] )
;
; INPUTS:
;     header : A FITS keyword header as a string array
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;     An integer, this is the highest processing step keyword found in the header.
;
; OPTIONAL OUTPUT:
;     count: An integer, the number of processing step keywords found.
;     pr_keywords: A string array, all processing step keywords found.
;     pr_keywords_ind: An integer array, indices of found processing step keywords in the header.
;     pr_versions: An integer array, version numbers of the found processing step keywords.
;               Note: Some keywords have letters after the version number.
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     22-Jun-2023: Martin Wiesmann
;-
; $Id: 2023-06-22 11:52 CEST $


FUNCTION get_last_prstep_keyword, header, count=count, pr_keywords=pr_keywords, pr_keywords_ind=pr_keywords_ind, $
  pr_versions=pr_versions
  compile_opt idl2

;  f='/Users/mawiesma/data/spice/user/level3/2023/04/04/solo_L3_spice-n-ras_20230404T040302_V15_184549660-000.fits'
;  d=readfits(f, header)
  prits_tools.parcheck, header, 1, "header", 'string', 1

  pr_keywords = header.extract('^PR(STEP|PROC|PVER|MODE|PARA|REF|LOG|ENV|VER|HSH|BRA|LIB)[1-9][0-9]{0,1}[^ =]?')
  pr_keywords_ind = where(pr_keywords NE '', count)
  if count eq 0 then begin
    pr_keywords = ''
    return, 0
  endif
  pr_keywords = pr_keywords[pr_keywords_ind]

  pr_versions = fix(pr_keywords.extract('[1-9][0-9]{0,1}'))
  pr_version_max = max(pr_versions)
  
  return, pr_version_max

;  help,pr
;  print,pr
;  for i=0,n_elements(pr)-1 do begin
;    if pr[i] ne '' then begin
;      print,i,' ',pr[i],';'
;      print,i,' ',header[i]
;    endif
;  endfor

END


;from a SPICE level 2 FITS file:
;----------------------------------------
;| Keywords describing processing steps |
;----------------------------------------
;PRSTEP1 = 'COMPRESSION'        / Type of processing, step 1
;PRPROC1 = 'JPEG Compression (On-board)' / Name of procedure, step 1
;
;PRSTEP2 = 'TELEMETRY-PARSING'  / XML decoding, decompression if applicable, etc
;PRPROC2 = 'spice_process_telemetry.pro' / Name of procedure, step 2
;PRPVER2 = '03.06.00'           / Version of procedure, step 2
;PRLIB2A = 'uio-spice-pipeline' / Software library containing PRPROC2
;
;PRSTEP3 = 'PIXEL-LEVEL-OFFSET-SUBTRACTION' / Type of processing, step 3
;PRPROC3 = 'spice_l0_to_l1.pro' / Name of procedure, step 3
;PRPVER3 = '03.06.00'           / Version of procedure, step 3
;PRPARA3 =                  200 / Parameters for PRPROC3
;PRLIB3A = 'uio-spice-pipeline' / Software library containing PRPROC3
;
;PRSTEP4 = 'INSTRUMENT-Y-COORDINATE-CORRECTION' / Type of processing, step 4
;PRPROC4 = 'correct_spice_offset_relative_to_spacecraft' / Name of procedure, ste
;PRPVER4 = '1.0     '           / Version of procedure, step 4
;PRPARA4 = 'delta_instrument_x=-68.993702' / =T_GRAT*0.46-85.0, param for PRPROC4
;PRLIB4A = 'uio-spice-pipeline' / Software library containing PRPROC4
;
;PRSTEP5 = 'INSTRUMENT-X-COORDINATE-CORRECTION' / Type of processing, step 5
;PRPROC5 = 'correct_spice_offset_relative_to_spacecraft' / Name of procedure, ste
;PRPVER5 = '1.0     '           / Version of procedure, step 5
;PRPARA5 = 'delta_instrument_y=-72.4' / =constant, parameter for PRPROC5
;PRLIB5A = 'uio-spice-pipeline' / Software library containing PRPROC5
;
;PRSTEP6 = 'DARK-SUBTRACTION'   / Type of processing, step 6
;PRPROC6 = 'spice_prep_dark_offset_correction.pro' / Name of procedure, step 6
;PRPVER6 = '1.8     '           / Version of procedure, step 6
;PRPARA6 = 'SPIOBSID_of_dark=184549579                                         &'
;CONTINUE  'RASTERNO_of_dark=6' / Parameters for PRPROC6
;PRLIB6A = 'uio-spice-pipeline' / Software library containing PRPROC6
;
;PRSTEP7 = 'FLATFIELDING'       / Type of processing, step 7
;PRPROC7 = 'spice_prep_flat_field_correction.pro' / Name of procedure, step 7
;PRPVER7 = '1.3     '           / Version of procedure, step 7
;PRPARA7 = 'ground-calibration flat field' / Parameters for PRPROC7
;PRLIB7A = 'uio-spice-pipeline' / Software library containing PRPROC7
;
;PRSTEP8 = 'SPATIAL-SPECTRAL-DISTORTION-CORRECTION' / Type of processing, step 8
;PRPROC8 = 'spice_prep_distortion_correction.pro' / Name of procedure, step 8
;PRPVER8 = '2.2     '           / Version of procedure, step 8
;PRPARA8 = 'distortion correction matrix version: 2021-09-08' / Parameters for PR
;PRLIB8A = 'uio-spice-pipeline' / Software library containing PRPROC8
;
;PRSTEP9 = 'RADIOMETRIC-CALIBRATION' / Type of processing, step 9
;PRPROC9 = 'spice_prep_radiometric_calibration.pro' / Name of procedure, step 9
;PRPVER9 = '1.6     '           / Version of procedure, step 9
;PRPARA9 = 'based on comparison to QS SUMER spectrum' / Parameters for PRPROC9
;PRLIB9A = 'uio-spice-pipeline' / Software library containing PRPROC9
;
;PRSTEP10= 'WINDOW-CONCATENATION' / Type of processing, step 10
;PRPROC10= 'spice_prep.pro'     / Name of procedure, step 10
;PRPVER10= '        '           / Version of procedure, step 10
;PRPARA10= '   WINNOs_of_concatenated_windows=[0,1],                           &'
;CONTINUE  'WINTABIDs_of_concatenated_windows=[56,66],                         &'
;CONTINUE  ' MISOWINs_of_concatenated_windows=[2905,2906],                     &'
;CONTINUE  ' EXTNAMEs_of_concatenated_windows="O III 703 / Mg IX 706 - SH,O III&'
;CONTINUE  ' 703 / Mg IX 706 - LH"                      &' / Parameters for
;CONTINUE  '' / PRPROC10
;PRLIB10A= 'uio-spice-pipeline' / Software library containing PRPROC10
;
;
;-------------------------------------------------
;| Hardware and software processing environment  |
;-------------------------------------------------
;LONGSTRN= 'OGIP 1.0'           / The OGIP long string convention may be used.
;COMMENT This FITS file may contain long string keyword values that are
;COMMENT continued over multiple keywords.  This convention uses the  '&'
;COMMENT character at the end of a string which is then continued
;COMMENT on subsequent keywords whose name = 'CONTINUE'.
;PRENV2  = '  Kernel: Linux                                                    &'
;CONTINUE  '  Kernel release number: 3.10.0-1160.81.1.el7.x86_64               &'
;CONTINUE  '  Architecture: x86_64                                             &'
;CONTINUE  '  Host name: astro-sdc-fs.uio.no                                   &'
;CONTINUE  '  OS: Red Hat Enterprise Linux Server release 7.9 (Maipo)          &'
;CONTINUE  '  CPU: Intel(R) Xeon(R) CPU E5-2630L v4 @ 1.80GHz                  &'
;CONTINUE  '  IDL 8.8.2 (Apr 06 2022 (430118)), memory bits: 64, file offset bi&'
;CONTINUE  'ts: 64      ' / Hardware and software

