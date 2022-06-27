;+
; NAME:
;      SPICE_ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This function returns a fits header made from the Results of an ANA object or file.
;      The fits header contains all fit components as keywords and the original
;      level 2 header keywords. The WCS keywords are adapted to the result
;      cube given by ANA.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      header = spice_ana2fitshdr_results(header_l2=header_l2, datetime=datetime, $
;        filename_l3=filename_l3, filename_l2=filename_l2, exension_names=exension_names, n_windows=n_windows, $
;        winno=winno, EXTENSION=EXTENSION, $
;        HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME, $
;        DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
;
; INPUTS:
;      header_l2: The header (string array) of the level 2 file.
;      datetime: Date and time string.
;      filename_l3: The filename the level 3 will/should get
;      filename_l2: The filename of the level 2 file
;      prefix_extension_name: A string defining the prefix to the names of the 7 extensions
;      n_windows: number of windows to be included in level 3 file.
;      HISTORY: A string array.
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;      FILENAME_ANA: The filename of the ANA-file.
;      DATASOURCE: A string.
;      DEFINITION: A string.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been
;               declared as "FAILED".
;      LABEL: A string.
;      winno: Window number (starting at 0) within this study in this level 3 file
; 
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the level 3 file.
;                 If not set, this will be the primary header.
; 
; OPTIONAL INPUTS:
;
; OUTPUTS:
;      a fits header (string array)
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2022-06-27 13:35 CEST $


FUNCTION spice_ana2fitshdr_results, header_l2=header_l2, datetime=datetime, $
  filename_l3=filename_l3, filename_l2=filename_l2, prefix_extension_name=prefix_extension_name, n_windows=n_windows, $
  winno=winno, EXTENSION=EXTENSION, $
  HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME_ANA, $
  DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL

stop
  fits_util = obj_new('oslo_fits_util')
  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', prefix_extension_name+'results', 'Extension name'
  fits_util->add, hdr, 'FILENAME', filename_l3, 'Filename of this FITS file'

  extname_l2 = fxpar(header_l2, 'EXTNAME', missing='')
  fits_util->add, hdr, 'L2EXTNAM', extname_l2, 'Extension name in level 2 file'
  fits_util->add, hdr, 'L2FILENA', filename_l2, 'Level 2 filename'

  ; Add keywords valid for whole ANA
  fits_util->add_description, hdr, 'Keywords describing the whole ANA'
  fits_util->add, hdr, 'ANA_FILE', filename_ana, 'ANA filename'
  fits_util->add, hdr, 'ANA_SRC', datasource, 'ANA datasource'
  fits_util->add, hdr, 'ANA_DEF', definition, 'ANA definition'
  fits_util->add, hdr, 'ANA_MISS', missing, 'ANA missing value in fitted data'
  fits_util->add, hdr, 'ANA_LABL', label, 'ANA label'
  ind = where(history NE '', count)
  if count gt 0 then history_string = strjoin(history[ind], ';') $
  else history_string = ''
  fits_util->add, hdr, 'ANA_HIST', history_string, 'ANA history'
  n_components = N_TAGS(fit)
  fits_util->add, hdr, 'ANA_NCMP', n_components, 'Number of fit components'

  fits_util->add, hdr, 'RESEXT', prefix_extension_name+'results', 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', prefix_extension_name+'data', 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', prefix_extension_name+'lambda', 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', prefix_extension_name+'residuals', 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', prefix_extension_name+'weights', 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', prefix_extension_name+'includes', 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', prefix_extension_name+'constants', 'Extension name of constants'

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'NXDIM', 1, 'Number of dimensions absorbed by analysis'
  fits_util->add, hdr, 'XDIMTY1', fxpar(header_l2, 'CTYPE1', missing=''), 'Type of 1st dimension absorbed by analysis'
  fits_util->add, hdr, 'XDIMEX1', prefix_extension_name+'lambda', 'Extension name of 1st dimension absorbed by analysis'

  for itag=0,n_components-1 do begin
    ; Add keywords for each fit component
    fitnr = strtrim(string(itag+1), 2)
    fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
    fit_cur = fit.(itag)
    fits_util->add, hdr, 'CMPTYP'+fitnr, fit_cur.FUNC_NAME, 'Type of fit component '+fitnr
    fits_util->add, hdr, 'CMPNAM'+fitnr, fit_cur.NAME, 'Name of fit component '+fitnr
    fits_util->add, hdr, 'CMPSTR'+fitnr, fit_cur.FUNC_STRING, 'Function string of fit component '+fitnr
    ind = where(fit_cur.description NE '', count)
    if count gt 0 then description = strjoin(fit_cur.description[ind], ';') $
    else description = ''
    fits_util->add, hdr, 'CMPDES'+fitnr, description, 'Description of fit component '+fitnr
    fits_util->add, hdr, 'CMPMUL'+fitnr, fit_cur.MULTIPLICATIVE, 'Indicates whether component is multiplicative'
    fits_util->add, hdr, 'CMPINC'+fitnr, fit_cur.INCLUDE, 'Indicates whether component is included in fit'
    n_params = N_ELEMENTS(fit_cur.param)
    fits_util->add, hdr, 'CMP_NP'+fitnr, n_params, 'Number of parameters in fit component '+fitnr

    for ipar=0,n_params-1 do begin
      ; Add keywords for each fit parameter
      param = fit_cur.param[ipar]
      parnr = string(byte(ipar+97))
      fits_util->add, hdr, 'PNAME'+fitnr+parnr, param.name, 'Name of parameter '+parnr+' for component '+fitnr
      ind = where(param.description NE '', count)
      if count gt 0 then description = strjoin(param.description[ind], ';') $
      else description = ''
      fits_util->add, hdr, 'PDESC'+fitnr+parnr, description, 'Description of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PINIT'+fitnr+parnr, param.initial, 'Initial value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PMAX'+fitnr+parnr, param.max_val, 'Maximum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PMIN'+fitnr+parnr, param.min_val, 'Minimum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PTRNA'+fitnr+parnr, param.trans_a, 'Linear coefficient A in Lambda=PVAL*PTRNA+PTRNB'
      fits_util->add, hdr, 'PTRNB'+fitnr+parnr, param.trans_b, 'Linear coefficient B in Lambda=PVAL*PTRNA+PTRNB'
      fits_util->add, hdr, 'PCONS'+fitnr+parnr, param.const, 'Indicates whether parameter '+parnr+' for component '+fitnr+'is constant'
    endfor ; ipar0,n_params-1
  endfor ; itag=0,N_TAGS(fit)-1

  ; Add keywords for Chi^2
  fitnr = strtrim(string(n_components+1), 2)
  fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
  fits_util->add, hdr, 'CMPTYP'+fitnr, 'Error of fit curve (Chi^2)', 'Type of component '+fitnr
  fits_util->add, hdr, 'CMPNAM'+fitnr, 'Chi^2', 'Name of component '+fitnr
  fits_util->add, hdr, 'CMP_NP'+fitnr, 1, 'Number of parameters in component '+fitnr
  ipar=0
  parnr = string(byte(ipar+97))
  fits_util->add, hdr, 'PNAME'+fitnr+parnr, 'Chi^2', 'Name of parameter '+parnr+' for component '+fitnr

  ; Add level 2 header to this header
  ind_start = where(strmatch(header_l2, '*Study parameters valid for all Obs-HDUs in this file*') eq 1, count_l2)
  if count_l2 eq 1 then begin
    ind_end = where(strmatch(hdr, 'END *') eq 1, count_l3)
    if count_l3 ne 1 then begin
      print, 'hm, no END'
      stop
    endif
    hdr = [hdr[0:ind_end-1], header_l2[ind_start-3:*]]
  endif

  ; Add WCS keywords
  fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
  fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
  fits_util->add, hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
  fits_util->add, hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
  fits_util->add, hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
  fits_util->add, hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'
  fits_util->add, hdr, 'PC1_1', 1.0, 'Default value, no rotation'
  sxdelpar, hdr, 'PC1_2'

  fits_util->add, hdr, 'CTYPE2', fxpar(header_l2, 'CTYPE1', missing=''), 'Type of 2nd coordinate'
  fits_util->add, hdr, 'CNAME2', fxpar(header_l2, 'CNAME1', missing=''), 'Name of 2nd coordinate'
  fits_util->add, hdr, 'CUNIT2', fxpar(header_l2, 'CUNIT1', missing=''), 'Units for 2nd coordinate (for CRVAL2, CDELT2)'
  fits_util->add, hdr, 'CRVAL2', fxpar(header_l2, 'CRVAL1', missing=0), '[arcsec] 2nd coordinate of reference point'
  fits_util->add, hdr, 'CDELT2', fxpar(header_l2, 'CDELT1', missing=0), '[arcsec] Increment of 2nd coord at ref point'
  fits_util->add, hdr, 'CRPIX2', fxpar(header_l2, 'CRPIX1', missing=0), '[pixel] 2nd pixel index of reference point '
  fits_util->add, hdr, 'PC2_2', fxpar(header_l2, 'PC1_1', missing=0), 'Non-default value due to CROTA degrees S/C roll'
  fits_util->add, hdr, 'PC2_3', fxpar(header_l2, 'PC1_2', missing=0), 'Contribution of dim 3 to coord 2 due to roll', after='PC2_2'
  sxdelpar, hdr, 'PC2_1'

  fits_util->add, hdr, 'CTYPE3', fxpar(header_l2, 'CTYPE2', missing=''), 'Type of 3rd coordinate'
  fits_util->add, hdr, 'CNAME3', fxpar(header_l2, 'CNAME2', missing=''), 'Name of 3rd coordinate'
  fits_util->add, hdr, 'CUNIT3', fxpar(header_l2, 'CUNIT2', missing=''), 'Units for 3rd coordinate (for CRVAL3, CDELT3)'
  fits_util->add, hdr, 'CRVAL3', fxpar(header_l2, 'CRVAL2', missing=0), '[arcsec] 3rd coordinate of reference point'
  fits_util->add, hdr, 'CDELT3', fxpar(header_l2, 'CDELT2', missing=0), '[arcsec] Increment of 3rd coord at ref point'
  fits_util->add, hdr, 'CRPIX3', fxpar(header_l2, 'CRPIX2', missing=0), '[pixel] 3rd pixel index of reference point '
  fits_util->add, hdr, 'PC3_2', fxpar(header_l2, 'PC2_1', missing=0), 'Contribution of dim 2 to coord 3 due to roll', before='PC3_3'
  fits_util->add, hdr, 'PC3_3', fxpar(header_l2, 'PC2_2', missing=0), 'Non-default value due to CROTA degrees S/C roll'

  pc4_1 = fxpar(header_l2, 'PC4_1', missing=-9999)
  if pc4_1 gt -9998 then begin
    fits_util->add, hdr, 'PC4_2', fxpar(header_l2, 'PC4_1', missing=0), 'Contribution of dim 2 to coord 4 due to roll', after='PC4_4'
  endif

  fits_util->add, hdr, 'BTYPE', ' '
  fits_util->add, hdr, 'UCD', ' '
  fits_util->add, hdr, 'BUNIT', ' '

  fits_util->add, hdr, 'NWIN', n_windows, 'Number of windows'
  fits_util->add, hdr, 'L2NWIN', fxpar(header_l2, 'NWIN', missing=0), 'Total number of windows (incl. db and int win) in level 2 file', after='NWIN'
  fits_util->add, hdr, 'WINNO', winno, 'Window number (starting at 0) within this study in this level 3 file'
  fits_util->add, hdr, 'L2WINNO', fxpar(header_l2, 'WINNO', missing=0), 'Window number (starting at 0) within this study in level 2 file', after='WINNO'

  fits_util->clean_header, hdr

  return, hdr
end
