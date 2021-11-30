;+
; NAME:
;      SPICE_ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This function returns an array of fits headers made from an ANA object or file.
;
; CATEGORY:
;      XXX -- utility
;
; CALLING SEQUENCE:
;      headers = spice_ana2fitshdr_results(file)
;
; INPUTS:
;      file: The name and path of an ANA file or an ANA object
;
; OUTPUTS:
;      array of fits headers
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2021-11-30 14:34 CET $


FUNCTION spice_ana2fitshdr_results, ana, header_l2=header_l2, $
  window_index=window_index, filename_l3=filename_l3, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, DATA=DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME=FILENAME, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  EXTENSION=EXTENSION

  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
    end

    8: begin
      handle_value,ana.history_h,history,/no_copy
      handle_value,ana.lambda_h,lambda,/no_copy
      handle_value,ana.data_h,data,/no_copy
      handle_value,ana.weights_h,weights,/no_copy
      handle_value,ana.fit_h,fit,/no_copy
      handle_value,ana.result_h,result,/no_copy
      handle_value,ana.residual_h,residual,/no_copy
      handle_value,ana.include_h,include,/no_copy
      handle_value,ana.const_h,const,/no_copy
      handle_value,ana.origin_h,origin,/no_copy
      handle_value,ana.scale_h,scale,/no_copy
      handle_value,ana.phys_scale_h,phys_scale,/no_copy
      handle_value,ana.dimnames_h,dimnames,/no_copy
      filename = ana.filename
      datasource = ana.datasource
      definition = ana.definition
      missing = ana.missing
      label = ana.label
    end

    0: begin
    end

    else: begin
      print, 'wrong input'
      return, -1
    end
  endcase

  n_dims = size(result, /n_dimensions)


  ; ------
  ; Create result header
  ; ------

  fits_util = obj_new('oslo_fits_util')
  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  ; Add time to DATE
  caldat, systime(/julian), month, day, year, hour, minute, second
  datetime = {CDS_EXT_TIME, $
    year:year, $
    month:month, $
    day:day, $
    hour:hour, $
    minute:minute, $
    second:second, $
    millisecond:0}
  datetime = anytim(datetime, /ccsds)
  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  filename_l2 = fxpar(header_l2, 'FILENAME', missing='')
  file_info_l2 = spice_file2info(filename_l2)
  obs_def = strtrim(string(file_info_l2.spiobsid), 2) + $
    fns('-###', file_info_l2.rasterno) + $
    fns('_##', window_index)
  fits_util->add, hdr, 'EXTNAME', 'Results of ANA for OBS ' + obs_def, 'Extension name'

  filename_l3 = filename_l2.replace('_L2_', '_L3_')
  fits_util->add, hdr, 'FILENAME', filename_l3, 'Filename of this FITS file'

  extname_l2 = fxpar(header_l2, 'EXTNAME', missing='')
  fits_util->add, hdr, 'EXTNAML2', extname_l2, 'Extension name in level 2 file'
  fits_util->add, hdr, 'FILENAL2', filename_l2, 'Level 2 filename'

  ; Add keywords valid for whole ANA
  fits_util->add_description, hdr, 'Keywords describing the whole ANA'
  fits_util->add, hdr, 'ANAFILE', filename, 'ANA filename'
  fits_util->add, hdr, 'ANADSRC', datasource, 'ANA datasource'
  fits_util->add, hdr, 'ANADEF', definition, 'ANA definition'
  fits_util->add, hdr, 'ANAMISS', missing, 'ANA missing value in fitted data'
  fits_util->add, hdr, 'ANALABEL', label, 'ANA label'
  ind = where(history NE '', count)
  if count gt 0 then history_string = strjoin(history[ind], ';') $
  else history_string = ''
  fits_util->add, hdr, 'ANAHISTO', history_string, 'ANA history'
  n_components = N_TAGS(fit)
  fits_util->add, hdr, 'COMPCNT', n_components, 'Number of fit components'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA for OBS '+obs_def, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA for OBS '+obs_def, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA for OBS '+obs_def, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA for OBS '+obs_def, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA for OBS '+obs_def, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA for OBS '+obs_def, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA for OBS '+obs_def, 'Extension name of constants'


  for itag=0,n_components-1 do begin
    ; Add keywords for each fit component
    fitnr = fns('##', itag)
    fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
    fit_cur = fit.(itag)
    fits_util->add, hdr, 'CMPTYP'+fitnr, fit_cur.FUNC_NAME, 'Type of fit component '+fitnr
    fits_util->add, hdr, 'CMPNAM'+fitnr, fit_cur.NAME, 'Name of fit component '+fitnr
    ind = where(fit_cur.description NE '', count)
    if count gt 0 then description = strjoin(fit_cur.description[ind], ';') $
    else description = ''
    fits_util->add, hdr, 'CMPDES'+fitnr, description, 'Description of fit component '+fitnr
    fits_util->add, hdr, 'CMPMUL'+fitnr, fit_cur.MULTIPLICATIVE, 'Indicates whether component is multiplicative'
    fits_util->add, hdr, 'CMPINC'+fitnr, fit_cur.INCLUDE, 'Indicates whether component is included in fit'
    n_params = N_ELEMENTS(fit_cur.param)
    fits_util->add, hdr, 'PARCNT'+fitnr, n_params, 'Number of parameters in fit component '+fitnr

    for ipar=0,n_params-1 do begin
      ; Add keywords for each fit parameter
      param = fit_cur.param[ipar]
      parnr = string(byte(ipar+97))
      fits_util->add, hdr, 'PRNAM'+fitnr+parnr, param.name, 'Name of parameter '+parnr+' for component '+fitnr
      ind = where(param.description NE '', count)
      if count gt 0 then description = strjoin(param.description[ind], ';') $
      else description = ''
      fits_util->add, hdr, 'PRDES'+fitnr+parnr, description, 'Description of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PRINI'+fitnr+parnr, param.initial, 'Initial value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PRVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PRMAX'+fitnr+parnr, param.max_val, 'Maximum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PRMIN'+fitnr+parnr, param.min_val, 'Minimum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PRTRA'+fitnr+parnr, param.trans_a, 'Linear coefficient in Lambda=PRVAL*PRTRA+PRTRB'
      fits_util->add, hdr, 'PRTRB'+fitnr+parnr, param.trans_b, 'Linear offset in Lambda=PRVAL*PRTRA+PRTRB'
      fits_util->add, hdr, 'PRCON'+fitnr+parnr, param.const, 'Indicates whether parameter is constant'
    endfor ; ipar0,n_params-1
  endfor ; itag=0,N_TAGS(fit)-1

  ; Add keywords for Chi^2
  fitnr = fns('##', N_TAGS(fit))
  fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
  fits_util->add, hdr, 'CMPTYP'+fitnr, 'Error of fit curve (Chi^2)', 'Type of component '+fitnr
  fits_util->add, hdr, 'CMPNAM'+fitnr, 'Chi^2', 'Name of component '+fitnr
  fits_util->add, hdr, 'CMPCNT'+fitnr, 1, 'Number of parameters in component '+fitnr

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
  fits_util->add, hdr, 'CDELT1', 0.0, '[] Increment of 1st coord at ref point'
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
  fits_util->add, hdr, 'CUNIT3', fxpar(header_l2, 'CUNIT2', missing=''), 'Units for 3rd coordinate (for CRVAL2, CDELT2)'
  fits_util->add, hdr, 'CRVAL3', fxpar(header_l2, 'CRVAL2', missing=0), '[arcsec] 3rd coordinate of reference point'
  fits_util->add, hdr, 'CDELT3', fxpar(header_l2, 'CDELT2', missing=0), '[arcsec] Increment of 3rd coord at ref point'
  fits_util->add, hdr, 'CRPIX3', fxpar(header_l2, 'CRPIX2', missing=0), '[pixel] 3rd pixel index of reference point '
  fits_util->add, hdr, 'PC3_2', fxpar(header_l2, 'PC2_1', missing=0), 'Contribution of dim 2 to coord 3 due to roll', before='PC3_3'
  fits_util->add, hdr, 'PC3_3', fxpar(header_l2, 'PC2_2', missing=0), 'Non-default value due to CROTA degrees S/C roll'

  pc4_1 = fxpar(header_l2, 'PC4_1', missing=-9999)
  if pc4_1 gt -9998 then begin
    fits_util->add, hdr, 'PC4_2', fxpar(header_l2, 'PC4_1', missing=0), 'Contribution of dim 2 to coord 4 due to roll', atfter='PC4_4'
  endif

  fits_util->add, hdr, 'BTYPE', ' '
  fits_util->add, hdr, 'UCD', ' '
  fits_util->add, hdr, 'BUNIT', ' '

  fits_util->clean_header, hdr

  print,''
  print,'--- results ---'
  print,''
  print,hdr

  return, hdr
end
