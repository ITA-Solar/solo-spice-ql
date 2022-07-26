;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This function returns an array of FITS headers made from an ANA object or file.
;      The fits headers contains all fit components as keywords.
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;      headers = ana2fitshdr(ana, n_windows=n_windows, filename_out=filename_out, $
;         winno=winno, /EXTENSION, $
;         HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;         FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;         CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;         DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL] )
;
; INPUTS:
;      ana: The name and path of an ANA file or an ANA object.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      n_windows: Total number of windows to be included in FITS file.
;      filename_out: Full path and filename of the resulting FITS file.
;      winno: Window number (starting at 0) within this file
;
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the level 3 file.
;                 If not set, this will be the primary header.
;
; OPTIONAL INPUTS/OUTPUTS:
;      All of the following optional inputs must be provided if 'ana' is not
;      provided. If 'ana' is provided, they will be overwritten and can be used
;      as output.
;      HISTORY: A string array.
;      LAMBDA: An array of wavelength values. Either one value for
;              every point in the data array, or a one-dimensional
;              array to go with all the spectra in the data array.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with spectra
;            along the first dimension.
;      WEIGHTS: Weights to use in the fitting process. No default!
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;      RESIDUAL: Array to contain the residual. Same size as DATA, may be
;                undefined on input.
;      INCLUDE: Array to keep the INCLUDE status of each component
;               at each point.
;      CONST: Array to keep the CONST status of each parameter at
;             each point.
;      FILENAME_ANA: The filename of the ANA-file.
;      DATASOURCE: A string.
;      DEFINITION: A string.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been
;               declared as "FAILED".
;      LABEL: A string.
;
; OUTPUTS:
;      a pointer array, containing 7 FITS keyword headers
;
; HISTORY:
;      Ver. 1, 28-Sep-2021, Martin Wiesmann
;-
; $Id: 2022-06-27 13:35 CEST $


FUNCTION ana2fitshdr, ana, n_windows=n_windows, filename_out=filename_out, winno=winno, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  EXTENSION=EXTENSION

  print_headers = 0
  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
    end

    8: begin
      handle_value,ana.history_h,history,/no_copy
      handle_value,ana.lambda_h,lambda,/no_copy
      handle_value,ana.data_h,input_data,/no_copy
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
      filename_ana = ana.filename
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

  all_headers = ptrarr(7)
  n_dims = size(result, /n_dimensions)
  fits_util = obj_new('oslo_fits_util')


  ; ------
  ; Create result header
  ; ------

  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  if filename_ana eq '' then postfix = fns('##########', randomu(seed, /long)) $
  else postfix = filename_ana
  fits_util->add, hdr, 'EXTNAME', 'Results of ANA ' + postfix, 'Extension name'
  fits_util->add, hdr, 'FILENAME', filename_out, 'Filename of this FITS file'

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

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'NXDIM', 1, 'Number of dimensions absorbed by analysis'
  fits_util->add, hdr, 'XDIMTY1', 'Original type of absorbed dimension', 'Type of 1st dimension absorbed by analysis'
  fits_util->add, hdr, 'XDIMEX1', 'Lambda of ANA '+postfix, 'Extension name of 1st dimension absorbed by analysis'

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

  ; Add WCS keywords
  fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
  fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->add, hdr, 'NWIN', n_windows, 'Number of windows'
  fits_util->add, hdr, 'WINNO', winno, 'Window number (starting at 0) within this file'

  fits_util->clean_header, hdr
  all_headers[0] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- results ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create data header
  ; ------

  mkhdr, hdr, INPUT_DATA, /image

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Data input to ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[1] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- data ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create lambda header
  ; ------

  mkhdr, hdr, lambda, /image

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Lambda of ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[2] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- lambda ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create residual header
  ; ------

  mkhdr, hdr, residual, /image
  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Residuals of ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[3] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- residuals ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create weights header
  ; ------

  mkhdr, hdr, weights, /image
  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Weights of ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[4] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- weights ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create include header
  ; ------

  mkhdr, hdr, include, /image
  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Weights of ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  fits_util->add, hdr, 'CTYPE1', 'FIT COMPONENT', 'Type of 1st coordinate'
  fits_util->add, hdr, 'CNAME1', 'Component', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[5] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- include ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create const header
  ; ------

  mkhdr, hdr, const, /image
  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'EXTNAME', 'Constants of ANA '+postfix, 'Extension name'

  fits_util->add, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fits_util->add, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
  fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  fits_util->clean_header, hdr
  all_headers[6] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- const ---'
    print,''
    print,hdr
  endif


  return, all_headers
END
