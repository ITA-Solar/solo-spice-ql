;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This function returns an array of fits headers made from an ANA object or file.
;
; CATEGORY:
;      XXX -- utility
;
; CALLING SEQUENCE:
;      headers = ana2fitshdr(file)
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
;      Ver. 1, 28-Sep-2021, Martin Wiesmann
;-
; $Id: 2021-11-23 15:28 CET $


FUNCTION ana2fitshdr, ana, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, DATA=DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME=FILENAME, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  EXTENSION=EXTENSION

  input_type = size(ana, /type)
  print,input_type
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

  all_headers = ptrarr(7)
  n_dims = size(result, /n_dimensions)


  ; ------
  ; Create result header
  ; ------

  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  if filename eq '' then postfix = fns('##########', randomu(seed, /long)) $
  else postfix = filename
  fxaddpar, hdr, 'EXTNAME', 'Results of ANA '+postfix, 'Extension name'

  ; Add keywords valid for whole ANA
  fxaddpar, hdr, 'ANAFILE', filename, 'ANA filename'
  fxaddpar, hdr, 'ANADSRC', datasource, 'ANA datasource'
  fxaddpar, hdr, 'ANADEF', definition, 'ANA definition'
  fxaddpar, hdr, 'ANAMISS', missing, 'ANA missing value in fitted data'
  fxaddpar, hdr, 'ANALABEL', label, 'ANA label'
  ind = where(history NE '', count)
  if count gt 0 then history_string = strjoin(history[ind], ';') $
  else history_string = ''
  fxaddpar, hdr, 'ANAHISTO', history_string, 'ANA history'

  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'


  for itag=0,N_TAGS(fit)-1 do begin
    ; Add keywords for each fit component
    fit_cur = fit.(itag)
    fitnr = fns('##', itag)
    fxaddpar, hdr, 'CMPTYP'+fitnr, fit_cur.FUNC_NAME, 'Type of fit component '+fitnr
    fxaddpar, hdr, 'CMPNAM'+fitnr, fit_cur.NAME, 'Name of fit component '+fitnr
    ind = where(fit_cur.description NE '', count)
    if count gt 0 then description = strjoin(fit_cur.description[ind], ';') $
    else description = ''
    fxaddpar, hdr, 'CMPDES'+fitnr, description, 'Description of fit component '+fitnr
    fxaddpar, hdr, 'CMPMUL'+fitnr, fit_cur.MULTIPLICATIVE, 'Indicates whether component is multiplicative'
    fxaddpar, hdr, 'CMPINC'+fitnr, fit_cur.INCLUDE, 'Indicates whether component is included in fit'
    n_params = N_ELEMENTS(fit_cur.param)
    fxaddpar, hdr, 'CMPCNT'+fitnr, n_params, 'Number of parameters in fit component '+fitnr

    for ipar=0,n_params-1 do begin
      ; Add keywords for each fit parameter
      param = fit_cur.param[ipar]
      parnr = string(byte(ipar+97))
      fxaddpar, hdr, 'PRNAM'+fitnr+parnr, param.name, 'Name of parameter '+parnr+' for component '+fitnr
      ind = where(param.description NE '', count)
      if count gt 0 then description = strjoin(param.description[ind], ';') $
      else description = ''
      fxaddpar, hdr, 'PRDES'+fitnr+parnr, description, 'Description of parameter '+parnr+' for component '+fitnr
      fxaddpar, hdr, 'PRINI'+fitnr+parnr, param.initial, 'Initial value of parameter '+parnr+' for component '+fitnr
      fxaddpar, hdr, 'PRVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
      fxaddpar, hdr, 'PRMAX'+fitnr+parnr, param.max_val, 'Maximum value of parameter '+parnr+' for component '+fitnr
      fxaddpar, hdr, 'PRMIN'+fitnr+parnr, param.min_val, 'Minimum value of parameter '+parnr+' for component '+fitnr
      fxaddpar, hdr, 'PRTRA'+fitnr+parnr, param.trans_a, 'Linear coefficient in Lambda=PRVAL*PRTRA+PRTRB'
      fxaddpar, hdr, 'PRTRB'+fitnr+parnr, param.trans_b, 'Linear offset in Lambda=PRVAL*PRTRA+PRTRB'
      fxaddpar, hdr, 'PRCON'+fitnr+parnr, param.const, 'Indicates whether parameter is constant'
    endfor ; ipar0,n_params-1
  endfor ; itag=0,N_TAGS(fit)-1

  ; Add keywords for Chi^2
  fitnr = fns('##', N_TAGS(fit))
  fxaddpar, hdr, 'CMPTYP'+fitnr, 'Error of fit curve (Chi^2)', 'Type of component '+fitnr
  fxaddpar, hdr, 'CMPNAM'+fitnr, 'Chi^2', 'Name of component '+fitnr
  fxaddpar, hdr, 'CMPCNT'+fitnr, 1, 'Number of parameters in component '+fitnr

  ; Add WCS keywords
  fxaddpar, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
  fxaddpar, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[0] = ptr_new(hdr)

  print,''
  print,'--- results ---'
  print,''
  print,hdr


  ; ------
  ; Create data header
  ; ------

  mkhdr, hdr, data, /image
  fxaddpar, hdr, 'EXTNAME', 'Data input to ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[1] = ptr_new(hdr)

  print,''
  print,'--- data ---'
  print,''
  print,hdr


  ; ------
  ; Create lambda header
  ; ------

  mkhdr, hdr, lambda, /image
  fxaddpar, hdr, 'EXTNAME', 'Lambda of ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[2] = ptr_new(hdr)

  print,''
  print,'--- lambda ---'
  print,''
  print,hdr


  ; ------
  ; Create residual header
  ; ------

  mkhdr, hdr, residual, /image
  fxaddpar, hdr, 'EXTNAME', 'Residuals of ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[3] = ptr_new(hdr)

  print,''
  print,'--- residuals ---'
  print,''
  print,hdr


  ; ------
  ; Create weights header
  ; ------

  mkhdr, hdr, weights, /image
  fxaddpar, hdr, 'EXTNAME', 'Weights of ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  for idim=0,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      0: dim_name = '1st'
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[4] = ptr_new(hdr)

  print,''
  print,'--- weights ---'
  print,''
  print,hdr


  ; ------
  ; Create include header
  ; ------

  mkhdr, hdr, include, /image
  fxaddpar, hdr, 'EXTNAME', 'Includes of ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  fxaddpar, hdr, 'CTYPE1', 'FIT COMPONENT', 'Type of 1st coordinate'
  fxaddpar, hdr, 'CNAME1', 'Component', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[5] = ptr_new(hdr)

  print,''
  print,'--- include ---'
  print,''
  print,hdr


  ; ------
  ; Create const header
  ; ------

  mkhdr, hdr, const, /image
  fxaddpar, hdr, 'EXTNAME', 'Constants of ANA '+postfix, 'Extension name'
  fxaddpar, hdr, 'RESEXT', 'Results of ANA '+postfix, 'Extension name of results'
  fxaddpar, hdr, 'DATAEXT', 'Data input to ANA '+postfix, 'Extension name of data'
  fxaddpar, hdr, 'LAMBDEXT', 'Lambda of ANA '+postfix, 'Extension name of lambda'
  fxaddpar, hdr, 'RESIDEXT', 'Residuals of ANA '+postfix, 'Extension name of residuals'
  fxaddpar, hdr, 'WGTEXT', 'Weights of ANA '+postfix, 'Extension name of weights'
  fxaddpar, hdr, 'INCLEXT', 'Includes of ANA '+postfix, 'Extension name of includes'
  fxaddpar, hdr, 'CONSTEXT', 'Constants of ANA '+postfix, 'Extension name of constants'

  ; Add WCS keywords
  fxaddpar, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
  fxaddpar, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
  for idim=1,n_dims-1 do begin
    idim_str = strtrim(string(idim+1), 2)
    case idim of
      1: dim_name = '2nd'
      2: dim_name = '3rd'
      else: dim_name = idim_str+'th'
    end
    fxaddpar, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
    fxaddpar, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
  endfor ; idim=1,n_dims-1

  all_headers[6] = ptr_new(hdr)

  print,''
  print,'--- const ---'
  print,''
  print,hdr


  return, all_headers
END
