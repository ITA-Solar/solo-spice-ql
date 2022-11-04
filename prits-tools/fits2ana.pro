;+
; NAME:
;     FITS2ANA
;
; PURPOSE:
;     fits2ana reads a FITS file and returns ana structure(s). The input FITS file must contain all the necessary
;     extensions and keywords. ANA2FITS creates the correct FITS files. An ana structure is used by e.g. xcfit_block,
;     it is created by e.g. mk_analysis.
;     If the FITS file is a SPICE level 3 file, the data will be rearranged, so that XCFIT_BLOCK can use it.
;     Since the data block is stored as the original data block from level 2.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     anas = fits2ana(fitsfile [ ,headers_results=headers_results, headers_data=headers_data, $
;       headers_lambda=headers_lambda, headers_residuals=headers_residuals, headers_weights=headers_weights, $
;       headers_include=headers_include, headers_contants=headers_contants])
;
; INPUTS:
;     fitsfile : name and path to a FITS file (e.g. SPICE level 3 file)
;
; OUTPUT:
;     Array of ana structure, number of elements is the same as number of windows in the FITS file.
;     Output is scalar if there is only one window.
;
; OPTIONAL OUTPUT:
;     headers_results: A pointer array, containing the headers of the results extensions as string arrays.
;     headers_data: A pointer array, containing the headers of the data extensions as string arrays.
;     headers_lambda: A pointer array, containing the headers of the lambda extensions as string arrays.
;     headers_residuals: A pointer array, containing the headers of the residuals extensions as string arrays.
;     headers_weights: A pointer array, containing the headers of the weights extensions as string arrays.
;     headers_include: A pointer array, containing the headers of the include extensions as string arrays.
;     headers_contants: A pointer array, containing the headers of the constants extensions as string arrays.
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
;     23-Nov-2021: Martin Wiesmann
;-
; $Id: 2022-10-13 14:44 CEST $


function fits2ana, fitsfile, headers_results=headers_results, headers_data=headers_data, $
  headers_lambda=headers_lambda, headers_residuals=headers_residuals, headers_weights=headers_weights, $
  headers_include=headers_include, headers_contants=headers_contants

  result = readfits(fitsfile, hdr)
  n_windows = fxpar(hdr, 'NWIN', missing=0)
  get_headers = bytarr(7)
  if arg_present(headers_results) then begin
    headers_results = ptrarr(n_windows)
    get_headers[0] = 1
    headers_results[0] = ptr_new(hdr)
  endif
  if arg_present(headers_data) then begin
    headers_data = ptrarr(n_windows)
    get_headers[1] = 1
  endif
  if arg_present(headers_lambda) then begin
    headers_lambda = ptrarr(n_windows)
    get_headers[2] = 1
  endif
  if arg_present(headers_residuals) then begin
    headers_residuals = ptrarr(n_windows)
    get_headers[3] = 1
  endif
  if arg_present(headers_weights) then begin
    headers_weights = ptrarr(n_windows)
    get_headers[4] = 1
  endif
  if arg_present(headers_include) then begin
    headers_include = ptrarr(n_windows)
    get_headers[5] = 1
  endif
  if arg_present(headers_contants) then begin
    headers_contants = ptrarr(n_windows)
    get_headers[6] = 1
  endif
  for iwin=0,n_windows-1 do begin
    extension = iwin*7
    if iwin gt 0 then result = readfits(fitsfile, hdr, ext=extension)
    if get_headers[0] then headers_results[iwin] = ptr_new(hdr)

    ;extract info from header
    filename = strtrim(fxpar(hdr, 'ANA_FILE', missing=''), 2)
    datasource = strtrim(fxpar(hdr, 'ANA_SRC', missing=''), 2)
    definition = strtrim(fxpar(hdr, 'ANA_DEF', missing=''), 2)
    missing = fxpar(hdr, 'ANA_MISS', missing=0)
    label = strtrim(fxpar(hdr, 'ANA_LABL', missing=''), 2)
    history = fxpar(hdr, 'ANA_HIST', missing='')
    history = strtrim(strsplit(history,';',/extract,count=count), 2)

    ; extract fit components
    tag_names = hash()
    fit_components_hash = orderedhash()
    n_components = fxpar(hdr, 'ANA_NCMP', missing=0)
    for icomp=0,n_components-1 do begin
      ; Get keywords for each fit component
      fitnr = strtrim(string(icomp+1), 2)
      n_params = fxpar(hdr, 'CMP_NP'+fitnr, missing=0)
      stc = mk_component_stc(n_params)
      stc.FUNC_NAME = strtrim(fxpar(hdr, 'CMPTYP'+fitnr, missing=''), 2)
      stc.NAME = strtrim(fxpar(hdr, 'CMPNAM'+fitnr, missing=''), 2)
      stc.FUNC_STRING = strtrim(fxpar(hdr, 'CMPSTR'+fitnr, missing=''), 2)
      stc.description[*] = ''
      description = fxpar(hdr, 'CMPDES'+fitnr, missing='')
      stc.description = strtrim(strsplit(description,';',/extract,count=count), 2)
      stc.MULTIPLICATIVE = fxpar(hdr, 'CMPMUL'+fitnr, missing=0)
      stc.INCLUDE = fxpar(hdr, 'CMPINC'+fitnr, missing=0)

      for ipar=0,n_params-1 do begin
        ; Get keywords for each fit parameter
        parnr = string(byte(ipar+97))
        param = stc.param[ipar]
        param.name = strtrim(fxpar(hdr, 'PNAME'+fitnr+parnr, missing=''), 2)
        param.description[*] = ''
        description = fxpar(hdr, 'PDESC'+fitnr+parnr, missing='')
        param.description = strtrim(strsplit(description,';',/extract,count=count), 2)
        param.initial = fxpar(hdr, 'PINIT'+fitnr+parnr, missing=0)
        param.value = fxpar(hdr, 'PVAL'+fitnr+parnr, missing=0)
        param.max_val = fxpar(hdr, 'PMAX'+fitnr+parnr, missing=0)
        param.min_val = fxpar(hdr, 'PMIN'+fitnr+parnr, missing=0)
        param.trans_a = fxpar(hdr, 'PTRNA'+fitnr+parnr, missing=0)
        param.trans_b = fxpar(hdr, 'PTRNB'+fitnr+parnr, missing=0)
        param.const = fxpar(hdr, 'PCONS'+fitnr+parnr, missing=0)
        stc.param[ipar] = param
      endfor ; ipar=0,n_params-1

      ;create tag name
      case stc.FUNC_STRING of
        'g': tag_name = 'igauss'
        'b': tag_name = 'bgauss'
        'v': tag_name = 'voigt'
        'p0': tag_name = 'bg'
        else: tag_name = stc.FUNC_STRING + '_'
      endcase
      if tag_names.HasKey(tag_name) then begin
        tag_names[tag_name] = tag_names[tag_name]+1
      endif else begin
        tag_names[tag_name] = 1
      endelse
      if tag_name NE 'bg' || tag_names[tag_name] gt 1 then begin
        tag_name = tag_name + strtrim(string(tag_names[tag_name]), 2)
      endif
      fit_components_hash[tag_name] = stc

    endfor ; icomp=0,component_n-1

    fit = fit_components_hash.tostruct(/no_copy)

    data = readfits(fitsfile, hdr, ext=extension+1)
    file_info = spice_file2info(fitsfile)
    if file_info.is_spice_file && file_info.level eq 3 then begin
      size_data = size(data)
      if size_data[0] eq 4 then data = transpose(data, [2, 0, 1, 3]) $
      else data = transpose(data, [2, 0, 1])
    endif
    if get_headers[1] then headers_data[iwin] = ptr_new(hdr)
    lambda = readfits(fitsfile, hdr, ext=extension+2)
    if get_headers[2] then headers_lambda[iwin] = ptr_new(hdr)
    residual = readfits(fitsfile, hdr, ext=extension+3)
    if get_headers[3] then headers_residuals[iwin] = ptr_new(hdr)
    weights = readfits(fitsfile, hdr, ext=extension+4)
    if get_headers[4] then headers_weights[iwin] = ptr_new(hdr)
    include = readfits(fitsfile, hdr, ext=extension+5)
    if get_headers[5] then headers_include[iwin] = ptr_new(hdr)
    const = readfits(fitsfile, hdr, ext=extension+6)
    if get_headers[6] then headers_contants[iwin] = ptr_new(hdr)

    IF NOT exist(ana) THEN ana = mk_analysis()

    ana.filename = filename
    ana.datasource = datasource
    ana.definition = definition
    ana.missing = missing
    ana.label = label

    handle_value,ana.history_h,history,/no_copy,/set
    handle_value,ana.lambda_h,lambda,/no_copy,/set
    handle_value,ana.data_h,data,/no_copy,/set
    handle_value,ana.weights_h,weights,/no_copy,/set
    handle_value,ana.fit_h,fit,/no_copy,/set
    handle_value,ana.result_h,result,/no_copy,/set
    handle_value,ana.residual_h,residual,/no_copy,/set
    handle_value,ana.include_h,include,/no_copy,/set
    handle_value,ana.const_h,const,/no_copy,/set
    handle_value,ana.origin_h,origin,/no_copy,/set
    handle_value,ana.scale_h,scale,/no_copy,/set
    handle_value,ana.phys_scale_h,phys_scale,/no_copy,/set
    handle_value,ana.dimnames_h,dimnames,/no_copy,/set

    if iwin eq 0 then ana_all = ana $
    else ana_all = [ana_all, ana]

  endfor ; iwin=0,n_windows-1

  return, ana_all
end