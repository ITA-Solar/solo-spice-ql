;+
; NAME:
;     FITS2ANA
;
; PURPOSE:
;     fits2ana reads a FITS file and returns ana structure(s). The input FITS file must contain all the necessary
;     extensions and keywords. ANA2FITS creates the correct FITS files. An ana structure is used by e.g. xcfit_block,
;     it is created by e.g. mk_analysis.
;     If the FITS file is a SPICE level 3 file, the data array will be rearranged, so that XCFIT_BLOCK can use it.
;     Since the data block is stored as the original data block from level 2.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     anas = fits2ana(fitsfile [ ,headers_results=headers_results, headers_data=headers_data, $
;       headers_lambda=headers_lambda, headers_weights=headers_weights, $
;       headers_include=headers_include, headers_constants=headers_constants])
;
; INPUTS:
;     fitsfile : name and path to a FITS file (e.g. SPICE level 3 file)
; 
; OPTIONAL INPUTS:
;     windows : A scalar or array of indices of windows to be returned. If not provided all windows will
;               be returned.
;
; OUTPUT:
;     Array of ana structure, number of elements is the same as number of windows in the FITS file.
;     Output is scalar if there is only one window.
;     Output is zero if an error occurred.
;
; OPTIONAL OUTPUT:
;     headers_results: A pointer array, containing the headers of the results extensions as string arrays.
;              One string array per ANA provided.
;     headers_data: A pointer array, containing the headers of the data extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_xdim1: A pointer array, containing the headers of the xdim1 extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_weights: A pointer array, containing the headers of the weights extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_include: A pointer array, containing the headers of the include extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_constants: A pointer array, containing the headers of the constants extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;
; CALLS:
;     prits_tools.parcheck, fits_open, fits_close, fits2ana_get_data_id, readfits, fxpar, mk_component_stc, 
;     mk_analysis, fitshead2wcs, ana_wcs_get_transform, ana_wcs_transform_vector
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
; $Id: 2023-11-29 10:49 CET $


function fits2ana, fitsfile, windows=windows, $
  headers_results=headers_results, headers_data=headers_data, $
  headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
  headers_include=headers_include, headers_constants=headers_constants

  prits_tools.parcheck, fitsfile, 1, "fitsfile", 'string', 0
  prits_tools.parcheck, windows, 0, "windows", 'integers', [0, 1], /optional

  fits_open, fitsfile, fits_content
  fits_close, fits_content
  data_ids = fits2ana_get_data_id(fits_content)
  n_windows = N_ELEMENTS(data_ids)

  IF N_ELEMENTS(windows) EQ 0 THEN BEGIN
    windows_process = indgen(n_windows)
    n_windows_process = n_windows
  ENDIF ELSE BEGIN
    ind = where(windows LT n_windows AND windows GE 0, count)
    IF count EQ 0 THEN BEGIN
      message, 'All provided window indices are outside of the available range. The FITS file contains ' + strtrim(n_windows, 2) + ' windows.', /info
      return, 0
    ENDIF ELSE BEGIN
      windows_process = windows[ind]
      n_windows_process = count
      IF count NE N_ELEMENTS(windows) THEN BEGIN
        message, 'Not all provided window indices are within the available range. The FITS file contains ' + strtrim(n_windows, 2) + ' windows.', /info
      ENDIF
    ENDELSE
  ENDELSE
  print, 'Reading ' + strtrim(n_windows_process, 2) + ' out of ' +  strtrim(n_windows, 2) + ' windows.'
  get_headers = bytarr(6)
  if arg_present(headers_results) then begin
    headers_results = ptrarr(n_windows_process)
    get_headers[0] = 1
  endif
  if arg_present(headers_data) then begin
    headers_data = ptrarr(n_windows_process)
    get_headers[1] = 1
  endif
  if arg_present(headers_xdim1) then begin
    headers_xdim1 = ptrarr(n_windows_process)
    get_headers[2] = 1
  endif
  if arg_present(headers_weights) then begin
    headers_weights = ptrarr(n_windows_process)
    get_headers[3] = 1
  endif
  if arg_present(headers_include) then begin
    headers_include = ptrarr(n_windows_process)
    get_headers[4] = 1
  endif
  if arg_present(headers_constants) then begin
    headers_constants = ptrarr(n_windows_process)
    get_headers[5] = 1
  endif

  
  for iwin=0,n_windows_process-1 do begin
    wind_ind = windows_process[iwin]
    extname = data_ids[wind_ind] + ' results'
    extension = where(fits_content.extname EQ extname, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find result extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Ignoring this window', /info
      hdr = ''
      if get_headers[0] then headers_results[iwin] = ptr_new(hdr)
      if get_headers[1] then headers_data[iwin] = ptr_new(hdr)
      if get_headers[2] then headers_lambda[iwin] = ptr_new(hdr)
      if get_headers[3] then headers_weights[iwin] = ptr_new(hdr)
      if get_headers[4] then headers_include[iwin] = ptr_new(hdr)
      if get_headers[5] then headers_constants[iwin] = ptr_new(hdr)
     continue
    ENDIF
    extension = extension[0]
    result = readfits(fitsfile, hdr, ext=extension)
    if get_headers[0] then headers_results[iwin] = ptr_new(hdr)

    ;extract info from header
;    filename = strtrim(fxpar(hdr, 'ANA_FILE', missing=''), 2)
;    datasource = strtrim(fxpar(hdr, 'ANA_SRC', missing=''), 2)
;    definition = strtrim(fxpar(hdr, 'ANA_DEF', missing=''), 2)
;    missing = fxpar(hdr, 'ANA_MISS', missing=0)
;    label = strtrim(fxpar(hdr, 'ANA_LABL', missing=''), 2)
;    history = fxpar(hdr, 'ANA_HIST', missing='')
;    history = strtrim(strsplit(history,';',/extract,count=count), 2)
    type_xdim1 = strtrim(fxpar(hdr, 'XDIMTY1', missing=''), 2)

    ; extract fit components
    tag_names = hash()
    fit_components_hash = orderedhash()
    n_components = fxpar(hdr, 'ANA_NCMP', missing=0)
    for icomp=0,n_components-1 do begin
      ; Get keywords for each fit component
      fitnr = strtrim(icomp+1, 2)
      n_params = fxpar(hdr, 'CMP_NP'+fitnr, missing=0)
      stc = mk_component_stc(n_params)
      FUNC_NAME = strtrim(fxpar(hdr, 'CMPTYP'+fitnr, missing=''), 2)
      CASE FUNC_NAME OF
        'Gaussian': component_type = 'comp_gauss'
        'Polynomial': component_type = 'comp_poly'
        'SSW comp_bgauss': component_type = 'comp_bgauss'
        'SSW comp_voigt': component_type = 'comp_voigt'
        else: component_type = FUNC_NAME
      ENDCASE
      stc.FUNC_NAME = component_type
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
        punit = strtrim(fxpar(hdr, 'PUNIT'+fitnr+parnr, missing=''), 2)
        param.description[*] = ''
        description = fxpar(hdr, 'PDESC'+fitnr+parnr, missing='')
        param.description = strtrim(strsplit(description,';',/extract,count=count), 2)
        param.initial = fxpar(hdr, 'PINIT'+fitnr+parnr, missing=0)
;        param.value = fxpar(hdr, 'PVAL'+fitnr+parnr, missing=0)
        param.max_val = fxpar(hdr, 'PMAX'+fitnr+parnr, missing=0)
        param.min_val = fxpar(hdr, 'PMIN'+fitnr+parnr, missing=0)
        param.trans_a = fxpar(hdr, 'PTRA'+fitnr+parnr, missing=0)
        param.trans_b = fxpar(hdr, 'PTRB'+fitnr+parnr, missing=0)
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
        tag_name = tag_name + strtrim(tag_names[tag_name], 2)
      endif
      fit_components_hash[tag_name] = stc

    endfor ; icomp=0,component_n-1

    fit = fit_components_hash.tostruct(/no_copy)


    ; Data extension
    
    extname = data_ids[wind_ind] + ' data'
    extension = where(fits_content.extname EQ extname, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find data extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Creating dummy data cube', /info
      hdr = ''
      wcs_result = fitshead2wcs(hdr)
      datasize = wcs_result.naxis
      datasize[0] = datasize[0]*2
      data = fltarr(datasize)
    ENDIF ELSE BEGIN
      extension = extension[0]
      data = readfits(fitsfile, hdr, ext=extension)
      size_data = size(data)
      progenitor_data = fxpar(hdr, 'PRGDATA', missing=0)
      wcs_data = ana_wcs_get_transform(TYPE_XDIM1, hdr, ind_xdim1=ind_xdim1)
      IF N_ELEMENTS(wcs_data) EQ 0 THEN BEGIN
        wcs_data = {naxis: size_data[1:size_data[0]]}
      ENDIF ELSE IF ind_xdim1 NE 0 THEN BEGIN
        data_dims = ana_wcs_transform_vector(indgen(size_data[0]), ind_xdim1, 0, size_data[0])
        data = transpose(data, data_dims)
      ENDIF
    ENDELSE
    if get_headers[1] then headers_data[iwin] = ptr_new(hdr)

    
    ; XDIM1 extension
    
    extname = data_ids[wind_ind] + ' xdim1'
    extname_old = data_ids[wind_ind] + ' lambda'
    extension = where(fits_content.extname EQ extname OR fits_content.extname EQ extname_old, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find xdim1 extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Creating xdim1 cube from WCS coordinates given in data extension', /info
      lambda = 0 ; TODO
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      lambda = readfits(fitsfile, hdr, ext=extension)
    ENDELSE
    if get_headers[2] then headers_lambda[iwin] = ptr_new(hdr)

    extname = data_ids[wind_ind] + ' weights'
    extension = where(fits_content.extname EQ extname, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find weights extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Creating weights cube with default values', /info
      weights = 0 ; TODO
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      weights = readfits(fitsfile, hdr, ext=extension)
    ENDELSE
    if get_headers[3] then headers_weights[iwin] = ptr_new(hdr)

    extname = data_ids[wind_ind] + ' includes'
    extension = where(fits_content.extname EQ extname, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find includes extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Creating includes cube with default values', /info
      include = 0 ; TODO
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      include = readfits(fitsfile, hdr, ext=extension)
    ENDELSE
    if get_headers[4] then headers_include[iwin] = ptr_new(hdr)

    extname = data_ids[wind_ind] + ' constants'
    extension = where(fits_content.extname EQ extname, count)
    IF count EQ 0 THEN BEGIN
      message, 'Could not find constants extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      message, 'Creating constants cube with default values', /info
      const = 0 ; TODO
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      const = readfits(fitsfile, hdr, ext=extension)
    ENDELSE
    if get_headers[5] then headers_constants[iwin] = ptr_new(hdr)

    ana = mk_analysis()

;    ana.filename = filename
    ana.datasource = fitsfile
;    ana.definition = definition
;    ana.missing = missing
;    ana.label = label

    handle_value,ana.history_h,history,/no_copy,/set
    handle_value,ana.lambda_h,lambda,/no_copy,/set
    handle_value,ana.data_h,data,/no_copy,/set
    handle_value,ana.weights_h,weights,/no_copy,/set
    handle_value,ana.fit_h,fit,/no_copy,/set
    handle_value,ana.result_h,result,/no_copy,/set
    ;handle_value,ana.residual_h,residual,/no_copy,/set
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
