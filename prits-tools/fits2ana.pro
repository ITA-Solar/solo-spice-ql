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
;       headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
;       headers_include=headers_include, headers_constants=headers_constants])
;
; INPUTS:
;     fitsfile : name and path to a FITS file (e.g. SPICE level 3 file)
; 
; KEYWORDS:
;     headers_only: If set, then the data is not loaded, only the headers, the function returns a zero.
;     loud: If set, then the function will output messages about non-existing extensions.
;     quiet: If set, warnings will be suppressed.
; 
; OPTIONAL INPUTS:
;     windows : A scalar or array of indices of windows to be returned. If not provided all windows will
;               be returned.
;               This can also be a scalar array of strings. Then it assumed that they are the DATA_ID of
;               the windows, see documentation of ANA2FITS.
;
; OUTPUT:
;     Array of ana structure, number of elements is the same as number of windows in the FITS file.
;     Output is scalar if there is only one window.
;     Output is zero if an error occurred, or if keyword 'headers_only' is set.
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
;     SPICE library: prits_tools.parcheck, fits2ana_get_data_id, ana_wcs_get_transform, ana_wcs_transform_vector
;     GEN library: fits_open, fits_close, readfits, fxpar, mk_analysis, mk_component_stc, fitshead2wcs, wcs_get_coord
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
; $Id: 2024-01-19 15:10 CET $


function fits2ana, fitsfile, windows=windows, $
  headers_results=headers_results, headers_data=headers_data, $
  headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
  headers_include=headers_include, headers_constants=headers_constants, $
  headers_only=headers_only, $
  loud=loud, quiet=quiet, debug=debug

  prits_tools.parcheck, fitsfile, 1, "fitsfile", 'string', 0
  prits_tools.parcheck, windows, 0, "windows", ['integers', 'string'], [0, 1], /optional

  headers_only = keyword_set(headers_only)
  loud = keyword_set(loud)
  quiet = keyword_set(quiet)
  debug = keyword_set(debug)
  
  fits_open, fitsfile, fits_content
  fits_close, fits_content
  data_ids = fits2ana_get_data_id(fits_content)
  n_windows = N_ELEMENTS(data_ids)

  IF N_ELEMENTS(windows) EQ 0 THEN BEGIN
    windows_process = indgen(n_windows)
    n_windows_process = n_windows
  ENDIF ELSE BEGIN
    IF size(windows, /type) EQ 7 THEN BEGIN
      windows_indices = []
      FOR i=0,N_ELEMENTS(windows)-1 DO BEGIN
        ind = where(data_ids EQ windows[i], count)
        IF count GT 0 THEN BEGIN
          windows_indices = [windows_indices, ind[0]]
        ENDIF ELSE BEGIN
          IF ~quiet THEN message, 'Did not find DATA_ID: ' + windows[i], /info
        ENDELSE
      ENDFOR
      IF N_ELEMENTS(windows_indices) EQ 0 THEN BEGIN
        IF ~quiet THEN message, 'Did not find any of the DATA_IDs', /info
        return, 0
      ENDIF
    ENDIF ELSE BEGIN ; size(windows, /type) EQ 7
      windows_indices = windows
    ENDELSE ; size(windows, /type) EQ 7
    ind = where(windows_indices LT n_windows AND windows_indices GE 0, count)
    IF count EQ 0 THEN BEGIN
      IF ~quiet THEN message, 'All provided window indices are outside of the available range. The FITS file contains ' + strtrim(n_windows, 2) + ' windows.', /info
      return, 0
    ENDIF ELSE BEGIN
      windows_process = windows_indices[ind]
      n_windows_process = count
      IF count NE N_ELEMENTS(windows_indices) && ~quiet THEN BEGIN
        IF ~quiet THEN message, 'Not all provided window indices are within the available range. The FITS file contains ' + strtrim(n_windows, 2) + ' windows.', /info
      ENDIF
    ENDELSE
  ENDELSE
  IF ~quiet THEN message, 'Reading file : ' + fitsfile, /info
  IF ~quiet THEN message, 'Reading ' + strtrim(n_windows_process, 2) + ' out of ' +  strtrim(n_windows, 2) + ' windows.', /info
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
      IF ~quiet THEN message, 'Could not find result extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + extname, /info
      IF ~quiet THEN message, 'Ignoring this window', /info
      hdr = ''
      if get_headers[0] then headers_results[iwin] = ptr_new(hdr)
      if get_headers[1] then headers_data[iwin] = ptr_new(hdr)
      if get_headers[2] then headers_xdim1[iwin] = ptr_new(hdr)
      if get_headers[3] then headers_weights[iwin] = ptr_new(hdr)
      if get_headers[4] then headers_include[iwin] = ptr_new(hdr)
      if get_headers[5] then headers_constants[iwin] = ptr_new(hdr)
     continue
    ENDIF
    extension = extension[0]
    IF headers_only THEN BEGIN
      result = 0
      hdr = headfits(fitsfile, ext=extension)
    ENDIF ELSE BEGIN
      result = readfits(fitsfile, hdr, ext=extension)
    ENDELSE
    if get_headers[0] then headers_results[iwin] = ptr_new(hdr)
    wcs_result = fitshead2wcs(hdr)
    
    ;extract info from header
;    filename = strtrim(fxpar(hdr, 'ANA_FILE', missing=''), 2)
;    datasource = strtrim(fxpar(hdr, 'ANA_SRC', missing=''), 2)
;    definition = strtrim(fxpar(hdr, 'ANA_DEF', missing=''), 2)
;    missing = fxpar(hdr, 'ANA_MISS', missing=0)
;    label = strtrim(fxpar(hdr, 'ANA_LABL', missing=''), 2)
;    history = fxpar(hdr, 'ANA_HIST', missing='')
;    history = strtrim(strsplit(history,';',/extract,count=count), 2)
    history = ''
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
    
    IF debug THEN BEGIN
      print,''
      print,''
      print,' - RESULTS -'
      print,''
      print,hdr
      help,result
      help,fit
    ENDIF

    DATAEXT = fxpar(hdr, 'DATAEXT', missing='')
    XDIMXT1 = fxpar(hdr, 'XDIMXT1', missing='')
    WGTEXT = fxpar(hdr, 'WGTEXT', missing='')
    INCLEXT = fxpar(hdr, 'INCLEXT', missing='')
    CONSTEXT = fxpar(hdr, 'CONSTEXT', missing='')


    ; DATA extension
    
    dataext_split = strsplit(dataext, ';', count=count, /extract)
    IF count EQ 1 THEN BEGIN
      EXT_DATA_PATH = ''
      dataext = dataext_split[0]
    ENDIF ELSE IF count EQ 2 THEN BEGIN
      EXT_DATA_PATH = dataext_split[0]
      dataext = dataext_split[1]
    ENDIF ELSE BEGIN
      IF loud THEN message, 'Unknown format of external extension: ' + dataext, /info
      IF loud THEN message, 'Only using first and last part', /info
      EXT_DATA_PATH = dataext_split[0]
      dataext = dataext_split[-1]
    ENDELSE

    extension = where(fits_content.extname EQ DATAEXT, count)
    IF count EQ 0 THEN BEGIN
      IF loud THEN message, 'Could not find data extension of window ' + strtrim(wind_ind, 2) + '. With EXTNAME: ' + DATAEXT, /info
      hdr = ''
      wcs_data_exists = 0
      data = 0
      size_data = size(data)
      ind_xdim1 = 0
    ENDIF ELSE BEGIN ; count EQ 0
      extension = extension[0]
      IF headers_only THEN BEGIN
        data = 0
        hdr = headfits(fitsfile, ext=extension)
      ENDIF ELSE BEGIN
        data = readfits(fitsfile, hdr, ext=extension)
      ENDELSE
      size_data = size(data)
      progenitor_data = fxpar(hdr, 'PRGDATA', missing=0)
      IF ~headers_only && size_data[0] EQ 0 THEN BEGIN
        IF loud THEN message, 'Loading data cube from external extension', /info
        prg_file = spice_find_file(EXT_DATA_PATH)
        prg_file = prg_file[0]
        IF prg_file NE '' && file_exist(prg_file) THEN BEGIN
          IF ~quiet THEN message, 'Reading file : '+ prg_file, /info
          fits_open, prg_file, prg_file_content
          fits_close, prg_file_content
          ind = where(prg_file_content.extname EQ dataext, count_prg)
          IF count_prg GT 0 THEN BEGIN
            data = readfits(prg_file, hdr, ext=ind[0])
            size_data = size(data)
          ENDIF ELSE BEGIN
            IF ~quiet THEN message, 'Did not find external extension "'+dataext+'" in the progenitor file.', /info
          ENDELSE
        ENDIF ELSE BEGIN ; prg_file NE ''
          IF ~quiet THEN message, 'Did not find progenitor file: ' + prg_file, /info
        ENDELSE ; prg_file NE ''
      ENDIF ; ~headers_only && size_data[0] EQ 0
      wcs_data = ana_wcs_get_transform(TYPE_XDIM1, hdr, ind_xdim1=ind_xdim1)
      wcs_data_exists =  N_ELEMENTS(wcs_data) GT 0
    ENDELSE ; count EQ 0

    IF ~headers_only THEN BEGIN
      IF ~wcs_data_exists THEN BEGIN
        IF size_data[0] EQ 0 THEN BEGIN
          box_message, ['No DATA cube found. Neither in this FITS file nor', $
            'in the progenitor file, or the progenitor file was not found.', $
            'If this is a SPICE file: LEVEL 2 FILE IS MISSING.', $
            'Creating dummy data cube.']
          datasize = wcs_result.naxis
          datasize[0] = datasize[0]*2
          IF hdr[0] NE '' THEN BEGIN
            prg_naxis = fxpar(hdr, 'XDIMNA*', missing=0)
            IF prg_naxis[0] GT 0 THEN BEGIN
              datasize = prg_naxis
            ENDIF
          ENDIF
          data = fltarr(datasize)
          wcs_data = {naxis: datasize}
        ENDIF ELSE BEGIN ; size_data[0] EQ 0
          wcs_data = {naxis: size_data[1:size_data[0]]}
        ENDELSE ; size_data[0] EQ 0
      ENDIF ELSE IF ind_xdim1 NE 0 THEN BEGIN ; ~wcs_data_exists
        data_dims = ana_wcs_transform_vector(indgen(size_data[0]), ind_xdim1, 0, size_data[0])
        data = transpose(data, data_dims)
      ENDIF ; ~wcs_data_exists
    ENDIF ; ~headers_only
    if get_headers[1] then headers_data[iwin] = ptr_new(hdr)

    IF debug THEN BEGIN
      print,''
      print,''
      print,' - DATA -'
      print,''
      print,hdr
      help,data
      help,wcs_data
    ENDIF

    
    ; XDIM1 extension
    
    extension = where(fits_content.extname EQ XDIMXT1, count)
    IF count EQ 0 THEN BEGIN
      IF loud THEN message, 'Could not find xdim1 extension of window ' + strtrim(wind_ind, 2), /info
      IF headers_only THEN BEGIN
        xdim1 = 0
      ENDIF ELSE BEGIN
        IF wcs_data_exists THEN BEGIN
          IF loud THEN message, 'Creating xdim1 cube from WCS coordinates given in data extension', /info
          xdim1 = wcs_get_coord(wcs_data)
          xdim1 = reform(xdim1[0,*,*,*,*,*,*,*])
        ENDIF ELSE BEGIN
          IF loud THEN message, 'No WCS parameters from data extension. Creating dummy XDIM1 cube', /info
          xdim1 = fltarr(wcs_data.naxis)
        ENDELSE
      ENDELSE
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      IF headers_only THEN BEGIN
        xdim1 = 0
        hdr = headfits(fitsfile, ext=extension)
      ENDIF ELSE BEGIN
        xdim1 = readfits(fitsfile, hdr, ext=extension)
      ENDELSE
    ENDELSE
    IF size(xdim1, /type) NE size(data, /type) THEN xdim1 = fix(xdim1, type=size(data, /type))
    if get_headers[2] then headers_xdim1[iwin] = ptr_new(hdr)

    IF debug THEN BEGIN
      print,''
      print,''
      print,' - XDIM1 -'
      print,''
      print,hdr
      help,xdim1
    ENDIF


    ; WEIGHTS
    
    extension = where(fits_content.extname EQ WGTEXT, count)
    IF count EQ 0 THEN BEGIN
      IF loud THEN message, 'Could not find weights extension of window ' + strtrim(wind_ind, 2), /info
      IF headers_only THEN BEGIN
        weights = 0
      ENDIF ELSE BEGIN
        IF loud THEN message, 'Creating weights cube with default values', /info
        weights = make_array(wcs_data.naxis, value=1.0)
      ENDELSE
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      IF headers_only THEN BEGIN
        weights = 0
        hdr = headfits(fitsfile, ext=extension)
      ENDIF ELSE BEGIN
        weights = readfits(fitsfile, hdr, ext=extension)
      ENDELSE
    ENDELSE
    if get_headers[3] then headers_weights[iwin] = ptr_new(hdr)

    IF debug THEN BEGIN
      print,''
      print,''
      print,' - WEIGHTS -'
      print,''
      print,hdr
      help,weights
    ENDIF


    ; INCLUDES

    extension = where(fits_content.extname EQ INCLEXT, count)
    IF count EQ 0 THEN BEGIN
      IF loud THEN message, 'Could not find includes extension of window ' + strtrim(wind_ind, 2), /info
      IF headers_only THEN BEGIN
        include = 0
      ENDIF ELSE BEGIN
        IF loud THEN message, 'Creating includes cube with default values', /info
        datasize = wcs_result.naxis
        datasize[0] = n_components
        include = make_array(datasize, value=1b)
      ENDELSE
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      IF headers_only THEN BEGIN
        include = 0
        hdr = headfits(fitsfile, ext=extension)
      ENDIF ELSE BEGIN
        include = readfits(fitsfile, hdr, ext=extension)
      ENDELSE
    ENDELSE
    if get_headers[4] then headers_include[iwin] = ptr_new(hdr)

    IF debug THEN BEGIN
      print,''
      print,''
      print,' - INCLUDES -'
      print,''
      print,hdr
      help,include
    ENDIF


    ; CONSTANTS

    extension = where(fits_content.extname EQ CONSTEXT, count)
    IF count EQ 0 THEN BEGIN
      IF loud THEN message, 'Could not find constants extension of window ' + strtrim(wind_ind, 2), /info
      IF headers_only THEN BEGIN
        const = 0
      ENDIF ELSE BEGIN
        IF loud THEN message, 'Creating constants cube with default values', /info
        datasize = wcs_result.naxis
        datasize[0] = datasize[0]-1
        const = make_array(datasize, value=0b)
      ENDELSE
      hdr = ''
    ENDIF ELSE BEGIN
      extension = extension[0]
      IF headers_only THEN BEGIN
        const = 0
        hdr = headfits(fitsfile, ext=extension)
      ENDIF ELSE BEGIN
        const = readfits(fitsfile, hdr, ext=extension)
      ENDELSE
    ENDELSE
    if get_headers[5] then headers_constants[iwin] = ptr_new(hdr)

    IF debug THEN BEGIN
      print,''
      print,''
      print,' - CONSTANTS -'
      print,''
      print,hdr
      help,const
    ENDIF
    
    
    ; RESIDUALS
    
    residual = data
    residual[*] = !VALUES.f_nan

    IF size_data[0] GT 0 THEN BEGIN
      
      sfit = make_sfit_stc(fit,/keep_limits)
      compile_sfit,sfit      
  
      ;; Convert to 7 dimensions in all cases!
      szd = size(data)
      dimen = szd[1:szd[0]]
      IF szd[0] LT 7 THEN dimen = [dimen,replicate(1L,7-szd[0])]
  
      ;;
      ;; This works for up to 7-dimensional data
      ;;
      FOR o=0L,dimen[6]-1 DO $
        FOR n=0L,dimen[5]-1 DO $
        FOR m=0L,dimen[4]-1 DO $
        FOR l=0L,dimen[3]-1 DO $
        FOR k=0L,dimen[2]-1 DO $
        FOR j=0L,dimen[1]-1 DO BEGIN

        spec = data[*,j,k,l,m,n,o]
        ix = where_not_missing(spec, ngood, missing=missing)

        IF ngood GT 0 THEN BEGIN
          lam = xdim1[*,j,k,l,m,n,o]
          params = result[0:-2,j,k,l,m,n,o]
          params = params * sfit.trans_a + sfit.trans_b
          call_procedure,sfit.compiledfunc,lam[ix], params, yfit
          residual[ix,j,k,l,m,n,o] = spec[ix] - yfit
        ENDIF
      ENDFOR

    ENDIF ; size_data[0] GT 0


    IF ~headers_only THEN BEGIN
      
      ana = mk_analysis()
  
  ;    ana.filename = filename
      ana.datasource = fitsfile
  ;    ana.definition = definition
  ;    ana.missing = missing
  ;    ana.label = label
  
      handle_value,ana.history_h,history,/no_copy,/set
      handle_value,ana.lambda_h,xdim1,/no_copy,/set
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

    ENDIF ELSE ana_all = 0 ; ~headers_only

  endfor ; iwin=0,n_windows-1

  return, ana_all
end
