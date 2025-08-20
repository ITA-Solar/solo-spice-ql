;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This is a subfunction of ANA2FITS.
;      This function returns an array of 6 FITS headers made from an ANA object.
;      The RESULT FITS header is the main header, and contains all analysis-specific
;      information, i.e. fit components and parameters. This header may also contain
;      additional project-related keywords.
;      The DATA FITS header may contain all information about the progenitor data, or
;      the data cube used in xcfit_block.
;      THE XDIM1 FITS header, i.e. the header with the absorbed dimension, is not saved
;      since it can be recreated with the WCS parameters from the DATA header.
;      The other headers (INCLUDE, CONST, WEIGHTS) are only saved if at least one
;      value is not the default value.
;      RESIDUAL is only saved upon request, since this is not required and can be recalculated
;      using xcfit_block.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS
;
; CALLING SEQUENCE:
;      see function definition
;
; PARAMETERS:
;     All parameters are described in ANA2FITS.
;     The only difference is that most of the parameters in ANA2FITS can be arrays, i.e. contain multiple
;     datasets/windows, whereas the parameters in this function are for one dataset/window only.
;
; OUTPUTS:
;      a pointer array, containing 6 FITS keyword headers, of which 5 may be empty strings.
;
; OPTIONAL OUTPUTS:
;      DATA_ARRAY: Contains the data array that should be saved into the data extension, if any.
;
; CALLS:
;     ptools.parcheck, caldat, ana_wcs_get_transform
;     ana2fitshdr_results, ana2fitshdr_data, ana2fitshdr_residual,
;     ana2fitshdr_weights, ana2fitshdr_include, ana2fitshdr_const
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2025-08-20 15:30 CEST $

FUNCTION ana2fitshdr, ana, filename_out = filename_out, $
  n_windows = n_windows, winno = winno, $
  data_id = data_id, XTYPE1 = XTYPE1, XDIMEN1 = XDIMEN1, $
  DATA_EXT_PATH = DATA_EXT_PATH, $
  is_extension = is_extension, level = level, version = version, creator = creator, SIGMADAT = SIGMADAT, $
  proc_steps = proc_steps, proj_keywords = proj_keywords, $
  xdim1 = xdim1, input_data = input_data, fit = fit, $
  result = result, residual = residual, weights = weights, include = include, $
  const = const, filename_ana = filename_ana, datasource = datasource, $
  definition = definition, missing = missing, label = label, history = history, $
  progenitor_data = progenitor_data, header_input_data = header_input_data, $
  SAVE_RESIDUALS = SAVE_RESIDUALS, SAVE_DATA = SAVE_DATA, print_headers = print_headers, $
  data_array = data_array
  ptools.parcheck, ana, 1, 'ANA', 'STRUCT', 0, structure_name = 'CFIT_ANALYSIS', /optional
  ana_given = n_elements(ana)
  ptools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = ana_given
  ptools.parcheck, fit, 0, 'FIT', 'STRUCT', 0, optional = ana_given
  ptools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [0, 2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, xdim1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, weights, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, const, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1

  ptools.parcheck, filename_out, 0, 'FILENAME_OUT', 'STRING', 0
  ptools.parcheck, n_windows, 0, 'N_WINDOWS', 'INTEGERS', 0
  ptools.parcheck, winno, 0, 'WINNO', 'INTEGERS', 0
  ptools.parcheck, XTYPE1, 0, 'XTYPE1', 'STRING', 0
  ptools.parcheck, XDIMEN1, 0, 'XDIMEN1', 'STRING', 0, /optional
  ptools.parcheck, header_input_data, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional = 1
  ptools.parcheck, data_id, 0, 'DATA_ID', 'STRING', 0, result = error
  IF error[0] NE '' THEN BEGIN
    data_id = 'Window ' + strtrim(winno, 2)
    IF n_elements(header_input_data) GT 0 THEN data_id = fxpar(header_input_data, 'EXTNAME', missing = data_id)
  ENDIF
  ptools.parcheck, DATA_EXT_PATH, 0, 'DATA_EXT_PATH', 'STRING', 0, result = error
  IF error[0] NE '' THEN BEGIN
    DATA_EXT_PATH = ''
    IF n_elements(header_input_data) GT 0 THEN DATA_EXT_PATH = fxpar(header_input_data, 'FILENAME', missing = DATA_EXT_PATH)
  ENDIF
  DATA_EXT_PATH = DATA_EXT_PATH + ';' + data_id
  data_id = strmid(data_id, 0, 58)
  ptools.parcheck, level, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  ptools.parcheck, version, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  ptools.parcheck, creator, 0, 'CREATOR', 'STRING', 0, /optional
  ptools.parcheck, proc_steps, 0, 'PROC_STEPS', 11, 1, /optional
  ptools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional

  ptools.parcheck, residual, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  ptools.parcheck, history, 0, 'HISTORY', 'STRING', [0, 1], optional = 1
  ptools.parcheck, filename_ana, 0, 'FILENAME_ANA', 'STRING', 0, optional = 1
  ptools.parcheck, datasource, 0, 'DATASOURCE', 'STRING', 0, optional = 1
  ptools.parcheck, definition, 0, 'DEFINITION', 'STRING', 0, optional = 1
  ptools.parcheck, missing, 0, 'MISSING', 'NUMERIC', 0, optional = 1
  ptools.parcheck, label, 0, 'LABEL', 'STRING', 0, optional = 1

  input_type = size(ana, /type)
  CASE input_type OF
    7: BEGIN
      restore, ana, /verbose
      handle_value, ana.history_h, history
      handle_value, ana.lambda_h, xdim1
      handle_value, ana.data_h, input_data
      handle_value, ana.weights_h, weights
      handle_value, ana.fit_h, fit
      handle_value, ana.result_h, result
      handle_value, ana.residual_h, residual
      handle_value, ana.include_h, include
      handle_value, ana.const_h, const
      handle_value, ana.origin_h, origin
      handle_value, ana.scale_h, scale
      handle_value, ana.phys_scale_h, phys_scale
      handle_value, ana.dimnames_h, dimnames
      filename_ana = ana.filename
      datasource = ana.datasource
      definition = ana.definition
      missing = ana.missing
      label = ana.label
    END

    8: BEGIN
      handle_value, ana.history_h, history
      handle_value, ana.lambda_h, xdim1
      handle_value, ana.data_h, input_data
      handle_value, ana.weights_h, weights
      handle_value, ana.fit_h, fit
      handle_value, ana.result_h, result
      handle_value, ana.residual_h, residual
      handle_value, ana.include_h, include
      handle_value, ana.const_h, const
      handle_value, ana.origin_h, origin
      handle_value, ana.scale_h, scale
      handle_value, ana.phys_scale_h, phys_scale
      handle_value, ana.dimnames_h, dimnames
      filename_ana = ana.filename
      datasource = ana.datasource
      definition = ana.definition
      missing = ana.missing
      label = ana.label
    END

    0: BEGIN
    END

    ELSE: BEGIN
      print, 'wrong input'
      return, -1
    END
  ENDCASE

  ; Add time to DATE
  caldat, systime(/julian), month, day, year, hour, minute, second
  datetime = {cds_ext_time, $
    year: year, $
    month: month, $
    day: day, $
    hour: hour, $
    minute: minute, $
    second: second, $
    millisecond: 0}
  datetime = anytim(datetime, /ccsds)

  extension_names = data_id + [ $
    ' results', $
    '', $
    ' weights', $
    ' includes', $
    ' constants', $
    ' residuals']
  IF ~keyword_set(SAVE_DATA) THEN extension_names[1] = DATA_EXT_PATH

  wcs = ana_wcs_get_transform(XTYPE1, header_input_data, ind_xdim1 = ind_xdim1)
  IF n_elements(XDIMEN1) EQ 0 THEN BEGIN
    XDIMEN1 = ind_xdim1
    IF XDIMEN1 GE 0 THEN XDIMEN1 += 1
  ENDIF

  ; Set parameters to NAN if not included in fit
  result = ana2fits_check_include(fit = fit, result = result, include = include)

  all_headers = ptrarr(6)

  ; ------
  ; Create result header
  ; ------

  hdr = ana2fitshdr_results(result = result, fit = fit, datetime = datetime, $
    filename_out = filename_out, n_windows = n_windows, winno = winno, $
    DATA_EXT_PATH = DATA_EXT_PATH, XTYPE1 = XTYPE1, XDIMEN1 = XDIMEN1, $
    extension_names = extension_names, is_extension = is_extension, $
    header_input_data = header_input_data, wcs = wcs, $
    level = level, version = version, creator = creator, SIGMADAT = SIGMADAT, $
    proc_steps = proc_steps, proj_keywords = proj_keywords, $
    history = history, filename_ana = filename_ana, $
    datasource = datasource, definition = definition, missing = missing, label = label)
  all_headers[0] = ptr_new(hdr)

  ; ------
  ; Create data header
  ; ------

  hdr = ana2fitshdr_data(datetime = datetime, extension_names = extension_names, input_data = input_data, $
    header_input_data = header_input_data, progenitor_data = progenitor_data, SAVE_DATA = SAVE_DATA, $
    data_array = data_array)
  all_headers[1] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[1] = ''

  ; ------
  ; Create weights header
  ; ------

  IF 0 THEN BEGIN
    ; This function is not used and should be rewritten to use SIGMADAT instead of WEIGHTS.
    hdr = ana2fitshdr_weights(datetime = datetime, extension_names = extension_names, weights = weights, wcs = wcs, SIGMADAT = SIGMADAT)
  ENDIF ELSE hdr = ''
  all_headers[2] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[2] = ''

  ; ------
  ; Create include header
  ; ------

  hdr = ana2fitshdr_include(datetime = datetime, extension_names = extension_names, include = include, wcs = wcs)
  all_headers[3] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[3] = ''

  ; ------
  ; Create const header
  ; ------

  hdr = ana2fitshdr_const(datetime = datetime, extension_names = extension_names, const = const, wcs = wcs)
  all_headers[4] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[4] = ''

  ; ------
  ; Create residual header
  ; ------

  hdr = ana2fitshdr_residual(datetime = datetime, extension_names = extension_names, residual = residual, wcs = wcs, SAVE_RESIDUALS = SAVE_RESIDUALS)
  all_headers[5] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[5] = ''

  ; Delete extension names if necessary
  FOR iext = 0, 5 DO BEGIN
    hdr = all_headers[iext]
    IF (*hdr)[0] NE '' THEN BEGIN
      fxaddpar, *hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
      fxaddpar, *hdr, 'INCLEXT', extension_names[3], 'Extension name of includes'
      fxaddpar, *hdr, 'CONSTEXT', extension_names[4], 'Extension name of constants'
      fxaddpar, *hdr, 'RESIDEXT', extension_names[5], 'Extension name of residuals'
    ENDIF
    IF keyword_set(print_headers) THEN BEGIN
      print, ''
      CASE iext OF
        0: print, '--- RESULTS ---'
        1: print, '--- DATA ---'
        2: print, '--- WEIGHTS --- -> should be SIGMADAT'
        3: print, '--- INCLUDE ---'
        4: print, '--- CONST ---'
        5: print, '--- RESIDUAL ---'
      ENDCASE
      print, ''
      print, *hdr
    ENDIF
  ENDFOR

  return, all_headers
END
