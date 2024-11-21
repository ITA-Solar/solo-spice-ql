;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This is a subfunction of ANA2FITS.
;      This function returns an array of 6 FITS headers made from an ANA object or file.
;      The RESULT FITS header is the main header, and contains all analysis-specific
;      information, i.e. fit components and parameters. This header may also contain
;      additional project-related keywords.
;      The DATA FITS header may contain all information about the progenitor data, or
;      the data cube used in xcfit_block.
;      THE XDIM1 FITS header, i.e. the header with the absorbed dimension, is only saved
;      upon request. This header and data cube can be recovered from the WCS parameters
;      from the DATA extension.
;      The other headers (INCLUDE, CONST, WEIGHTS) are only saved if at least one
;      value is not the default value.
;      RESIDUAL is not saved at all, since this is not required and can be recalculated
;      using xcfit_block.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS
;
; CALLING SEQUENCE:
;      headers = ana2fitshdr(ANA, FILENAME_OUT=FILENAME_OUT, $
;           N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
;           DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
;           IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
;           PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
;           XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
;           RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
;           CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;           DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
;           PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
;           SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
;           DATA_ARRAY=DATA_ARRAY)
;
; INPUTS:
;      ANA: An ANA object or the name and path of an ANA file.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      FILENAME_OUT: Filename of the resulting FITS file. May include the path.
;      N_WINDOWS: Total number of windows to be included in FITS file.
;      WINNO: Window number (starting at 0) within this study in this FITS file.
;      TYPE_XDIM1: CTYPE of the absorbed dimension (e.g. 'WAVE').
;
; KEYWORDS:
;      IS_EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the FITS file.
;                 If not set, this will be the primary header.
;      SAVE_XDIM1: If set, then XDIM1 will be saved, otherwise not. XDIM1 can usually be
;             calculated from the WCS parameters from the data array.
;      NO_SAVE_DATA: If set, then the data cube is not saved, only the header.
;             It is then assumed, that HEADER_INPUT_DATA contains a link to the data.
;             This is the same as not providing INPUT_DATA nor PROGENITOR_DATA.
;      PRINT_HEADERS: If set, then all headers created will be printed out.
;
; OPTIONAL INPUTS:
;      HEADER_INPUT_DATA: A string array, containing the header of the data extension.
;              This is used to describe the data. WCS parameters should correspond with INPUT_DATA, or with PROGENITOR_DATA respectively.
;      PROGENITOR_DATA: A data array. Up to 7-dimensional. Absorbed dimensions (e.g. spectra) does not have to be
;              along the first dimension. If this data array is provided, it will be saved into the XDIM1 extension instead of INPUT_DATA.
;      DATA_ID: A string defining the prefix to the names of the 6 extensions.
;              Default is the value of the keyword 'EXTNAME' from HEADER_INPUT_DATA. If this is provided then the data extension
;              will have he this EXTNAME (without 'data') as its extension name.
;              If this is not provided then default is the dataset indices.
;      EXT_DATA_PATH: A string array or a string. This contains the relative path to the external extension, which contains
;              the data cube. If this is provided the data is not saved in the new FITS file, but the header is.
;              The header keyword DATAEXT in the headers will get EXT_DATA_PATH as a prefix to point to the external extension.
;              See also Appendix VII aobut External Extensions in https://arxiv.org/abs/2011.12139
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      CREATOR: String. The name of the creator of this FITS file. If not provided this keyword will not be in the header.
;      PROJ_KEYWORDS: A list or array of hashes with entries ('name',xxx1, 'value',xxx2, 'comment',xxx3}
;              where, xxx123 can be a string or a number. These are additional project-related
;              keywords that should be added to the header.
;      PROC_STEPS: A list, each element stands for one processing step, i.e. gets a new number.
;              Each processing step consists of an array of hashes with entries ('name',xxx1, 'value',xxx2, 'comment',xxx3}
;              where, xxx123 can be a string or a number.
;              The name can be any of the following:
;              PRSTEP|PRPROC|PRPVER|PRMODE|PRPARA|PRREF|PRLOG|PRENV|PRVER|PRHSH|PRBRA|PRLIB
;              PRSTEP should be included. The name and the comment will get the processing step number added.
;
; OPTIONAL INPUTS/OUTPUTS:
;      All of the following optional inputs must be provided if 'ANA' is not provided.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
;      RESULT: The array to contain the result parameter values (and the Chi^2) values.
;      FIT: The component fit structure
;
;      All of the following optional inputs can be provided if 'ANA' is not provided. If not
;      provided, it is assumed they contain only default values.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
;      HISTORY: A string array.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with absorbed dimension (e.g. spectra)
;              along the first dimension. This is ignored if PROGENITOR_DATA is provided.
;      XDIM1: Array of same size as the input data to xcfit_block. It contains the values of the
;             absorbed dimension for each point (e.g wavelength).
;      WEIGHTS: Weights to use in the fitting process.
;      INCLUDE: Array to keep the INCLUDE status of each component at each point.
;      CONST: Array to keep the CONST status of each parameter at each point.
;
;      The following optional inputs will be ignored.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
;      RESIDUAL: Array to contain the residual. Same size as INPUT_DATA, this will
;              be ignored and not saved into the FITS file.
;      FILENAME_ANA: The filename of the ANA-file.
;      DATASOURCE: A string.
;      DEFINITION: A string.
;      MISSING: The MISSING value, used to flag missing data points,
;              and parameter values at points where the fit has been
;              declared as "FAILED". This is assumed to be NAN.
;      LABEL: A string.
;
; OUTPUTS:
;      a pointer array, containing 6 FITS keyword headers, of which 5 may be empty strings.
;
; OPTIONAL OUTPUTS:
;      DATA_ARRAY: Contains the data array that should be saved into the data extension, if any.
;
; CALLS:
;     prits_tools.parcheck, caldat, ana_wcs_get_transform
;     ana2fitshdr_results, ana2fitshdr_data, ana2fitshdr_xdim,
;     ana2fitshdr_weights, ana2fitshdr_include, ana2fitshdr_const
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2024-11-21 13:41 CET $

FUNCTION ana2fitshdr, ana, filename_out = filename_out, $
  n_windows = n_windows, winno = winno, $
  data_id = data_id, type_xdim1 = type_xdim1, $
  ext_data_path = ext_data_path, $
  is_extension = is_extension, level = level, version = version, creator = creator, $
  proc_steps = proc_steps, proj_keywords = proj_keywords, $
  xdim1 = xdim1, input_data = input_data, fit = fit, $
  result = result, residual = residual, weights = weights, include = include, $
  const = const, filename_ana = filename_ana, datasource = datasource, $
  definition = definition, missing = missing, label = label, history = history, $
  progenitor_data = progenitor_data, header_input_data = header_input_data, $
  save_xdim1 = save_xdim1, no_save_data = no_save_data, print_headers = print_headers, $
  data_array = data_array
  prits_tools.parcheck, ana, 1, 'ANA', 'STRUCT', 0, structure_name = 'CFIT_ANALYSIS', /optional
  ana_given = n_elements(ana)
  prits_tools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = ana_given
  prits_tools.parcheck, fit, 0, 'FIT', 'STRUCT', 0, optional = ana_given
  prits_tools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [0, 2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, xdim1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, weights, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, const, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1

  prits_tools.parcheck, filename_out, 0, 'FILENAME_OUT', 'STRING', 0
  prits_tools.parcheck, n_windows, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, winno, 0, 'WINNO', 'INTEGERS', 0
  prits_tools.parcheck, type_xdim1, 0, 'TYPE_XDIM1', 'STRING', 0
  prits_tools.parcheck, header_input_data, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional = 1
  prits_tools.parcheck, data_id, 0, 'DATA_ID', 'STRING', 0, result = error
  IF error[0] NE '' THEN BEGIN
    data_id = strtrim(winno, 2)
    IF n_elements(header_input_data) GT 0 THEN data_id = fxpar(header_input_data, 'EXTNAME', missing = data_id) $
    ELSE data_id = data_id + ' data'
  ENDIF
  prits_tools.parcheck, level, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, version, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, creator, 0, 'CREATOR', 'STRING', 0, /optional
  prits_tools.parcheck, proc_steps, 0, 'PROC_STEPS', 11, 1, /optional
  prits_tools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional

  prits_tools.parcheck, residual, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional = 1
  prits_tools.parcheck, history, 0, 'HISTORY', 'STRING', [0, 1], optional = 1
  prits_tools.parcheck, filename_ana, 0, 'FILENAME_ANA', 'STRING', 0, optional = 1
  prits_tools.parcheck, datasource, 0, 'DATASOURCE', 'STRING', 0, optional = 1
  prits_tools.parcheck, definition, 0, 'DEFINITION', 'STRING', 0, optional = 1
  prits_tools.parcheck, missing, 0, 'MISSING', 'NUMERIC', 0, optional = 1
  prits_tools.parcheck, label, 0, 'LABEL', 'STRING', 0, optional = 1

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
  datetime = {CDS_EXT_TIME, $
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
    ' xdim1', $
    ' weights', $
    ' includes', $
    ' constants']

  wcs = ana_wcs_get_transform(type_xdim1, header_input_data)

  all_headers = ptrarr(6)

  ; ------
  ; Create result header
  ; ------

  hdr = ana2fitshdr_results(result = result, fit = fit, datetime = datetime, $
    filename_out = file_basename(filename_out), n_windows = n_windows, winno = winno, $
    extension_names = extension_names, is_extension = is_extension, $
    header_input_data = header_input_data, wcs = wcs, $
    level = level, version = version, creator = creator, $
    proc_steps = proc_steps, proj_keywords = proj_keywords, $
    history = history, filename_ana = filename_ana, $
    datasource = datasource, definition = definition, missing = missing, label = label)
  all_headers[0] = ptr_new(hdr)

  ; ------
  ; Create data header
  ; ------

  IF keyword_set(ext_data_path) THEN no_save_data = 1
  hdr = ana2fitshdr_data(datetime = datetime, extension_names = extension_names, input_data = input_data, $
    header_input_data = header_input_data, progenitor_data = progenitor_data, no_save_data = no_save_data, $
    data_array = data_array)
  all_headers[1] = ptr_new(hdr)
  IF keyword_set(ext_data_path) THEN extension_names[1] = ext_data_path + ';' + extension_names[1] $
  ELSE IF hdr[0] EQ '' THEN extension_names[1] = ''

  ; ------
  ; Create xdim header
  ; ------

  hdr = ana2fitshdr_xdim(datetime = datetime, extension_names = extension_names, xdim1 = xdim1, wcs = wcs, $
    save_xdim1 = save_xdim1, type_xdim1 = type_xdim1)
  all_headers[2] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[2] = ''

  ; ------
  ; Create weights header
  ; ------

  hdr = ana2fitshdr_weights(datetime = datetime, extension_names = extension_names, weights = weights, wcs = wcs)
  all_headers[3] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[3] = ''

  ; ------
  ; Create include header
  ; ------

  hdr = ana2fitshdr_include(datetime = datetime, extension_names = extension_names, include = include, wcs = wcs)
  all_headers[4] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[4] = ''

  ; ------
  ; Create const header
  ; ------

  hdr = ana2fitshdr_const(datetime = datetime, extension_names = extension_names, const = const, wcs = wcs)
  all_headers[5] = ptr_new(hdr)
  IF hdr[0] EQ '' THEN extension_names[5] = ''

  ; Delete extension names if necessary
  FOR iext = 0, 5 DO BEGIN
    hdr = all_headers[iext]
    IF (*hdr)[0] NE '' THEN BEGIN
      fxaddpar, *hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
      fxaddpar, *hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
      fxaddpar, *hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
      fxaddpar, *hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
      fxaddpar, *hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'
    ENDIF
    IF keyword_set(print_headers) THEN BEGIN
      print, ''
      CASE iext OF
        0: print, '--- RESULTS ---'
        1: print, '--- DATA ---'
        2: print, '--- XDIM ---'
        3: print, '--- WEIGHTS ---'
        4: print, '--- INCLUDE ---'
        5: print, '--- CONST ---'
      ENDCASE
      print, ''
      print, *hdr
    ENDIF
  ENDFOR

  return, all_headers
END