;+
; NAME:
;      ANA2FITS
;
; PURPOSE:
;      This procedure saves the content of one or more ANA structures into one FITS file.
;      The FITS file will contain up to 6 extensions per ANA, where the first contains the results
;      and the fit components as header keywords. The resulting FITS file can be read and converted
;      into one or more ANA structures with the procedure FITS2ANA.
;      It is possible to call this procedure multiple times with the same filepath_out,
;      if in these cases the EXTENSION keyword is set, the windows will be appended to the
;      existing FITS file. However, one needs to MAKE SURE THAT THE HEADER KEYWORD "NWIN" IS CORRECTLY SET.
;      See description of n_windows for more details.
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;      ana2fits, ana, filepath_out=filepath_out [, data_id=data_id, $
;         n_windows=n_windows, winno=winno, $
;         HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;         FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;         CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;         DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
;         EXTENSION=EXTENSION]
;
; INPUTS:
;      ANA: An ANA object.
;              If this is not provided, then at the least RESULTS and FIT
;              must be provided. If more than one ANA should be saved into one FITS file,
;              then 'ana' must be provided as an array of either file paths or objects.
;      FILEPATH_OUT: Full path and filename of the resulting FITS file.
;      TYPE_XDIM1: CTYPE of the absorbed dimension (e.g. 'WAVE'). A string array, or a scalar, in which case
;              the same value will be used for all windows.
;
; KEYWORDS:
;      IS_EXTENSION: If set, then the first ANA's result array will be an extension,
;              i.e. this should be set if the FITS file already exists and data should be appended.
;              If not set, the first ANA's result array will be the primary header.
;      SAVE_XDIM1: If set, then the XDIM1 cube will be saved into the FITS file. Default is
;              not to save it. This cube can be recalculated using the WCS parameters given either
;              in HEADER_INPUT_DATA.
;              This keyword can also be an array of zeros and ones,
;              setting/unsetting this feature separately for each window.
;      NO_SAVE_DATA: If set, then the data cube is not saved, only the header.
;              It is then assumed, that HEADER_INPUT_DATA contains a link to the data.
;              This is the same as not providing INPUT_DATA nor PROGENITOR_DATA.
;              This keyword can also be an array of zeros and ones,
;              setting/unsetting this feature separately for each window.
;      PRINT_HEADERS: If set, then all headers created will be printed to the terminal.
;      SAVE_NOT: If set, then the FITS file will not be saved. The optional outputs are created though.
;
; OPTIONAL INPUTS:
;      N_WINDOWS: Total number of windows that will be included in this FITS file.
;              By default, this will be the number of 'ana' structures provided, or 1
;              in case ana is not provided. But if you call this procedure mutliple times
;              with the same filepath_out and EXTENSION keyword set, the procedure can not know
;              what the final total number of windows will be, and thus the header keyword 'NWIN' in the result extension
;              may have the wrong number. This will NOT cause any problems when reading the FITS file
;              with FITS2ANA.
;      WINNO: Window number (starting at 0) of the first 'ana' provided within this study in this FITS file.
;              If you call this procedure mutliple times with the same filepath_out and
;              EXTENSION keyword set, you can define here what the index of the currently provided
;              first 'ana' should be. This will be set in the header keyword 'WINNO' in the result extension.
;              A wrong number in this keyword won't create any problems when reading the FITS file
;              with FITS2ANA. Default is the dataset indices.
;      HEADER_INPUT_DATA: A pointer array or string array, containing the headers of the data extensions as string arrays.
;              One string array per ANA provided. Can be a string array, if only one ANA is provided.
;              This is used to describe the data. WCS parameters should correspond with INPUT_DATA, or with PROGENITOR_DATA respectively.
;      PROGENITOR_DATA: A pointer array of Data Arrays or a data array. Up to 7-dimensional. Absorbed dimensions (e.g. spectra) does not have to be
;              along the first dimension. If these data arrays are provided, they will be saved into the XDIM1 extensions instead of INPUT_DATA.
;              One data array per ANA provided. Can be a data array, if only one ANA is provided.
;      DATA_ID: A string vector of same length as 'ana', or if 'ana' is not provided, same number of windows provided.
;              These strings are used to identify the data, i.e. they will
;              be used in the extension names of the FITS file. Each dataset will get up to
;              6 extensions, which all have the same ID, but the extension name will be
;              'data_id'+' '+extension_type (='results', 'data', 'xdim1', 'weights', 'includes', 'constants').
;              Default is the value of the keyword 'EXTNAME' from HEADER_INPUT_DATA. If this is provided then the data extension
;              will have this EXTNAME (without 'data') as its extension name.
;              If this is not provided then default is the dataset indices.
;      EXT_DATA_PATH: A string array or a string. This contains the relative path to the external extension, which contains
;              the data cube. If this is provided the data is not saved in the new FITS file, but the header is.
;              The header keyword DATAEXT in the headers will get EXT_DATA_PATH as a prefix to point to the external extension.
;              See also Appendix VII aobut External Extensions in https://arxiv.org/abs/2011.12139
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      CREATOR: String. The name of the creator of this FITS file. If not provided this keyword will not be in the header.
;      PROJ_KEYWORDS: A list or array of hashes with entries ('name',xxx1, 'value',xxx2, 'comment',xxx3}
;              where, xxx2 can be a string or a number. These are additional project-related
;              keywords that should be added to the header.
;              This can also be a pointer array, if each window should get their own sets of keywords.
;              It must then be of the same size as the RESULT pointer array or the ANA array.
;      PROC_STEPS: A list, each element stands for one processing step, i.e. gets a new number.
;              Each processing step consists of an array of hashes with entries ('name',xxx1, 'value',xxx2, 'comment',xxx3}
;              where, xxx2 can be a string or a number.
;              The name can be any of the following:
;              PRSTEP|PRPROC|PRPVER|PRMODE|PRPARA|PRREF|PRLOG|PRENV|PRVER|PRHSH|PRBRA|PRLIB
;              PRSTEP should be included. The name and the comment will get the processing step number added.
;              This can also be a pointer array, if each window should get their own sets of keywords.
;              It must then be of the same size as the RESULT pointer array or the ANA array.
;
; OPTIONAL INPUTS/OUTPUTS:
;      All of the following optional inputs must be provided if 'ANA' is not provided.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
;      RESULT: The array to contain the result parameter values (and the Chi^2) values.
;              This may also be a pointer array, if more than one window should be saved at a time.
;      FIT: The component fit structure
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
;
;      All of the following optional inputs may be provided if 'ANA' is not provided. If not
;      provided, it is assumed they contain only default values.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
;      HISTORY: A string array.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with absorbed dimension (e.g. spectra)
;              along the first dimension. This is ignored if PROGENITOR_DATA is provided.
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
;      XDIM1: Array of same size as the input data to xcfit_block. It contains the values of the
;             absorbed dimension for each point (e.g wavelength).
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
;      WEIGHTS: Weights to use in the fitting process.
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
;      INCLUDE: Array to keep the INCLUDE status of each component at each point.
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
;      CONST: Array to keep the CONST status of each parameter at each point.
;              This may also be a pointer array, if more than one window should be saved at a time.
;              It must then be of the same size as the RESULT pointer array.
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
; This procedure saves one or more ANA structures into a FITS file.
;
; OPTIONAL OUTPUTS:
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
;     SPICE library: prits_tools.parcheck, ana2fitshdr
;     GEN library: writefits
;
; HISTORY:
;      Ver. 1, 19-Jan-2022, Martin Wiesmann
;-
; $Id: 2024-11-21 11:37 CET $

PRO ana2fits, ANA, filepath_out = filepath_out, $
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
  save_not = save_not, $
  headers_results = headers_results, headers_data = headers_data, $
  headers_xdim1 = headers_xdim1, headers_weights = headers_weights, $
  headers_include = headers_include, headers_constants = headers_constants
  prits_tools.parcheck, ANA, 1, 'ANA', 'STRUCT', [0, 1], structure_name = 'CFIT_ANALYSIS', /optional
  n_ana = n_elements(ANA)
  prits_tools.parcheck, type_xdim1, 0, 'TYPE_XDIM1', 'STRING', [0, 1]
  prits_tools.parcheck, filepath_out, 0, 'FILEPATH_OUT', 'STRING', 0
  prits_tools.parcheck, n_windows, 0, 'N_WINDOWS', 'INTEGERS', 0, default = max([n_ana, 1])
  prits_tools.parcheck, winno, 0, 'WINNO', 'INTEGERS', 0, default = 0
  prits_tools.parcheck, level, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, version, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional

  result_ptr = 0
  fit_ptr = 0
  in_data_ptr = 0
  xdim1_ptr = 0
  weights_ptr = 0
  incl_ptr = 0
  const_ptr = 0

  prg_data_ptr = 0
  hdr_in_data_ptr = 0
  proc_st_ptr = 0
  proj_kwd_ptr = 0

  IF ~n_ana THEN BEGIN
    prits_tools.parcheck, result, 0, 'RESULT', 'POINTER', [0, 1], result = error
    IF error[0] NE '' THEN prits_tools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7] $
    ELSE result_ptr = n_elements(result)
    IF result_ptr GT 0 THEN n_ana = result_ptr ELSE n_ana = 1

    IF n_ana GT 1 THEN BEGIN
      prits_tools.parcheck, fit, 0, 'FIT', 'POINTER', 1, valid_nelements = n_ana
      prits_tools.parcheck, input_data, 0, 'INPUT_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
      prits_tools.parcheck, xdim1, 0, 'XDIM1', 'POINTER', 1, /optional, valid_nelements = n_ana
      prits_tools.parcheck, weights, 0, 'WEIGHTS', 'POINTER', 1, /optional, valid_nelements = n_ana
      prits_tools.parcheck, include, 0, 'INCLUDE', 'POINTER', 1, /optional, valid_nelements = n_ana
      prits_tools.parcheck, const, 0, 'CONST', 'POINTER', 1, /optional, valid_nelements = n_ana
    ENDIF ELSE BEGIN ; n_ana GT 1

      prits_tools.parcheck, fit, 0, 'FIT', 'POINTER', [0, 1], valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, fit, 0, 'FIT', 'STRUCT', 0 $
      ELSE fit_ptr = n_elements(fit)
      prits_tools.parcheck, input_data, 0, 'INPUT_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE in_data_ptr = n_elements(input_data)
      prits_tools.parcheck, xdim1, 0, 'XDIM1', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, xdim1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], /optional $
      ELSE xdim1_ptr = n_elements(xdim1)
      prits_tools.parcheck, weights, 0, 'WEIGHTS', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, weights, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE weights_ptr = n_elements(weights)
      prits_tools.parcheck, include, 0, 'INCLUDE', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE incl_ptr = n_elements(include)
      prits_tools.parcheck, const, 0, 'CONST', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN prits_tools.parcheck, const, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE const_ptr = n_elements(const)
    ENDELSE ; n_ana GT 1
  ENDIF ; ~n_ana

  IF n_ana GT 1 THEN BEGIN
    prits_tools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
    prg_data_ptr = n_elements(progenitor_data)
    prits_tools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
    hdr_in_data_ptr = n_elements(header_input_data)
  ENDIF ELSE BEGIN ; n_ana GT 1
    prits_tools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
    IF error[0] NE '' THEN prits_tools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
    ELSE prg_data_ptr = n_elements(progenitor_data)
    prits_tools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
    IF error[0] NE '' THEN prits_tools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'STRING', 1, /optional $
    ELSE hdr_in_data_ptr = n_elements(header_input_data)
  ENDELSE ; n_ana GT 1

  prits_tools.parcheck, proc_steps, 0, 'PROC_STEPS', 'POINTER', 1, /optional, valid_nelements = n_ana, result = error
  IF error[0] NE '' THEN prits_tools.parcheck, proc_steps, 0, 'PROC_STEPS', 11, 1, /optional $
  ELSE proc_st_ptr = n_elements(proc_steps)
  prits_tools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', 'POINTER', 1, /optional, valid_nelements = n_ana, result = error
  IF error[0] NE '' THEN prits_tools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional $
  ELSE proj_kwd_ptr = n_elements(proj_keywords)

  prits_tools.parcheck, data_id, 0, 'DATA_ID', 'STRING', [0, 1], valid_nelements = n_ana, /optional

  filename_out = file_basename(filepath_out)

  get_headers = bytarr(6)
  IF arg_present(headers_results) THEN BEGIN
    headers_results = ptrarr(n_ana)
    get_headers[0] = 1
  ENDIF
  IF arg_present(headers_data) THEN BEGIN
    headers_data = ptrarr(n_ana)
    get_headers[1] = 1
  ENDIF
  IF arg_present(headers_xdim1) THEN BEGIN
    headers_xdim1 = ptrarr(n_ana)
    get_headers[2] = 1
  ENDIF
  IF arg_present(headers_weights) THEN BEGIN
    headers_weights = ptrarr(n_ana)
    get_headers[3] = 1
  ENDIF
  IF arg_present(headers_include) THEN BEGIN
    headers_include = ptrarr(n_ana)
    get_headers[4] = 1
  ENDIF
  IF arg_present(headers_constants) THEN BEGIN
    headers_constants = ptrarr(n_ana)
    get_headers[5] = 1
  ENDIF

  n_windows_use = max([n_ana, n_windows])
  FOR iwindow = 0, n_ana - 1 DO BEGIN
    IF result_ptr THEN result_use = *result[iwindow] ELSE IF n_elements(result) GT 0 THEN result_use = result
    IF fit_ptr THEN fit_use = *fit[iwindow] ELSE IF n_elements(fit) GT 0 THEN fit_use = fit
    IF in_data_ptr THEN INPUT_DATA_use = *input_data[iwindow] ELSE IF n_elements(input_data) GT 0 THEN INPUT_DATA_use = input_data
    IF xdim1_ptr THEN xdim1_use = *xdim1[iwindow] ELSE IF n_elements(xdim1) GT 0 THEN xdim1_use = xdim1
    IF weights_ptr THEN weights_use = *weights[iwindow] ELSE IF n_elements(weights) GT 0 THEN weights_use = weights
    IF incl_ptr THEN include_use = *include[iwindow] ELSE IF n_elements(include) GT 0 THEN include_use = include
    IF const_ptr THEN const_use = *const[iwindow] ELSE IF n_elements(const) GT 0 THEN const_use = const

    IF prg_data_ptr THEN PROGENITOR_DATA_use = *progenitor_data[iwindow] ELSE IF n_elements(progenitor_data) GT 0 THEN PROGENITOR_DATA_use = progenitor_data
    IF hdr_in_data_ptr THEN HEADER_INPUT_DATA_use = *header_input_data[iwindow] ELSE IF n_elements(header_input_data) GT 0 THEN HEADER_INPUT_DATA_use = header_input_data
    IF proc_st_ptr THEN PROC_STEPS_use = *proc_steps[iwindow] ELSE IF n_elements(proc_steps) GT 0 THEN PROC_STEPS_use = proc_steps
    IF proj_kwd_ptr THEN PROJ_KEYWORDS_use = *proj_keywords[iwindow] ELSE IF n_elements(proj_keywords) GT 0 THEN PROJ_KEYWORDS_use = proj_keywords

    IF keyword_set(data_id) THEN data_id_use = data_id[iwindow]
    IF n_elements(type_xdim1) GT 1 THEN TYPE_XDIM1_use = type_xdim1[iwindow] ELSE TYPE_XDIM1_use = type_xdim1
    IF n_elements(no_save_data) GT 1 THEN NO_SAVE_DATA_use = no_save_data[iwindow] ELSE $
      IF n_elements(no_save_data) EQ 1 THEN NO_SAVE_DATA_use = no_save_data
    IF n_elements(save_xdim1) GT 1 THEN SAVE_XDIM1_use = save_xdim1[iwindow] ELSE $
      IF n_elements(save_xdim1) EQ 1 THEN SAVE_XDIM1_use = save_xdim1
    IF n_elements(ext_data_path) GT 1 THEN EXT_DATA_PATH_use = ext_data_path[iwindow] ELSE $
      IF n_elements(ext_data_path) EQ 1 THEN EXT_DATA_PATH_use = ext_data_path

    extension = keyword_set(is_extension) || iwindow GT 0

    IF n_elements(ANA) THEN BEGIN
      headers = ana2fitshdr(ANA[iwindow], filename_out = filename_out, $
        n_windows = n_windows_use, winno = winno + iwindow, $
        data_id = data_id_use, type_xdim1 = TYPE_XDIM1_use, $
        ext_data_path = EXT_DATA_PATH_use, $
        is_extension = extension, level = level, version = version, creator = creator, $
        proc_steps = PROC_STEPS_use, proj_keywords = PROJ_KEYWORDS_use, $
        xdim1 = xdim1_use, input_data = INPUT_DATA_use, fit = fit_use, $
        result = result_use, residual = residual, weights = weights_use, include = include_use, $
        const = const_use, filename_ana = filename_ana, datasource = datasource, $
        definition = definition, missing = missing, label = label, history = history, $
        progenitor_data = PROGENITOR_DATA_use, header_input_data = HEADER_INPUT_DATA_use, $
        save_xdim1 = SAVE_XDIM1_use, no_save_data = NO_SAVE_DATA_use, print_headers = print_headers, $
        data_array = DATA_ARRAY)
    ENDIF ELSE BEGIN
      headers = ana2fitshdr(filename_out = filename_out, $
        n_windows = n_windows_use, winno = winno + iwindow, $
        data_id = data_id_use, type_xdim1 = TYPE_XDIM1_use, $
        ext_data_path = EXT_DATA_PATH_use, $
        is_extension = extension, level = level, version = version, creator = creator, $
        proc_steps = PROC_STEPS_use, proj_keywords = PROJ_KEYWORDS_use, $
        xdim1 = xdim1_use, input_data = INPUT_DATA_use, fit = fit_use, $
        result = result_use, residual = residual, weights = weights_use, include = include_use, $
        const = const_use, filename_ana = filename_ana, datasource = datasource, $
        definition = definition, missing = missing, label = label, history = history, $
        progenitor_data = PROGENITOR_DATA_use, header_input_data = HEADER_INPUT_DATA_use, $
        save_xdim1 = SAVE_XDIM1_use, no_save_data = NO_SAVE_DATA_use, print_headers = print_headers, $
        data_array = DATA_ARRAY)
    ENDELSE

    IF ~keyword_set(save_not) THEN BEGIN
      writefits, filepath_out, result_use, *headers[0], append = extension
      IF (*headers[1])[0] NE '' THEN $
        writefits, filepath_out, DATA_ARRAY, *headers[1], /append
      IF (*headers[2])[0] NE '' THEN $
        writefits, filepath_out, xdim1_use, *headers[2], /append
      IF (*headers[3])[0] NE '' THEN $
        writefits, filepath_out, weights_use, *headers[3], /append
      IF (*headers[4])[0] NE '' THEN $
        writefits, filepath_out, include_use, *headers[4], /append
      IF (*headers[5])[0] NE '' THEN $
        writefits, filepath_out, const_use, *headers[5], /append
    ENDIF

    IF get_headers[0] THEN headers_results[iwindow] = ptr_new(*headers[0])
    IF get_headers[1] THEN headers_data[iwindow] = ptr_new(*headers[1])
    IF get_headers[2] THEN headers_xdim1[iwindow] = ptr_new(*headers[2])
    IF get_headers[3] THEN headers_weights[iwindow] = ptr_new(*headers[3])
    IF get_headers[4] THEN headers_include[iwindow] = ptr_new(*headers[4])
    IF get_headers[5] THEN headers_constants[iwindow] = ptr_new(*headers[5])
  ENDFOR ; iwindow=0,n_windows-1
END