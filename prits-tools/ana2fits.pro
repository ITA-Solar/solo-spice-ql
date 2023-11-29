;+
; NAME:
;      ANA2FITS
;
; PURPOSE:
;      This procedure saves the content of one or more ANA structures into one FITS file.
;      The FITS file will contain up to 6 extension per ANA, where the first contains the results,
;      and the fit components as header keywords. The resulting FITS file can be converted
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
;      ANA: The name and path of an ANA file or an ANA object.
;              If this is not provided, then all of the optional inputs
;              must be provided. If more than one ANA should be saved into one FITS file,
;              then 'ana' must be provided as an array of either file paths or objects.
;      FILEPATH_OUT: Full path and filename of the resulting FITS file.
;      N_WINDOWS: Total number of windows that will be included in this FITS file.
;              By default, this will be the number of 'ana' structures provided, or 1
;              in case ana is not provided. But if you call this procedure mutliple times
;              with the same filepath_out and EXTENSION keyword set, the procedure can not know
;              what the final total number of windows will be, and thus the header keyword 'NWIN' in the result extension
;              will have the wrong number. This will cause problems when reading the FITS file
;              with FITS2ANA. It is therefore highly recommended to provide this number,
;              when it is planned to add windows to the FITS file in different sessions.
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
;      HEADER_INPUT_DATA: A pointer array or string array, containing the headers of the data extensions as string arrays.
;              One string array per ANA provided. Can be a string array, if only one ANA is provided.
;              This is used to describe the data. WCS parameters should correspond with INPUT_DATA, or with PROGENITOR_DATA respectively.
;      PROGENITOR_DATA: A pointer array of Data Arrays or a data array. Up to 7-dimensional. Absorbed dimensions (e.g. spectra) does not have to be
;              alog the first dimension. If these data arrays are provided, they will be saved into the XDIM1 extensions instead of INPUT_DATA.
;              One data array per ANA provided. Can be a string array, if only one ANA is provided.
;      DATA_ID: A string vector of same length as 'ana', or if 'ana' is not provided, same number of windows provided.
;              These strings are used to identify the data, i.e. they will
;              be used in the extension names of the FITS file. Each dataset will get
;              6 extensions, which all have the same ID, but the extension name will be
;              'data_id'+' '+extension_type (='results', 'data', 'xdim1', 'weights', 'includes', 'constants').
;              Default is the dataset indices.
;      WINNO: Window number (starting at 0) of the first 'ana' provided within this study in this FITS file.
;              If you call this procedure mutliple times with the same filepath_out and
;              EXTENSION keyword set, you can define here what the index of the currently provided
;              first 'ana' should be. This will be set in the header keyword 'WINNO' in the result extension.
;              A wrong number in this keyword won't create any problems when reading the FITS file
;              with FITS2ANA. Default is the dataset indices.
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      PROJ_KEYWORDS: A list or array of structures of type {name:'', value:'', comment:''} with additional project-related
;              keywords that should be added to the header.
;              This can also be a pointer array, if each window should get their own sets of keywords.
;              It must then be of the same size as the RESULT pointer array or the ANA array.
;      PROC_STEPS: A list, each element stands for one processing step, i.e. gets a new number.
;              Each processing step consists of an array of hashes with entries ('name',xxx1, 'value',xxx2, 'comment',xxx3}
;              where, xxx123 can be a string or a number.
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
;      All of the following optional inputs can be provided if 'ANA' is not provided. If not
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
;     prits_tools.parcheck, ana2fitshdr, writefits
;
; RESTRICTIONS:
; It is possible to call this procedure multiple times with the same filepath_out,
; if in these cases the EXTENSION keyword is set, the windows will be appended to the
; existing FITS file. However, one needs to
;
; MAKE SURE THAT THE HEADER KEYWORD "NWIN" IS CORRECTLY SET.
;
; See description of N_WINDOWS for more details.
;
; HISTORY:
;      Ver. 1, 19-Jan-2022, Martin Wiesmann
;-
; $Id: 2023-11-29 14:26 CET $


PRO ana2fits, ANA, FILEPATH_OUT=FILEPATH_OUT, $
  N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
  DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
  IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
  PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
  XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
  RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
  PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
  SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
  SAVE_NOT=SAVE_NOT, $
  headers_results=headers_results, headers_data=headers_data, $
  headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
  headers_include=headers_include, headers_constants=headers_constants

  prits_tools.parcheck, ANA, 1, 'ANA', 'STRUCT', [0, 1], structure_name='CFIT_ANALYSIS', /optional
  n_ana = N_ELEMENTS(ANA)
  prits_tools.parcheck, TYPE_XDIM1, 0, 'TYPE_XDIM1', 'STRING', 0
  prits_tools.parcheck, FILEPATH_OUT, 0, 'FILEPATH_OUT', 'STRING', 0
  prits_tools.parcheck, N_WINDOWS, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, WINNO, 0, 'WINNO', 'INTEGERS', 0, default=0
  prits_tools.parcheck, LEVEL, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, VERSION, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional

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

    prits_tools.parcheck, RESULT, 0, 'RESULT', 'POINTER', [0, 1], result=error
    IF error[0] NE '' THEN prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7] $
    ELSE result_ptr = N_ELEMENTS(RESULT)
    IF result_ptr GT 0 THEN n_ana = result_ptr ELSE n_ana = 1

    IF n_ana GT 1 THEN BEGIN

      prits_tools.parcheck, FIT, 0, 'FIT', 'POINTER', 1, VALID_NELEMENTS=n_ana
      prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
      prits_tools.parcheck, XDIM1, 0, 'XDIM1', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
      prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
      prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
      prits_tools.parcheck, CONST, 0, 'CONST', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana

    ENDIF ELSE BEGIN ; n_ana GT 1

      prits_tools.parcheck, FIT, 0, 'FIT', 'POINTER', [0, 1], VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0 $
      ELSE fit_ptr = N_ELEMENTS(FIT)
      prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE in_data_ptr = N_ELEMENTS(INPUT_DATA)
      prits_tools.parcheck, XDIM1, 0, 'XDIM1', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, XDIM1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], /optional $
      ELSE xdim1_ptr = N_ELEMENTS(XDIM1)
      prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE weights_ptr = N_ELEMENTS(WEIGHTS)
      prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE incl_ptr = N_ELEMENTS(INCLUDE)
      prits_tools.parcheck, CONST, 0, 'CONST', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
      IF error[0] NE '' THEN prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE const_ptr = N_ELEMENTS(CONST)

    ENDELSE ; n_ana GT 1

  ENDIF ; ~n_ana

  IF n_ana GT 1 THEN BEGIN
    prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
    prg_data_ptr = N_ELEMENTS(PROGENITOR_DATA)
    prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADER_INPUT_DATA', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana
    hdr_in_data_ptr = N_ELEMENTS(HEADER_INPUT_DATA)
  ENDIF ELSE BEGIN ; n_ana GT 1
    prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
    IF error[0] NE '' THEN prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
    ELSE prg_data_ptr = N_ELEMENTS(PROGENITOR_DATA)
    prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADER_INPUT_DATA', 'POINTER', [0, 1], /optional, VALID_NELEMENTS=n_ana, result=error
    IF error[0] NE '' THEN prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADER_INPUT_DATA', 'STRING', 1, /optional $
    ELSE hdr_in_data_ptr = N_ELEMENTS(HEADER_INPUT_DATA)
  ENDELSE ; n_ana GT 1

  prits_tools.parcheck, PROC_STEPS, 0, 'PROC_STEPS', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana, result=error
  IF error[0] NE '' THEN prits_tools.parcheck, PROC_STEPS, 0, 'PROC_STEPS', 11, 1, /optional $
  ELSE proc_st_ptr = N_ELEMENTS(PROC_STEPS)
  prits_tools.parcheck, PROJ_KEYWORDS, 0, 'PROJ_KEYWORDS', 'POINTER', 1, /optional, VALID_NELEMENTS=n_ana, result=error
  IF error[0] NE '' THEN   prits_tools.parcheck, PROJ_KEYWORDS, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional $
  ELSE proj_kwd_ptr = N_ELEMENTS(PROJ_KEYWORDS)

  prits_tools.parcheck, DATA_ID, 0, 'DATA_ID', 'STRING', [0, 1], VALID_NELEMENTS=max([1, n_ana]), $
    default=strtrim(indgen(n_ana), 2)


  filename_out = file_basename(filepath_out)

  get_headers = bytarr(6)
  if arg_present(headers_results) then begin
    headers_results = ptrarr(n_ana)
    get_headers[0] = 1
  endif
  if arg_present(headers_data) then begin
    headers_data = ptrarr(n_ana)
    get_headers[1] = 1
  endif
  if arg_present(headers_xdim1) then begin
    headers_xdim1 = ptrarr(n_ana)
    get_headers[2] = 1
  endif
  if arg_present(headers_weights) then begin
    headers_weights = ptrarr(n_ana)
    get_headers[3] = 1
  endif
  if arg_present(headers_include) then begin
    headers_include = ptrarr(n_ana)
    get_headers[4] = 1
  endif
  if arg_present(headers_constants) then begin
    headers_constants = ptrarr(n_ana)
    get_headers[5] = 1
  endif

  n_windows_use = max([n_ana, n_windows])
  for iwindow=0,n_ana-1 do begin

    IF result_ptr THEN result_use = *result[iwindow] ELSE IF N_ELEMENTS(result) GT 0 THEN result_use = result
    IF fit_ptr THEN fit_use = *fit[iwindow] ELSE IF N_ELEMENTS(fit) GT 0 THEN fit_use = fit
    IF in_data_ptr THEN INPUT_DATA_use = *INPUT_DATA[iwindow] ELSE IF N_ELEMENTS(input_data) GT 0 THEN input_data_use = input_data
    IF xdim1_ptr THEN xdim1_use = *xdim1[iwindow] ELSE IF N_ELEMENTS(xdim1) GT 0 THEN xdim1_use = xdim1
    IF weights_ptr THEN weights_use = *weights[iwindow] ELSE IF N_ELEMENTS(weights) GT 0 THEN weights_use = weights
    IF incl_ptr THEN include_use = *include[iwindow] ELSE IF N_ELEMENTS(include) GT 0 THEN include_use = include
    IF const_ptr THEN const_use = *const[iwindow] ELSE IF N_ELEMENTS(const) GT 0 THEN const_use = const

    IF prg_data_ptr THEN PROGENITOR_DATA_use = *PROGENITOR_DATA[iwindow] ELSE IF N_ELEMENTS(PROGENITOR_DATA) GT 0 THEN PROGENITOR_DATA_use = PROGENITOR_DATA
    IF hdr_in_data_ptr THEN HEADER_INPUT_DATA_use = *HEADER_INPUT_DATA[iwindow] ELSE IF N_ELEMENTS(HEADER_INPUT_DATA) GT 0 THEN HEADER_INPUT_DATA_use = HEADER_INPUT_DATA
    IF proc_st_ptr THEN PROC_STEPS_use = *PROC_STEPS[iwindow] ELSE IF N_ELEMENTS(PROC_STEPS) GT 0 THEN PROC_STEPS_use = PROC_STEPS
    IF proj_kwd_ptr THEN PROJ_KEYWORDS_use = *PROJ_KEYWORDS[iwindow] ELSE IF N_ELEMENTS(PROJ_KEYWORDS) GT 0 THEN PROJ_KEYWORDS_use = PROJ_KEYWORDS

    extension = keyword_set(IS_EXTENSION) || iwindow GT 0
    
    if N_ELEMENTS(ana) then begin
      headers = ana2fitshdr(ana[iwindow], FILENAME_OUT=FILENAME_OUT, $
        N_WINDOWS=n_windows_use, WINNO=WINNO+iwindow, $
        DATA_ID=DATA_ID[iwindow], TYPE_XDIM1=TYPE_XDIM1, $
        IS_EXTENSION=extension, LEVEL=LEVEL, VERSION=VERSION, $
        PROC_STEPS=PROC_STEPS_use, PROJ_KEYWORDS=PROJ_KEYWORDS_use, $
        XDIM1=xdim1_use, INPUT_DATA=INPUT_DATA_use, FIT=fit_use, $
        RESULT=result_use, RESIDUAL=RESIDUAL, WEIGHTS=weights_use, INCLUDE=include_use, $
        CONST=const_use, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
        PROGENITOR_DATA=PROGENITOR_DATA_use, HEADER_INPUT_DATA=HEADER_INPUT_DATA_use, $
        SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
        DATA_ARRAY=DATA_ARRAY)
    endif else begin
      headers = ana2fitshdr( FILENAME_OUT=FILENAME_OUT, $
        N_WINDOWS=n_windows_use, WINNO=WINNO+iwindow, $
        DATA_ID=DATA_ID[iwindow], TYPE_XDIM1=TYPE_XDIM1, $
        IS_EXTENSION=extension, LEVEL=LEVEL, VERSION=VERSION, $
        PROC_STEPS=PROC_STEPS_use, PROJ_KEYWORDS=PROJ_KEYWORDS_use, $
        XDIM1=xdim1_use, INPUT_DATA=INPUT_DATA_use, FIT=fit_use, $
        RESULT=result_use, RESIDUAL=RESIDUAL, WEIGHTS=weights_use, INCLUDE=include_use, $
        CONST=const_use, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
        PROGENITOR_DATA=PROGENITOR_DATA_use, HEADER_INPUT_DATA=HEADER_INPUT_DATA_use, $
        SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
        DATA_ARRAY=DATA_ARRAY)
    endelse

    IF ~keyword_set(save_not) THEN BEGIN
      writefits, filepath_out, result_use, *headers[0], append=extension
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
    endif

    if get_headers[0] then headers_results[iwindow] = ptr_new(*headers[0])
    if get_headers[1] then headers_data[iwindow] = ptr_new(*headers[1])
    if get_headers[2] then headers_xdim1[iwindow] = ptr_new(*headers[2])
    if get_headers[3] then headers_weights[iwindow] = ptr_new(*headers[3])
    if get_headers[4] then headers_include[iwindow] = ptr_new(*headers[4])
    if get_headers[5] then headers_constants[iwindow] = ptr_new(*headers[5])

  endfor ; iwindow=0,n_windows-1

END
