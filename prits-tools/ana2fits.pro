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
;      XDIM1_TYPE: CTYPE of the absorbed dimension (e.g. 'WAVE'). A string array, or a scalar, in which case
;              the same value will be used for all windows.
;
; KEYWORDS:
;      EXTENSION: If set, then the first ANA's result array will be an extension,
;              i.e. this should be set if the FITS file already exists and data should be appended.
;              If not set, the first ANA's result array will be the primary header.
;      SAVE_XDIM1: If set, then the XDIM1 cube will be saved into the FITS file. Default is
;              not to save it. This cube can be recalculated using the WCS parameters given either
;              in HEADERS_INPUT_XDIM1 or in HEADERS_INPUT_DATA.
;      PRINT_HEADERS: If set, then all headers created will be printed to the terminal.
;
; OPTIONAL INPUTS:
;      HEADERS_INPUT_XDIM1: A pointer array or string array, containing the headers of the absorbed dimension extensions as string arrays.
;              One string array per ANA provided. Can be a string array, if ANA is scalar or not provided.
;              This is used to save the WCS parameters to recreate the XDIM1 cube when loading the FITS file again.
;              This is only required if HEADERS_INPUT_DATA is not provided, or if it doesn't (fully) contain the WCS parameters.
;      HEADERS_INPUT_DATA: A pointer array or string array, containing the headers of the data extensions as string arrays.
;              One string array per ANA provided. Can be a string array, if ANA is scalar or not provided.
;              This is used to describe the data. WCS parameters should correspond with INPUT_DATA, or with PROGENITOR_DATA respectively.
;      PROGENITOR_DATA: A pointer array of Data Arrays or a data array. Up to 7-dimensional. Absorbed dimensions (e.g. spectra) does not have to be
;              alog the first dimension. If these data arrays are provided, they will be saved into the XDIM1 extensions instead of INPUT_DATA.
;              One data array per ANA provided. Can be a string array, if ANA is scalar or not provided.
;      DATA_ID: A string vector of same length as 'ana', or if 'ana' is not provided
;              scalar string. These strings are used to identify the data, i.e. they will
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
;
; OPTIONAL INPUTS/OUTPUTS:
;      All of the following optional inputs, except RESIDUAL, must be provided if 'ANA' is not provided. 
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;      
;      HISTORY: A string array.
;      LAMBDA: An array of wavelength values. Either one value for
;              every point in the data array, or a one-dimensional
;              array to go with all the spectra in the data array or same size as INPUT_DATA.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with absorbed dimension (e.g. spectra)
;              along the first dimension.
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;       
;      All of the following optional inputs can be provided if 'ANA' is not provided. If not
;      provided, it is assumed they contain only default values.
;      If 'ANA' is provided, they will be overwritten and can be used as OPTIONAL OUTPUT.
;
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
; This procedure saves one or more ANA structures into a FITS file.
; 
; OPTIONAL OUTPUT:
;     headers_results: A pointer array, containing the headers of the results extensions as string arrays.
;              One string array per ANA provided.
;     headers_data: A pointer array, containing the headers of the data extensions as string arrays.
;              One string array per ANA provided.
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
; $Id: 2023-11-15 15:32 CET $


PRO ana2fits, ANA, FILEPATH_OUT=FILEPATH_OUT, $
  N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
  DATA_ID=DATA_ID, XDIM1_TYPE=XDIM1_TYPE, $
  EXTENSION=EXTENSION, $
  LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, FIT=FIT, $
  RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
  PROGENITOR_DATA=PROGENITOR_DATA, HEADERS_INPUT_DATA=HEADERS_INPUT_DATA, $
  SAVE_XDIM1=SAVE_XDIM1, HEADERS_INPUT_XDIM1=HEADERS_INPUT_XDIM1, $
  PRINT_HEADERS=PRINT_HEADERS, $
  headers_results=headers_results, headers_data=headers_data, $
  headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
  headers_include=headers_include, headers_constants=headers_constants

  prits_tools.parcheck, ANA, 1, 'ANA', 'STRUCT', [0, 1], structure_name='CFIT_ANALYSIS', /optional
  ana_given = N_ELEMENTS(ANA)
  prits_tools.parcheck, HISTORY, 0, 'HISTORY', 'STRING', 1, optional=ana_given
  prits_tools.parcheck, LAMBDA, 0, 'LAMBDA', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given || N_ELEMENTS(PROGENITOR_DATA)
  prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0, optional=ana_given
  prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, RESIDUAL, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, FILENAME_ANA, 0, 'FILENAME_ANA', 'STRING', 0, optional=1
  prits_tools.parcheck, DATASOURCE, 0, 'DATASOURCE', 'STRING', 0, optional=1
  prits_tools.parcheck, DEFINITION, 0, 'DEFINITION', 'STRING', 0, optional=1
  prits_tools.parcheck, MISSING, 0, 'MISSING', 'NUMERIC', 0, optional=1
  prits_tools.parcheck, LABEL, 0, 'LABEL', 'STRING', 0, optional=1

  prits_tools.parcheck, FILEPATH_OUT, 0, 'FILEPATH_OUT', 'STRING', 0
  prits_tools.parcheck, N_WINDOWS, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, WINNO, 0, 'WINNO', 'INTEGERS', 0, default=indgen(max([1, ana_given]))
  prits_tools.parcheck, DATA_ID, 0, 'DATA_ID', 'STRING', [0, 1], VALID_NELEMENTS=max([1, ana_given]), $
    default=strtrim(indgen(max([1, ana_given])), 2)
  prits_tools.parcheck, XDIM1_TYPE, 0, 'XDIM1_TYPE', 'STRING', 0

  filename_out = file_basename(filepath_out)

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

  n_ana = N_ELEMENTS(ana)
  if n_ana eq 0 then n_ana=1
  n_windows = max([n_ana, n_windows])
  for iwindow=0,n_ana-1 do begin
    input_type = size(ana[iwindow], /type)

    extension_win = keyword_set(extension) || iwindow GT 0

    if input_type then begin
      headers = ana2fitshdr(ana[iwindow], filename_out=filename_out, n_windows=n_windows, $
        winno=iwindow+winno, data_id=data_id[iwindow], extension=extension_win, $
        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, print_headers=print_headers)
    endif else begin
      headers = ana2fitshdr(filename_out=filename_out, n_windows=n_windows, $
        winno=iwindow+winno, data_id=data_id[iwindow], extension=extension_win, $
        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, print_headers=print_headers)
    endelse

    writefits, filepath_out, RESULT, *headers[0], append=extension_win
    writefits, filepath_out, INPUT_DATA, *headers[1], /append
    writefits, filepath_out, LAMBDA, *headers[2], /append
    IF (*headers[3])[0] NE '' THEN $
      writefits, filepath_out, WEIGHTS, *headers[3], /append
    IF (*headers[4])[0] NE '' THEN $
      writefits, filepath_out, INCLUDE, *headers[4], /append
    IF (*headers[5])[0] NE '' THEN $
      writefits, filepath_out, CONST, *headers[5], /append

    if get_headers[0] then headers_results[iwindow] = ptr_new(*headers[0])
    if get_headers[1] then headers_data[iwindow] = ptr_new(*headers[1])
    if get_headers[2] then headers_xdim1[iwindow] = ptr_new(*headers[2])
    if get_headers[3] then headers_weights[iwindow] = ptr_new(*headers[3])
    if get_headers[4] then headers_include[iwindow] = ptr_new(*headers[4])
    if get_headers[5] then headers_constants[iwindow] = ptr_new(*headers[5])

  endfor ; iwindow=0,n_windows-1

END
