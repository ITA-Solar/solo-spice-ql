;+
; NAME:
;      ANA2FITS
;
; PURPOSE:
;      This procedure saves the content of one or more ANA structures into a
;      [level P FITS file](https://solarnet-metadata.readthedocs.io/en/latest/generated/appendix-9.html).
;      The FITS file will contain up to 6 extensions per ANA, where the first contains the results and
;      the fit components as header keywords. The resulting FITS file can be converted into one or more
;      ANA structures with the procedure FITS2ANA.
;
;      The input can be either an ANA structure (see also mk_analysis() documentation), or an array of such
;      structures, or the input can be provided as separate data cubes,
;      see paragraph 'OPTIONAL INPUTS/OUTPUTS' for more details.
;
;      It is strongly recommended that the input [[HEADER_INPUT_DATA|ANA2FITS#HEADER_INPUT_DATA]] is provided.
;      This is needed to get the WCS parameters of the original data. The WCS is required to recreate the
;      XDIM1 cube, which contains the values of the absorbed dimension (i.e. the wavelength) for each pixel
;      of the data cube. This, in turn, is required for the CFIT routines. Without this cube, any adjustments
;       to the fit components or further fitting won't be possible.
;
;      By default, the original data cube is not saved into the level P FITS file, but referenced to as an
;      external extension. See also [Solarnet recommendations](https://solarnet-metadata.readthedocs.io/en/latest/generated/appendix-7.html).
;      The keyword DATA_EXT_PATH must be provided if the data is not saved in this FITS file. This saves disk space,
;      but makes the user of the level P FITS file dependent on having the original data available. The data cube
;      can be saved into the FITS file by setting the keyword SAVE_DATA.
;
;      It is possible to call this procedure multiple times with the same filepath_out. The windows will then be
;      appended to the existing FITS file, if the keyword IS_EXTENSION is set, otherwise the FITS file will be
;      overwritten. However, one needs to **MAKE SURE THAT THE HEADER KEYWORDS "N_WINDOWS" AND "WINNO" ARE
;      CORRECTLY SET.**
;      See description of N_WINDOWS and WINNO for more details.
;
;      The structure and keywords will be the same as a [[SPICE level 3 FITS file|Level-3-FITS-file#structure-of-a-level-3-spice-fits-file]].
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;         see procedure definition
;
; INPUTS:
;      ANA: An ANA object. (see also mk_analysis() documentation)
;              If this is not provided, then at the least RESULTS and FIT
;              must be provided. If more than one ANA should be saved into one FITS file,
;              then 'ana' must be provided as an array of objects.
;              Alternatively, this procedure can be called multiple times with the same filepath_out
;              and the IS_EXTENSION keyword set. In this case, the new data will be appended to the
;              existing FITS file. The first ANA's result array will be the primary header.
;      FILEPATH_OUT: Full path and filename of the resulting FITS file.
;
; KEYWORDS:
;      IS_EXTENSION: If set, the first ANA's result array will be an extension,
;              i.e. this should be set if the FITS file already exists and data should be appended.
;              If not set, the first ANA's result array will be the primary header.
;      SAVE_DATA: If set, the data cube is saved into the data extension.
;              The default is not to save it and to use the external extension mechanism instead.
;              See description of DATA_EXT_PATH for more details.
;              This keyword can also be an array of zeros and ones,
;              setting/unsetting this feature separately for each window.
;      SAVE_RESIDUALS: If set, the residuals will be saved into the FITS file. The default is
;              not to save it. This cube can be recalculated using the original data and the fit parameter.
;              To save the residuals can be useful in case the original data may not be available when reading the file.
;              This keyword can also be an array of zeros and ones,
;              setting/unsetting this feature separately for each window.
;      PRINT_HEADERS: If set, all headers created will be printed to the terminal.
;      SAVE_NOT: If set, the FITS file will not be saved. The optional outputs are created though.
;
; SEMI-OPTIONAL INPUTS:
;      HEADER_INPUT_DATA: Strongly recommended.
;              A pointer array or string array, containing the header(s) of the data extensions as string arrays.
;              One string array per ANA provided. Can be a string array, if only one ANA is provided.
;              This is used to describe the data. WCS parameters should correspond with INPUT_DATA or with PROGENITOR_DATA, respectively.
;              These parameters will be used to create the WCS structure of the data cube and the WCS keywords in the header.
;              They will also be used to recreate the XDIM1 cube.
;              This header will be saved into the data extension even if the data is not saved.
;      DATA_EXT_PATH: Required if SAVE_DATA is NOT set, otherwise recommended.
;              A string array or a string. This contains the relative path to and the name of the file that contains
;              the original data cube from which the P-level data was calculated.
;              The extension name of the original data cube MUST NOT be included. This name will be taken from DATA_ID.
;              The path and extension name will be used in the header keyword PARENTXT.
;              In case the data cube is not saved into the FITS file, but linked to an external extension, the header keyword DATAEXT in the headers will
;              point to the external extension.
;              See also Appendix VII about External Extensions in https://solarnet-metadata.readthedocs.io/en/latest/generated/appendix-7.html
;      XTYPE1: CTYPE of the absorbed dimension. A string array, or a scalar, in which case
;              the same value will be used for all windows. The default is 'WAVE' (i.e. wavelength).
;      XDIMEN1: The dimension number(s), counting left to right starting with 1, of dimensions
;              that was absorbed/removed during the fitting process (for SPICE Level 3 P files XDIMEN1=3).
;              An array of integers, or a scalar, in which case the same value will be used for all windows.
;              The default is the dimension number that corresponds to XTYPE1 found in HEADER_INPUT_DATA.
;      N_WINDOWS: Total number of windows that will be included in this FITS file.
;              By default, this will be the number of 'ana' structures or the number of RESULT pointers provided, or 1
;              in case RESULT is an array.
;              BUT if you call this procedure multiple times with the same FILEPATH_OUT and IS_EXTENSION keyword set, the procedure can not know
;              what the final total number of windows will be, and thus the header keyword 'NWIN' in the result extension
;              may have the wrong number. This will NOT cause any problems when reading the FITS file
;              with FITS2ANA. But you may want to have the correct number in the header.
;      WINNO: Window number (starting at 0) of the first 'ana' provided within this study in this FITS file.
;              If you call this procedure multiple times with the same FILEPATH_OUT and
;              IS_EXTENSION keyword set, you can define here what the index of the currently provided
;              first 'ana' should be. This will be set in the header keyword 'WINNO' in the result extension.
;              A wrong number in this keyword won't create any problems when reading the FITS file
;              with FITS2ANA. But you may want to have the correct number in the header. The default is the dataset indices.
;
; OPTIONAL INPUTS:
;      DATA_ID: A string vector of same length as 'ana', or if 'ana' is not provided, same number of windows provided.
;              These strings are used to identify the data, i.e. they will
;              be used in the extension names of the FITS file. Each dataset will get up to
;              6 extensions, which all have the same ID, but the extension name will be
;              'data_id'+' '+extension_type (='results', 'data', 'xdim1', 'weights', 'includes', 'constants').
;              The default is the value of the keyword 'EXTNAME' from HEADER_INPUT_DATA. If this header is provided, then the data extension
;              will have this EXTNAME (without 'data') as its extension name.
;              If this is not provided, then the default is the window index.
;      PROGENITOR_DATA: A pointer array of Data Arrays or a data array. Up to 7-dimensional. Absorbed dimensions (e.g. spectra) does not have to be
;              along the first dimension. If these data arrays are provided, they will be saved into the DATA extensions instead of INPUT_DATA
;              if the keyword SAVE_DATA is set.
;              One data array per ANA provided. Can be a data array if only one ANA is provided.
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      CREATOR: String. The name of the creator of this FITS file. If not provided this keyword will not be in the header.
;      SIGMADAT: String. The function that is used to calculate sigma (= 1/sqrt(WEIGHT)). This function is also used when
;              reading the level P FITS file with FITS2ANA. However, so far this has not been implemented and for
;              SPICE we only check that the function is correct. Eventually, we may implement this.
;              The function should use IDL syntax and use existing header keywords as variables. DATA is allowed to be used as a variable.
;              The required header keywords can be added using the keyword PROJ_KEYWORDS.
;              If this is provided, the WEIGHT cube will not be saved in the FITS file, even if the values are not identical.
;              If not provided this keyword will not be in the header.
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
;      RESIDUAL: Array to contain the residual. Same size as INPUT_DATA.
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
;      XDIM1: Array of same size as the input data to xcfit_block. It contains the values of the
;             absorbed dimension for each point (e.g wavelength). This can be calculated by using
;             the WCS structure of the data cube and is thus not saved in the FITS file.
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
;     headers_weights: A pointer array, containing the headers of the weights extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_includes: A pointer array, containing the headers of the include extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_constants: A pointer array, containing the headers of the constants extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;     headers_residuals: A pointer array, containing the headers of the residuals extensions as string arrays.
;              One string array per ANA provided. May be empty strings if this extension was not saved.
;
; CALLS:
;     SPICE library: ptools.parcheck, ana2fitshdr
;     GEN library: writefits
;
; HISTORY:
;      Ver. 1, 19-Jan-2022, Martin Wiesmann (prits-group@astro.uio.no)
;-
; $Id: 2025-07-31 13:25 CEST $

PRO ana2fits, ANA, filepath_out = filepath_out, $
  header_input_data = header_input_data, $
  DATA_EXT_PATH = DATA_EXT_PATH, $
  XTYPE1 = XTYPE1, XDIMEN1 = XDIMEN1, data_id = data_id, $
  is_extension = is_extension, n_windows = n_windows, winno = winno, $
  level = level, version = version, creator = creator, SIGMADAT = SIGMADAT, $
  proc_steps = proc_steps, proj_keywords = proj_keywords, $
  SAVE_RESIDUALS = SAVE_RESIDUALS, SAVE_DATA = SAVE_DATA, print_headers = print_headers, $
  save_not = save_not, $
  result = result, fit = fit, $
  progenitor_data = progenitor_data, input_data = input_data, $
  xdim1 = xdim1, residual = residual, weights = weights, $
  include = include, const = const, $
  filename_ana = filename_ana, datasource = datasource, $
  definition = definition, missing = missing, label = label, history = history, $
  headers_results = headers_results, headers_data = headers_data, $
  headers_weights = headers_weights, headers_includes = headers_includes, $
  headers_constants = headers_constants, headers_residuals = headers_residuals
  ptools.parcheck, ANA, 1, 'ANA', 'STRUCT', [0, 1], structure_name = 'CFIT_ANALYSIS', /optional
  n_ana = n_elements(ANA)
  ptools.parcheck, XTYPE1, 0, 'XTYPE1', 'STRING', [0, 1], default = 'WAVE'
  ptools.parcheck, filepath_out, 0, 'FILEPATH_OUT', 'STRING', 0
  ptools.parcheck, n_windows, 0, 'N_WINDOWS', 'INTEGERS', 0, default = max([n_ana, 1])
  ptools.parcheck, winno, 0, 'WINNO', 'INTEGERS', 0, default = 0
  ptools.parcheck, level, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  ptools.parcheck, version, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  ptools.parcheck, SIGMADAT, 0, 'SIGMADAT', 'STRING', 0, /optional

  result_ptr = 0
  fit_ptr = 0
  in_data_ptr = 0
  xdim1_ptr = 0
  weights_ptr = 0
  incl_ptr = 0
  const_ptr = 0
  residual_ptr = 0

  prg_data_ptr = 0
  hdr_in_data_ptr = 0
  proc_st_ptr = 0
  proj_kwd_ptr = 0

  IF ~n_ana THEN BEGIN
    ptools.parcheck, result, 0, 'RESULT', 'POINTER', [0, 1], result = error
    IF error[0] NE '' THEN ptools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7] $
    ELSE result_ptr = n_elements(result)
    IF result_ptr GT 0 THEN n_ana = result_ptr ELSE n_ana = 1

    IF n_ana GT 1 THEN BEGIN
      ptools.parcheck, fit, 0, 'FIT', 'POINTER', 1, valid_nelements = n_ana
      ptools.parcheck, input_data, 0, 'INPUT_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
      ptools.parcheck, xdim1, 0, 'XDIM1', 'POINTER', 1, /optional, valid_nelements = n_ana
      ptools.parcheck, weights, 0, 'WEIGHTS', 'POINTER', 1, /optional, valid_nelements = n_ana
      ptools.parcheck, include, 0, 'INCLUDE', 'POINTER', 1, /optional, valid_nelements = n_ana
      ptools.parcheck, const, 0, 'CONST', 'POINTER', 1, /optional, valid_nelements = n_ana
    ENDIF ELSE BEGIN ; n_ana GT 1

      ptools.parcheck, fit, 0, 'FIT', 'POINTER', [0, 1], valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, fit, 0, 'FIT', 'STRUCT', 0 $
      ELSE fit_ptr = n_elements(fit)
      ptools.parcheck, input_data, 0, 'INPUT_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, input_data, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE in_data_ptr = n_elements(input_data)
      ptools.parcheck, xdim1, 0, 'XDIM1', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, xdim1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], /optional $
      ELSE xdim1_ptr = n_elements(xdim1)
      ptools.parcheck, weights, 0, 'WEIGHTS', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, weights, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE weights_ptr = n_elements(weights)
      ptools.parcheck, include, 0, 'INCLUDE', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, include, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE incl_ptr = n_elements(include)
      ptools.parcheck, const, 0, 'CONST', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, const, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE const_ptr = n_elements(const)
      ptools.parcheck, residual, 0, 'RESIDUAL', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
      IF error[0] NE '' THEN ptools.parcheck, residual, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
      ELSE residual_ptr = n_elements(residual)
    ENDELSE ; n_ana GT 1
  ENDIF ; ~n_ana

  IF n_ana GT 1 THEN BEGIN
    ptools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
    prg_data_ptr = n_elements(progenitor_data)
    ptools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'POINTER', 1, /optional, valid_nelements = n_ana
    hdr_in_data_ptr = n_elements(header_input_data)
  ENDIF ELSE BEGIN ; n_ana GT 1
    ptools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
    IF error[0] NE '' THEN ptools.parcheck, progenitor_data, 0, 'PROGENITOR_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], /optional $
    ELSE prg_data_ptr = n_elements(progenitor_data)
    ptools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'POINTER', [0, 1], /optional, valid_nelements = n_ana, result = error
    IF error[0] NE '' THEN ptools.parcheck, header_input_data, 0, 'HEADER_INPUT_DATA', 'STRING', 1, /optional $
    ELSE hdr_in_data_ptr = n_elements(header_input_data)
  ENDELSE ; n_ana GT 1

  ptools.parcheck, proc_steps, 0, 'PROC_STEPS', 'POINTER', 1, /optional, valid_nelements = n_ana, result = error
  IF error[0] NE '' THEN ptools.parcheck, proc_steps, 0, 'PROC_STEPS', 11, 1, /optional $
  ELSE proc_st_ptr = n_elements(proc_steps)
  ptools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', 'POINTER', 1, /optional, valid_nelements = n_ana, result = error
  IF error[0] NE '' THEN ptools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional $
  ELSE proj_kwd_ptr = n_elements(proj_keywords)

  ptools.parcheck, data_id, 0, 'DATA_ID', 'STRING', [0, 1], valid_nelements = n_ana, /optional

  get_headers = bytarr(6)
  IF arg_present(headers_results) THEN BEGIN
    headers_results = ptrarr(n_ana)
    get_headers[0] = 1
  ENDIF
  IF arg_present(headers_data) THEN BEGIN
    headers_data = ptrarr(n_ana)
    get_headers[1] = 1
  ENDIF
  IF arg_present(headers_weights) THEN BEGIN
    headers_weights = ptrarr(n_ana)
    get_headers[2] = 1
  ENDIF
  IF arg_present(headers_includes) THEN BEGIN
    headers_includes = ptrarr(n_ana)
    get_headers[3] = 1
  ENDIF
  IF arg_present(headers_constants) THEN BEGIN
    headers_constants = ptrarr(n_ana)
    get_headers[4] = 1
  ENDIF
  IF arg_present(headers_residuals) THEN BEGIN
    headers_residuals = ptrarr(n_ana)
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
    IF residual_ptr THEN residual_use = *residual[iwindow] ELSE IF n_elements(residual) GT 0 THEN residual_use = residual

    IF prg_data_ptr THEN PROGENITOR_DATA_use = *progenitor_data[iwindow] ELSE IF n_elements(progenitor_data) GT 0 THEN PROGENITOR_DATA_use = progenitor_data
    IF hdr_in_data_ptr THEN HEADER_INPUT_DATA_use = *header_input_data[iwindow] ELSE IF n_elements(header_input_data) GT 0 THEN HEADER_INPUT_DATA_use = header_input_data
    IF proc_st_ptr THEN PROC_STEPS_use = *proc_steps[iwindow] ELSE IF n_elements(proc_steps) GT 0 THEN PROC_STEPS_use = proc_steps
    IF proj_kwd_ptr THEN PROJ_KEYWORDS_use = *proj_keywords[iwindow] ELSE IF n_elements(proj_keywords) GT 0 THEN PROJ_KEYWORDS_use = proj_keywords

    IF keyword_set(data_id) THEN IF n_elements(data_id) GT 1 THEN data_id_use = data_id[iwindow] ELSE data_id_use = data_id
    IF n_elements(XTYPE1) GT 1 THEN XTYPE1_use = XTYPE1[iwindow] ELSE XTYPE1_use = XTYPE1
    IF keyword_set(XDIMEN1) THEN IF n_elements(XDIMEN1) GT 1 THEN XDIMEN1_use = XDIMEN1[iwindow] ELSE XDIMEN1_use = XDIMEN1
    IF n_elements(SAVE_DATA) GT 1 THEN SAVE_DATA_use = SAVE_DATA[iwindow] ELSE $
      IF n_elements(SAVE_DATA) EQ 1 THEN SAVE_DATA_use = SAVE_DATA
    IF n_elements(SAVE_RESIDUALS) GT 1 THEN SAVE_RESIDUALS_use = SAVE_RESIDUALS[iwindow] ELSE $
      IF n_elements(SAVE_RESIDUALS) EQ 1 THEN SAVE_RESIDUALS_use = SAVE_RESIDUALS
    IF n_elements(DATA_EXT_PATH) GT 1 THEN DATA_EXT_PATH_use = DATA_EXT_PATH[iwindow] ELSE $
      IF n_elements(DATA_EXT_PATH) EQ 1 THEN DATA_EXT_PATH_use = DATA_EXT_PATH

    extension = keyword_set(is_extension) || iwindow GT 0

    IF n_elements(ANA) THEN BEGIN
      headers = ana2fitshdr(ANA[iwindow], filename_out = filepath_out, $
        n_windows = n_windows_use, winno = winno + iwindow, $
        data_id = data_id_use, XTYPE1 = XTYPE1_use, XDIMEN1 = XDIMEN1_use, $
        DATA_EXT_PATH = DATA_EXT_PATH_use, $
        is_extension = extension, level = level, version = version, creator = creator, SIGMADAT = SIGMADAT, $
        proc_steps = PROC_STEPS_use, proj_keywords = PROJ_KEYWORDS_use, $
        xdim1 = xdim1_use, input_data = INPUT_DATA_use, fit = fit_use, $
        result = result_use, residual = residual_use, weights = weights_use, include = include_use, $
        const = const_use, filename_ana = filename_ana, datasource = datasource, $
        definition = definition, missing = missing, label = label, history = history, $
        progenitor_data = PROGENITOR_DATA_use, header_input_data = HEADER_INPUT_DATA_use, $
        SAVE_RESIDUALS = SAVE_RESIDUALS_use, SAVE_DATA = SAVE_DATA_use, print_headers = print_headers, $
        data_array = DATA_ARRAY)
    ENDIF ELSE BEGIN
      headers = ana2fitshdr(filename_out = filepath_out, $
        n_windows = n_windows_use, winno = winno + iwindow, $
        data_id = data_id_use, XTYPE1 = XTYPE1_use, XDIMEN1 = XDIMEN1_use, $
        DATA_EXT_PATH = DATA_EXT_PATH_use, $
        is_extension = extension, level = level, version = version, creator = creator, SIGMADAT = SIGMADAT, $
        proc_steps = PROC_STEPS_use, proj_keywords = PROJ_KEYWORDS_use, $
        xdim1 = xdim1_use, input_data = INPUT_DATA_use, fit = fit_use, $
        result = result_use, residual = residual, weights = weights_use, include = include_use, $
        const = const_use, filename_ana = filename_ana, datasource = datasource, $
        definition = definition, missing = missing, label = label, history = history, $
        progenitor_data = PROGENITOR_DATA_use, header_input_data = HEADER_INPUT_DATA_use, $
        SAVE_RESIDUALS = SAVE_RESIDUALS_use, SAVE_DATA = SAVE_DATA_use, print_headers = print_headers, $
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
    IF get_headers[2] THEN headers_weights[iwindow] = ptr_new(*headers[3])
    IF get_headers[3] THEN headers_includes[iwindow] = ptr_new(*headers[4])
    IF get_headers[4] THEN headers_constants[iwindow] = ptr_new(*headers[5])
    IF get_headers[5] THEN headers_residuals[iwindow] = ptr_new(*headers[5])
  ENDFOR ; iwindow=0,n_windows-1
END
