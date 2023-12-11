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
;      PATH_EXTERNAL_EXTENSION: String. A path, relative to this FITS file, which points to the
;            progenitor FITS file that contains the original data cube. This will be added as a prefix
;            to DATAEXT keyword. If this is provided the keyword NO_SAVE_DATA will be set, and PRGDATA is set to True.
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
; $Id: 2023-12-11 13:49 CET $


FUNCTION ana2fitshdr, ANA, FILENAME_OUT=FILENAME_OUT, $
  N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
  DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
  EXT_DATA_PATH=EXT_DATA_PATH, $
  IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
  PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
  XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
  RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
  PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
  PATH_EXTERNAL_EXTENSION=PATH_EXTERNAL_EXTENSION, $ ; TODO
  SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
  DATA_ARRAY=DATA_ARRAY


  prits_tools.parcheck, ANA, 1, 'ANA', 'STRUCT', 0, structure_name='CFIT_ANALYSIS', /optional
  ana_given = N_ELEMENTS(ANA)
  prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0, optional=ana_given
  prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'NUMERIC', [0, 2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, XDIM1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1

  prits_tools.parcheck, FILENAME_OUT, 0, 'FILENAME_OUT', 'STRING', 0
  prits_tools.parcheck, N_WINDOWS, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, WINNO, 0, 'WINNO', 'INTEGERS', 0
  prits_tools.parcheck, TYPE_XDIM1, 0, 'TYPE_XDIM1', 'STRING', 0
  prits_tools.parcheck, DATA_ID, 0, 'DATA_ID', 'STRING', 0, default=strtrim(winno, 2)
  prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional=1
  prits_tools.parcheck, LEVEL, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, VERSION, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, PROC_STEPS, 0, 'PROC_STEPS', 11, 1, /optional
  prits_tools.parcheck, PROJ_KEYWORDS, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional

  prits_tools.parcheck, RESIDUAL, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, HISTORY, 0, 'HISTORY', 'STRING', [0, 1], optional=1
  prits_tools.parcheck, FILENAME_ANA, 0, 'FILENAME_ANA', 'STRING', 0, optional=1
  prits_tools.parcheck, DATASOURCE, 0, 'DATASOURCE', 'STRING', 0, optional=1
  prits_tools.parcheck, DEFINITION, 0, 'DEFINITION', 'STRING', 0, optional=1
  prits_tools.parcheck, MISSING, 0, 'MISSING', 'NUMERIC', 0, optional=1
  prits_tools.parcheck, LABEL, 0, 'LABEL', 'STRING', 0, optional=1

  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
      handle_value,ana.history_h,history
      handle_value,ana.lambda_h,xdim1
      handle_value,ana.data_h,input_data
      handle_value,ana.weights_h,weights
      handle_value,ana.fit_h,fit
      handle_value,ana.result_h,result
      handle_value,ana.residual_h,residual
      handle_value,ana.include_h,include
      handle_value,ana.const_h,const
      handle_value,ana.origin_h,origin
      handle_value,ana.scale_h,scale
      handle_value,ana.phys_scale_h,phys_scale
      handle_value,ana.dimnames_h,dimnames
      filename_ana = ana.filename
      datasource = ana.datasource
      definition = ana.definition
      missing = ana.missing
      label = ana.label
    end

    8: begin
      handle_value,ana.history_h,history
      handle_value,ana.lambda_h,xdim1
      handle_value,ana.data_h,input_data
      handle_value,ana.weights_h,weights
      handle_value,ana.fit_h,fit
      handle_value,ana.result_h,result
      handle_value,ana.residual_h,residual
      handle_value,ana.include_h,include
      handle_value,ana.const_h,const
      handle_value,ana.origin_h,origin
      handle_value,ana.scale_h,scale
      handle_value,ana.phys_scale_h,phys_scale
      handle_value,ana.dimnames_h,dimnames
      filename_ana = ana.filename
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

  ; Add time to DATE
  caldat, systime(/julian), month, day, year, hour, minute, second
  datetime = {CDS_EXT_TIME, $
    year:year, $
    month:month, $
    day:day, $
    hour:hour, $
    minute:minute, $
    second:second, $
    millisecond:0}
  datetime = anytim(datetime, /ccsds)

  extension_names = data_id + [ $
    ' results', $
    ' data', $
    ' xdim1', $
    ' weights', $
    ' includes', $
    ' constants']

  wcs = ana_wcs_get_transform(TYPE_XDIM1, HEADER_INPUT_DATA)

  all_headers = ptrarr(6)


  ; ------
  ; Create result header
  ; ------

  hdr = ana2fitshdr_results(RESULT=RESULT, FIT=FIT, datetime=datetime, $
    filename_out=file_basename(FILENAME_OUT), n_windows=n_windows, winno=winno, $
    EXTENSION_NAMES=EXTENSION_NAMES, IS_EXTENSION=IS_EXTENSION, $
    HEADER_INPUT_DATA=HEADER_INPUT_DATA, WCS=WCS, $
    LEVEL=LEVEL, VERSION=VERSION, $
    PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
    HISTORY=HISTORY, FILENAME_ANA=FILENAME_ANA, $
    DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
  all_headers[0] = ptr_new(hdr)


  ; ------
  ; Create data header
  ; ------

  hdr = ana2fitshdr_data(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
    HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA, NO_SAVE_DATA=NO_SAVE_DATA, $
    DATA_ARRAY=DATA_ARRAY)
  all_headers[1] = ptr_new(hdr)
  if hdr[0] eq '' then EXTENSION_NAMES[1] = ''


  ; ------
  ; Create xdim header
  ; ------

  hdr = ana2fitshdr_xdim(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, XDIM1=XDIM1, WCS=WCS, SAVE_XDIM1=SAVE_XDIM1)
  all_headers[2] = ptr_new(hdr)
  if hdr[0] eq '' then EXTENSION_NAMES[2] = ''


  ; ------
  ; Create weights header
  ; ------

  hdr = ana2fitshdr_weights(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, WEIGHTS=WEIGHTS, WCS=WCS)
  all_headers[3] = ptr_new(hdr)
  if hdr[0] eq '' then EXTENSION_NAMES[3] = ''


  ; ------
  ; Create include header
  ; ------

  hdr = ana2fitshdr_include(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INCLUDE=INCLUDE, WCS=WCS)
  all_headers[4] = ptr_new(hdr)
  if hdr[0] eq '' then EXTENSION_NAMES[4] = ''


  ; ------
  ; Create const header
  ; ------

  hdr = ana2fitshdr_const(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, CONST=CONST, WCS=WCS)
  all_headers[5] = ptr_new(hdr)
  if hdr[0] eq '' then EXTENSION_NAMES[5] = ''
  
  
  ; Delete extension names if necessary
  FOR iext=0,5 DO BEGIN
    hdr = all_headers[iext]
    IF (*hdr)[0] NE '' THEN BEGIN
      fxaddpar, *hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
      fxaddpar, *hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
      fxaddpar, *hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
      fxaddpar, *hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
      fxaddpar, *hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'
    ENDIF
    if keyword_set(print_headers) then begin
      print,''
      case iext of
        0: print,'--- RESULTS ---'
        1: print,'--- DATA ---'
        2: print,'--- XDIM ---'
        3: print,'--- WEIGHTS ---'
        4: print,'--- INCLUDE ---'
        5: print,'--- CONST ---'
      endcase
      print,''
      print,*hdr
    endif
  ENDFOR


  return, all_headers
end
