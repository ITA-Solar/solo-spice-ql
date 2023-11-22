;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This is a subfunction of ANA2FITS.
;      This function returns an array of FITS headers made from an ANA object or file.
;      The fits headers contains all fit components as keywords and the original
;      level 2 header keywords in the first (zeroth) extension.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS
;
; CALLING SEQUENCE:
;      headers = ana2fitshdr(ana, n_windows=n_windows, winno=winno, data_id=data_id, $
;        FILENAME_OUT=FILENAME_OUT, $
;        /EXTENSION, $
;        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
;        /spice, $
;        original_data=original_data, header_l2=header_l2, $
;        /print_headers )
;
; INPUTS:
;      ANA: An ANA object or the name and path of an ANA file.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      FILENAME_OUT: Filename of the resulting FITS file. May include the path.
;      N_WINDOWS: Total number of windows to be included in FITS file.
;      WINNO: Window number (starting at 0) within this study in this FITS file.
;      TYPE_XDIM1
;
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
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
;      DATA_ID: A string defining the prefix to the names of the 6 extensions, default is WINNO as a string.
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


;      SPICE: If set, then 'header_l2' will be assumed to be from a level 2 SPICE FITS file
;                 and incorporated into this level 3 FITS file. And the 'data' extension
;                 will get 'original_data' as its data array, instead of the data array
;                 saved in the ana or 'input_data'.
;                 This keyword should be set to a structure or array of structures.
;                 Each structure contains the tags: step, proc, version, lib and params.
;                 Those describe the processing steps taken to produce a SPICE level 3 file.
;
; OUTPUTS:
;      a pointer array, containing 6 FITS keyword headers, of which 5 may be empty strings.
;
; OPTIONAL OUTPUTS:
;      DATA_ARRAY: Contains the data array that should be saved into the data extension, if any.
;
; CALLS:
;     prits_tools.parcheck, caldat, ana2fitshdr_results, ana2fitshdr_data, ana2fitshdr_xdim,
;     ana2fitshdr_weights, ana2fitshdr_include, ana2fitshdr_const
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2023-11-22 13:36 CET $


FUNCTION ana2fitshdr, ANA, FILENAME_OUT=FILENAME_OUT, $
  N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
  DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
  EXTENSION=EXTENSION, $
  XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
  RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
  PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
  SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
  DATA_ARRAY=DATA_ARRAY


  prits_tools.parcheck, ANA, 1, 'ANA', 'STRUCT', 0, structure_name='CFIT_ANALYSIS', /optional
  ana_given = N_ELEMENTS(ANA)
  prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0, optional=ana_given
  prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, PROGENITOR_DATA, 0, 'PROGENITOR_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, XDIM1, 0, 'XDIM1', 'NUMERIC', [0, 1, 2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, RESIDUAL, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=1
  prits_tools.parcheck, HISTORY, 0, 'HISTORY', 'STRING', [0, 1], optional=1
  prits_tools.parcheck, FILENAME_ANA, 0, 'FILENAME_ANA', 'STRING', 0, optional=1
  prits_tools.parcheck, DATASOURCE, 0, 'DATASOURCE', 'STRING', 0, optional=1
  prits_tools.parcheck, DEFINITION, 0, 'DEFINITION', 'STRING', 0, optional=1
  prits_tools.parcheck, MISSING, 0, 'MISSING', 'NUMERIC', 0, optional=1
  prits_tools.parcheck, LABEL, 0, 'LABEL', 'STRING', 0, optional=1

  prits_tools.parcheck, FILENAME_OUT, 0, 'FILENAME_OUT', 'STRING', 0
  prits_tools.parcheck, N_WINDOWS, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, WINNO, 0, 'WINNO', 'INTEGERS', 0
  prits_tools.parcheck, DATA_ID, 0, 'DATA_ID', 'STRING', 0, default=strtrim(winno, 2)
  prits_tools.parcheck, TYPE_XDIM1, 0, 'TYPE_XDIM1', 'STRING', 0
  prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional=1


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
  help,wcs

  all_headers = ptrarr(6)


  ; ------
  ; Create result header
  ; ------

;  hdr = ana2fitshdr_results(header_l2=header_l2, DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, $
;    filename_out=file_basename(FILENAME_OUT), n_windows=n_windows, $
;    winno=winno, EXTENSION=EXTENSION, spice=spice, $
;    HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME, $
;    DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
hdr=''

  all_headers[0] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- RESULTS ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create data header
  ; ------

  hdr = ana2fitshdr_data(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INPUT_DATA=INPUT_DATA, $
    HEADER_INPUT_DATA=HEADER_INPUT_DATA, PROGENITOR_DATA=PROGENITOR_DATA, NO_SAVE_DATA=NO_SAVE_DATA, $
    DATA_ARRAY=DATA_ARRAY)

  all_headers[1] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- DATA ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create xdim header
  ; ------

  hdr = ana2fitshdr_xdim(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, XDIM1=XDIM1, WCS=WCS, SAVE_XDIM1=SAVE_XDIM1)

  all_headers[2] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- XDIM ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create weights header
  ; ------

  hdr = ana2fitshdr_weights(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, WEIGHTS=WEIGHTS, WCS=WCS)

  all_headers[3] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- WEIGHTS ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create include header
  ; ------

  hdr = ana2fitshdr_include(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, INCLUDE=INCLUDE, WCS=WCS)

  all_headers[4] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- INCLUDE ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create const header
  ; ------

  hdr = ana2fitshdr_const(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, CONST=CONST, WCS=WCS)

  all_headers[5] = ptr_new(hdr)

  if keyword_set(print_headers) then begin
    print,''
    print,'--- CONST ---'
    print,''
    print,hdr
  endif


  return, all_headers
end
