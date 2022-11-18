;+
; NAME:
;      ANA2FITSHDR
;
; PURPOSE:
;      This function returns an array of FITS headers made from an ANA object or file.
;      The fits headers contains all fit components as keywords and the original
;      level 2 header keywords in the first (zeroth) extension.
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;      headers = ana2fitshdr(ana, n_windows=n_windows, winno=winno, data_id=data_id, $
;        filename_out=filename_out, $
;        EXTENSION=EXTENSION, $
;        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
;        spice=spice, $
;        original_data=original_data, header_l2=header_l2] )
;
; INPUTS:
;      ana: An ANA object or the name and path of an ANA file.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      filename_out: Full path and filename of the resulting FITS file.
;      data_id: A string defining the prefix to the names of the 7 extensions
;      n_windows: Total number of windows to be included in FITS file.
;      winno: Window number (starting at 0) within this study in this FITS file.
;      header_l2: The header (string array) of the SPICE level 2 file.
;      original_data: Data Array. Up to 7-dimensional data array, the original SPICE data
;                     from the level 2 FITS file. Spectra is not in the first dimension.
;                     This data arra will be saved into the FITS file. Thus it cannot be used
;                     directly in the ANA structure when read back in.
;
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the FITS file.
;                 If not set, this will be the primary header.
;      SPICE: If set, then 'header_l2' will be assumed to be from a level 2 SPICE FITS file
;                 and incorporated into this level 3 FITS file. And the 'data' extension
;                 will get 'original_data' as its data array, instead of the data array
;                 saved in the ana or 'input_data'.
;
; OPTIONAL INPUTS/OUTPUTS:
;      All of the following optional inputs must be provided if 'ana' is not
;      provided. If 'ana' is provided, they will be overwritten and can be used
;      as output.
;      HISTORY: A string array.
;      LAMBDA: An array of wavelength values. Either one value for
;              every point in the data array, or a one-dimensional
;              array to go with all the spectra in the data array.
;      INPUT_DATA: Data Array. Up to 7-dimensional data array, with spectra
;            along the first dimension.
;      WEIGHTS: Weights to use in the fitting process. No default!
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;      RESIDUAL: Array to contain the residual. Same size as DATA, may be
;                undefined on input.
;      INCLUDE: Array to keep the INCLUDE status of each component
;               at each point.
;      CONST: Array to keep the CONST status of each parameter at
;             each point.
;      FILENAME_ANA: The filename of the ANA-file.
;      DATASOURCE: A string.
;      DEFINITION: A string.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been
;               declared as "FAILED".
;      LABEL: A string.
;
; OUTPUTS:
;      a pointer array, containing 7 FITS keyword headers
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2022-11-18 14:40 CET $


FUNCTION ana2fitshdr, ana, n_windows=n_windows, winno=winno, data_id=data_id, $
  filename_out=filename_out, $
  EXTENSION=EXTENSION, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  spice=spice, $
  original_data=original_data, header_l2=header_l2

  print_headers = 0

  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
      handle_value,ana.history_h,history
      handle_value,ana.lambda_h,lambda
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
      handle_value,ana.lambda_h,lambda
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

  all_headers = ptrarr(7)


  ; ------
  ; Create result header
  ; ------

  hdr = ana2fitshdr_results(header_l2=header_l2, datetime=datetime, $
    filename_out=filename_out, data_id=data_id, n_windows=n_windows, $
    winno=winno, EXTENSION=EXTENSION, spice=spice, $
    HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME, $
    DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)

  all_headers[0] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- results ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create data header
  ; ------

  IF keyword_set(spice) && keyword_set(original_data) THEN data_array=original_data $
  ELSE data_array=input_data
  hdr = ana2fitshdr_data(datetime=datetime, data_id=data_id, data_array=data_array, $
    header_l2=header_l2)

  all_headers[1] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- data ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create lambda header
  ; ------

  hdr = ana2fitshdr_lambda(datetime=datetime, data_id=data_id, LAMBDA=LAMBDA, $
  header_l2=header_l2)

  all_headers[2] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- lambda ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create residuals header
  ; ------

  hdr = ana2fitshdr_residuals(datetime=datetime, data_id=data_id, RESIDUAL=RESIDUAL, $
  header_l2=header_l2)

  all_headers[3] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- residuals ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create weights header
  ; ------

  hdr = ana2fitshdr_weights(datetime=datetime, data_id=data_id, WEIGHTS=WEIGHTS, $
  header_l2=header_l2)

  all_headers[4] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- weights ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create include header
  ; ------

  hdr = ana2fitshdr_include(datetime=datetime, data_id=data_id, INCLUDE=INCLUDE, $
  header_l2=header_l2)

  all_headers[5] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- include ---'
    print,''
    print,hdr
  endif


  ; ------
  ; Create const header
  ; ------

  hdr = ana2fitshdr_const(datetime=datetime, data_id=data_id, CONST=CONST, $
  header_l2=header_l2)

  all_headers[6] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- const ---'
    print,''
    print,hdr
  endif


  return, all_headers
end
