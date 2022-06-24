;+
; NAME:
;      SPICE_ANA2FITSHDR
;
; PURPOSE:
;      This function returns an array of FITS headers made from an ANA object or file.
;      The fits headers contains all fit components as keywords and the original
;      level 2 header keywords in the first (zeroth) extension. 
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      headers = spice_ana2fitshdr(ana, header_l2=header_l2, $
;         n_windows=n_windows, original_data=original_data [, filename_l3=filename_l3, $
;         /EXTENSION, $
;         HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;         FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;         CONST=CONST, FILENAME=FILENAME, DATASOURCE=DATASOURCE, $
;         DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL] )
;
; INPUTS:
;      ana: The name and path of an ANA file or an ANA object.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      header_l2: The header (string array) of the level 2 file.
;      n_windows: total number of windows to be included in level 3 file.
;      original_data: Data Array. Up to 7-dimensional data array, the original SPICE data
;                     from the level 2 FITS file. Spectra is not in the first dimension.
;                     This data cube will saved into the FITS file. Thus it cannot be used
;                     directly in the ANA structure when read back in.
;
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the level 3 file.
;                 If not set, this will be the primary header.
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
;      filename_l3: The filename the level 3 will/should get
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2022-06-24 11:30 CEST $


FUNCTION spice_ana2fitshdr, ana, header_l2=header_l2, n_windows=n_windows, $
  filename_l3=filename_l3, EXTENSION=EXTENSION, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  original_data=original_data

  print_headers = 0
  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
    end

    8: begin
      handle_value,ana.history_h,history,/no_copy
      handle_value,ana.lambda_h,lambda,/no_copy
      handle_value,ana.data_h,input_data,/no_copy
      handle_value,ana.weights_h,weights,/no_copy
      handle_value,ana.fit_h,fit,/no_copy
      handle_value,ana.result_h,result,/no_copy
      handle_value,ana.residual_h,residual,/no_copy
      handle_value,ana.include_h,include,/no_copy
      handle_value,ana.const_h,const,/no_copy
      handle_value,ana.origin_h,origin,/no_copy
      handle_value,ana.scale_h,scale,/no_copy
      handle_value,ana.phys_scale_h,phys_scale,/no_copy
      handle_value,ana.dimnames_h,dimnames,/no_copy
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

  filename_l2 = fxpar(header_l2, 'FILENAME', missing='')
  filename_l3 = filename_l2.replace('_L2_', '_L3_')
  file_info_l2 = spice_file2info(filename_l2)
  extension_name_prefix = 'V' + fns('##', file_info_l2.version) + $
    '_' + strtrim(string(file_info_l2.spiobsid), 2) + $
    fns('-###', file_info_l2.rasterno) + $
    fns(' ext## ', fxpar(header_l2, 'WINNO', missing=99))

  all_headers = ptrarr(7)


  ; ------
  ; Create result header
  ; ------

  hdr = spice_ana2fitshdr_results(header_l2=header_l2, datetime=datetime, $
    filename_l3=filename_l3, filename_l2=filename_l2, extension_name_prefix=extension_name_prefix, n_windows=n_windows, $
    EXTENSION=EXTENSION, $
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

  hdr = spice_ana2fitshdr_data(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    original_data=original_data)

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

  hdr = spice_ana2fitshdr_lambda(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    LAMBDA=LAMBDA)

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

  hdr = spice_ana2fitshdr_residuals(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    RESIDUAL=RESIDUAL)

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

  hdr = spice_ana2fitshdr_weights(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    WEIGHTS=WEIGHTS)

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

  hdr = spice_ana2fitshdr_include(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    INCLUDE=INCLUDE)

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

  hdr = spice_ana2fitshdr_const(header_l2=header_l2, datetime=datetime, $
    extension_name_prefix=extension_name_prefix, $
    CONST=CONST)

  all_headers[6] = ptr_new(hdr)

  if print_headers then begin
    print,''
    print,'--- const ---'
    print,''
    print,hdr
  endif


  return, all_headers
end
