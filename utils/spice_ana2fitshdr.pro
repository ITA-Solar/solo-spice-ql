;+
; NAME:
;      SPICE_ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This function returns a fits header made from an ANA object or file.
;      The fits header contains all fit components as keywords and the original
;      level 2 header keywords. The WCS keywords are adapted to the result
;      cube given by ANA.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      header = spice_ana2fitshdr_results(ana, header_l2=header_l2, $
;         [filename_l3=filename_l3, $
;         /EXTENSION, $
;         HISTORY=HISTORY, LAMBDA=LAMBDA, DATA=DATA, WEIGHTS=WEIGHTS, $
;         FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;         CONST=CONST, FILENAME=FILENAME, DATASOURCE=DATASOURCE, $
;         DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL] )
;
; INPUTS:
;      ana: The name and path of an ANA file or an ANA object.
;           If this is not provided, then all of the optional inputs
;           must be provided
;      header_l2: The header (string array) of the level 2 file.
; 
; KEYWORDS:
;      extension: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the level 3 file.
;                 If not set, this will be the primary header.
; 
; OPTIONAL INPUTS:
;      All of the following optional inputs must be provided if 'ana' is not
;      provided. If 'ana' is provided, they will be ignored/overwritten.
;      HISTORY: A string array.
;      LAMBDA: An array of wavelength values. Either one value for
;              every point in the data array, or a one-dimensional
;              array to go with all the spectra in the data array.
;      DATA: Data Array. Up to 7-dimensional data array, with spectra
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
;      a fits header (string array)
;
; OPTIONAL OUTPUTS:
;      filename_l3: The filename the level 3 will/should get
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2021-12-01 11:56 CET $


FUNCTION spice_ana2fitshdr, ana, header_l2=header_l2, $
  filename_l3=filename_l3, EXTENSION=EXTENSION, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, DATA=DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL

  input_type = size(ana, /type)
  case input_type of
    7: begin
      restore, ana, /verbose
    end

    8: begin
      handle_value,ana.history_h,history,/no_copy
      handle_value,ana.lambda_h,lambda,/no_copy
      handle_value,ana.data_h,data,/no_copy
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
  obs_def = strtrim(string(file_info_l2.spiobsid), 2) + $
    fns('-###', file_info_l2.rasterno) + $
    fns('_##', fxpar(header_l2, 'WINNO', missing=99))


  ; ------
  ; Create result header
  ; ------

  hdr = spice_ana2fitshdr_results(header_l2=header_l2, datetime=datetime, $
    filename_l3=filename_l3, filename_l2=filename_l2, obs_def=obs_def, $
    EXTENSION=EXTENSION, $
    HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME, $
    DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
    
    
    hdr = spice_ana2fitshdr_data(header_l2=header_l2, datetime=datetime, $
    obs_def=obs_def, $
    DATA=DATA)
    
  return, hdr
end
