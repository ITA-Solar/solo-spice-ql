;+
; NAME:
;      ANA2FITS
;
; PURPOSE:
;      This procedure saves the content of one or more ANA structures into one FITS file.
;      The FITS file will contain 7 extension per ANA, where the first contains the results,
;      and the fit components as header keywords. The resulting FITS file can be converted
;      into one or more ANA structures with the procedure fits2ana.
;
; CATEGORY:
;      FITS -- utility
;
; CALLING SEQUENCE:
;      headers = ana2fits(ana, filename_out=filename_out, $
;         HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
;         FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
;         CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
;         DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL] )
;
; INPUTS:
;      ana: The name and path of an ANA file or an ANA object.
;           If this is not provided, then all of the optional inputs
;           must be provided. If more than one ANA should be saved into one FITS file,
;           then 'ana' must be provided as an array of either file paths or objects.
;      filename_out: Full path and filename of the resulting FITS file.
;
; KEYWORDS:
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
; HISTORY:
;      Ver. 1, 19-Jan-2022, Martin Wiesmann
;-
; $Id: 2022-01-19 14:55 CET $


PRO ana2fits, ana, n_windows=n_windows, filename_out=filename_out, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  EXTENSION=EXTENSION

  n_windows = N_ELEMENTS(ana)
  if n_windows eq 0 then n_windows=1
  for iwindow=0,n_windows-1 do begin
    input_type = size(ana[iwindow], /type)
    case input_type of
      7: begin
        restore, ana[iwindow], /verbose
      end

      8: begin
        handle_value,ana[iwindow].history_h,history,/no_copy
        handle_value,ana[iwindow].lambda_h,lambda,/no_copy
        handle_value,ana[iwindow].data_h,input_data,/no_copy
        handle_value,ana[iwindow].weights_h,weights,/no_copy
        handle_value,ana[iwindow].fit_h,fit,/no_copy
        handle_value,ana[iwindow].result_h,result,/no_copy
        handle_value,ana[iwindow].residual_h,residual,/no_copy
        handle_value,ana[iwindow].include_h,include,/no_copy
        handle_value,ana[iwindow].const_h,const,/no_copy
        handle_value,ana[iwindow].origin_h,origin,/no_copy
        handle_value,ana[iwindow].scale_h,scale,/no_copy
        handle_value,ana[iwindow].phys_scale_h,phys_scale,/no_copy
        handle_value,ana[iwindow].dimnames_h,dimnames,/no_copy
        filename_ana = ana[iwindow].filename
        datasource = ana[iwindow].datasource
        definition = ana[iwindow].definition
        missing = ana[iwindow].missing
        label = ana[iwindow].label
      end

      0: begin
      end

      else: begin
        print, 'wrong input'
        return, -1
      end
    endcase


    if iwindow gt 0 then extension=1 else extension=0

    headers = ana2fitshdr(ana[iwindow], filename_out=filename_out, n_windows=n_windows, $
      extension=extension, $
      HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
      FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
      CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
      DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)

    writefits, filename_out, RESULT, *headers[0], append=extension
    writefits, filename_out, INPUT_DATA, *headers[1], /append
    writefits, filename_out, LAMBDA, *headers[2], /append
    writefits, filename_out, RESIDUAL, *headers[3], /append
    writefits, filename_out, WEIGHTS, *headers[4], /append
    writefits, filename_out, INCLUDE, *headers[5], /append
    writefits, filename_out, CONST, *headers[6], /append

  endfor ; iwindow=0,n_windows-1


END
