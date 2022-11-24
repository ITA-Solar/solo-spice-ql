;+
; NAME:
;      ANA2FITS
;
; PURPOSE:
;      This procedure saves the content of one or more ANA structures into one FITS file.
;      The FITS file will contain 7 extension per ANA, where the first contains the results,
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
;      ana: The name and path of an ANA file or an ANA object.
;           If this is not provided, then all of the optional inputs
;           must be provided. If more than one ANA should be saved into one FITS file,
;           then 'ana' must be provided as an array of either file paths or objects.
;      filepath_out: Full path and filename of the resulting FITS file.
;      data_id: A string vector of same length as 'ana', or if 'ana' is not provided
;               scalar string. These strings are used to identify the data, i.e. they will
;               be used in the extension names of the FITS file. Each dataset will get
;               7 extensions, which all have the same ID, but the extension name will be
;               'data_id'+' '+extension_type (='results', 'data', 'lambda', 'residuals', 'weights', 'includes', 'constants').
;               Default is the dataset numbers.
;
; KEYWORDS:
;      EXTENSION: If set, then the first ANA's result array will be an extension,
;                 i.e. this should be set if the FITS file already exists and data should be appended.
;                 If not set, the first ANA's result array will be the primary header.
;
; OPTIONAL INPUTS/OUTPUTS:
;      n_windows: Total number of windows that will be included in this FITS file.
;              By default, this will be the number of 'ana' structures provided, or 1
;              in case ana is not provided. But if you call this procedure mutliple times
;              with the same filepath_out and EXTENSION keyword set, the procedure can not know
;              what the final total number of windows will be, and thus the header keyword 'NWIN' in the result extension
;              will have the wrong number. This will cause problems when reading the FITS file
;              with FITS2ANA. It is therefore highly recommended to provide this number,
;              when it is planned to add windows to the FITS file in different sessions.
;      winno: Window number (starting at 0) of the first 'ana' provided within this study in this FITS file.
;              Default is zero. If you call this procedure mutliple times with the same filepath_out and 
;              EXTENSION keyword set, you can define here what the index of the currently provided
;              first 'ana' should be. This will be set in the header keyword 'WINNO' in the result extension.
;              A wrong number in this keyword won't create any problems when reading the FITS file
;              with FITS2ANA.
;
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
; This procedure saves one or more ANA structures as FITS files.
; 
; RESTRICTIONS:
; It is possible to call this procedure multiple times with the same filepath_out,
; if in these cases the EXTENSION keyword is set, the windows will be appended to the 
; existing FITS file. However, one needs to 
; 
; MAKE SURE THAT THE HEADER KEYWORD "NWIN" IS CORRECTLY SET.
; 
; See description of n_windows for more details.
;
; HISTORY:
;      Ver. 1, 19-Jan-2022, Martin Wiesmann
;-
; $Id: 2022-11-24 14:52 CET $


PRO ana2fits, ana, filepath_out=filepath_out, data_id=data_id, $
  n_windows=n_windows, winno=winno, $
  HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
  FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  EXTENSION=EXTENSION

  prits_tools.parcheck, ana, 1, 'ana', 'STRUCT', [0, 1], structure_name='CFIT_ANALYSIS', /optional
  ana_given = N_ELEMENTS(ana)
  prits_tools.parcheck, HISTORY, 0, 'HISTORY', 'STRING', 1, optional=ana_given
  prits_tools.parcheck, LAMBDA, 0, 'LAMBDA', 'NUMERIC', [1, 2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, INPUT_DATA, 0, 'INPUT_DATA', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, WEIGHTS, 0, 'WEIGHTS', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0, optional=ana_given
  prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, RESIDUAL, 0, 'RESIDUAL', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, INCLUDE, 0, 'INCLUDE', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, CONST, 0, 'CONST', 'NUMERIC', [2, 3, 4, 5, 6, 7], optional=ana_given
  prits_tools.parcheck, FILENAME_ANA, 0, 'FILENAME_ANA', 'STRING', 0, optional=ana_given
  prits_tools.parcheck, DATASOURCE, 0, 'DATASOURCE', 'STRING', 0, optional=ana_given
  prits_tools.parcheck, DEFINITION, 0, 'DEFINITION', 'STRING', 0, optional=ana_given
  prits_tools.parcheck, MISSING, 0, 'MISSING', 'NUMERIC', 0, optional=ana_given
  prits_tools.parcheck, LABEL, 0, 'LABEL', 'STRING', 0, optional=ana_given

  prits_tools.parcheck, filepath_out, 0, 'filepath_out', 'STRING', 0
  prits_tools.parcheck, n_windows, 0, 'n_windows', 'INTEGERS', 0, default=0
  prits_tools.parcheck, winno, 0, 'winno', 'INTEGERS', 0, default=0
  prits_tools.parcheck, data_id, 0, 'data_id', 'STRING', [0, 1], VALID_NELEMENTS=max([1, ana_given]), $
    default=strtrim(indgen(max([1, ana_given])), 2)

  filename_out = file_basename(filepath_out)

  n_ana = N_ELEMENTS(ana)
  if n_ana eq 0 then n_ana=1
  n_windows = max([n_ana, n_windows])
  for iwindow=0,n_ana-1 do begin
    input_type = size(ana[iwindow], /type)

    extension = keyword_set(extension) || iwindow GT 0

    if input_type then begin
      headers = ana2fitshdr(ana[iwindow], filename_out=filename_out, n_windows=n_windows, $
        winno=iwindow+winno, data_id=data_id[iwindow], extension=extension, $
        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
    endif else begin
      headers = ana2fitshdr(filename_out=filename_out, n_windows=n_windows, $
        winno=iwindow+winno, data_id=data_id[iwindow], extension=extension, $
        HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
        FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
        CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
        DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
    endelse

    writefits, filepath_out, RESULT, *headers[0], append=extension
    writefits, filepath_out, INPUT_DATA, *headers[1], /append
    writefits, filepath_out, LAMBDA, *headers[2], /append
    writefits, filepath_out, RESIDUAL, *headers[3], /append
    writefits, filepath_out, WEIGHTS, *headers[4], /append
    writefits, filepath_out, INCLUDE, *headers[5], /append
    writefits, filepath_out, CONST, *headers[6], /append

  endfor ; iwindow=0,n_windows-1

END
