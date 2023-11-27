;+
; NAME:
;      ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the results of an ANA object or file.
;      The fits header contains all fit components as keywords, as well as
;      other ANA structure tags.
;      It is possible to add project-related keywords or processing steps keywords
;      to the header, by using the keywords PROJ_KEYWORDS and PROC_STEPS, respectively.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_results(RESULT=RESULT, FIT=FIT, datetime=datetime, $
;           filename_out=filename_out, n_windows=n_windows, winno=winno, EXTENSION_NAMES=EXTENSION_NAMES, $
;           /IS_EXTENSION, $
;           HEADER_INPUT_DATA=HEADER_INPUT_DATA, WCS=WCS, $
;           LEVEL=LEVEL, VERSION=VERSION, $
;           PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
;           HISTORY=HISTORY, FILENAME_ANA=FILENAME_ANA, $
;           DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
;
; INPUTS:
;      datetime: Date and time string.
;      filename_out: The filename the FITS file will/should get.
;      n_windows: Number of windows to be included in the FITS file.
;      winno: Window number (starting at 0) within this study in this FITS file
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;
; KEYWORDS:
;      IS_EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the FITS file.
;                 If not set, this will be the primary header.
;
; OPTIONAL INPUTS:
;      HEADER_INPUT_DATA: The header (string array), that belongs to either INPUT_DATA or PROGENITOR_DATA,
;            respectively. If not provided a generic header will be created.
;      WCS: Structure. The structure from which the WCS parameters
;             should be taken. If not provided the header won't include any WCS parameters.
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      PROJ_KEYWORDS: A list or array of structures of type {name:'', value:'', comment:''} with additional project-related 
;                 keywords that should be added to the header.
;      PROC_STEPS: A list, each element stands for one processing step, i.e. gets a new number.
;                 Each processing step consists of an array of structures of type {name:'', value:'', comment:''}
;                 The type can be any of the following:
;                 PRSTEP|PRPROC|PRPVER|PRMODE|PRPARA|PRREF|PRLOG|PRENV|PRVER|PRHSH|PRBRA|PRLIB
;                 PRSTEP should be included. The name and the comment will get a number between 1 and 99 added.
;      HISTORY: A string array.
;      FILENAME_ANA: The filename of the ANA-file. This will be ignored.
;      DATASOURCE: A string. This will be ignored.
;      DEFINITION: A string. This will be ignored.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been
;               declared as "FAILED". This will be ignored. It is assumed
;               that missing values is NAN.
;      LABEL: A string. This will be ignored.
;
; OUTPUTS:
;      a fits header (string array).
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, mkhdr, fxpar, ana2fitshdr_addwcs, fxaddpar, get_last_prstep_keyword
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2023-11-27 13:42 CET $


FUNCTION ana2fitshdr_results, RESULT=RESULT, FIT=FIT, datetime=datetime, $
  filename_out=filename_out, n_windows=n_windows, winno=winno, EXTENSION_NAMES=EXTENSION_NAMES, $
  IS_EXTENSION=IS_EXTENSION, $
  HEADER_INPUT_DATA=HEADER_INPUT_DATA, WCS=WCS, $
  LEVEL=LEVEL, VERSION=VERSION, $
  PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
  HISTORY=HISTORY, FILENAME_ANA=FILENAME_ANA, $
  DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL

  prits_tools.parcheck, RESULT, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7]
  prits_tools.parcheck, FIT, 0, 'FIT', 'STRUCT', 0

  prits_tools.parcheck, datetime, 0, 'datetime', 'STRING', 0
  prits_tools.parcheck, FILENAME_OUT, 0, 'FILENAME_OUT', 'STRING', 0
  prits_tools.parcheck, N_WINDOWS, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, WINNO, 0, 'WINNO', 'INTEGERS', 0
  prits_tools.parcheck, EXTENSION_NAMES, 0, 'EXTENSION_NAMES', 'STRING', 1, VALID_NELEMENTS=6

  prits_tools.parcheck, HEADER_INPUT_DATA, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional=1
  prits_tools.parcheck, WCS, 0, 'WCS', 8, 0, /optional
  prits_tools.parcheck, LEVEL, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, VERSION, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, PROC_STEPS, 0, 'PROC_STEPS', 11, 1, /optional
  prits_tools.parcheck, PROJ_KEYWORDS, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional

  prits_tools.parcheck, HISTORY, 0, 'HISTORY', 'STRING', [0, 1], optional=1
  prits_tools.parcheck, FILENAME_ANA, 0, 'FILENAME_ANA', 'STRING', 0, optional=1
  prits_tools.parcheck, DATASOURCE, 0, 'DATASOURCE', 'STRING', 0, optional=1
  prits_tools.parcheck, DEFINITION, 0, 'DEFINITION', 'STRING', 0, optional=1
  prits_tools.parcheck, MISSING, 0, 'MISSING', 'NUMERIC', 0, optional=1
  prits_tools.parcheck, LABEL, 0, 'LABEL', 'STRING', 0, optional=1

  header_exists = keyword_set(HEADER_INPUT_DATA)
  wcs_exists = keyword_set(WCS)

  fits_util = obj_new('oslo_fits_util')
  if keyword_set(IS_EXTENSION) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_names[0], 'Extension name'
  fits_util->add, hdr, 'FILENAME', filename_out, 'Filename of this FITS file'

  IF header_exists THEN BEGIN
    fits_util->add, hdr, 'PGFILENA', fxpar(HEADER_INPUT_DATA, 'FILENAME', missing=''), 'Progenitor filename'
    fits_util->add, hdr, 'PARENT', fxpar(HEADER_INPUT_DATA, 'FILENAME', missing=''), 'Parent filename'  ; TODO: Don't need both, which one?
    bunit = fxpar(HEADER_INPUT_DATA, 'BUNIT', missing='')
  ENDIF ELSE BEGIN
    bunit = ''
  ENDELSE

  fits_util->add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', extension_names[1], 'Extension name of data'
  fits_util->add, hdr, 'XDIMXT1', extension_names[2], 'Extension name of 1st dim absorbed by analysis'
  fits_util->add, hdr, 'WGTEXT', extension_names[3], 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', extension_names[4], 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', extension_names[5], 'Extension name of constants'

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'NXDIM', 1, 'Number of dimensions absorbed by analysis'
  IF wcs_exists THEN BEGIN
    cunit_absorb = WCS.cunit[0]
    fits_util->add, hdr, 'XDIMTY1', WCS.CTYPE[0], 'Type of 1st dim absorbed by analysis'
  ENDIF ELSE BEGIN
    cunit_absorb = ''
    fits_util->add, hdr, 'XDIMTY1', 'Original type of absorbed dimension', 'Type of 1st dim absorbed by analysis'
  ENDELSE

  fits_util->add, hdr, 'SOLARNET', 1, 'Fully/Partially/No SOLARNET compliant (1/0.5/-1)'
  fits_util->add, hdr, 'OBS_HDU', 2, 'HDU contains SOLARNET Type P data'   ; TODO: comment?

  fits_util->add, hdr, 'NWIN', n_windows, 'Number of windows'
  fits_util->add, hdr, 'WINNO', winno, 'Win no (starting at 0) within this study in this FITS file'

  IF keyword_set(LEVEL) THEN $
    fits_util->add, hdr, 'LEVEL', strtrim(LEVEL,2), 'Data processing level'
  IF keyword_set(VERSION) THEN $
    fits_util->add, hdr, 'VERSION', strtrim(VERSION,2), 'Incremental file version number'

  ; Add keywords valid for whole ANA
  fits_util->add_description, hdr, 'Keywords describing the whole ANA'
  ;  fits_util->add, hdr, 'ANA_FILE', filename_ana, 'ANA filename'
  ;  fits_util->add, hdr, 'ANA_SRC', datasource, 'ANA datasource'
  ;  fits_util->add, hdr, 'ANA_DEF', definition, 'ANA definition'
  ;  fits_util->add, hdr, 'ANA_MISS', missing, 'ANA missing value in fitted data'
  ;  fits_util->add, hdr, 'ANA_LABL', label, 'ANA label'
  ;  if N_ELEMENTS(history) EQ 0 then history=''
  ;  ind = where(history NE '', count)
  ;  if count gt 0 then begin
  ;    history_string = strjoin(history[ind], ';')
  ;    history_string = 'ANA_HISTORY: ' + history_string
  ;  endif else history_string = ''
  ;  fits_util->add, hdr, 'ANA_HIST', history_string, 'ANA history'
  n_components = N_TAGS(fit)
  fits_util->add, hdr, 'ANA_NCMP', n_components, 'Number of fit components'

  for itag=0,n_components-1 do begin
    ; Add keywords for each fit component
    fitnr = strtrim(string(itag+1), 2)
    fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
    fit_cur = fit.(itag)
    CASE fit_cur.FUNC_NAME OF
      'comp_gauss': component_type = 'Gaussian'
      'comp_poly': component_type = 'Polynomial'
      'comp_bgauss': component_type = 'SSW comp_bgauss'
      'comp_voigt': component_type = 'SSW comp_voigt'
      else: component_type = fit_cur.FUNC_NAME
    ENDCASE
    fits_util->add, hdr, 'CMPTYP'+fitnr, component_type, 'Type of fit component '+fitnr
    fits_util->add, hdr, 'CMPNAM'+fitnr, fit_cur.NAME, 'Name of fit component '+fitnr
    fits_util->add, hdr, 'CMPSTR'+fitnr, fit_cur.FUNC_STRING, 'Function string of fit component '+fitnr
    ind = where(fit_cur.description NE '', count)
    if count gt 0 then description = strjoin(fit_cur.description[ind], ';') $
    else description = ''
    fits_util->add, hdr, 'CMPDES'+fitnr, description, 'Description of fit component '+fitnr
    fits_util->add, hdr, 'CMPMUL'+fitnr, fit_cur.MULTIPLICATIVE, 'Indicates whether component is multiplicative'
    fits_util->add, hdr, 'CMPINC'+fitnr, fit_cur.INCLUDE, 'Indicates whether component is included in fit'
    n_params = N_ELEMENTS(fit_cur.param)
    fits_util->add, hdr, 'CMP_NP'+fitnr, n_params, 'Number of parameters in fit component '+fitnr
  
    velocity = 0
    for ipar=0,n_params-1 do begin
      ; Add keywords for each fit parameter
      param = fit_cur.param[ipar]
      parnr = string(byte(ipar+97))
      fits_util->add, hdr, 'PNAME'+fitnr+parnr, param.name, 'Name of parameter '+parnr+' for component '+fitnr
      IF param.name EQ 'intensity' THEN BEGIN
        punit = bunit
      ENDIF ELSE BEGIN
        IF param.name EQ 'velocity' OR velocity THEN BEGIN
          punit = 'km/s'
          velocity = 1
        ENDIF ELSE BEGIN
          punit = cunit_absorb
        ENDELSE
      ENDELSE
      fits_util->add, hdr, 'PUNIT'+fitnr+parnr, punit, 'Phys. unit of parameter '+parnr+' for component '+fitnr
      ind = where(param.description NE '', count)
      if count gt 0 then description = strjoin(param.description[ind], ';') $
      else description = ''
      fits_util->add, hdr, 'PDESC'+fitnr+parnr, description, 'Description of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PINIT'+fitnr+parnr, param.initial, 'Initial value of parameter '+parnr+' for component '+fitnr
      ;fits_util->add, hdr, 'PVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PMAX'+fitnr+parnr, param.max_val, 'Maximum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PMIN'+fitnr+parnr, param.min_val, 'Minimum value of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PTRA'+fitnr+parnr, param.trans_a, 'Linear coefficient A in Lambda=PVAL*PTRA+PTRB'
      fits_util->add, hdr, 'PTRB'+fitnr+parnr, param.trans_b, 'Linear coefficient B in Lambda=PVAL*PTRA+PTRB'
      fits_util->add, hdr, 'PCONS'+fitnr+parnr, param.const, '1 if parameter '+parnr+' for component '+fitnr+' is constant'
    endfor ; ipar0,n_params-1
  endfor ; itag=0,N_TAGS(fit)-1
  
  ; Add keywords for Chi^2
  fitnr = strtrim(string(n_components+1), 2)
  fits_util->add_description, hdr, 'Keywords describing fit component '+fitnr
  fits_util->add, hdr, 'CMPTYP'+fitnr, 'Polynomial', 'Type of component '+fitnr
  fits_util->add, hdr, 'CMPNAM'+fitnr, 'Error of fit curve (Chi^2)', 'Name of component '+fitnr
  fits_util->add, hdr, 'CMP_NP'+fitnr, 1, 'Number of parameters in component '+fitnr
  ipar=0
  parnr = string(byte(ipar+97))
  fits_util->add, hdr, 'PNAME'+fitnr+parnr, 'Chi^2', 'Name of parameter '+parnr+' for component '+fitnr
  
  fits_util->add, hdr, ' ', ' '
  fits_util->add, hdr, 'BTYPE', ' '
  fits_util->add, hdr, 'BUNIT', ' '

  hdr = ana2fitshdr_addwcs(HDR, WCS, /RESULT)


  ; Add additional project-related keywords to the header
  IF N_ELEMENTS(PROJ_KEYWORDS) GT 0 THEN BEGIN
    fits_util->add_description, hdr, 'Project-related keywords'
    for ipr=0,N_ELEMENTS(PROJ_KEYWORDS)-1 DO BEGIN
      fits_util->add, hdr, PROJ_KEYWORDS[ipr].name, PROJ_KEYWORDS[ipr].value, PROJ_KEYWORDS[ipr].comment
    endfor ; ipr
    fits_util->add, hdr, '', ' ', after=after    
  ENDIF
  

  ; Processing steps
  fits_util->add_description, hdr, 'Processing steps'
  max_version_number = get_last_prstep_keyword(HEADER_INPUT_DATA, count=count, pr_keywords=pr_keywords, ind_pr_keywords=ind_pr_keywords, $
    pr_versions=pr_versions, pr_types=pr_types)
  IF count gt 0 THEN BEGIN
    FOR ipr=0,count-1 DO BEGIN
      pr_value = fxpar(HEADER_INPUT_DATA, pr_keywords[ipr], missing='', comment=comment)
      fits_util->add, hdr, pr_keywords[ipr], pr_value, comment
    ENDFOR ; ipr
    ind = where(pr_versions eq max_version_number)
    max_ind_hdr = max(ind_pr_keywords[ind], max_ind)
    after = pr_keywords[max_ind]
    procstep1 = N_ELEMENTS(PROC_STEPS)-1
    procstep2 = 0
    procdstep = -1
  ENDIF ELSE BEGIN
    procstep1 = 0
    procstep2 = N_ELEMENTS(PROC_STEPS)-1
    procdstep = 1
  ENDELSE
  for istep=procstep1,procstep2,procdstep do begin
    new_version = strtrim(string(max_version_number+1+istep), 2)
    prstep = PROC_STEPS[istep]
    if count gt 0 then begin
      prstep1 = N_ELEMENTS(prstep)-1
      prstep2 = 0
      prdstep = -1
    endif else begin
      prstep1 = 0
      prstep2 = N_ELEMENTS(prstep)-1
      prdstep = 1
    endelse
    for ipr=prstep1,prstep2,prdstep DO BEGIN
      fits_util->add, hdr, prstep[ipr].name+new_version, prstep[ipr].value, prstep[ipr].comment+new_version, after=after
    endfor ; ipr
    fits_util->add, hdr, '', ' ', after=after
  endfor ; istep

  fits_util->clean_header, hdr

  IF header_exists THEN BEGIN
    prg_history = fxpar(HEADER_INPUT_DATA, 'HISTORY', missing='')
    IF prg_history[0] NE '' THEN BEGIN
      FOR i=0,N_ELEMENTS(prg_history)-1 DO FXADDPAR, hdr, 'HISTORY', prg_history[i]
    ENDIF
  ENDIF
  IF history[0] NE '' THEN BEGIN
    FOR i=0,N_ELEMENTS(history)-1 DO FXADDPAR, hdr, 'HISTORY', 'ANA: '+history[i]
  ENDIF

  return, hdr
end
