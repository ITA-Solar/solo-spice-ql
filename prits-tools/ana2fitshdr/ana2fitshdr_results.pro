;+
; NAME:
;      ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the results of an ANA object.
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
;           LEVEL=LEVEL, VERSION=VERSION, CREATOR=CREATOR, $
;           PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
;           HISTORY=HISTORY, FILENAME_ANA=FILENAME_ANA, $
;           DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)
;
; PARAMETERS:
;     Most parameters are described in ANA2FITS.
;     The only difference is that most of the parameters in ANA2FITS can be arrays, i.e. contain multiple
;     datasets/windows, whereas the parameters in this function are for one dataset/window only.
;     Parameters not described in ANA2FITS are described here.
;
; INPUTS:
;      DATETIME: Date and time string.
;      EXTENSION_NAMES: String array with the names of the 6 other extensions of the same dataset/window.
;
; OPTIONAL INPUTS:
;      WCS: Structure. The structure from which the WCS parameters
;             should be taken. If not provided the header won't include any WCS parameters.
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
; $Id: 2025-05-05 14:52 CEST $

FUNCTION ana2fitshdr_results, result = result, fit = fit, datetime = datetime, $
  filename_out = filename_out, n_windows = n_windows, winno = winno, extension_names = extension_names, $
  DATA_EXT_PATH = DATA_EXT_PATH, $
  is_extension = is_extension, $
  header_input_data = header_input_data, wcs = wcs, $
  level = level, version = version, creator = creator, $
  proc_steps = proc_steps, proj_keywords = proj_keywords, $
  history = history, filename_ana = filename_ana, $
  datasource = datasource, definition = definition, missing = missing, label = label
  prits_tools.parcheck, result, 0, 'RESULT', 'NUMERIC', [2, 3, 4, 5, 6, 7]
  prits_tools.parcheck, fit, 0, 'FIT', 'STRUCT', 0

  prits_tools.parcheck, datetime, 0, 'datetime', 'STRING', 0
  prits_tools.parcheck, filename_out, 0, 'FILENAME_OUT', 'STRING', 0
  prits_tools.parcheck, n_windows, 0, 'N_WINDOWS', 'INTEGERS', 0
  prits_tools.parcheck, winno, 0, 'WINNO', 'INTEGERS', 0
  prits_tools.parcheck, extension_names, 0, 'EXTENSION_NAMES', 'STRING', 1, valid_nelements = 6

  prits_tools.parcheck, header_input_data, 0, 'HEADERS_INPUT_DATA', 'STRING', 1, optional = 1
  prits_tools.parcheck, wcs, 0, 'WCS', 8, 0, /optional
  prits_tools.parcheck, level, 0, 'LEVEL', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, version, 0, 'VERSION', ['NUMERIC', 'STRING'], 0, /optional
  prits_tools.parcheck, proc_steps, 0, 'PROC_STEPS', 11, 1, /optional
  prits_tools.parcheck, proj_keywords, 0, 'PROJ_KEYWORDS', [8, 11], [0, 1], /optional

  prits_tools.parcheck, history, 0, 'HISTORY', 'STRING', [0, 1], optional = 1
  prits_tools.parcheck, filename_ana, 0, 'FILENAME_ANA', 'STRING', 0, optional = 1
  prits_tools.parcheck, datasource, 0, 'DATASOURCE', 'STRING', 0, optional = 1
  prits_tools.parcheck, definition, 0, 'DEFINITION', 'STRING', 0, optional = 1
  prits_tools.parcheck, missing, 0, 'MISSING', 'NUMERIC', 0, optional = 1
  prits_tools.parcheck, label, 0, 'LABEL', 'STRING', 0, optional = 1

  header_exists = keyword_set(header_input_data)
  wcs_exists = keyword_set(wcs)

  fits_util = obj_new('oslo_fits_util')
  IF keyword_set(is_extension) THEN mkhdr, hdr, result, /image $
  ELSE mkhdr, hdr, result, /extend

  fits_util.add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util.add, hdr, '', ' '

  fits_util.add, hdr, 'SOLARNET', 1, 'Fully/Part/Not SOLARNET compliant (1/0.5/-1)'
  fits_util.add, hdr, 'OBS_HDU', 2, 'HDU contains SOLARNET Type P data'
  fits_util.add, hdr, '', ' '

  fits_util.add, hdr, 'EXTNAME', extension_names[0], 'Extension name'
  fits_util.add, hdr, 'FILENAME', file_basename(filename_out), 'Filename of this FITS file'

  IF header_exists THEN BEGIN
    fits_util.add, hdr, 'PARENTXT', DATA_EXT_PATH, 'Parent filename and path;extension name'
    bunit = fxpar(header_input_data, 'BUNIT', missing = '')
  ENDIF ELSE BEGIN
    bunit = ''
  ENDELSE

  fits_util.add, hdr, 'RESEXT', extension_names[0], 'Extension name of results'
  fits_util.add, hdr, 'DATAEXT', extension_names[1], 'Extension name of original data'
  fits_util.add, hdr, 'WGTEXT', extension_names[2], 'Extension name of weights'
  fits_util.add, hdr, 'INCLEXT', extension_names[3], 'Extension name of includes'
  fits_util.add, hdr, 'CONSTEXT', extension_names[4], 'Extension name of constants'
  fits_util.add, hdr, 'RESIDEXT', extension_names[5], 'Extension name of residuals'

  fits_util.add, hdr, '', ' '
  IF wcs_exists THEN BEGIN
    cunit_absorb = wcs.cunit[0]
    fits_util.add, hdr, 'XTYPE1', wcs.CTYPE[0], 'Type of 1st dim absorbed by analysis'
  ENDIF ELSE BEGIN
    cunit_absorb = ''
  ENDELSE

  fits_util.add, hdr, 'NWIN', n_windows, 'Number of windows'
  fits_util.add, hdr, 'WINNO', winno, 'Win no (starting at 0) within this study in this FITS file'

  IF keyword_set(level) THEN $
    fits_util.add, hdr, 'LEVEL', level, 'Data processing level'
  IF keyword_set(version) THEN $
    fits_util.add, hdr, 'VERSION', version, 'File version number'

  IF header_exists THEN BEGIN
    fits_util.add, hdr, 'DATE-BEG', fxpar(header_input_data, 'DATE-BEG', missing = ''), 'Beginning of data acquisition'
    instrume = fxpar(header_input_data, 'INSTRUME', missing = '')
    IF instrume NE '' THEN fits_util.add, hdr, 'INSTRUME', instrume, 'Instrument name'
    OBSRVTRY = fxpar(header_input_data, 'OBSRVTRY', missing = '')
    IF OBSRVTRY NE '' THEN fits_util.add, hdr, 'OBSRVTRY', OBSRVTRY, 'Observatory name'
  ENDIF ELSE BEGIN
    fits_util.add, hdr, 'DATE-BEG', '', 'Beginning of data acquisition'
  ENDELSE

  IF keyword_set(creator) THEN $
    fits_util.add, hdr, 'CREATOR', creator, 'Name of creator'

  ; Add keywords valid for whole ANA
  fits_util.add_description, hdr, 'Keywords describing the whole ANA'
  ; fits_util->add, hdr, 'ANA_FILE', filename_ana, 'ANA filename'
  ; fits_util->add, hdr, 'ANA_SRC', datasource, 'ANA datasource'
  ; fits_util->add, hdr, 'ANA_DEF', definition, 'ANA definition'
  ; fits_util->add, hdr, 'ANA_MISS', missing, 'ANA missing value in fitted data'
  ; fits_util->add, hdr, 'ANA_LABL', label, 'ANA label'
  ; if N_ELEMENTS(history) EQ 0 then history=''
  ; ind = where(history NE '', count)
  ; if count gt 0 then begin
  ; history_string = strjoin(history[ind], ';')
  ; history_string = 'ANA_HISTORY: ' + history_string
  ; endif else history_string = ''
  ; fits_util->add, hdr, 'ANA_HIST', history_string, 'ANA history'
  n_components = n_tags(fit)
  fits_util.add, hdr, 'ANA_NCMP', n_components, 'Number of fit components'

  FOR itag = 0, n_components - 1 DO BEGIN
    ; Add keywords for each fit component
    fitnr = strtrim(string(itag + 1), 2)
    fits_util.add_description, hdr, 'Keywords describing fit component ' + fitnr
    fit_cur = fit.(itag)
    CASE fit_cur.FUNC_NAME OF
      'comp_gauss': component_type = 'Gaussian'
      'comp_poly': component_type = 'Polynomial'
      'comp_bgauss': component_type = 'SSW comp_bgauss'
      'comp_voigt': component_type = 'SSW comp_voigt'
      ELSE: component_type = fit_cur.FUNC_NAME
    ENDCASE
    fits_util.add, hdr, 'CMPTYP' + fitnr, component_type, 'Type of fit component ' + fitnr
    fits_util.add, hdr, 'CMPNAM' + fitnr, fit_cur.NAME, 'Name of fit component ' + fitnr
    fits_util.add, hdr, 'CMPSTR' + fitnr, fit_cur.FUNC_STRING, 'Function string of fit component ' + fitnr
    ind = where(fit_cur.description NE '', count)
    IF count GT 0 THEN description = strjoin(fit_cur.description[ind], ';') $
    ELSE description = ''
    fits_util.add, hdr, 'CMPDES' + fitnr, description, 'Description of fit component ' + fitnr
    fits_util.add, hdr, 'CMPMUL' + fitnr, fit_cur.MULTIPLICATIVE, 'Indicates whether component is multiplicative'
    fits_util.add, hdr, 'CMPINC' + fitnr, fit_cur.INCLUDE, 'Indicates whether component is included in fit'
    n_params = n_elements(fit_cur.param)
    fits_util.add, hdr, 'CMP_NP' + fitnr, n_params, 'Number of parameters in fit component ' + fitnr

    velocity = 0
    FOR ipar = 0, n_params - 1 DO BEGIN
      ; Add keywords for each fit parameter
      param = fit_cur.param[ipar]
      parnr = string(byte(ipar + 97))
      fits_util.add, hdr, 'PNAME' + fitnr + parnr, param.name, 'Name of parameter ' + parnr + ' for component ' + fitnr
      IF param.name EQ 'intensity' || strtrim(param.name, 2) EQ 'c0' THEN BEGIN
        punit = bunit
      ENDIF ELSE BEGIN
        IF param.name EQ 'velocity' || velocity THEN BEGIN
          punit = 'km/s'
          velocity = 1
        ENDIF ELSE BEGIN
          punit = cunit_absorb
        ENDELSE
      ENDELSE
      fits_util.add, hdr, 'PUNIT' + fitnr + parnr, punit, 'Phys. unit of parameter ' + parnr + ' for component ' + fitnr
      ind = where(param.description NE '', count)
      IF count GT 0 THEN description = strjoin(param.description[ind], ';') $
      ELSE description = ''
      fits_util.add, hdr, 'PDESC' + fitnr + parnr, description, 'Description of parameter ' + parnr + ' for component ' + fitnr
      fits_util.add, hdr, 'PINIT' + fitnr + parnr, param.initial, 'Initial value of parameter ' + parnr + ' for component ' + fitnr
      ; fits_util->add, hdr, 'PVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
      fits_util.add, hdr, 'PMAX' + fitnr + parnr, param.max_val, 'Maximum value of parameter ' + parnr + ' for component ' + fitnr
      fits_util.add, hdr, 'PMIN' + fitnr + parnr, param.min_val, 'Minimum value of parameter ' + parnr + ' for component ' + fitnr
      fits_util.add, hdr, 'PTRA' + fitnr + parnr, param.trans_a, 'Linear coefficient A in Lambda=PVAL*PTRA+PTRB'
      fits_util.add, hdr, 'PTRB' + fitnr + parnr, param.trans_b, 'Linear coefficient B in Lambda=PVAL*PTRA+PTRB'
      fits_util.add, hdr, 'PCONS' + fitnr + parnr, param.const, '1 if parameter ' + parnr + ' for component ' + fitnr + ' is constant'
    ENDFOR ; ipar0,n_params-1
  ENDFOR ; itag=0,N_TAGS(fit)-1

  ; Add keywords for Chi^2
  fitnr = strtrim(string(n_components + 1), 2)
  fits_util.add_description, hdr, 'Keywords describing fit component ' + fitnr
  fits_util.add, hdr, 'CMPTYP' + fitnr, 'Polynomial', 'Type of component ' + fitnr
  fits_util.add, hdr, 'CMPNAM' + fitnr, 'Error of fit curve (Chi^2)', 'Name of component ' + fitnr
  fits_util.add, hdr, 'CMP_NP' + fitnr, 1, 'Number of parameters in component ' + fitnr
  ipar = 0
  parnr = string(byte(ipar + 97))
  fits_util.add, hdr, 'PNAME' + fitnr + parnr, 'Chi^2', 'Name of parameter ' + parnr + ' for component ' + fitnr

  hdr = ana2fitshdr_addwcs(hdr, wcs, /result)

  fits_util.add, hdr, ' ', ' '
  fits_util.add, hdr, 'BTYPE', 'Fit Parameter', 'Type of data'
  fits_util.add, hdr, 'UCD', 'stat.fit.param', 'Unified Content Descriptors v1.23'
  fits_util.add, hdr, 'BUNIT', ' ', 'Units of the data'

  ; Add additional project-related keywords to the header
  IF n_elements(proj_keywords) GT 0 THEN BEGIN
    fits_util.add_description, hdr, 'Project-related keywords'
    FOR ipr = 0, n_elements(proj_keywords) - 1 DO BEGIN
      fits_util.add, hdr, (proj_keywords[ipr])['name'], (proj_keywords[ipr])['value'], (proj_keywords[ipr])['comment']
    ENDFOR ; ipr
    fits_util.add, hdr, '', ' ', after = after
  ENDIF

  ; Processing steps
  fits_util.add_description, hdr, 'Processing steps'
  max_version_number = get_last_prstep_keyword(header_input_data, count = count, pr_keywords = pr_keywords, ind_pr_keywords = ind_pr_keywords, $
    pr_versions = pr_versions)
  IF count GT 0 THEN BEGIN
    FOR ipr = 0, count - 1 DO BEGIN
      pr_value = fxpar(header_input_data, pr_keywords[ipr], missing = '', comment = comment)
      fits_util.add, hdr, pr_keywords[ipr], pr_value, comment
    ENDFOR ; ipr
    ind = where(pr_versions EQ max_version_number)
    !NULL = max(ind_pr_keywords[ind], max_ind)
    after = pr_keywords[ind[max_ind]]
    procstep1 = n_elements(proc_steps) - 1
    procstep2 = 0
    procdstep = -1
  ENDIF ELSE BEGIN
    procstep1 = 0
    procstep2 = n_elements(proc_steps) - 1
    procdstep = 1
  ENDELSE
  FOR istep = procstep1, procstep2, procdstep DO BEGIN
    new_version = strtrim(string(max_version_number + 1 + istep), 2)
    prstep = proc_steps[istep]
    IF count GT 0 THEN BEGIN
      prstep1 = n_elements(prstep) - 1
      prstep2 = 0
      prdstep = -1
    ENDIF ELSE BEGIN
      prstep1 = 0
      prstep2 = n_elements(prstep) - 1
      prdstep = 1
    ENDELSE
    FOR ipr = prstep1, prstep2, prdstep DO BEGIN
      fits_util.add, hdr, (prstep[ipr])['name'] + new_version, (prstep[ipr])['value'], (prstep[ipr])['comment'] + new_version, after = after
    ENDFOR ; ipr
    fits_util.add, hdr, '', ' ', after = after
  ENDFOR ; istep

  fits_util.clean_header, hdr

  IF header_exists THEN BEGIN
    prg_history = fxpar(header_input_data, 'HISTORY', missing = '')
    IF prg_history[0] NE '' THEN BEGIN
      FOR i = 0, n_elements(prg_history) - 1 DO fxaddpar, hdr, 'HISTORY', prg_history[i]
    ENDIF
  ENDIF
  IF n_elements(history) GT 0 && history[0] NE '' THEN BEGIN
    FOR i = 0, n_elements(history) - 1 DO fxaddpar, hdr, 'HISTORY', 'ANA: ' + history[i]
  ENDIF

  return, hdr
END
