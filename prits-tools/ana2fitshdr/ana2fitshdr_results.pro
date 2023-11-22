;+
; NAME:
;      ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This is a subfunction of ANA2FITSHDR, which is a subfunction of ANA2FITS.
;      This function returns a fits header made from the results of an ANA object or file.
;      The fits header contains all fit components as keywords.
;      In case the SPICE keyword has been set, the the header will also contain original
;      level 2 header keywords.
;
; CATEGORY:
;      FITS -- utility -- ANA2FITS -- ANA2FITSHDR
;
; CALLING SEQUENCE:
;      header = ana2fitshdr_results(datetime=datetime, $
;        filename_out=filename_out, data_id=data_id, n_windows=n_windows, $
;        winno=winno, EXTENSION=EXTENSION, $
;        HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME_ANA, $
;        DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
;        spice=spice, header_l2=header_l2)
;
; INPUTS:
;      datetime: Date and time string.
;      filename_out: The filename the FITS file will/should get
;      data_id: A string defining the prefix to the names of the 6 extensions
;      n_windows: Number of windows to be included in the FITS file.
;      winno: Window number (starting at 0) within this study in this FITS file
;      HISTORY: A string array.
;      FIT: The component fit structure
;      RESULT: The array to contain the result parameter values (and
;              the Chi^2) values. May contain current results.
;      FILENAME_ANA: The filename of the ANA-file.
;      DATASOURCE: A string.
;      DEFINITION: A string.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been
;               declared as "FAILED".
;      LABEL: A string.
;
; KEYWORDS:
;      EXTENSION: If set, then this header will be marked to be an extension,
;                 i.e. if this is not the first window in the FITS file.
;                 If not set, this will be the primary header.
;
; OPTIONAL INPUTS:
;      WCS: Structure. The structure from which the WCS parameters
;             should be taken. If not provided the header won't include any WCS parameters.
;      HEADER_INPUT_DATA: The header (string array), that belongs to either INPUT_DATA or PROGENITOR_DATA,
;            respectively. If not provided a generic header will be created.
;      SPICE: If set, then 'header_l2' will be assumed to be from a level 2 SPICE FITS file
;                 and incorporated into this level 3 FITS file.
;                 This keyword should be set to a structure or array of structures.
;                 Each structure contains the tags: step, proc, version, lib and params.
;                 Those describe the processing steps taken to produce a SPICE level 3 file.
;      PROC_STEPS: Structure.
;      LEVEL: Number or string. The data level. If not provided this keyword will not be in the header.
;      VERSION: Number or string. The version number of this file. If not provided this keyword will not be in the header.
;      PROJ_KEYWORDS: Structure with additional project-related keywords that should be
;                 added to the header.
;
; OUTPUTS:
;      a fits header (string array).
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      oslo_fits_util, mkhdr, fxpar, ana2fitshdr_addwcs, fxaddpar
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2023-11-22 15:22 CET $


FUNCTION ana2fitshdr_results, datetime=datetime, $
  filename_out=filename_out, data_id=data_id, n_windows=n_windows, $
  winno=winno, EXTENSION=EXTENSION, $
  HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, WCS=WCS, $
  PROC_STEPS=PROC_STEPS, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
  LEVEL=LEVEL, VERSION=VERSION, PROJ_KEYWORDS=PROJ_KEYWORDS, $

  FILENAME_ANA=FILENAME_ANA, $
  DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL

  n_dims = size(result, /n_dimensions)
  header_exists = keyword_set(HEADER_INPUT_DATA)
  wcs_exists = keyword_set(WCS)

  fits_util = obj_new('oslo_fits_util')
  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', extension_names[0], 'Extension name'
  fits_util->add, hdr, 'FILENAME', filename_out, 'Filename of this FITS file'

  IF header_exists THEN BEGIN
    fits_util->add, hdr, 'PGFILENA', fxpar(HEADER_INPUT_DATA, 'FILENAME', missing=''), 'Progenitor filename'
    fits_util->add, hdr, 'PARENT', fxpar(HEADER_INPUT_DATA, 'FILENAME', missing=''), 'Parent filename'  ; TODO: Don't need both, which one?
    bunit = fxpar(header_l2, 'BUNIT', missing='')
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
  if N_ELEMENTS(history) EQ 0 then history=''
  ind = where(history NE '', count)
  if count gt 0 then begin
    history_string = strjoin(history[ind], ';')
    history_string = 'ANA_HISTORY: ' + history_string
  endif else history_string = ''
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

  
  
  
; Processing steps
; ; TODO  
  
fits_util->add_description, hdr, 'Processing steps'
    max_version_number = get_last_prstep_keyword(header_l2, count=count, pr_keywords=pr_keywords, ind_pr_keywords=ind_pr_keywords, $
      pr_versions=pr_versions, pr_types=pr_types)
    IF count gt 0 THEN BEGIN
      ind = where(pr_types eq 'LIB' AND pr_versions eq max_version_number, count)
      after = pr_keywords[ind[0]]
    ENDIF
    for istep=N_ELEMENTS(PROC_STEPS)-1,0,-1 do begin
      new_version = strtrim(string(max_version_number+1+istep), 2)
      prstep = PROC_STEPS[istep]
      fits_util->add, hdr, 'PRLIB'+new_version+'A', prstep.lib, 'Software library containing PRPROC'+new_version, after=after
      IF prstep.params NE '' THEN $
        fits_util->add, hdr, 'PRPARA'+new_version, prstep.params, 'Parameters for PRPROC'+new_version, after=after
      fits_util->add, hdr, 'PRPVER'+new_version, prstep.version, 'Version of procedure PRPROC'+new_version, after=after
      fits_util->add, hdr, 'PRPROC'+new_version, prstep.proc, 'Name of procedure performing PRSTEP'+new_version, after=after
      fits_util->add, hdr, 'PRSTEP'+new_version, prstep.step, 'Processing step type ', after=after
      fits_util->add, hdr, '', ' ', after=after
    endfor
  
  



  fits_util->clean_header, hdr
  
  IF history_string NE '' THEN FXADDPAR, hdr, 'HISTORY', history_string
  
  return, hdr
end
