;+
; NAME:
;      ANA2FITSHDR_RESULTS
;
; PURPOSE:
;      This function returns a fits header made from the results of an ANA object or file.
;      The fits header contains all fit components as keywords. 
;      In case the SPICE keyword has been set, the the header will also contain original
;      level 2 header keywords. 
;
; CATEGORY:
;      FITS -- utility
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
;      data_id: A string defining the prefix to the names of the 7 extensions
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
;      header_l2: The header (string array) of the SPICE level 2 file.
;      SPICE: If set, then 'header_l2' will be assumed to be from a level 2 SPICE FITS file
;                 and incorporated into this level 3 FITS file.
;                 This keyword should be set to a structure or array of structures.
;                 Each structure contains the tags: step, proc, version, lib and params.
;                 Those describe the processing steps taken to produce a SPICE level 3 file.
;
; OUTPUTS:
;      a fits header (string array)
;
; OPTIONAL OUTPUTS:
;
; HISTORY:
;      Ver. 1, 23-Nov-2021, Martin Wiesmann
;-
; $Id: 2023-06-27 13:35 CEST $


FUNCTION ana2fitshdr_results, datetime=datetime, $
  filename_out=filename_out, data_id=data_id, n_windows=n_windows, $
  winno=winno, EXTENSION=EXTENSION, $
  HISTORY=HISTORY, FIT=FIT, RESULT=RESULT, FILENAME_ANA=FILENAME_ANA, $
  DATASOURCE=DATASOURCE, DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
  spice=spice, header_l2=header_l2_in

  spice_header = keyword_set(spice) && keyword_set(header_l2_in)
  n_dims = size(result, /n_dimensions)

  fits_util = obj_new('oslo_fits_util')
  if keyword_set(extension) then mkhdr, hdr, result, /image $
  else mkhdr, hdr, result, /extend

  fits_util->add, hdr, 'DATE', datetime, 'Date and time of FITS file creation'
  fits_util->add, hdr, '', ' '

  fits_util->add, hdr, 'EXTNAME', data_id+' results', 'Extension name'
  fits_util->add, hdr, 'FILENAME', filename_out, 'Filename of this FITS file'

  IF spice_header THEN BEGIN
    header_l2 = header_l2_in
    fits_util->add, hdr, 'PGEXTNAM', fxpar(header_l2, 'EXTNAME', missing=''), 'Extension name in progenitor file'
    fits_util->add, hdr, 'PGFILENA', fxpar(header_l2, 'FILENAME', missing=''), 'Progenitor filename'
  ENDIF

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

  fits_util->add, hdr, 'RESEXT', data_id+' results', 'Extension name of results'
  fits_util->add, hdr, 'DATAEXT', data_id+' data', 'Extension name of data'
;  fits_util->add, hdr, 'LAMBDEXT', data_id+' lambda', 'Extension name of lambda'
  fits_util->add, hdr, 'RESIDEXT', data_id+' residuals', 'Extension name of residuals'
  fits_util->add, hdr, 'WGTEXT', data_id+' weights', 'Extension name of weights'
  fits_util->add, hdr, 'INCLEXT', data_id+' includes', 'Extension name of includes'
  fits_util->add, hdr, 'CONSTEXT', data_id+' constants', 'Extension name of constants'

  fits_util->add, hdr, '', ' '
  fits_util->add, hdr, 'NXDIM', 1, 'Number of dimensions absorbed by analysis'
  IF spice_header THEN BEGIN
    fits_util->add, hdr, 'XDIMTY1', fxpar(header_l2, 'CTYPE3', missing=''), 'Type of 1st dim absorbed by analysis'
  ENDIF ELSE BEGIN
    fits_util->add, hdr, 'XDIMTY1', 'Original type of absorbed dimension', 'Type of 1st dim absorbed by analysis'
  ENDELSE
  fits_util->add, hdr, 'XDIMXT1', data_id+' lambda', 'Extension name of 1st dim absorbed by analysis'

  IF spice_header THEN BEGIN
    bunit = fxpar(header_l2, 'BUNIT', missing='')
    cunit_absorb = fxpar(header_l2, 'CTYPE3', missing='')
  ENDIF ELSE BEGIN
    bunit = ''
    cunit_absorb = ''
  ENDELSE
  
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
      fits_util->add, hdr, 'PUNIT'+fitnr+parnr, param.name, 'Phys. unit of parameter '+parnr+' for component '+fitnr
      ind = where(param.description NE '', count)
      if count gt 0 then description = strjoin(param.description[ind], ';') $
      else description = ''
      fits_util->add, hdr, 'PDESC'+fitnr+parnr, description, 'Description of parameter '+parnr+' for component '+fitnr
      fits_util->add, hdr, 'PINIT'+fitnr+parnr, param.initial, 'Initial value of parameter '+parnr+' for component '+fitnr
;      fits_util->add, hdr, 'PVAL'+fitnr+parnr, param.value, 'Value of parameter '+parnr+' for component '+fitnr
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

  IF spice_header THEN BEGIN

    ; Add level 2 header to this header
    ; but withou doubling this keyword
    temp = fxpar(hdr, 'LONGSTRN', count=count_l3)
    temp = fxpar(header_l2, 'LONGSTRN', count=count_l2)
    IF count_l3 && count_l2 then fits_util->remove_keyword, header_l2, 'LONGSTRN', /remove_comment

    ind_start = where(strmatch(header_l2, '*Study parameters valid for all Obs-HDUs in this file*') eq 1, count_l2)
    if count_l2 eq 1 then begin
      ind_end = where(strmatch(hdr, 'END *') eq 1, count_l3)
      if count_l3 ne 1 then begin
        print, 'hm, no END'
        stop
      endif
      hdr = [hdr[0:ind_end-1], header_l2[ind_start-3:*]]
    endif
    
    hdr.replace('Study parameters valid for all Obs-HDUs in this file', 'Study parameters valid for all Obs-HDUs in the L2 file')
    hdr.replace('Other keywords valid for all Obs-HDUs in this file', 'Other keywords valid for all Obs-HDUs in the L2 file')
    hdr.replace('Keywords valid for this HDU', 'Keywords valid for the L2 HDU')
    
    fits_util->add, hdr, 'SOLARNET', 1, 'Fully/Partially/No SOLARNET compliant (1/0.5/-1)'
    fits_util->add, hdr, 'OBS_HDU', 2, 'HDU contains SOLARNET Type P data'   ; TODO: comment?
    fits_util->add, hdr, 'PARENT', fxpar(header_l2, 'FILENAME', missing=''), 'L2 file'

    ; Adapt WCS keywords
    fits_util->add, hdr, 'CTYPE1', 'FIT PARAMETER', 'Type of 1st coordinate'
    fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
    fits_util->add, hdr, 'CUNIT1', ' ', 'Units for 1st coordinate (for CRVAL1, CDELT1)'
    fits_util->add, hdr, 'CRVAL1', 1.0, '[] 1st coordinate of reference point'
    fits_util->add, hdr, 'CDELT1', 1.0, '[] Increment of 1st coord at ref point'
    fits_util->add, hdr, 'CRPIX1', 1.0, '[pixel] 1st pixel index of reference point'
    fits_util->add, hdr, 'PC1_1', 1.0, 'Default value, no rotation'
    sxdelpar, hdr, 'PC1_2'
  
    fits_util->add, hdr, 'CTYPE2', fxpar(header_l2, 'CTYPE1', missing=''), 'Type of 2nd coordinate'
    fits_util->add, hdr, 'CNAME2', fxpar(header_l2, 'CNAME1', missing=''), 'Name of 2nd coordinate'
    fits_util->add, hdr, 'CUNIT2', fxpar(header_l2, 'CUNIT1', missing=''), 'Units for 2nd coordinate (for CRVAL2, CDELT2)'
    fits_util->add, hdr, 'CRVAL2', fxpar(header_l2, 'CRVAL1', missing=0), '[arcsec] 2nd coordinate of reference point'
    fits_util->add, hdr, 'CDELT2', fxpar(header_l2, 'CDELT1', missing=0), '[arcsec] Increment of 2nd coord at ref point'
    fits_util->add, hdr, 'CRPIX2', fxpar(header_l2, 'CRPIX1', missing=0), '[pixel] 2nd pixel index of reference point '
    fits_util->add, hdr, 'PC2_2', fxpar(header_l2, 'PC1_1', missing=0), 'Non-default value due to CROTA degrees S/C roll'
    fits_util->add, hdr, 'PC2_3', fxpar(header_l2, 'PC1_2', missing=0), 'Contribution of dim 3 to coord 2 due to roll', after='PC2_2'
    sxdelpar, hdr, 'PC2_1'
  
    fits_util->add, hdr, 'CTYPE3', fxpar(header_l2, 'CTYPE2', missing=''), 'Type of 3rd coordinate'
    fits_util->add, hdr, 'CNAME3', fxpar(header_l2, 'CNAME2', missing=''), 'Name of 3rd coordinate'
    fits_util->add, hdr, 'CUNIT3', fxpar(header_l2, 'CUNIT2', missing=''), 'Units for 3rd coordinate (for CRVAL3, CDELT3)'
    fits_util->add, hdr, 'CRVAL3', fxpar(header_l2, 'CRVAL2', missing=0), '[arcsec] 3rd coordinate of reference point'
    fits_util->add, hdr, 'CDELT3', fxpar(header_l2, 'CDELT2', missing=0), '[arcsec] Increment of 3rd coord at ref point'
    fits_util->add, hdr, 'CRPIX3', fxpar(header_l2, 'CRPIX2', missing=0), '[pixel] 3rd pixel index of reference point '
    fits_util->add, hdr, 'PC3_2', fxpar(header_l2, 'PC2_1', missing=0), 'Contribution of dim 2 to coord 3 due to roll', before='PC3_3'
    fits_util->add, hdr, 'PC3_3', fxpar(header_l2, 'PC2_2', missing=0), 'Non-default value due to CROTA degrees S/C roll'
  
    pc4_1 = fxpar(header_l2, 'PC4_1', missing=-9999)
    if pc4_1 gt -9998 then begin
      fits_util->add, hdr, 'PC4_2', fxpar(header_l2, 'PC4_1', missing=0), 'Contribution of dim 2 to coord 4 due to roll', after='PC4_4'
    endif
  
    file_info_l3 = spice_file2info(filename_out)
    fits_util->add, hdr, 'LEVEL', 'L3', 'Data processing level'
    fits_util->add, hdr, 'VERSION', fns('##',file_info_l3.version), 'Incremental file version number'
    fits_util->add, hdr, 'L2NWIN', fxpar(header_l2, 'NWIN', missing=0), 'Total no of windows (incl. db and int win) in L2 file', after='NWIN'
    fits_util->add, hdr, 'L2WINNO', fxpar(header_l2, 'WINNO', missing=0), 'Win no (starting at 0) within this study in L2 file', after='WINNO'
    
    max_version_number = get_last_prstep_keyword(header_l2, count=count, pr_keywords=pr_keywords, ind_pr_keywords=ind_pr_keywords, $
      pr_versions=pr_versions, pr_types=pr_types)
    IF count gt 0 THEN BEGIN
      ind = where(pr_types eq 'LIB' AND pr_versions eq max_version_number, count)
      after = pr_keywords[ind[0]]
    ENDIF
    for istep=N_ELEMENTS(spice)-1,0,-1 do begin
      new_version = strtrim(string(max_version_number+1+istep), 2)
      prstep = spice[istep]
      fits_util->add, hdr, 'PRLIB'+new_version+'A', prstep.lib, 'Software library containing PRPROC'+new_version, after=after
      IF prstep.params NE '' THEN $
        fits_util->add, hdr, 'PRPARA'+new_version, prstep.params, 'Parameters for PRPROC'+new_version, after=after
      fits_util->add, hdr, 'PRPVER'+new_version, prstep.version, 'Version of procedure PRPROC'+new_version, after=after
      fits_util->add, hdr, 'PRPROC'+new_version, prstep.proc, 'Name of procedure performing PRSTEP'+new_version, after=after
      fits_util->add, hdr, 'PRSTEP'+new_version, prstep.step, 'Processing step type ', after=after
      fits_util->add, hdr, '', ' ', after=after
    endfor
    
  ENDIF ELSE BEGIN ; spice_header

    fits_util->add, hdr, 'SOLARNET', 1, 'Fully/Partially/No SOLARNET compliant (1/0.5/-1)'
    fits_util->add, hdr, 'OBS_HDU', 2, 'HDU contains SOLARNET Type P data'   ; TODO: comment?

    ; Add WCS keywords
    fits_util->add, hdr, 'CTYPE1', 'PARAMETER', 'Type of 1st coordinate'
    fits_util->add, hdr, 'CNAME1', 'Parameter', 'Name of 1st coordinate'
    for idim=1,n_dims-1 do begin
      idim_str = strtrim(string(idim+1), 2)
      case idim of
        1: dim_name = '2nd'
        2: dim_name = '3rd'
        else: dim_name = idim_str+'th'
      end
      fits_util->add, hdr, 'CTYPE'+idim_str, 'Original type of '+dim_name+' coordinate', 'Type of '+dim_name+' coordinate'
      fits_util->add, hdr, 'CNAME'+idim_str, 'Original name of '+dim_name+' coordinate', 'Name of '+dim_name+' coordinate'
    endfor ; idim=1,n_dims-1
  
    fits_util->add_description, hdr, 'Keywords valid for this HDU'
  
  ENDELSE ; spice_header

  fits_util->add, hdr, 'BTYPE', ' '
  fits_util->add, hdr, 'UCD', ' '
  fits_util->add, hdr, 'BUNIT', ' '

  fits_util->add, hdr, 'NWIN', n_windows, 'Number of windows'
  fits_util->add, hdr, 'WINNO', winno, 'Win no (starting at 0) within this study in this FITS file'

  fits_util->clean_header, hdr
  
  IF history_string NE '' THEN FXADDPAR, hdr, 'HISTORY', history_string

  return, hdr
end
