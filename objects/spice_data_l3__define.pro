;+
; NAME:
;     SPICE_DATA_L3__DEFINE
;
; PURPOSE:
;     spice_data_l3__define defines the class structure 'spice_data_l3'.
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     The SPICE_DATA_L3__DEFINE procedure is not called directly. An
;     object of class SPICE_DATA_L3 is created with the following
;     statement:
;                 spice_data = obj_new('spice_data_l3', file)
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     Object of type SPICE_DATA_L3 which describes and contains a SPICE raster.
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;     The procedure opens an object of class SPICE_DATA_L3.
;     This procedure includes various functions/methods of
;     class  'spice_data_l3' whose purpose is to get and/or manipulate
;     the different fields of the object.
;
; RESTRICTIONS:
;
; HISTORY:
;     15-Jun-2023: Martin Wiesmann
;     18-Oct-2023: Terje Fredvik - PARAMETER-FITTING -> LINE-FITTING
;-
; $Id: 2023-10-18 15:27 CEST $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise
;-
FUNCTION spice_data_l3::init, file
  COMPILE_OPT IDL2

  prits_tools.parcheck, file, 1, "file", 'string', 0
  file_info = spice_file2info(file)
  if ~file_info.is_spice_file then begin
    print, 'File is not a SPICE file: '+file
    return, 0
  endif
  if file_info.level ne 3 then begin
    print, 'This is not a SPICE level 3 file: '+file
    return, 0
  endif
  self.file = file
  self.filename = file_info.filename
  self.filepath = file_dirname(file, /mark_directory)
  self.level = file_info.level
  self.study_type = file_info.study_type
  self.datetime = file_info.datetime
  self.version = file_info.version
  self.spiobsid = file_info.spiobsid
  self.rasterno = file_info.rasterno
  hdr = headfits(file, exten=0)
  self.nwin = fxpar(hdr, 'NWIN', missing=0)
  self.l2_filename = fxpar(hdr, 'PGFILENA', missing='')
  headers_results = ptrarr(self.nwin)
  headers_results[0] = ptr_new(hdr)
  for i=1,self.nwin-1 do begin
    hdr = headfits(file, exten=7*i)
    headers_results[i] = ptr_new(hdr)
  endfor
  self.headers_results = ptr_new(headers_results)
  return, 1
END


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro spice_data_l3::cleanup
  COMPILE_OPT IDL2

  FOR i=0,N_ELEMENTS(*self.headers_results)-1 DO ptr_free, (*self.headers_results)[i]
  ptr_free,self.headers_results
END


;+
; Description:
;     This routine prints out information about the class, such as name, location of definition file
;     and version if there is a line in the header comment beginning with '$ID: ' (comes from CVS).
;     Then it prints out each procedure and function that has a comment line right after the definition.
;
; KEYWORD PARAMETERS:
;     description : If set, the header info of the class will also be printed.
;
;-
pro spice_data_l3::help, description=description, _extra=_extra
  ;Prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  IF arg_present(description) || keyword_set(description) THEN $
    obj_help, self, description=description, _extra=_extra $
  ELSE $
    obj_help, self, _extra=_extra
END


;+
; Description:
;     This routine calls xcfit_block with the data of the chosen window.
;
; OPTIONAL INPUTS:
;     window_index : The index of the desired window, default is 0.
;
; OUTPUT:
;     Array of ana structure, number of elements is the same as number of windows in the FITS file.
;     Output is scalar if there is only one window.
;     Output is zero if an error occurred.
;
; OPTIONAL OUTPUT:
;     headers_results: A pointer array, containing the headers of the results extensions as string arrays.
;     headers_data: A pointer array, containing the headers of the data extensions as string arrays.
;     headers_lambda: A pointer array, containing the headers of the lambda extensions as string arrays.
;     headers_residuals: A pointer array, containing the headers of the residuals extensions as string arrays.
;     headers_weights: A pointer array, containing the headers of the weights extensions as string arrays.
;     headers_include: A pointer array, containing the headers of the include extensions as string arrays.
;     headers_constants: A pointer array, containing the headers of the constants extensions as string arrays.
;-
function spice_data_l3::xcfit_block, window_index, $
  headers_results=headers_results, headers_data=headers_data, $
  headers_lambda=headers_lambda, headers_residuals=headers_residuals, headers_weights=headers_weights, $
  headers_include=headers_include, headers_constants=headers_constants
  ;Calls xcfit_block with the data of the chosen window(s)
  COMPILE_OPT IDL2

  ana = fits2ana(self.file, windows=window_index, $
    headers_results=headers_results, headers_data=headers_data, $
    headers_lambda=headers_lambda, headers_residuals=headers_residuals, headers_weights=headers_weights, $
    headers_include=headers_include, headers_constants=headers_constants)
  
  if size(ana, /type) EQ 8 then begin
    origin = [0,0,0]
    scale = [1,1,1]
    phys_scale = [0,0,0]
    spice_data_l3.get_plot_variables, *headers_data[0], origin=origin, scale=scale, phys_scale=phys_scale
    SPICE_XCFIT_BLOCK, ana=ana, origin=origin, scale=scale, phys_scale=phys_scale
  endif else begin
    print, 'Something went wrong when trying to reproduce an ANA structure.'
  endelse

  return, ana
END


;+
; Description:
;     This routine calculates the origin and the scale of the datacube, to be used in xcfit_block.
;
; INPUTS:
;     header_data : A string array. The header of the data extension of a SPICE level 3 FTIS file.
;
; OPTIONAL OUTPUT:
;     origin: A 3-element array. The starting values of each axis.
;     scale: A 3-element array. The CDELT values of each exis.
;     phys_scale: A 3-element array. Boolean values, whether the axis should be scaled physically.
;-
pro spice_data_l3::get_plot_variables, header_data, origin=origin, scale=scale, phys_scale=phys_scale
  COMPILE_OPT IDL2, static
  
  crval = fxpar(header_data, 'CRVAL1', missing=1)
  naxis = fxpar(header_data, 'NAXIS1', missing=1)
  crpix = fxpar(header_data, 'CRPIX1', missing=0)
  cdelt1 = fxpar(header_data, 'CDELT1', missing=1)
  pc1_1 = fxpar(header_data, 'PC1_1', missing=0)
  x_vector = crval + cdelt1 * pc1_1 * (findgen(naxis)+1.0-crpix)
  IF naxis EQ 1 THEN BEGIN
    naxis = fxpar(header_data, 'NAXIS4', missing=1)
    x_vector = replicate(x_vector, naxis)
  ENDIF

  crval = fxpar(header_data, 'CRVAL2', missing=1)
  naxis = fxpar(header_data, 'NAXIS2', missing=1)
  crpix = fxpar(header_data, 'CRPIX2', missing=0)
  cdelt2 = fxpar(header_data, 'CDELT2', missing=1)
  pc2_2 = fxpar(header_data, 'PC2_2', missing=0)
  y_vector = crval + cdelt2 * pc2_2 * (findgen(naxis)+1.0-crpix)

  crval = fxpar(header_data, 'CRVAL3', missing=1)
  naxis = fxpar(header_data, 'NAXIS3', missing=1)
  crpix = fxpar(header_data, 'CRPIX3', missing=0)
  cdelt3 = fxpar(header_data, 'CDELT3', missing=1)
  lambda_vector = crval + cdelt3 * (findgen(naxis)+1.0-crpix)
  
  origin = [ lambda_vector[0], x_vector[0], y_vector[0] ]
  scale = [ cdelt3, cdelt1, cdelt2 ]
  phys_scale = [ 0, 1, 1 ]
end


;+
; Description:
;     Returns the level 2 filename that was used to create this level 3 file.
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     Name of the level 2 file.
;-
FUNCTION spice_data_l3::get_l2_filename
  COMPILE_OPT IDL2

  return, self.l2_filename
END


;+
; Description:
;     Finds and returns the level 2 file that was used to create this level 3 file.
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; KEYWORD PARAMETERS:
;     USER_DIR: If set, the procedure searches in TOP_DIR/user/ instead of TOP_DIR/.
;
; OUTPUT:
;     Full path and name of the level 2 file, if it exists, otherwise an empty string.
;-
FUNCTION spice_data_l3::find_l2_file, user_dir=user_dir
  COMPILE_OPT IDL2

  file_l2 = spice_find_file(self.datetime, remove_duplicates=0, user_dir=user_dir)
  filename_l2 = file_basename(file_l2)
  pgfilena = self->get_l2_filename()
  ind = where(filename_l2 eq pgfilena, count)
  if count gt 0 then result = file_l2[ind[0]] else result = ''
  return, result
END


;+
; Description:
;     Finds and returns the level 3 file(s) that was produced with the given level 2 file.
;
; INPUTS:
;     file_l2 : filename of a SPICE FITS level 2 file.
;
; KEYWORD PARAMETERS:
;     USER_DIR: If set, the procedure searches in TOP_DIR/user/ instead of TOP_DIR/.
;     latest: If set, then only the level 3 file with highest version number will be returned.
;
; OPTIONAL OUTPUTS:
;     l3_objects : the found level 3 files as SPICE_DATA_L3 objects. Either a scalar object,
;                  or an array of objects.
;     count: Number of level 3 files found.
;
; OUTPUT:
;     Full path and name of the level 3 file(s), that made by the given level 2 file.
;     This is an empty string if no files were found, a scalar string if one file was found or /latest keyword was set
;     or an array of strings if multiple files were found.
;-
FUNCTION spice_data_l3::find_l3_file_from_l2, file_l2, user_dir=user_dir, l3_objects=l3_objects, COUNT=COUNT, latest=latest
  COMPILE_OPT IDL2, static

  file_l2_info = spice_file2info(file_l2)
  file_l3_all = spice_find_file(file_l2_info.datetime, remove_duplicates=0, user_dir=user_dir, level=3, COUNT_FILE=COUNT_FILE)
  count = 0
  IF count_file eq 0 then return, ''
  FOR ifile=0,count_file-1 DO BEGIN
    file_l3_info = spice_file2info(file_l3_all[ifile])
    IF file_l3_info.spiobsid NE file_l2_info.spiobsid || $
      file_l3_info.rasterno NE file_l2_info.rasterno THEN continue

    l3_obj_temp = spice_data_l3(file_l3_all[ifile])
    l2_file_temp = l3_obj_temp->get_l2_filename()
    l2_version = l2_file_temp.extract('V[0-9]{2}')
    l2_version = fix(l2_version.substring(1,2))
    IF l2_version EQ file_l2_info.version THEN BEGIN
      count++
      IF N_ELEMENtS(found_l3_file) EQ 0 THEN BEGIN
        found_l3_file = file_l3_all[ifile]
        l3_objects = l3_obj_temp
      ENDIF ELSE BEGIN
        found_l3_file = [found_l3_file, file_l3_all[ifile]]
        l3_objects = [l3_objects, l3_obj_temp]
      ENDELSE
    ENDIF
  ENDFOR

  IF keyword_set(latest) && count GT 1 THEN BEGIN
    versions = found_l3_file.extract('V[0-9]{2}')
    versions = fix(versions.substring(1,2))
    max_version = max(versions, maxind)
    found_l3_file = found_l3_file[maxind]
    l3_objects = l3_objects[maxind]
  ENDIF
  return, found_l3_file
END


;+
; Description:
;     Returns all processing step keywords that are related with producing this level 3 file.
;
; OUTPUT:
;     A pointer array of pointers containting each
;     an array of structures, containing the keywords PRSTEPn, PRPROCn, PRPVERn, PRLIBn, PRPARA.
;     This structure was used as input to ana2fitshdr
;-
FUNCTION spice_data_l3::get_l3_processing_steps, headers_results
  COMPILE_OPT IDL2
  
  IF N_ELEMENTS(headers_results) EQ 0 THEN headers_results = *self.headers_results
  nwin = N_ELEMENTS(headers_results)
  l3_pr_steps_all = ptrarr(nwin)

  for iwin=0,nwin-1 do begin
    
    hdr = *(headers_results)[iwin]

    max_version = get_last_prstep_keyword(hdr, count=count, pr_keywords=pr_keywords, ind_pr_keywords=ind_pr_keywords, $
      pr_versions=pr_versions, pr_types=pr_types)
    ind = where(pr_keywords.startswith('PRSTEP'), count_step)
    l3_pr_steps = !NULL
    FOR istep=0,count_step-1 DO BEGIN
      prstep = fxpar(hdr, pr_keywords[ind[istep]], missing='')
      IF prstep EQ 'LINE-FITTING' || prstep EQ 'PEAK-FINDING' THEN BEGIN
        
        ind_proc = where(pr_keywords.startswith('PRPROC') AND pr_versions EQ pr_versions[ind[istep]], count_proc)
        IF count_proc GT 0 THEN proc = fxpar(hdr, pr_keywords[ind_proc[0]], missing='') $
        ELSE proc = '' 
        
        ind_version = where(pr_keywords.startswith('PRPVER') AND pr_versions EQ pr_versions[ind[istep]], count_version)
        IF count_version GT 0 THEN version = fix(fxpar(hdr, pr_keywords[ind_version[0]], missing=0L), type=3) $
        ELSE version = 0L
  
        ind_lib = where(pr_keywords.startswith('PRLIB') AND pr_versions EQ pr_versions[ind[istep]], count_lib)
        IF count_lib GT 0 THEN lib = fxpar(hdr, pr_keywords[ind_lib[0]], missing='') $
        ELSE lib = '' 
  
        ind_params = where(pr_keywords.startswith('PRPARA') AND pr_versions EQ pr_versions[ind[istep]], count_params)
        IF count_params GT 0 THEN params = fxpar(hdr, pr_keywords[ind_params[0]], missing='') $
        ELSE params = ''
        
        temp = {step:prstep, proc:proc, version:version, lib:lib, params:params}
        
        IF N_ELEMENTS(l3_pr_steps) EQ 0 THEN BEGIN
          l3_pr_steps = [temp]
        ENDIF ELSE BEGIN
          l3_pr_steps = [l3_pr_steps, temp]
        ENDELSE
      ENDIF
    ENDFOR
    
    l3_pr_steps_all[iwin] = ptr_new(l3_pr_steps)
    
  endfor

  return, l3_pr_steps_all
END


;+
; Description:
;     Class definition procedure
;-
PRO spice_data_l3__define
  COMPILE_OPT IDL2

  struct = {spice_data_l3, $
    file:'', $            ; full filename as given by the user
    filename:'', $        ; name of the file, without the path
    filepath:'', $        ; path of the file, without the name
    level:-1, $           ; data level (0, 1, 2 or 3), -1 if unknown
    study_type:'', $      ; type of study
    datetime:'', $        ; date and time in CCSDS format of observation
    version:-1, $         ; version number (version of the spice data pipeline)
    spiobsid:-1L, $       ; SPICE OBS ID
    rasterno:-1, $        ; raster repetition number
    nwin:-1, $            ; number of windows in this file
    l2_filename:'', $     ; filename of level 2 file
    headers_results:ptr_new() $ ; A pointer array to the header strings of the results extensions
  }
END
