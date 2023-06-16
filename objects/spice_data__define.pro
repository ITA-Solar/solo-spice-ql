;+
; NAME:
;     SPICE_DATA__DEFINE
;
; PURPOSE:
;     spice_data__define defines the class structure 'spice_data'.
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     The SPICE_DATA__DEFINE procedure is not called directly. An
;     object of class SPICE_DATA is created with the following
;     statement:
;                 spice_data = obj_new('spice_data', file)
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     Object of type SPICE_DATA which describes and contains a SPICE raster
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;     The procedure opens an object of class SPICE_DATA.
;     This procedure includes various functions/methods of
;     class  'spice_data' whose purpose is to get and/or manipulate
;     the different fields of the object.
;
; RESTRICTIONS:
;
; HISTORY:
;     26-Nov-2019: Martin Wiesmann (based on IRIS_DATA__DEFINE)
;     31-Jan-2022: Terje Fredvik - New method ::mask_region_outside_slit with
;                  a little army of help methods is now called when the
;                  SLIT_ONLY keyword is set when calling ::get_window_data.
;                  * The SLIT_ONLY keyword is set when xcfit_block is called.
;     26-Apr-2023: Terje Fredvik: add keyword no_line in call of ::xcfit_block
;                                 and ::mk_analysis
;-
; $Id: 2023-06-16 10:58 CEST $


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
FUNCTION spice_data::init, file
  COMPILE_OPT IDL2

  self.title = 'SPICE'
  self.ccd_size = [1024, 1024]
  IF n_elements(file) EQ 1 THEN BEGIN
    self->read_file, file
    return, 1
  ENDIF ELSE return, 0
END


;+
; Description:
;     frees pointer to main data array "window_data" and closes all associated files.
;     used by cleanup and when object should be populated with new data
;-
pro spice_data::close
  COMPILE_OPT IDL2

  FOR i=0,self.nwin-1 DO BEGIN
    ptr_free, (*self.window_headers)[i]
    ptr_free, (*self.window_headers_string)[i]
    ptr_free, (*self.window_wcs)[i]
    IF ptr_valid((*self.window_data)[i]) THEN ptr_free, (*self.window_data)[i]
  ENDFOR
  ptr_free, self.window_headers
  ptr_free, self.window_headers_string
  ptr_free, self.window_wcs
  ptr_free, self.window_descaled
  ptr_free, self.window_data
  ptr_free, self.slit_y_range
  for i=0,self.n_bintable_columns-1 do begin
    if ptr_valid((*self.bintable_columns)[i].values) then ptr_free, (*self.bintable_columns)[i].values
  endfor
  ptr_free, self.bintable_columns
  self.dumbbells = [-1, -1]
  self.nwin = 0
END


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro spice_data::cleanup
  COMPILE_OPT IDL2

  self->close
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
pro spice_data::help, description=description, _extra=_extra
  ;Prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  IF arg_present(description) || keyword_set(description) THEN $
    obj_help, self, description=description, _extra=_extra $
  ELSE $
    obj_help, self, _extra=_extra
END


;+
; Description:
;     This routine calls xcfit_block with the data of the chosen window. The data is arranged
;     so that xcfit_block can read it. The routine also estimates the positions of the main peaks
;     and adds fit components to the analysis structure. After exiting xcfit_block
;     by using the 'Exit' button, the routine returns the analysis structure.
;
; OPTIONAL INPUTS:
;     window_index : The index of the desired window, default is 0.
;     VELOCITY : Set this equal to the initial velocity if you want
;                 the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength
;                 is taken from the supplied POSITION, i.e., INT_POS_FWHM(1), which is
;                 calculated/estimated within the procedure 'generate_adef'.
;                 This input is ignored if /POSITION is set.
;                 Default is zero.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;                 The keyword is ignored if NO_MASKING is set.
;     position: If set, then the line position is NOT represented by the velocity
;                 relative to a lab wavelength, but as the wavelength.
;
;-
function spice_data::xcfit_block, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  position=position, velocity=velocity, no_line_list=no_line_list
  ;Calls xcfit_block with the data of the chosen window(s)
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then window_index = 0
  ana = self->mk_analysis(window_index, no_masking=no_masking, approximated_slit=approximated_slit, position=position, velocity=velocity, no_line_list=no_line_list)
  if size(ana, /type) EQ 8 then begin
    SPICE_XCFIT_BLOCK, ana=ana
  endif else begin
    print, 'Something went wrong when trying to produce an ANA structure.'
  endelse

  return, ana
END


;+
; Description:
;     This routine creates a level 3 FITS file for the chosen window(s).
;     The data is arranged so that cfit_block can read it. The routine estimates
;     the positions of the main peaks and fits those to the data by calling cfit_block.
;     This may take a while. After that, the xcfit_block routine is called with
;     the data and the fit, where one can view the result and make adjustments to the fit.
;     After exiting xcfit_block by using the 'Exit' button, the routine saves the fits
;     into a level 3 FITS file and moves this file into the $SPICE_DATA/user/ directory.
;     The data cube will be saved in its original untransformed configuration.
;
; OPTIONAL INPUTS:
;     window_index : The index of the desired window(s), default is all windows.
;     VELOCITY : Set this equal to the initial velocity if you want
;                 the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength
;                 is taken from the supplied POSITION, i.e., INT_POS_FWHM(1), which is
;                 calculated/estimated within the procedure 'generate_adef'.
;                 This input is ignored if /POSITION is set.
;                 Default is zero.
;     TOP_DIR : A path to a directory in which the file should be saved. The necessary subdirectories
;                 will be created (e.g. level2/2020/06/21).
;     PATH_INDEX: If $SPICE_DATA contains multiple paths, then this
;                 keyword allows you to specify to which path you send
;                 the file. Default is 0.
;     progress_widget: An object of type SPICE_CREATE_L3_PROGRESS, to display the progress of the creation.
;     group_leader: Widget ID of parent widget.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;     no_fitting: If set, fitting won't be computed. This can still be done manually in xcfit_block.
;     no_widget:  If set, xcfit_block and small window to stopp fitting will not be called.
;     no_xcfit_block: If set, xcfit_block will not be called, but small window to stopp fitting will
;                 appear.
;     position: If set, then the line position is NOT represented by the velocity
;                 relative to a lab wavelength, but as the wavelength.
;     no_line_list: If set, then no predefined line list will be used to define gaussian fit components.
;                 By default, the list returned by the function spice_line_list() will be used.
;                 
;                 IMPORTANT NOTE: For now, this keyword is set by default. One has to set it explicitly to zero
;                 if one wants to use the predefined line list. This implementation may change in the future.
;                 
;                 Due to instrument temperature variations the wavelength scale changes significantly during
;                 the Solar Orbiter orbit, and this variation is not accounted for in L2 files. The wavelength shift is so large
;                 that using the line list when fitting fails in many cases.
;                 
;     official_l3dir: If set, the file will be moved to the directory $SPICE_DATA/level3, the directory
;                     for the official level 3 files, instead of $SPICE_DATA/user/level3.
;     save_not:   If set, then the FITS file will not be saved. The output is the path and name of the
;                 level 3 FITS file, if it would have been saved.
;     quiet:      If set, print messages will be suppressed.
;
; OPTIONAL OUTPUTS:
;     all_ana:    Array of ana structure, number of elements is the same as number of windows in the FITS file.
;     all_result_headers: A pointer array, containing the headers of the results extensions as string arrays.
;
; OUTPUT:
;     The path and name of the Level 3 FITS file.
;     Level 3 file, as FITS file, saved to directory $SPICE_DATA/level3/ .
;-

FUNCTION spice_data::get_version_l3, filename_l3, official_l3dir=official_l3dir
  ;; Finds all existing L3 files with the same SPIOBSID and RASTERNO as
  ;; filename_l3, then returns the highest existing version number incremented
  ;; by 1. If no L3 files exist the version number is set to 'V01' 
  stop
  spice_ingest,filename_l3, user_dir=~keyword_set(official_l3dir), /dry_run,/force, destination=destination
  l3_dir = file_dirname(destination)
  spiobsid_rasterno = filename_l3.extract('[0-9]+-[0-9]{3}')
  existing_l3_files = file_search(l3_dir, '*'+spiobsid_rasterno+'*', count=n_l3_files)
  IF n_l3_files EQ 0 THEN this_version = 'V01' ELSE BEGIN 
     versions = existing_l3_files.extract('V[0-9]{2}')
     versions = fix(versions.substring(1,2))
     this_version = 'V'+fns('##',max(versions)+1)
  ENDELSE 
  
  return, this_version
END

FUNCTION spice_data::get_filename_l3, official_l3dir=official_l3dir, version_l3=version_l3
  ;; Returns L3 filename based on L2 filename, with version number being the
  ;; highest version number of any existing L3 files incremented by 1.
  ;;
  filename_l2 = self.get_header_keyword('FILENAME', 0, '')
  version_l2 = filename_l2.extract('V[0-9]{2}')
  
  filename_l3 = filename_l2.replace('_L2_', '_L3_')
  stop
  version_l3 = self.get_version_l3(filename_l3,  official_l3dir=official_l3dir)
  
  filename_l3 = filename_l3.replace(version_l2, version_l3)
  
  return, filename_l3
END

FUNCTION spice_data::create_l3_file, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  no_fitting=no_fitting, no_widget=no_widget, no_xcfit_block=no_xcfit_block, position=position, velocity=velocity, $
  official_l3dir=official_l3dir, top_dir=top_dir, path_index=path_index, save_not=save_not, $
  all_ana=all_ana, all_result_headers=all_result_headers, no_line_list=no_line_list, $
  progress_widget=progress_widget, group_leader=group_leader, quiet=quiet
  ; Creates a level 3 file from the level 2
  COMPILE_OPT IDL2

  prits_tools.parcheck, progress_widget, 0, "progress_widget", 11, 0, object_name='spice_create_l3_progress', /optional
  IF N_ELEMENTS(progress_widget) EQ 0 && ~keyword_set(no_widget) THEN progress_widget=spice_create_l3_progress(1, group_leader=group_leader)

  IF ~ARG_PRESENT(no_line_list) THEN no_line_list=1 ; See note for this keyword in documentation
  
  if N_ELEMENTS(window_index) eq 0 then window_index = indgen(self->get_number_windows())
  IF ARG_PRESENT(all_ana) THEN collect_ana=1 ELSE collect_ana=0
  IF ARG_PRESENT(all_result_headers) THEN BEGIN
    all_result_headers = ptrarr(N_ELEMENTS(window_index))
    collect_hdr=1
  ENDIF ELSE collect_hdr=0

  filename_l2 = self.get_header_keyword('FILENAME', 0, '')
;  filename_l3 = filename_l2.replace('_L2_', '_L3_')
  stop
  filename_l3 = self.get_filename_l3(official_l3dir = official_l3dir, version_l3 = version_l3)
  stop
  filename_out = filepath(filename_l3, /tmp)
  file_info_l2 = spice_file2info(filename_l2)
   
 ; file_id = 'V' + fns('##', file_info_l2.version) + $
 ;   '_' + strtrim(string(file_info_l2.spiobsid), 2) + $
 ;           fns('-###', file_info_l2.rasterno)
    file_id = version_l3 + $
    '_' + strtrim(string(file_info_l2.spiobsid), 2) + $
    fns('-###', file_info_l2.rasterno)

  IF ~keyword_set(no_widget) THEN $
    progress_widget->next_file, N_ELEMENTS(window_index), filename=filename_l2, halt=halt

  for iwindow=0,N_ELEMENTS(window_index)-1 do begin

    IF ~keyword_set(no_widget) THEN BEGIN
      progress_widget->next_window, window_name=self.get_header_keyword('EXTNAME', window_index[iwindow], fns('Window ##', iwindow)), halt=halt
      if halt then begin
        print,'Calculation stopped'
        return, 'Cancelled'
      endif
    ENDIF

    ana = self->mk_analysis(window_index[iwindow], no_masking=no_masking, approximated_slit=approximated_slit, $
      position=position, velocity=velocity, /init_all_cubes, no_line_list=no_line_list)
    if size(ana, /type) NE 8 then continue
    IF collect_ana THEN BEGIN
      if iwindow eq 0 then all_ana = ana $
      else all_ana = [all_ana, ana]
    ENDIF

    if ~keyword_set(no_fitting) then begin
      if ~keyword_set(quiet) then begin
        print, '====================='
        print, 'fitting data'
        print, 'this may take a while'
        print, '====================='
      endif
      spice_cfit_block, analysis=ana, /quiet, /double, x_face=~keyword_set(no_widget), smart=1
    endif

    if ~keyword_set(no_widget) && ~keyword_set(no_xcfit_block) then begin
      SPICE_XCFIT_BLOCK, ana=ana, group_leader=group_leader
    endif

    data_id = file_id + fns(' ext##', self.get_header_keyword('WINNO', window_index[iwindow], 99))
    original_data = self->get_window_data(window_index[iwindow], no_masking=no_masking, approximated_slit=approximated_slit)
    if iwindow gt 0 then extension=1 else extension=0

    headers = ana2fitshdr(ana, header_l2=self->get_header(window_index[iwindow]), data_id=data_id, $
      extension=extension, filename_out=filename_l3, n_windows=N_ELEMENTS(window_index), winno=iwindow, $
      HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
      FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
      CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
      DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
      original_data=original_data, /spice)

    if iwindow eq 0 then file = filepath(filename_l3, /tmp)
    IF ~keyword_set(save_not) THEN BEGIN
      writefits, file, RESULT, *headers[0], append=extension
      writefits, file, original_data, *headers[1], /append
      writefits, file, LAMBDA, *headers[2], /append
      writefits, file, RESIDUAL, *headers[3], /append
      writefits, file, WEIGHTS, *headers[4], /append
      writefits, file, INCLUDE, *headers[5], /append
      writefits, file, CONST, *headers[6], /append
    ENDIF

    IF collect_hdr THEN all_result_headers[iwindow] = ptr_new(*headers[0])

  endfor ; iwindow=0,N_ELEMENTS(window_index)-1

  spice_ingest, file, destination=destination, file_moved=file_moved, files_found=files_found, $
    user_dir=~keyword_set(official_l3dir), top_dir=top_dir, path_index=path_index, /force, $
    dry_run=keyword_set(save_not)
  IF ~keyword_set(save_not) THEN print, 'Level 3 file saved to: ', destination
  return, destination
END


;+
; Description:
;     This procedure transforms the data of a chosen window, so that it can be used
;     in CFIT_BLOCK and XCFIT_BLOCK.
;
; OPTIONAL INPUTS:
;     window_index : The index of the desired window, default is 0.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;                 The keyword is ignored if NO_MASKING is set.
;     debug_plot: If set, make plots to illustrate which part of the window is being masked.
;                 This keyword is ignored if NO_MASKING is set.
;
; OPTIONAL OUTPUTS:
;      DATA: Data Array. Rearranged so that the spectra is on the first dimension.
;      LAMBDA: An array of wavelength values. One value for every point in the data array.
;      WEIGHTS: Weights to use in the fitting process. Same dimensions as DATA.
;               All pixels are set to 1.0.
;      MISSING: The MISSING value, used to flag missing data points,
;               and parameter values at points where the fit has been declared as "FAILED".
;
;-
PRO spice_data::transform_data_for_ana, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  debug_plot=debug_plot, $
  DATA=DATA, LAMBDA=LAMBDA, WEIGHTS=WEIGHTS, MISSING=MISSING
  ;Transforms data so that it can be used with cfit_block and xcfit_block.
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then window_index = 0

  data = self->get_window_data(window_index, no_masking=no_masking, approximated_slit=approximated_slit, debug_plot=debug_plot)
  ;; Only do fit on the spectral part of the window!
  lambda = self->get_wcs_coord(window_index, /lambda)

  size_data = size(data)
  if self->get_sit_and_stare() then begin
    lambda = transpose(lambda, [2, 0, 1, 3])
    data = transpose(data, [2, 0, 1, 3])
    weights = make_array(size_data[3], size_data[1], size_data[2], size_data[4], value=1.0)
  endif else begin
    naxis1 = self.get_header_keyword('naxis1', window_index)
    naxis2 = self.get_header_keyword('naxis2', window_index)
    naxis3 = self.get_header_keyword('naxis3', window_index)
    lambda = reform(lambda, [naxis1, naxis2, naxis3])
    lambda = transpose(lambda, [2, 0, 1])
    data = transpose(data, [2, 0, 1])
    weights = make_array(size_data[3], size_data[1], size_data[2], value=1.0)
  endelse
  type_data = size(data, /type)
  lambda = fix(lambda, type=type_data)
  missing = self->get_missing_value()
END


;+
; Description:
;     This procedure creates an ANA (analysis structure) with the data of a chosen window, so that it can be used
;     in CFIT_BLOCK and XCFIT_BLOCK. Fit components are estimated and added to ANA.
;     Calls TRANSFORM_DATA_FOR_ANA.
;
; OPTIONAL INPUTS:
;     window_index : The index of the desired window, default is 0.
;     VELOCITY : Set this equal to the initial velocity if you want
;                 the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength
;                 is taken from the supplied POSITION, i.e., INT_POS_FWHM(1), which is
;                 calculated/estimated within the procedure 'generate_adef'.
;                 This input is ignored if /POSITION is set.
;                 Default is zero.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;                 The keyword is ignored if NO_MASKING is set.
;     init_all_cubes: If set, then all cubes within the ANA will be initialised,
;                 otherwise, the cubes RESULT, RESIDUALS, INCLUDE and CONSTANT will
;                 be undefined.
;     position: If set, then the line position is NOT represented by the velocity
;                 relative to a lab wavelength, but as the wavelength.
;     no_line_list: If set, then no predefined line list will be used to define gaussian fit components.
;                 By default, the list returned by the function spice_line_list() will be used.
;     debug_plot: If set, make plots to illustrate which part of the window is being masked.
;                 This keyword is ignored if NO_MASKING is set.
;
; OUTPUT:
;     Returns an ANA structure.
;
;-
FUNCTION spice_data::mk_analysis, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  init_all_cubes=init_all_cubes, debug_plot=debug_plot, position=position, velocity=velocity, $
  no_line_list=no_line_list
  ;Creates an ANA (analysis structure) to be used with cfit_block and xcfit_block.
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then window_index = 0

  self->transform_data_for_ana, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
    debug_plot=debug_plot, $
    DATA=DATA, LAMBDA=LAMBDA, WEIGHTS=WEIGHTS, MISSING=MISSING

  detector = self->get_header_keyword('DETECTOR', window_index)
  widmin_pixels_2_arcsec_slit = (detector EQ 'SW') ? 7.8 : 9.4 ;; Fludra et al., A&A Volume 656, 2021
  slitwid_factor = 2./self->get_header_keyword('SLIT_WID', window_index)
  widmin_pixels = widmin_pixels_2_arcsec_slit * slitwid_factor
  widmin = widmin_pixels * self->get_header_keyword('CDELT3', window_index)

  IF ~keyword_set(no_line_list) THEN line_list=spice_line_list()
  adef = generate_adef(data, LAMbda, widmin=widmin, position=position, velocity=velocity, line_list=line_list)
  badix = where(data ne data, n_bad)
  IF n_bad GT 0 THEN data[badix] = missing

  ana = mk_analysis(LAMbda, DAta, WeighTS, adef, MISSing)

  if keyword_set(init_all_cubes) then begin
    handle_value, ana.fit_h, fit
    n_components = N_TAGS(fit)
    n_params = 0
    init_values = dblarr(1)
    for itag=0,n_components-1 do begin
      fit_cur = fit.(itag)
      n_params = n_params + N_ELEMENTS(fit_cur.param)
      for iparam=0,N_ELEMENTS(fit_cur.param)-1 do begin
        init_values = [init_values, fit_cur.param[iparam].initial]
      endfor
    endfor
    init_values = [init_values, missing]
    init_values = init_values[1:*]

    handle_value, ana.data_h, data
    sdata = size(data)
    if sdata[0] eq 3 then begin
      result = dblarr(n_params+1, sdata[2], sdata[3])
      for i=0,n_params do result[i,*,*] = init_values[i]
      residual = fltarr(sdata[1], sdata[2], sdata[3])
      include = bytarr(n_components, sdata[2], sdata[3])
      include[*] = 1
      const = bytarr(n_params, sdata[2], sdata[3])
    endif else if sdata[0] eq 4 then begin
      result = dblarr(n_params+1, sdata[2], sdata[3], sdata[4])
      for i=0,n_params do result[i,*,*,*] = init_values[i]
      residual = fltarr(sdata[1], sdata[2], sdata[3], sdata[4])
      include = bytarr(n_components, sdata[2], sdata[3], sdata[4])
      include[*] = 1
      const = bytarr(n_params, sdata[2], sdata[3], sdata[4])
    endif else begin
      print, 'data cube has wrong number of dimensions.'
      stop
    endelse
    handle_value, ana.result_h, result, /no_copy, /set
    handle_value, ana.residual_h, residual, /no_copy, /set
    handle_value, ana.include_h, include, /no_copy, /set
    handle_value, ana.const_h, const, /no_copy, /set
  endif
  return, ana
END



;---------------------------------------------------------
; window data and information
;---------------------------------------------------------




;------------------------------------------------
; ::mask_region_outside_slit and its help methods
;------------------------------------------------

; First a few help methods that are only used for making plots for debugging purposes:
PRO spice_data::debug_plot_dumbbell_range, window_index, lower_dumbbell_range, upper_dumbbell_range
  loadct,12,/silent
  naxis3 = self->get_header_keyword('NAXIS3',window_index)
  plots,[0,naxis3],[upper_dumbbell_range[0],upper_dumbbell_range[0]],color=150,line=2
  plots,[0,naxis3],[upper_dumbbell_range[1],upper_dumbbell_range[1]],color=150,line=2

  plots,[0,naxis3],[lower_dumbbell_range[0],lower_dumbbell_range[0]],color=150,line=2
  plots,[0,naxis3],[lower_dumbbell_range[1],lower_dumbbell_range[1]],color=150,line=2
  loadct,3,/silent
END


PRO spice_data::debug_put_unmasked_and_masked_data_both_detectors, window_index
  window,1, xp=4200
  loadct,3,/silent

  data = self->get_window_data(window_index, /no_masking)
  !p.multi = [0,2,1]

  plot_image,transpose(sigrange(reform(data[0,*,*]),fraction=0.9)),title='Unmasked',ytitle='Y pixel index',xtitle='Lambda pixel index'
  lower_dumbbell_range = self->get_dumbbell_range(window_index)
  upper_dumbbell_range = self->get_dumbbell_range(window_index, /upper)

  nbin2 = self->get_header_keyword('NBIN2',window_index)
  IF nbin2 NE 1 THEN BEGIN
    lower_dumbbell_range = self->get_rebinned_indices(lower_dumbbell_range, nbin2)
    upper_dumbbell_range = self->get_rebinned_indices(upper_dumbbell_range, nbin2)
  ENDIF

  self->debug_plot_dumbbell_range, window_index, lower_dumbbell_range, upper_dumbbell_range

  masked_data = self->get_window_data(window_index)
  plot_image,transpose(sigrange(reform(masked_data[0,*,*]),fraction=0.9)),title='Masked',ytitle='Y pixel index',xtitle='Lambda pixel index'

  loadct,12,/silent

  naxis3 = self->get_header_keyword('NAXIS3',window_index)
  nbin2 = self->get_header_keyword('NBIN2',window_index)
  slit_y_range = self->get_rebinned_indices(*self.slit_y_range, nbin2)
  plots,[-4000,naxis3],[slit_y_range[0],slit_y_range[0]],/data,color=50
  plots,[-4000,naxis3],[slit_y_range[1],slit_y_range[1]],/data,color=50

END


PRO spice_data::debug_plot_intensity, slit_y_range, y_intensity, dumbbell_max_ix, upper=upper
  loadct,12,/silent
  default, upper, keyword_set(upper)

  slit_y_range = slit_y_range
  smooth_factor = 32

  detector = (upper) ? ' SW' : ' LW'
  title = file_basename(self.file)+detector
  plot,y_intensity, ytitle='Intensity', xtitle='Y pixel index',title=title,yr=[0,max(y_intensity)],/yst,psym=-2,xr=[0,n_elements(y_intensity)],/xst
  y_intensity_smooth = smooth(y_intensity,smooth_factor)
  oplot,y_intensity_smooth,color=150
  xyouts, n_elements(y_intensity)*0.9, max(y_intensity)*0.9, detector,color=255,charsize=4

  plots,[slit_y_range[0], slit_y_range[0]], [0,max(y_intensity)], thick=2,color=50, line=(upper) ? 2 : 0
  plots,[slit_y_range[1], slit_y_range[1]], [0,max(y_intensity)], thick=2,color=50, line=(upper) ? 0 : 2

  xyouts, dumbbell_max_ix, max(y_intensity)*0.1,trim(dumbbell_max_ix),color=150

  ix = (upper) ? slit_y_range[1] : slit_y_range[0]
  xyouts, ix, max(y_intensity)*0.85,trim(ix),charsize=2,color=50
  xyouts, ix, max(y_intensity)*0.80,((upper)?'Upper':'Lower')+' slit edge',charsize=1.5,color=50

  xyouts, n_elements(y_intensity)/2.5, max(y_intensity)*0.9, 'Estimated slit length: '+trim(slit_y_range[1]-slit_y_range[0]+1)+' pixels',charsize=1.5,color=50
END


PRO spice_data::debug_plot_slit_y_range, window_index, slit_y_range, $
  y_intensity_SW, upper_dumbbell_max_ix, $
  y_intensity_LW, lower_dumbbell_max_ix

  window,0,xp=2570,xs=1450,yp=-70,ys=1140
  !p.multi = [0, 1, 2]

  self->debug_plot_intensity, slit_y_range, y_intensity_SW, upper_dumbbell_max_ix, /upper
  self->debug_plot_intensity, slit_y_range, y_intensity_LW, lower_dumbbell_max_ix

  !p.multi = 0

  self->debug_put_unmasked_and_masked_data_both_detectors, window_index
END

; End of help debug plot help methods


FUNCTION spice_data::get_slit_edge, dumbbell_max_ix, upper=upper
  ;;
  ;; +
  ;;  Description:
  ;;      Returns the index of the end pixel closest to the dumbbell_max_ix
  ;;      index, i.e. the upper slit end if upper keyword is set.
  ;;
  ;;      (See ::mask_region_outside_slit for full description of the masking
  ;;      code!)
  ;;
  ;;      The half width of the dumbbell and width of the gap between the slit
  ;;      edges and the dumbbell as seen in a geometrically corrected L2 file
  ;;      are nothing but conservative estimations. It's better to
  ;;      overestimate the length of the slit region, and thereby ending up
  ;;      with a few unusable pixels at both ends, instead of throwing away
  ;;      perfectly good pixels.
  ;;
  ;;      The hardcoded numbers below are not set in stone and may be
  ;       changed after more testing.
  ;; -
  dumbbell_half_width = floor(32./2)
  gap_width = floor(46.)
  dumbbell_max_displacement = dumbbell_half_width+gap_width

  sign = (keyword_set(upper)) ? -1 : 1
  dumbbell_max_displacement *= sign

  return, dumbbell_max_ix+dumbbell_max_displacement
END


FUNCTION spice_data::data_includes_dumbbell, window_index, upper=upper
  ;;
  ;; Description:
  ;; 1) The same y range is downlinked for all windows on both detectors.
  ;; Due to the tilt of the spectrum on the detector most observations are
  ;; downlinked in 768 y pixels high windows in order to ensure that the slit
  ;; data is complete for all windows on both detectors. This means that
  ;; normally the
  ;;   - SW detector windows contain data from the upper dumbbell region, the
  ;;   - LW detector windows contain data from the lower dumbbell region.
  ;; 2) The spectrum is not always placed at the same y pixels on the detector, i.e. the
  ;; dumbbells do not cover the same y pixels from one observation to the
  ;; next.
  ;; As a result, when checking that dumbbell data is indeed included in the
  ;; data cube, we look at a L1 file where the spectrum's placement on the
  ;; detector is
  ;;     - Low:  to find the upper edge of the lower LW dumbbell (the low  file will give a  lower limit)
  ;;     - High: to find the lower edge of the upper SW dumbbell (the high file will give an upper limit)
  ;; -
  ;;

  upper_edge_of_lower_dumbbell_in_L1_full_detector_LW = 160 ;; From L1 67109112-000, spectrum is low on the detector
  lower_edge_of_upper_dumbbell_in_L1_full_detector_SW = 810 ;; From L1 50331769-000, spectrum is high on the detector


  pxbeg2 = self->get_header_keyword('PXBEG2', window_index)
  pxend2 = self->get_header_keyword('PXEND2', window_index)

  data_includes_dumbbell = (keyword_set(upper)) ? pxend2 GT lower_edge_of_upper_dumbbell_in_L1_full_detector_SW : $
    pxbeg2 LT upper_edge_of_lower_dumbbell_in_L1_full_detector_LW

  return, data_includes_dumbbell
END


FUNCTION spice_data::get_dumbbell_range, window_index, upper=upper
  ;;
  ;;+
  ;; Description:
  ;;    Returns the approximate y range of the dumbbell in the datacube, given
  ;;    in debinned indices.
  ;;
  ;;    Hard coded numbers giving the approximate y range of the dumbbells were
  ;;    found by going through hundreds of L2 files. The range should be wide
  ;;    enough to cover the dumbbells both for files where the spectrum is
  ;;    placed high and files where the placement is low.
  ;;-
  nbin2 = self->get_header_keyword('NBIN2',window_index)
  n_y = self->get_header_keyword('NAXIS2', window_index)*nbin2
  n_y_full_detector = 1024

  half_diff = (n_y_full_detector-n_y)/2.

  dumbbell_range = (keyword_set(upper)) ? [840, 915] - half_diff : [105,180] - half_diff

  return, dumbbell_range
END


FUNCTION spice_data::get_approximated_dumbbell_max_ix, window_index, upper=upper
  dumbbell_range = self->get_dumbbell_range(window_index, upper = upper)
  just_to_be_on_the_safe_side = (keyword_set(upper)) ? 10 : -10
  return, round(dumbbell_range[0]+ (dumbbell_range[1]-dumbbell_range[0])/2.) + just_to_be_on_the_safe_side
END


FUNCTION spice_data::get_dumbbell_max_ix, y_intensity, window_index, upper=upper, approximated_slit=approximated_slit

  IF ~self->data_includes_dumbbell(window_index, upper=upper) THEN $
    return, (keyword_set(upper)) ? self->get_header_keyword('NAXIS2',window_index)-1 : 0

  IF keyword_set(approximated_slit) THEN return, self->get_approximated_dumbbell_max_ix(window_index, upper=upper)

  smooth_factor = 32 ;; This seems to be enough to smooth out any structures in the dumbbells
  y_intensity_smooth = smooth(y_intensity,smooth_factor)

  dumbbell_range = self->get_dumbbell_range(window_index, upper=upper)

  dumbbell_max_intensity = max(y_intensity_smooth[dumbbell_range[0]:dumbbell_range[1]], max_ix)
  mean_intensity = mean(y_intensity_smooth)

  dumbbell_probably_off_disc = dumbbell_max_intensity LT mean_intensity
  IF dumbbell_probably_off_disc THEN return, self->get_approximated_dumbbell_max_ix(window_index, upper = upper)

  dumbbell_max_ix =  max_ix + dumbbell_range[0]

  return, dumbbell_max_ix
END


FUNCTION spice_data::get_y_intensity, all_data
  all_data[where(all_data ne all_data)]=median(all_data)
  sz = size(all_data)
  y_intensity = reform(rebin(all_data,1,sz[2],1)) > 0

  return, y_intensity
END

FUNCTION spice_data::check_if_already_included, window_index, included_winnos
  already_included = 0

  header = self->get_header(window_index)
  prsteps =  fxpar(header,'PRSTEP*')        ;; MARTIN: the get_header_keyword method doesn't support wildcards, so I have to use fxpar!
  IF prsteps[-1] NE 'WINDOW-CONCATENATION' THEN return,0

  prparan = self->get_header_keyword('PRPARA'+trim(n_elements(prsteps)),window_index)
  concatenated_winnos = prparan.extract('([0-9],*)+')
  FOR included_ct=0,n_elements(included_winnos)-1 DO IF concatenated_winnos.contains(trim(included_winnos[included_ct])) THEN already_included =  1

  return, already_included
END


PRO spice_data::add_window, all_data, data, window_index, included_winnos
  IF all_data EQ !NULL THEN BEGIN
    data_has_been_debinned_in_y = (self->get_header_keyword('NBIN2',window_index) NE 0)
    naxis1 =  self->get_header_keyword('NAXIS1',window_index)
    naxis3 =  self->get_header_keyword('NAXIS3',window_index)
    naxis4 =  self->get_header_keyword('NAXIS4',window_index)

    size_y = (data_has_been_debinned_in_y) ? (size(data))[2] : naxis2
    all_data = reform(data, naxis1, size_y, naxis3, naxis4)
    included_winnos = 0
  ENDIF ELSE BEGIN
    this_window_has_already_been_included_as_a_part_of_a_concatenated_window = self->check_if_already_included(window_index,included_winnos)
    IF NOT this_window_has_already_been_included_as_a_part_of_a_concatenated_window THEN BEGIN
      size_all_data = size(all_data)
      width_all_windows_including_this =  size_all_data[3] + self->get_header_keyword('NAXIS3',window_index)

      all_data_new = make_array(size_all_data[1], size_all_data[2], width_all_windows_including_this, size_all_data[4])

      all_data_new[*, *, 0:size_all_data[3]-1,*] = all_data

      completeness = self->get_header_keyword('COMPLETE',window_index)
      incomplete = completeness.contains('I')
      last_x_ix = (incomplete) ? (size(data))[1]-1 : size_all_data[1]-1

      all_data_new[0:last_x_ix, *, size_all_data[3]:*, *] = data

      all_data = reform(all_data_new, size_all_data[1], size_all_data[2], width_all_windows_including_this, size_all_data[4])
      included_winnos = [included_winnos, self->get_header_keyword('WINNO',window_index)]
    ENDIF

  ENDELSE
END


PRO spice_data::debin_y_dimension, data, nbin2
  sz = size(data)
  sit_and_stare = sz[0] EQ 4
  sz4 = (sit_and_stare) ? sz[4] : 1

  data = rebin(data, sz[1], sz[2]*nbin2, sz[3], sz4)
END


PRO spice_data::get_all_data_both_detectors, all_data_SW, all_data_LW
  ;;
  ;;+
  ;; Description:
  ;;     Create one data array per detector, each containing all data cubes
  ;;     of all the detector's windows, i.e.
  ;;     (NAXIS1, NAXIS2, total(NAXIS3),NAXIS4). If windows have been concatenated
  ;;     include the data from a concatenated window only once. Any windows that are binned
  ;;     in the y dimension are debinned to ensure that all windows have the
  ;;     same NAXIS2.
  ;;-
  n_windows = self->get_number_windows()
  FOR window_index = 0,n_windows-1 DO BEGIN
    data = self->get_window_data(window_index, /no_masking)

    dumbbell = self->get_header_keyword('DUMBBELL', window_index) NE 0
    intensity_window = self->get_header_keyword('WIN_TYPE', window_index) EQ 'Intensity-window'
    IF NOT dumbbell AND NOT intensity_window THEN BEGIN
      nbin2 = self->get_header_keyword('NBIN2', window_index)

      IF nbin2 NE 1 THEN self->debin_y_dimension, data, nbin2

      CASE trim(self->get_header_keyword('DETECTOR', window_index)) OF
        'SW': self->add_window, all_data_SW, data, window_index, included_winnos
        'LW': self->add_window, all_data_LW, data, window_index, included_winnos
      ENDCASE

    ENDIF

  ENDFOR
END


PRO spice_data::get_y_intensity_both_detectors, y_intensity_SW, y_intensity_LW
  self->get_all_data_both_detectors, all_data_SW, all_data_LW

  IF all_data_SW EQ !NULL OR all_data_LW EQ !NULL THEN BEGIN
    message, (all_data_SW EQ !NULL) ? 'SW':'LW' + ' detector contains no data - using approximated slit range',/info
    y_intensity_sw = !NULL
    y_intensity_lw = !NULL
    return
  ENDIF

  y_intensity_SW = self->get_y_intensity(all_data_SW)
  y_intensity_LW = self->get_y_intensity(all_data_LW)
END


FUNCTION spice_data::get_rebinned_indices, indices, nbin2
  binned_indices = (indices-1)/nbin2+1
  return, binned_indices
END



FUNCTION spice_data::get_slit_y_range, data, window_index, approximated_slit=approximated_slit, debug_plot=debug_plot

  nbin2 = self->get_header_keyword('NBIN2', window_index)

  IF *self.slit_y_range NE !NULL THEN return, (nbin2 EQ 1) ? *self.slit_y_range : self->get_rebinned_indices(*self.slit_y_range, nbin2)

  IF ~keyword_set(approximated_slit) THEN BEGIN
    self->get_y_intensity_both_detectors, y_intensity_SW, y_intensity_LW
    approximated_slit = (y_intensity_SW EQ !NULL OR y_intensity_LW EQ !NULL)
    IF approximated_slit THEN message,'No windows on the ' + ((y_intensity_sw EQ !NULL) ? 'SW':'LW')+' detector, using approximated slit y range',/info
  ENDIF

  lower_dumbbell_max_ix = self->get_dumbbell_max_ix(y_intensity_LW, window_index, approximated_slit = approximated_slit)
  lower_slit_edge_ix = self->get_slit_edge(lower_dumbbell_max_ix)

  upper_dumbbell_max_ix = self->get_dumbbell_max_ix(y_intensity_SW, window_index, approximated_slit = approximated_slit, /upper)
  upper_slit_edge_ix = self->get_slit_edge(upper_dumbbell_max_ix,/upper)

  slit_y_range = [lower_slit_edge_ix, upper_slit_edge_ix]

  *self.slit_y_range = slit_y_range

  IF keyword_set(debug_plot) THEN BEGIN
    IF keyword_set(approximated_slit) THEN self->get_y_intensity_both_detectors, y_intensity_SW, y_intensity_LW
    self->debug_plot_slit_y_range, window_index, slit_y_range, $
      y_intensity_SW, upper_dumbbell_max_ix,  $
      y_intensity_LW, lower_dumbbell_max_ix
  ENDIF

  return, (nbin2 EQ 1) ? *self.slit_y_range : self->get_rebinned_indices(*self.slit_y_range, nbin2)
END


FUNCTION spice_data::check_if_data_may_be_masked, window_index
  level = self->get_level()
  IF level NE 2 THEN BEGIN
    message,'MASK_REGIONS_OUTSIDE_SLIT applies to L2 files only, returning unmodified data array',/info
    return, 0
  ENDIF

  dumbbell  = self->get_header_keyword('DUMBBELL', window_index) NE 0
  wide_slit = self->get_header_keyword('SLIT_WID', window_index) EQ 30
  IF dumbbell OR wide_slit THEN BEGIN
    message,'MASK_REGIONS_OUTSIDE_SLIT applies to narrow slit observations only, returning unmodified data array',/info
    return, 0
  ENDIF

  return,1
END

FUNCTION spice_data::mask_regions_outside_slit, data, window_index, approximated_slit=approximated_slit, debug_plot=debug_plot
  ;;
  ;;+
  ;; Description:
  ;;     Returns the input data array with any pixels that are above or
  ;;     below the narrow slit region set to NaN. This method is called when
  ;;     ::get_window_data is called if the no_masking keyword is NOT set.
  ;;
  ;;     And now a little background story:
  ;;     The height of the 2",4" and 6" slits is 600 pixels. At both ends of the slits
  ;;     there is a ~48 y-pixel gap with no throughput, then a dumbbell region of
  ;;     ~32 y-pixels.
  ;;
  ;;     A standard readout window was originally planned to include only
  ;;     the 600 pixels of the slit region (or a sub-range of pixels in this
  ;;     region), and optionally one or both dumbbells for a single of these
  ;;     windows.
  ;;
  ;;     However, the spectra turned out to be tilted on the detectors, the
  ;;     tilt angles are not the same on the detetors, and the spectra of the
  ;;     two detetors are displaced in the y direction relative to one another.
  ;;     In order to ensure that the windows contain the full slit on both
  ;;     detectors the new default window height is therefore 768 pixels. The
  ;;     result is that all narrow slit windows on the SW detector will contain the
  ;;     full upper dumbbell and the LW windows will contain the full lower
  ;;     dumbbell.
  ;;
  ;;     The L1 to L2 calibration includes a geometrical correction
  ;;     of tilts, slants, rotations and displacements. In theory the
  ;;     dumbbells should fall on the same pixels in every observation and it
  ;;     should be easy to mask them. However, the spectra for a given
  ;;     detector may be shifted +/- 10 pixels or so from one observation to
  ;;     another. The shift probably happens when the slit is changed but it's
  ;;     not fully understood.
  ;;
  ;;     Therefore, we need to determine where the y slit range based on the
  ;;     observational data. It's hard to set up fool proof criteria for
  ;;     determining the slit edges based on the signal from the slit
  ;;     itself, since the signal often is very weak. The dumbbell signal on the
  ;;     other hand is normally much stronger. Therefore, the help methods of
  ;;     this method determine the slit y range by first estimating the
  ;;     locations of the dumbbell regions:
  ;;
  ;;         1: For each detector, make a 1D array containg the intensity
  ;;            along the y direction [::get_y_intensity_both_detectors]
  ;;         2: Smooth the y intensity arrays heavily in order to estimate the
  ;;            midpoint of the dumbbell (i.e. the upper dumbbell for the SW y
  ;;            intensity array, the lower dummbell for the LW y array). Special
  ;;            care is taken if the dumbbells are not present in the data
  ;;            cube due to a non-standard window height, or if one (or both) of the
  ;;            dumbbells can't be identified (typically if a dumbbell falls
  ;;            off-limb). [::get_dumbbell_max_ix]
  ;;         3: Take the approximate number of pixels between the dumbbell
  ;;            midpoints and slit edges into account when estimating the slit
  ;;            y range [::get_slit_edge]
  ;;         5: Set all pixels in the data cube that are below or above the slit y
  ;;            range to NaN
  ;;
  ;; INPUTS:
  ;;     data:         a 4D data cube returned by ::get_window_data
  ;;     window_index: the index of the desired window
  ;;
  ;; KEYWORD PARAMETERS:
  ;;     approximated_slit: if set, a default value of the dumbbell midpoint
  ;;                        is used instead of trying to estimating the
  ;;                        midpoint. To be on the safe side a few pixels are
  ;;                        added (upper dumbbell) or subtracted (lower
  ;;                        dumbbell) from the estimated midpoint to make sure
  ;;                        that no slit data is masked in the case of an
  ;;                        unusually low or high placement of the spectra.
  ;;
  ;; OUTPUT:
  ;;     Returns the data cube that was given as input, but with any pixels
  ;;     below or above the narrow slit set to NaN.
  ;;
  ;; SIDE EFFECTS:
  ;;     If approximated_slit is not set, then ::get_window_data() will be called
  ;;     for all windows in the file once. The structure tag self.slit_y_range
  ;;     will be set the first time this method is called. A later call of
  ;;     this method will use the value of self.slit_y_range.
  ;;-
  ;;

  data_may_be_masked = self->check_if_data_may_be_masked(window_index)
  IF ~data_may_be_masked THEN return, data

  slit_y_range = self->get_slit_y_range(data, window_index, approximated_slit = approximated_slit, debug_plot=debug_plot)

  data[*,0:slit_y_range[0],*,*] = !values.f_nan
  data[*,slit_y_range[1]:*,*,*] = !values.f_nan

  return, data
END


;+
; Description:
;     Returns the data of the specified window.
;     Pixels below and above the slit are set to NaN in the returned array, except if
;     'no_masking' is set.
;
; INPUTS:
;     window_index : The index of the desired window
;
; KEYWORD PARAMETERS:
;     noscale : If present and non-zero, then the output data will not be
;                 scaled using the optional BSCALE and BZERO keywords in the
;                 FITS header.   Default is to scale.
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;                 The keyword is ignored if NO_MASKING is set.
;     debug_plot: If set, make plots to illustrate which part of the window is being masked.
;                 This keyword is ignored if NO_MASKING is set.
;     load : Obsolete and ignored. This is here for backwards-compatibility.
;     slit_only : Obsolete and ignored. This is here for backwards-compatibility.
;     nodescale : Obsolete. If set, then NOSCALE is set. This is here for backwards-compatibility.
;
; OUTPUT:
;     Returns either the data of the window as an array or a link to the data.
;-
FUNCTION spice_data::get_window_data, window_index, noscale=noscale, $
  no_masking=no_masking, approximated_slit=approximated_slit, debug_plot=debug_plot, $
  load=load, slit_only=slit_only, nodescale=nodescale
  ;Returns the data of a window
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 1 THEN BEGIN
    message, 'missing input, usage: get_window_data, window_index [, load=load, noscale=noscale]', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_window_index(window_index) THEN return, !NULL

  IF keyword_set(noscale) || keyword_set(nodescale) THEN descaled=2 ELSE descaled=1
  IF keyword_set(no_masking) THEN masked=0 ELSE $
    IF keyword_set(approximated_slit) THEN masked=2 ELSE masked=1
  IF (*self.window_descaled)[window_index] EQ descaled && $
    (*self.window_masked)[window_index] EQ masked THEN BEGIN
    data = *(*self.window_data)[window_index]
  ENDIF ELSE BEGIN
    data = readfits(self.get_filename(), hdr, noscale=noscale, ext=window_index)
    IF ~keyword_set(no_masking) THEN BEGIN
      data = self.mask_regions_outside_slit(data, window_index, approximated_slit = approximated_slit, debug_plot = debug_plot)
    ENDIF
    IF ptr_valid((*self.window_data)[window_index]) THEN ptr_free, (*self.window_data)[window_index]
    (*self.window_data)[window_index] = ptr_new(data)
    (*self.window_descaled)[window_index] = descaled
    (*self.window_masked)[window_index] = masked
  ENDELSE
  return, data
END


;+
; Description:
;     Returns the data of the specified window and exposure index. If 'noscale' keyword is set,
;     the output data will not be scaled using the optional BSCALE and BZERO keywords in the
;     FITS header.
;     The exposure index is in the first dimension of the 4D cube in case the study type is 'Raster',
;     and in the fourth dimension if study type is 'Sit-and-stare'.
;     The array is also transposed, so that it can be directly plotted, i.e.
;     array = [lambda, instrument-Y].
;
; INPUTS:
;     window_index : The index of the desired window.
;     exposure_index : The index of the desired exposure.
;
; KEYWORD PARAMETERS:
;     noscale : If present and non-zero, then the output data will not be
;                 scaled using the optional BSCALE and BZERO keywords in the
;                 FITS header.   Default is to scale.
;     debin : If set, the image will be expanded if binning is GT 1, and data values
;             will be divided by the binning value.
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;                 The keyword is ignored if NO_MASKING is set.
;     nodescale : Obsolete. If set, then NOSCALE is set. This is here for backwards-compatibility.
;
; OUTPUT:
;     Returns a transposed 2D subset of the data from the specified window and exposure (array = [lambda, instrument-Y]).
;-
FUNCTION spice_data::get_one_image, window_index, exposure_index, debin=debin, noscale=noscale, $
  no_masking=no_masking, approximated_slit=approximated_slit, $
  nodescale=nodescale
  ;Returns a transposed 2D subset of the data from the specified window and exposure (array = [lambda, instrument-Y])
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: get_one_image, window_index, exposure_index [, noscale=noscale]', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_window_index(window_index) THEN return, !NULL
  IF self.get_sit_and_stare() THEN naxis = self.get_header_keyword('NAXIS4', window_index) $
  ELSE naxis = self.get_header_keyword('NAXIS1', window_index)
  IF exposure_index LT 0 || exposure_index GE naxis  THEN BEGIN
    print, 'exposure_index needs to be a scalar number between 0 and '+strtrim(string(naxis-1),2)
    return, !NULL
  ENDIF
  IF keyword_set(nodescale) then noscale=1

  data = self.get_window_data(window_index, noscale=noscale, no_masking=no_masking, approximated_slit=approximated_slit)
  IF self.get_sit_and_stare() THEN BEGIN
    data = reform(data[0,*,*,exposure_index])
  ENDIF ELSE BEGIN
    data = reform(data[exposure_index,*,*])
  ENDELSE
  data = transpose(data)
  IF keyword_set(debin) THEN BEGIN
    size_data = size(data)
    bin_y = (self.get_spatial_binning(window_index))[0]
    IF bin_y GT 1 THEN BEGIN
      new_data = rebin(data,size_data[1],size_data[2]*bin_y)
      FOR i=0,size_data[2]-1 DO BEGIN
        one_line = data[*,i]/bin_y
        FOR j=0,bin_y-1 DO BEGIN
          index = i*bin_y+j
          new_data[*,index] = one_line
        ENDFOR
      ENDFOR
      data = new_data
      size_data = size(data)
    ENDIF

    bin_l = (self.get_spectral_binning(window_index))[0]
    IF bin_l GT 1 THEN BEGIN
      new_data = rebin(data,size_data[1]*bin_l,size_data[2])
      FOR i=0,size_data[1]-1 DO BEGIN
        one_line = data[i,*]/bin_l
        FOR j=0,bin_l-1 DO BEGIN
          index = i*bin_l+j
          new_data[index,*] = one_line
        ENDFOR
      ENDFOR
      data = new_data
    ENDIF
  ENDIF
  return, data
END


;+
; Description:
;     Descales the array, using BSCALE and BZERO keywords in the header.
;     If you get the data from this object via get_window_data() while
;     setting the keyword 'noscale', you will have to call this method yourself.
;
; INPUTS:
;     array : A numeric array, which is returned by SPICE_DATA::get_window_data.
;     window_index : The index of the window this array belongs to.
;
; OUTPUT:
;     Returns the descaled array (=array * bscale + bzero)
;-
FUNCTION spice_data::descale_array, array, window_index
  ;Descales the array, using BSCALE and BZERO keywords in the header
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: descale_array, array, window_index', /info
    return, !NULL
  ENDIF

  bscale = self.get_header_keyword('BSCALE', window_index, 1)
  bzero = self.get_header_keyword('BZERO', window_index, 0)
  return, array * bscale + bzero
END


;+
; Description:
;     Returns the window index/indices which contain a given wavelength or window name.
;
; INPUTS:
;     input : Scalar or array of numbers or string.
;             Ff input is one or more numbers, it is interpreted as wavelengths
;             and indices of windows including those wavelengths are returned.
;             If input is one or more strings, it is interpreted as the window ID
;             and indices of the corresponding windows are returned.
;
; OUTPUT:
;     Integer array, with as many elements as input.
;-
FUNCTION spice_data::get_window_index, input
  ;Returns window index of a given wavelength or window name
  COMPILE_OPT IDL2

  IF n_params() EQ 0 THEN BEGIN
    message,'getwindx,input',/info
    return,-1
  ENDIF
  iwin=intarr(n_elements(input))
  FOR iw=0,n_elements(input)-1 DO BEGIN
    IF datatype(input[iw]) EQ 'STR' THEN BEGIN
      iwin[iw]=(where((strupcase(self->get_window_id())) EQ $
        strupcase(input[iw]),c))[0]
      IF c EQ 0 THEN BEGIN
        message,'Line_id not found : '+input[iw],/info
        iwin[iw]=-1
      ENDIF
    ENDIF ELSE BEGIN
      IF input[iw] GE 0 && input[iw] LE (self->get_number_windows())-1 THEN BEGIN
        iwin[iw]=input[iw]
      ENDIF ELSE BEGIN
        ;   else e.g. input=1334.
        nwin=self->getnwin()
        winmax=fltarr(nwin)
        winmin=fltarr(nwin)
        FOR i=0,nwin-1 DO BEGIN
          winmax[i]=max(self->get_lambda_vector(i), min=mintemp)
          winmin[i]=mintemp
        ENDFOR
        prod=(winmax-input[iw])*(input[iw]-winmin)
        iwin[iw]=(where(prod gt 0,c))[0]
        IF c EQ 0 THEN BEGIN
          message,'wavelength not found '+trim(input[iw],'(f10.2)'),/info
          iwin[iw]=-1
        ENDIF
      ENDELSE
    ENDELSE
  ENDFOR
  return,iwin
END


;+
; Description:
;     This function returns the position of the window on the CCD, starting with 0 if idl_coord is set, 1 otherwise.
;     The position is given as a 4-element vector, with [lambda0, lambda1, y0, y1].
;     Note: y0 > y1, but lambda0 < lambda1.
;
; INPUTS:
;     window_index : The index of the window.
;
; KEYWORD PARAMETERS:
;     idl_coord : If set, the coordinates start with zero, instead of with 1.
;     reverse_y : Y-coordinates are given as (CCD-size +1 - (original y-coords)).
;     reverse_x : For dumbbells x-coordinates are flipped. If this keyword is set, the coordinates will
;                 be flipped again, i.e. values of PXBEG3 and PXEND3 will be swapped.
;     loud      : If set, warnings will be printed.
;
; OUTPUT:
;     Integer array with 4 elements [lambda0, lambda1, y0, y1].
;
; OPTIONAL OUTPUT:
;     detector : int, 1 or 2 to indicate on which detector the window is.
;-
FUNCTION spice_data::get_window_position, window_index, detector=detector, $
  idl_coord=idl_coord, reverse_y=reverse_y, reverse_x=reverse_x, loud=loud
  ;Returns the position of the window on the CCD, starting with 0 if idl_coord is set, 1 otherwise
  COMPILE_OPT IDL2

  ccd_size = self.get_ccd_size()

  PXBEG3 = self.get_header_keyword('PXBEG3', window_index)
  IF PXBEG3 LT 0 THEN BEGIN
    message, 'PXBEG3 < 0: '+strtrim(string(PXBEG3))+' < 0', /info
    detector = 1
  ENDIF ELSE IF PXBEG3 GT 2*ccd_size[0] THEN BEGIN
    message, 'PXBEG3 > 2 * CCD-size: '+strtrim(string(PXBEG3))+' > '+strtrim(string(2*ccd_size[0])), /info
    detector = 2
  ENDIF ELSE IF PXBEG3 GT ccd_size[0] THEN BEGIN
    detector = 2
  ENDIF ELSE BEGIN
    detector = 1
  ENDELSE

  PXEND3 = self.get_header_keyword('PXEND3', window_index)
  IF PXEND3 LT 0 THEN message, 'PXEND3 < 0: '+strtrim(string(PXEND3))+' < 0', /info
  IF PXEND3 LT PXBEG3 THEN BEGIN
    IF self.has_dumbbells(window_index) && keyword_set(reverse_x) THEN BEGIN
      beg_temp = PXEND3
      PXEND3 = PXBEG3
      PXBEG3 = beg_temp
    ENDIF ELSE BEGIN
      IF keyword_set(loud) THEN message, 'PXEND3 < PXBEG3: '+strtrim(string(PXEND3))+' < '+strtrim(string(PXBEG3)), /info
    ENDELSE
  ENDIF
  IF PXEND3 GT 2*ccd_size[0] THEN $
    message, 'PXEND3 > 2 * CCD-size: '+strtrim(string(PXEND3))+' > '+strtrim(string(2*ccd_size[0])), /info

  PXBEG2 = self.get_header_keyword('PXBEG2', window_index)
  IF PXBEG2 LT 0 THEN message, 'PXBEG2 < 0: '+strtrim(string(PXBEG2))+' < 0', /info
  IF PXBEG2 GT ccd_size[1] THEN $
    message, 'PXBEG2 > CCD-size: '+strtrim(string(PXBEG2))+' > '+strtrim(string(ccd_size[1])), /info

  PXEND2 = self.get_header_keyword('PXEND2', window_index)
  IF PXEND2 LT 0 THEN message, 'PXEND2 < 0: '+strtrim(string(PXEND2))+' < 0', /info
  IF keyword_set(loud) && PXEND2 GT PXBEG2 THEN $
    message, 'PXEND2 > PXBEG2: '+strtrim(string(PXEND2))+' > '+strtrim(string(PXBEG2)), /info
  IF PXEND2 GT ccd_size[1] THEN $
    message, 'PXEND2 > CCD-size: '+strtrim(string(PXEND2))+' > '+strtrim(string(ccd_size[1])), /info

  position = [PXBEG3, PXEND3, PXBEG2, PXEND2]
  IF keyword_set(reverse_y) THEN position[2:3] = ccd_size[1] + 1 - position[2:3]
  IF keyword_set(idl_coord) THEN position = position - 1
  return, position
END


;+
; Description:
;     This method returns the specified keyword from the given extension, if the keyword does not exist
;     'missing_value' is returned if it is provided, !NULL otherwise. This method can also return
;     the variable values of a keyword, if it is available in the binary table extension.
;     See keyword VARIABLE_VALUES.
;
; INPUTS:
;     keyword : string, The header keyword for which the value should be returned.
;     extension_index : The index of the extension this keyword belongs to.
;
; OPTIONAL INPUTS:
;     missing_value : the value that should be returned, if the keyword does not exist
;                     if this is not provided !NULL is returned
;
; OPTIONAL OUTPUT:
;     exists : boolean, Set this to a named variable. This variable will be set to 1, if the keyword exists, 0 otherwise.
;     variable_values : array, contains the variable values for this keyword, if this keyword is present
;                       in the binary table extension 'VARIABLE-KEYWORDS', otherwise !NULL.
;                       Calls the method spice_data::get_bintable_data with the VALUES_ONLY keyword.
;
; KEYWORDS:
;     values_only: If set then only the values in the binary table extension are returned to VARIABLE_VALUES as an array,
;                  instead of the default output structure with metadata.
;
; OUTPUT:
;     Returns either the keyword value, the MISSING_VALUE or !NULL.
;-
FUNCTION spice_data::get_header_keyword, keyword, extension_index, missing_value, exists=exists, $
  variable_values=variable_values, values_only=values_only
  ;Returns the specified keyword from the extension, or 'missing_value' if provided, !NULL otherwise
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: get_header_keyword, keyword, extension_index [, missing_value, exists=exists, variable_values=variable_values, values_only=values_only]', /info
    return, !NULL
  ENDIF ELSE IF N_ELEMENTS(keyword) NE 1 || SIZE(keyword, /TYPE) NE 7 THEN BEGIN
    message, 'keyword needs to be a scalar string', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_extension_index(extension_index) THEN return, !NULL

  ; keywords with a '-' in the name, will be renamed when they are transformed into structures (in fitshead2struct),
  ; '-' becomes '_D$'
  ;temp = strsplit(keyword, '-', count=count, /extract)
  ;IF count GT 1 THEN keyword = strjoin(temp, '_D$')

  IF ARG_PRESENT(variable_values) THEN BEGIN
    variable_values = self.get_bintable_data(keyword, values_only=values_only)
  ENDIF

  result = fxpar(*(*self.window_headers_string)[extension_index], keyword, missing=missing_value, count=count)
  if size(result, /type) eq 7 then result = result.trim()

  exists = count gt 0
  IF exists THEN BEGIN
    return, result
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(missing_value) EQ 0 THEN return, !NULL $
    ELSE return, missing_value
  ENDELSE

END


;+
; Description:
;     This method is deprecated, but still available for compatibility reasons.
;     get_header_keyword instead replaces this method. See there for documentation
;-
FUNCTION spice_data::get_header_info, keyword, extension_index, missing_value, exists=exists
  message, 'This function is deprecated. Use SPICE_DATA::get_header_keyword instead', /informational
  return, self.get_header_keyword(keyword, extension_index, missing_value, exists=exists)
END


;+
; Description:
;     Returns the header of the given extension, either as a string array or as a structure.
;
; INPUTS:
;     extension_index : The index of the extension for which the header should be returned.
;                    This index will be ignored if either LOWER_DUMBBELL or UPPER_DUMBBELL is set.
;
; KEYWORD PARAMETERS:
;     lower_dumbbell : If set, the header of the lower dumbbell will be returned.
;     upper_dumbbell : If set, the header of the upper dumbbell will be returned.
;     structure : If set, the header will be returned as a structure instead of a string array.
;
; OUTPUT:
;     Returns the header as a string array or a structure.
;-
FUNCTION spice_data::get_header, extension_index, lower_dumbbell=lower_dumbbell, upper_dumbbell=upper_dumbbell, $
  structure=structure
  ;Returns the header of the given extension as a string array or a structure
  COMPILE_OPT IDL2

  IF keyword_set(lower_dumbbell) THEN extension_index=self.get_dumbbells_index(/lower)
  IF keyword_set(upper_dumbbell) THEN extension_index=self.get_dumbbells_index(/upper)
  IF ~self.check_extension_index(extension_index) THEN return, !NULL
  IF keyword_set(structure) then return, *(*self.window_headers)[extension_index] $
  ELSE return, *(*self.window_headers_string)[extension_index]
END


;+
; Description:
;     Returns the number of windows this file/object contains.
;
; OUTPUT:
;     int: number of windows
;-
FUNCTION spice_data::get_number_windows
  ;Returns the number of windows this file contains
  COMPILE_OPT IDL2

  return, self.nwin
END


;+
; Description:
;     Returns the number of extensions this file/object contains.
;
; OUTPUT:
;     int: number of extensions
;-
FUNCTION spice_data::get_number_extensions
  ;Returns the number of extensions this file contains
  COMPILE_OPT IDL2

  return, self.next
END


;+
; Description:
;     Returns the title.
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_title
  ;Returns the title, i.e. 'SPICE'
  COMPILE_OPT IDL2

  return, self.title
END


;+
; Description:
;     Returns SPICE OBS ID.
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_obs_id
  ;Returns SPICE OBS ID
  COMPILE_OPT IDL2

  obs_id = self.get_header_keyword('SPIOBSID', 0, -1)
  IF size(obs_id, /TYPE) NE 7 THEN obs_id = string(obs_id)
  return, strtrim(obs_id,2)
END


;+
; Description:
;     Returns start date and time of observation in UTC format.
;
; OUTPUT:
;     int: number of windows
;-
FUNCTION spice_data::get_start_time
  ;Returns start date and time of observation in UTC format
  COMPILE_OPT IDL2

  start_time = self.get_header_keyword('DATE-BEG', 0, '')
  return, start_time
END


;+
; Description:
;     Returns end date and time of observation in UTC format.
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_end_time
  ;Returns end date and time of observation in UTC format
  COMPILE_OPT IDL2

  end_time = self.get_header_keyword('DATE-END', 0, '')
  return, end_time
END


;+
; Description:
;     Returns 1 if the raster is a sit-and-stare, 0 otherwise.
;
; OUTPUT:
;     boolean
;-
FUNCTION spice_data::get_sit_and_stare
  ;Returns 1 if raster is a sit-and-stare, 0 otherwise
  COMPILE_OPT IDL2

  sit_and_stare = self.get_header_keyword('STUDYTYP', 0) EQ 'Sit-and-stare'
  return, sit_and_stare
END


;+
; Description:
;     Returns the level of the file.
;
; OUTPUT:
;     int: level number (0, 1 or 2)
;
; OPTIONAL OUTPUT:
;     low_latency: boolean, 1 if this file is a low latency file (i.e. LL0x)
;-
FUNCTION spice_data::get_level, low_latency=low_latency
  ;Returns the level of the file
  COMPILE_OPT IDL2

  level_string = self.get_header_keyword('LEVEL', 0)
  low_latency = 0
  CASE level_string OF
    'LL01': BEGIN
      low_latency = 1
      level = 1
    END
    'LL02': BEGIN
      low_latency = 1
      level = 2
    END
    'L0': level = 0
    'L1': level = 1
    'L2': level = 2
    'L3': level = 3
    else: level = -1
  ENDCASE
  return, level
END


;+
; Description:
;     Returns BUNIT, physical units of the data.
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_variable_unit
  ;Returns BUNIT, physical units of the data
  COMPILE_OPT IDL2

  bunit = self.get_header_keyword('BUNIT', 0, '')
  return, bunit
END


;+
; Description:
;     Returns BTYPE, type of data in images.
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_variable_type
  ;Returns BTYPE, type of data in images
  COMPILE_OPT IDL2

  btype = self.get_header_keyword('BTYPE', 0, '')
  return, btype
END


;+
; Description:
;     Returns S/C CCW roll relative to Solar north in degrees.
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_satellite_rotation
  ;Returns S/C CCW roll relative to Solar north in degrees
  COMPILE_OPT IDL2

  crota = self.get_header_keyword('CROTA', 0)
  return, crota
END


;+
; Description:
;     Returns the value for missing pixels.
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_missing_value
  ;Returns the value for missing pixels
  COMPILE_OPT IDL2

  missing = self.get_header_keyword('BLANK', 0)
  if N_ELEMENTS(missing) EQ 0 && self->get_level() EQ 2 THEN missing = !values.f_nan
  return, missing
END


;+
; Description:
;     Returns the 2-element vector containing the CCD size.
;
; OUTPUT:
;     int array
;-
FUNCTION spice_data::get_ccd_size
  ;Returns the 2-element vector containing the CCD size
  COMPILE_OPT IDL2

  return, self.ccd_size
END


;+
; Description:
;     Returns the window ID of one or more windows. window_index is optional
;     if not provided, window IDs of all windows are returned, if it is
;     scalar, the result will be a scalar string, and if window_index is
;     an array, the result will be a string array of same size.
;
; INPUTS:
;     window_index : the index of the window the ID is asked for
;                    scalar or 1D-int-array. Default is all windows.
;
; OUTPUT:
;     string or string array
;-
FUNCTION spice_data::get_window_id, window_index
  ;Returns the window ID, as string or string array
  COMPILE_OPT IDL2

  IF n_params() EQ 0 THEN BEGIN
    window_id = strarr(self.get_number_windows())
    FOR i = 0, self.get_number_windows()-1 DO BEGIN
      window_id[i] = self.get_header_keyword('EXTNAME', i)
    ENDFOR
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(window_index) eq 1 THEN BEGIN
      window_id = self.get_header_keyword('EXTNAME', window_index[0])
    ENDIF ELSE BEGIN
      window_id = strarr(N_ELEMENTS(window_index))
      FOR i = 0,N_ELEMENTS(window_index)-1 DO BEGIN
        window_id[i] = self.get_header_keyword('EXTNAME', window_index[i])
      ENDFOR
    ENDELSE
  ENDELSE

  return, window_id
END


;+
; Description:
;     Prints all window indices and their IDs to the command line.
;-
PRO spice_data::show_lines
  ;Prints all window indices and their IDs to the command line.
  COMPILE_OPT IDL2

  window_id = self.get_window_id()
  FOR i=0,self.get_number_windows()-1 DO BEGIN
    print, i, ': ' + window_id[i]
  ENDFOR
END


;+
; Description:
;     Returns the number of exposures in the window, or if window_index
;     is not provided, a vector containing the numbers of exposures
;     for each window
;
; OPTIONAL INPUTS:
;     window_index : the index of the window(s). Default all windows.
;
; OUTPUT:
;     int or int-array
;-
FUNCTION spice_data::get_number_exposures, window_index
  ;Returns the number of exposures in the window
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) EQ 0 THEN BEGIN
    n_exp = intarr(self.get_number_windows())
    FOR iwin=0,self.get_number_windows()-1 DO BEGIN
      IF self.get_sit_and_stare() THEN n_exp[iwin] = self.get_header_keyword('NAXIS4', iwin) $
      ELSE n_exp[iwin] = self.get_header_keyword('NAXIS1', iwin)
    ENDFOR
  ENDIF ELSE BEGIN
    IF self.get_sit_and_stare() THEN n_exp = self.get_header_keyword('NAXIS4', window_index) $
    ELSE n_exp = self.get_header_keyword('NAXIS1', window_index)
  ENDELSE
  return, n_exp
END


;+
; Description:
;     Returns the number of pixels in y in the window, or if window_index
;     is not provided, a vector containing the numbers of pixels in y
;     for each window
;
; OPTIONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     int or int-array
;-
FUNCTION spice_data::get_number_y_pixels, window_index
  ;Returns the number of pixels in y in the window
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) EQ 0 THEN BEGIN
    n_slit = intarr(self.get_number_windows)
    FOR iwin=0,self.get_number_windows-1 DO BEGIN
      n_slit[iwin] = self.get_header_keyword('NAXIS2', iwin)
    ENDFOR
  ENDIF ELSE BEGIN
    n_slit = self.get_header_keyword('NAXIS2', window_index)
  ENDELSE
  return, n_slit
END


;+
; Description:
;     Returns the exposure time of the given window per exposure.
;
; INPUTS:
;     window_index : The index of the window.
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_exposure_time, window_index
  ;Returns the exposure time of the given window per exposure
  COMPILE_OPT IDL2

  exptime = self.get_header_keyword('XPOSURE', window_index)
  return, exptime
END


;+
; Description:
;     Returns name of axis, if axis is not provided a string vector
;     will be returned that contains the names of all axes.
;     The name of the axis includes its unit in square brackets,
;     except if the pixels keyword is set, then it says pixels instead
;     of units, or if no_unit keyword is set.
;
; OPTIONAL INPUTS:
;     axis : the index of the axis, may be a vector
;
; KEYWORD PARAMETERS:
;     pixels : Return 'pixels' as unit
;     no_unit : Do not include units in axis name
;
; OUTPUT:
;     string or string array
;-
FUNCTION spice_data::get_axis_title, axis, pixels=pixels, no_unit=no_unit
  ;Returns name of axis, if axis is not provided a string vector will be returned
  COMPILE_OPT IDL2

  axes = ['Solar X', 'Solar Y', 'Wavelength', 'Time']
  IF ~keyword_set(no_unit) THEN BEGIN
    IF keyword_set(pixels) THEN BEGIN
      axes = axes + ' [pixels]'
    ENDIF ELSE BEGIN
      axes[0] = axes[0] + ' [' + strtrim(self.get_header_keyword('CUNIT1', 0),2) + ']'
      axes[1] = axes[1] + ' [' + strtrim(self.get_header_keyword('CUNIT2', 0),2) + ']'
      axes[2] = axes[2] + ' [' + strtrim(self.get_header_keyword('CUNIT3', 0),2) + ']'
      axes[3] = axes[3] + ' [' + strtrim(self.get_header_keyword('CUNIT4', 0),2) + ']'
    ENDELSE
  ENDIF
  IF N_ELEMENTS(axis) eq 0 THEN return, axes
  return, axes[axis]
END


;+
; Description:
;     Returns a vector containing the coordinate (instrument x-direction) for each pixel in the first dimension.
;
; INPUTS:
;     window_index : The index of the window.
;
; OUTPUT:
;     float array, coordinate in arcsec
;-
FUNCTION spice_data::get_instr_x_vector, window_index
  ;Returns a vector containing the coordinate for each pixel in instrument x-direction
  COMPILE_OPT IDL2

  crval = self.get_header_keyword('crval1', window_index)
  naxis = self.get_header_keyword('naxis1', window_index)
  crpix = self.get_header_keyword('crpix1', window_index)
  cdelt = self.get_header_keyword('cdelt1', window_index)
  pc1_1 = self.get_header_keyword('PC1_1', window_index)
  x_vector = crval + cdelt * pc1_1 * (findgen(naxis)+1.0-crpix)
  IF naxis EQ 1 THEN BEGIN
    naxis = self.get_header_keyword('naxis4', window_index)
    x_vector = replicate(x_vector, naxis)
  ENDIF
  return, x_vector
END


;+
; Description:
;     Returns a vector containing the coordinate for each pixel in the second dimension, instrument y-direction
;     for the selected window, or the full CCD this window belongs to
;
; INPUTS:
;     window_index : the index of the window
;
; OPTIONAL KEYWORDS:
;     full_ccd : If set, a vector of size CCD-size[1] is returned with coordinate values
;                for the whole detector
;
; OUTPUT:
;     float array, coordinate in arcsec
;-
FUNCTION spice_data::get_instr_y_vector, window_index, full_ccd=full_ccd
  ;Returns a vector containing the coordinate for each pixel in instrument y-direction
  COMPILE_OPT IDL2

  crval = self.get_header_keyword('crval2', window_index)
  crpix = self.get_header_keyword('crpix2', window_index)
  cdelt = self.get_header_keyword('cdelt2', window_index)
  pc2_2 = self.get_header_keyword('PC2_2', window_index)
  IF keyword_set(full_ccd) THEN BEGIN
    PXBEG3 = (self.get_window_position(window_index, /reverse_y))[2]
    cripx = crpix + PXBEG3
    naxis = (self.get_ccd_size())[1]
  ENDIF ELSE BEGIN
    naxis = self.get_header_keyword('naxis2', window_index)
  ENDELSE
  y_vector = crval + cdelt * pc2_2 * (findgen(naxis)+1.0-crpix)
  return, y_vector
END


;+
; Description:
;     Returns a vector containing the wavelength for each pixel in third dimension for
;     the selected window, or the full CCD this window belongs to
;
; INPUTS:
;     window_index : the index of the window
;
; OPTIONAL KEYWORDS:
;     full_ccd : if set, a vector of size CCD-size[0] is returned with lamda values
;                for the whole detector
;
; OUTPUT:
;     float array, wavelength in nm
;-
FUNCTION spice_data::get_lambda_vector, window_index, full_ccd=full_ccd
  ;Returns a vector containing the wavelength for each pixel in third dimension for window or full CCD
  COMPILE_OPT IDL2

  crval = self.get_header_keyword('crval3', window_index)
  cdelt = self.get_header_keyword('cdelt3', window_index)
  crpix = self.get_header_keyword('crpix3', window_index)
  IF keyword_set(full_ccd) THEN BEGIN
    PXBEG3 = self.get_header_keyword('PXBEG3', window_index)
    cripx = crpix + PXBEG3
    naxis = (self.get_ccd_size())[0]
  ENDIF ELSE BEGIN
    naxis = self.get_header_keyword('naxis3', window_index)
  ENDELSE
  lambda_vector = crval + (findgen(naxis)+1.0-crpix) * cdelt
  return, lambda_vector
END


;+
; Description:
;     Returns a vector containing the time for each pixel in fourth dimension
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, time in seconds
;-
FUNCTION spice_data::get_time_vector, window_index
  ;Returns a vector containing the time for each pixel in fourth dimension
  COMPILE_OPT IDL2

  crval = self.get_header_keyword('crval4', window_index)
  naxis = self.get_header_keyword('naxis4', window_index)
  IF naxis EQ 1  THEN BEGIN
    naxis = self.get_header_keyword('naxis1', window_index)
    crpix = self.get_header_keyword('crpix1', window_index)
    factor = self.get_header_keyword('PC4_1', window_index, 1)
  ENDIF ELSE BEGIN
    crpix = self.get_header_keyword('crpix4', window_index)
    factor = 1
  ENDELSE
  cdelt = self.get_header_keyword('cdelt4', window_index)
  time_vector = crval + factor * (findgen(naxis)+1.0-crpix) * cdelt
  return, time_vector
END


;+
; Description:
;     Returns XCEN in arcsec.
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : xcen in arcseconds
;-
FUNCTION spice_data::get_xcen, window_index
  ;Returns XCEN in archsec
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  crval = self.get_header_keyword('crval1', window_index)
  return, crval
END


;+
; Description:
;     Returns YCEN in archsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : ycen in arcseconds
;-
FUNCTION spice_data::get_ycen, window_index
  ;Returns YCEN in archsec
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  crval = self.get_header_keyword('crval2', window_index)
  return, crval
END


;+
; Description:
;     Returns FOV in solar x direction, in arcsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : fovx in arcseconds
;-
FUNCTION spice_data::get_fovx, window_index
  ;Returns FOV in solar x direction, in arcsec
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  x_coords = self.get_wcs_coord(window_index, /x)
  minx = min(x_coords, max=maxx)
  return, maxx-minx
END


;+
; Description:
;     Returns FOV in solar y direction, in arcsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : fovy in arcseconds
;-
FUNCTION spice_data::get_fovy, window_index
  ;Returns FOV in solar y direction, in arcsec
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  x_coords = self.get_wcs_coord(window_index, /y)
  minx = min(x_coords, max=maxx)
  return, maxx-minx
END


;+
; Description:
;     Returns the coordinate(s) of one or more specified pixels, or if
;     pixels is not provided, for all pixels. returns coordinate(s) either
;     for all dimensions or just the one specified.
;
; INPUTS:
;     window_index : The index of the window.
;
; OPTIONAL INPUTS:
;     pixels : The pixel for which the coordinates should be returned. Values can be
;              outside of the actual data volume and can be floating point numbers.
;              Must be either a 4-element vector, or a 2D array of the form (4,n)
;              where n is the number of desired pixels.
;
; OPTIONAL KEYWORDS:
;     x : If set, only coordinates of the first dimension (x-direction) are returned.
;     y : If set, only coordinates of the second dimension (y-direction) are returned.
;     lambda : If set, only coordinates of the third dimension (wavelength) are returned.
;     time : If set, only coordinates of the fourth dimension (time) are returned.
;
; OUTPUT:
;     float array,
;         scalar: If one pixel is provided and one of the keywords is set
;         1D: - 1 pixel provided, no keywords set (4-element vector)
;             - Several (n) pixels provided, one of the keywords set (n-element vector)
;         2D: Several (n) pixels provided, no keywords set (4 x n array)
;         4D: No pixels provided, one of the keywords set (NAXIS1 x NAXIS2 x NAXIS3 x NAZIS4 array)
;         5D: No pixels provided, no keywords set (4 x NAXIS1 x NAXIS2 x NAXIS3 x NAZIS4 array)
;-
FUNCTION spice_data::get_wcs_coord, window_index, pixels, x=x, y=y, lambda=lambda, time=time
  ;Returns the coordinate(s) of one or more specified pixels, or all if pixels not provided
  COMPILE_OPT IDL2

  IF ~self.check_window_index(window_index) THEN return, !NULL
  size_pixels = size(pixels)
  IF (size_pixels[0] GT 0 && size_pixels[1] NE 4) || size_pixels[0] GT 2 THEN BEGIN
    message, 'pixels must have size (4,x) where x=any natural number', /info
    return, !NULL
  ENDIF

  coords = wcs_get_coord(*(*self.window_wcs)[window_index], pixels)
  CASE 1 OF
    keyword_set(x): axis_ind = 0
    keyword_set(y): axis_ind = 1
    keyword_set(lambda): axis_ind = 2
    keyword_set(time): axis_ind = 3
    ELSE: axis_ind = indgen(4)
  ENDCASE

  IF size_pixels[0] EQ 0 THEN BEGIN
    IF N_ELEMENTS(axis_ind) EQ 1 THEN BEGIN
      naxis1 = self.get_header_keyword('naxis1', window_index)
      naxis2 = self.get_header_keyword('naxis2', window_index)
      naxis3 = self.get_header_keyword('naxis3', window_index)
      naxis4 = self.get_header_keyword('naxis4', window_index)
      return, reform(coords[axis_ind,*,*,*,*], [naxis1, naxis2, naxis3, naxis4])
    ENDIF
    return, coords

  ENDIF ELSE IF size_pixels[0] EQ 1 THEN BEGIN
    return, coords[axis_ind]
  ENDIF ELSE BEGIN
    return, reform(coords[axis_ind, *])
  ENDELSE
END


;+
; Description:
;     Returns a vector containing the resolution of each dimension, or a
;     scalar number representing the resolution of one dimension.
;
; INPUTS:
;     window_index : the index of the window
;
; KEYWORD PARAMETERS:
;     x : only resolution in x-direction is returned (i.e. 'YLIF-TAN')
;     y : only resolution in y-direction is returned (i.e. 'ZLIF-TAN')
;     lambda : only spectral resolution is returned (i.e. 'WAVE')
;     time : only temporal resolution is returned (i.e. 'TIME')
;   these keyword parameters are exclusive, and if more than one is set, then the first one
;   in the list above is returned
;
; OUTPUT:
;     float array or float
;-
FUNCTION spice_data::get_resolution, window_index, x=x, y=y, lambda=lambda, time=time
  ;Returns a vector containing the resolution of each dimension, or a scalar if a keyword is set
  COMPILE_OPT IDL2

  cdelt1 = self.get_header_keyword('cdelt1', window_index)
  IF keyword_set(x) then return, cdelt1
  cdelt2 = self.get_header_keyword('cdelt2', window_index)
  IF keyword_set(y) then return, cdelt2
  cdelt3 = self.get_header_keyword('cdelt3', window_index)
  IF keyword_set(lambda) then return, cdelt3
  cdelt4 = self.get_header_keyword('cdelt4', window_index)
  IF keyword_set(time) then return, cdelt4
  return, [cdelt1, cdelt2, cdelt3, cdelt4]
END


;+
; Description:
;     Returns the binning factor in spatial y-direction.
;     If window_index is not provided a vector with binning factors for all
;     windows is returned.
;
; OPTIONAL INPUTS:
;     window_index : the index of the window (can be a list of indices)
;
; OUTPUT:
;     int array
;-
FUNCTION spice_data::get_spatial_binning, window_index
  ;Returns the binning factor in spatial y-direction (vector if window_index not provided)
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) eq 0 THEN window_index = indgen(self.get_number_windows())
  bin2 = intarr(N_ELEMENTS(window_index))
  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    bin2[i] = self.get_header_keyword('NBIN2', window_index[i])
  ENDFOR
  return, bin2
END


;+
; Description:
;     Returns the binning factor in spectral direction.
;     If window_index is not provided a vector with binning factors for all
;     windows is returned.
;
; OPTIONAL INPUTS:
;     window_index : the index of the window (can be a list of indices)
;
; OUTPUT:
;     int array
;-
FUNCTION spice_data::get_spectral_binning, window_index
  ;Returns the binning factor in the spectral direction (vector if window_index not provided)
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) eq 0 THEN window_index = indgen(self.get_number_windows())
  bin3 = intarr(N_ELEMENTS(window_index))
  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    bin3[i] = self.get_header_keyword('NBIN3', window_index[i])
  ENDFOR
  return, bin3
END


;+
; Description:
;     Checks whether a given window index is valid
;
; INPUTS:
;     window_index : the index of the window to be checked
;
; OUTPUT:
;     boolean, True if input is a valid window index
;-
FUNCTION spice_data::check_window_index, window_index
  COMPILE_OPT IDL2

  input_type = size(window_index, /type)
  input_index = where([1, 2, 3, 12, 13, 14, 15] EQ input_type)
  IF N_ELEMENTS(window_index) NE 1 || input_index EQ -1 || $
    window_index LT 0 || window_index GE self.nwin THEN BEGIN
    message, 'window_index needs to be a scalar number between 0 and ' + strtrim(string(self.nwin-1),2), /info
    return, 0
  ENDIF ELSE return, 1

END


;+
; Description:
;     Checks whether a given extension index is valid
;
; INPUTS:
;     extension_index : the index of the extension to be checked
;
; OUTPUT:
;     boolean, True if input is a valid extension index
;-
FUNCTION spice_data::check_extension_index, extension_index
  COMPILE_OPT IDL2

  input_type = size(extension_index, /type)
  input_index = where([1, 2, 3, 12, 13, 14, 15] EQ input_type)
  IF N_ELEMENTS(extension_index) NE 1 || input_index EQ -1 || $
    extension_index LT 0 || extension_index GE self.next THEN BEGIN
    message, 'extension_index needs to be a scalar number between 0 and ' + strtrim(string(self.next-1),2), /info
    return, 0
  ENDIF ELSE return, 1

END



;---------------------------------------------------------
; dumbbell info
;---------------------------------------------------------


;+
; Description:
;     Returns 1 if data object contains one or two dumbbells.
;     If window_index is provided, 1 is returned if the given
;     given window contains a dumbbell
;
; OPTIONAL INPUT:
;     window_index : if provided, the method checks whether
;                    this specific window or these specific
;                    windows contain a dumbbell
;
; OUTPUT:
;     boolean
;-
FUNCTION spice_data::has_dumbbells, window_index
  ;Returns 1 if data object contains one or two dumbbells, or if window_index is dumbbell
  COMPILE_OPT IDL2

  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    IF self.dumbbells[0] EQ window_index[i] || self.dumbbells[1] EQ window_index[i] THEN BEGIN
      return, 1
    ENDIF
  ENDFOR
  IF N_ELEMENTS(window_index) GT 0 THEN return, 0
  return, self.dumbbells[0] GE 0 || self.dumbbells[1] GE 0
END


;+
; Description:
;     Returns the indices of the windows that contain the dumbbells
;     2-element vector, or scalar if /lower or /upper is set.
;
; KEYWORD PARAMETERS:
;     lower : If set, only returns the index of the lower dumbbell
;     upper : If set, only returns the index of the upper dumbbell
;
; OUTPUT:
;     2-element vector, or scalar if /lower or /upper is set.
;-
FUNCTION spice_data::get_dumbbells_index, lower=lower, upper=upper
  ;Returns the indices of the windows that contain the dumbbells
  COMPILE_OPT IDL2

  if keyword_set(lower) then return, self.dumbbells[0]
  if keyword_set(upper) then return, self.dumbbells[1]
  return, self.dumbbells
END



;---------------------------------------------------------
; Binary table methods
;---------------------------------------------------------


;+
; Description:
;     This method returns a list of column tags that can be found in the binary extension table.
;
; OUTPUT:
;     string array
;-
FUNCTION spice_data::get_bintable_ttypes
  ;Returns a list of column tags that can be found in the binary extension table.
  COMPILE_OPT IDL2

  return, (*self.bintable_columns).ttype
END


;+
; Description:
;     This method returns the content of one or more columns found in the binary extension table.
;     If the given tag does not exist, the same structure is returned with empty fields, except the
;     tag 'TTYPE' is populated with the provided ttype. When requesting the data of only one TTYPE,
;     one can set the keyword VALUES_ONLY to receive the data only as an array, instead of the structure.
;
; OPTIONAL INPUTS:
;     ttypes : one or more column tags to be returned (e.g. 'MIRRPOS'). If not provided, all columns will
;            be returned.
;
; OUTPUT:
;     array of structure of type:
;             {wcsn:'', tform:'', ttype:'', tdim:'', tunit:'', tunit_desc:'', tdmin:'', tdmax:'', tdesc:'', $
;               tag:'', bin_extension_name:'', data_extension_name:'', data_extension_index:-1, values:ptr_new()}
;     or the data only, i.e. an array of numbers.
;
; KEYWORDS:
;     values_only: If set then only the values in the binary table extension are returned as an array,
;                  instead of the default output structure with metadata. This keyword is ignored if more than
;                  one TTYPES have been provided. If the desired TTYPE does not exist, a !NULL is returned.
;-
FUNCTION spice_data::get_bintable_data, ttypes, values_only=values_only
  ;Returns the content of one or more columns found in the binary extension table.
  COMPILE_OPT IDL2

  IF self.n_bintable_columns EQ 0 THEN BEGIN
    print, 'No binary table extension with variable keywords in this FITS file.'
  ENDIF
  IF N_ELEMENTS(ttypes) eq 0 THEN BEGIN
    ttypes = self.get_bintable_ttypes()
  ENDIF
  ttypes_up = strup(strtrim(ttypes, 2))
  temp_column = {wcsn:'', tform:'', ttype:'', tdim:'', tunit:'', tunit_desc:'', tdmin:'', tdmax:'', tdesc:'', $
    extension:'', values:ptr_new()}
  result = make_array(N_ELEMENTS(ttypes_up), value=temp_column)
  file_open = 0
  FOR i=0,N_ELEMENTS(ttypes_up)-1 DO BEGIN
    ind = where((*self.bintable_columns).ttype eq ttypes_up[i], count)
    IF count GT 0 && self.n_bintable_columns GT 0 THEN BEGIN
      ind=ind[0]

      IF ~ptr_valid((*self.bintable_columns)[ind].values) THEN BEGIN
        ;load column values
        IF ~file_open THEN BEGIN
          FXBOPEN, unit, self.get_filename(), (*self.bintable_columns)[ind].extension
          file_open = 1
        ENDIF ; ~file_open
        data = !NULL
        FXBREAD, unit, data, ttypes_up[i]
        (*self.bintable_columns)[ind].values = ptr_new(data)

        hdr = fxbheader(unit)
        col_num = strtrim(string(fxbcolnum(unit, ttypes_up[i])), 2)
        (*self.bintable_columns)[ind].wcsn = strtrim(fxpar(hdr, 'WCSN'+col_num, missing=''), 2)
        (*self.bintable_columns)[ind].tform = strtrim(fxpar(hdr, 'TFORM'+col_num, missing=''), 2)
        (*self.bintable_columns)[ind].ttype = strup(strtrim(fxpar(hdr, 'TTYPE'+col_num, missing='', comment=comment), 2))
        comment = strtrim(strcompress(comment), 2)
        (*self.bintable_columns)[ind].tdim = strtrim(fxpar(hdr, 'TDIM'+col_num, missing=''), 2)
        (*self.bintable_columns)[ind].tdmin = strtrim(fxpar(hdr, 'TDMIN'+col_num, missing=''), 2)
        (*self.bintable_columns)[ind].tdmax = strtrim(fxpar(hdr, 'TDMAX'+col_num, missing=''), 2)
        (*self.bintable_columns)[ind].tdesc = strtrim(fxpar(hdr, 'TDESC'+col_num, missing=''), 2)
        tunit = strtrim(fxpar(hdr, 'TUNIT'+col_num, missing=''), 2)
        tunit_comment = stregex(comment, '\[.*\]', /extract)
        IF strlen(tunit_comment) GE 2 THEN BEGIN
          tunit_temp = strtrim(strmid(tunit_comment, 1, strlen(tunit_comment)-2), 2)
          tunit_desc = strtrim(strmid(comment, strlen(tunit_comment)+1), 2)
        ENDIF ELSE BEGIN
          tunit_temp = ''
          tunit_desc = comment
        ENDELSE
        IF TUNIT EQ '' THEN BEGIN
          tunit = tunit_temp
        ENDIF
        (*self.bintable_columns)[ind].tunit = tunit
        (*self.bintable_columns)[ind].tunit_desc = tunit_desc

      ENDIF ; ~ptr_valid((*self.bintable_columns)[ind].values)
      result[i] = (*self.bintable_columns)[ind]

    ENDIF ELSE BEGIN ; count GT 0 && self.n_bintable_columns GT 0
      result[i].ttype = ttypes[i]

    ENDELSE ; count GT 0 && self.n_bintable_columns GT 0
  ENDFOR ; i=0,N_ELEMENTS(ttypes_up)-1

  IF file_open THEN FXBCLOSE, unit

  IF keyword_set(values_only) && N_ELEMENTS(ttypes) EQ 1 THEN BEGIN
    IF ptr_valid(result.values) THEN BEGIN
      result = *result.values
    ENDIF ELSE BEGIN
      result = !NULL
    ENDELSE
  ENDIF

  return, result
END



;---------------------------------------------------------
; I/O and related methods for loading data
;---------------------------------------------------------


;+
; Description:
;     Reads a file and sets some variables of this instant of the object.
;     If another file was read before, this link and its settings will be overwritten.
;
; INPUTS:
;     file : path of a SPICE FITS file.
;-
PRO spice_data::read_file, file
  ;Reads a file, overwrites any existing data in this object.
  COMPILE_OPT IDL2

  IF n_elements(file) NE 1 || size(file, /TYPE) NE 7 THEN BEGIN
    message, 'spice_data->read_file, file', /info
    return
  ENDIF
  self.close
  message, 'reading file: ' + file, /info
  self.file = file
  hdr = headfits(file, exten=0)
  self.nwin = fxpar(hdr, 'NWIN')
  fits_open, file, fcb
  self.next = fcb.nextend + 1
  fits_close, fcb

  headers = ptrarr(self.next)
  headers_string = ptrarr(self.next)
  wcs = ptrarr(self.nwin)
  FOR iwin = 0, self.next-1 DO BEGIN
    if iwin gt 0 then begin
      hdr = headfits(file, exten=iwin)
    endif
    headers_string[iwin] = ptr_new(hdr)
    hdr = spice_fitshead2struct(hdr, /multivalue, /silent)
    headers[iwin] = ptr_new(hdr)
    IF iwin LT self.nwin THEN BEGIN
      wcs[iwin] = ptr_new(fitshead2wcs(hdr))
      IF hdr.DUMBBELL EQ 1 THEN self.dumbbells[0] = iwin $
      ELSE IF hdr.DUMBBELL EQ 2 THEN self.dumbbells[1] = iwin
    ENDIF
  ENDFOR ; iwin = 0, self.next-1

  self.window_data = ptr_new(ptrarr(self.next))
  self.window_descaled = ptr_new(bytarr(self.next))
  self.window_masked = ptr_new(bytarr(self.next))
  self.window_headers = ptr_new(headers)
  self.window_headers_string = ptr_new(headers_string)
  self.window_wcs = ptr_new(wcs)
  self.slit_y_range = ptr_new(/allocate)

  self.get_bintable_info
END


;+
; Description:
;     Returns the input filename
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_filename
  ;Returns the input filename
  COMPILE_OPT IDL2

  return, self.file
END


;+
; Description:
;     This methods collects information about the binary table extension(s)
;     It will not load the data itself. This is done when the user calls the method
;     spice_data::get_bintable_data(ttypes)
;-
PRO spice_data::get_bintable_info
  COMPILE_OPT IDL2

  temp_column = {wcsn:'', tform:'', ttype:'', tdim:'', tunit:'', tunit_desc:'', tdmin:'', tdmax:'', tdesc:'', $
    tag:'', bin_extension_name:'', data_extension_name:'', data_extension_index:-1, values:ptr_new()}

  self.n_bintable_columns = 0
  bintable_columns = []
  FOR iwin=0,self.get_number_windows()-1 DO BEGIN
    var_keys = self.get_header_keyword('VAR_KEYS', iwin, '')
    var_keys = strsplit(var_keys, ',', count=count, /extract)
    bin_extension_name = ''
    self.n_bintable_columns += count
    foreach column, var_keys, index do begin
      entry = strsplit(column, ';', count=count, /extract)
      if count eq 2 then begin
        bin_extension_name = strtrim(entry[0], 2)
        column = entry[1]
      endif
      column = strsplit(column, '[', count=count, /extract)
      if count eq 2 then begin
        tag = strsplit(column[1], ']', count=count, /extract)
        tag = tag[0]
      endif else begin
        tag = ''
      endelse
      ttype = column[0]
      column_current = temp_column
      column_current.ttype = strup(strtrim(ttype, 2))
      column_current.tag = strup(strtrim(tag, 2))
      column_current.bin_extension_name = bin_extension_name
      column_current.data_extension_name = self.get_header_keyword('EXTNAME', iwin, '')
      column_current.data_extension_index = iwin
      bintable_columns = [bintable_columns, column_current]
    endforeach
  ENDFOR ; iwin=0,self.get_number_windows()-1
  if N_ELEMENTS(bintable_columns) eq 0 then bintable_columns = make_array(1, value=temp_column)
  self.bintable_columns = ptr_new(bintable_columns)
END


;+
; Description:
;     Class definition procedure
;-
PRO spice_data__define
  COMPILE_OPT IDL2

  struct = {spice_data, $
    file: '', $                 ; input filename
    title: '', $                ; instrument name
    ccd_size: [0,0], $          ; size of the detector, set in init
    nwin: 0, $                  ; number of windows
    next: 0, $                  ; number of extensions
    window_data: ptr_new(), $   ; loaded window data (ptrarr)
    window_descaled: ptr_new(), $; indicates for each window, whether data was loaded, 0:no, 1:yes, descaled, 2: yes, not descaled (bytarr)
    window_masked: ptr_new(), $; indicates for each window, whether data was masked, 0:no, 1:yes, default, 2: yes, approximated (bytarr)
    window_headers: ptr_new(), $; a pointer array, each pointing to a header structure of one extension
    window_headers_string: ptr_new(), $; a pointer array, each pointing to a header string array of one extension
    window_wcs: ptr_new(), $    ; pointers to wcs structure for each window
    dumbbells: [-1, -1], $      ; contains the index of the window with [lower, upper] dumbbell
    slit_y_range:ptr_new(), $   ; contains the (approximate) bottom/top pixel indices of the part of the window that stems from the slit
    bintable_columns: ptr_new(), $; Pointer to string array which contains all columns in the binary extension table
    n_bintable_columns: 0}     ; Number of columns in the binary extension table
END
