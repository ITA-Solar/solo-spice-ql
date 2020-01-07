;+
; NAME:
;     SPICE_DATA__DEFINE
;
; PURPOSE:
;     spice_data__define defines the class structure 'spice_data'.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     The SPICE_DATA__DEFINE procedure is not called directly. An
;     object of class IRIS_DATA is created with the following
;     statement:
;                 spice_data = obj_new('spice_data', file, verbose=verbose)
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; KEYWORD PARAMETERS:
;     verbose : if set, the initiation of the object prints out some information (not used)
;
; OUTPUT:
;     Objects of type SPICE_DATA which describes and contains a SPICE raster
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;     The procedure opens an object of class SPICE_DATA.
;     This procedure includes various functions (methods of
;     class  'spice_data' whose purpose is to get and/or manipulate
;     the different fields of the object.
;
; RESTRICTIONS:
;
; HISTORY:
;     26-Nov-2019: Martin Wiesmann (based on IRIS_DATA__DEFINE)
;-


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; KEYWORD PARAMETERS:
;     verbose : if set, the initiation of the object prints out some information (not used)
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise (not implemented)
;-
FUNCTION spice_data::init, file, verbose=verbose
  COMPILE_OPT IDL2

  self.title='SPICE'
  IF n_elements(file) EQ 1 THEN BEGIN
    self->read_file, file, verbose=verbose
  ENDIF
  return, 1
END


;+
; Description:
;     frees pointer to main data array "window_data" and closes all associated files.
;     used by cleanup and when object should be populated with new data
;-
pro spice_data::close
  COMPILE_OPT IDL2

  FOR i=0,self.nwin-1 DO BEGIN
    ptr_free, (*self.window_data)[i]
    ptr_free, (*self.window_headers)[i]
  ENDFOR
  ptr_free, self.window_data
  ptr_free, self.window_headers
  IF self.file_lun GE 100 && self.file_lun LE 128 THEN free_lun, self.file_lun
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
;     prints out information about the class, such as name, location of definition file
;     and version if there is a line in the header comment beginning with '$ID: ' (comes from CVS).
;     then prints out each procedure and function that have a comment line right after the definition.
;
; KEYWORD PARAMETERS:
;     description : if set, the header info of the class will also be printed.
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


;---------------------------------------------------------
; window data and information
;---------------------------------------------------------


;+
; Description:
;     Returns the data of the specified window. If 'load' keyword is set,
;     the function returns a copy of the array, otherwise a link to the
;     array in the file.
;
; INPUTS:
;     window_index : the index of the desired window
;
; KEYWORD PARAMETERS:
;     load : if set, the data is read from the file and returned as an array
;     nodescale : if set, does not call descale_array, ignored if 'load' is not set
;
; OUTPUT:
;     returns either a link to the data, or the array itself
;-
FUNCTION spice_data::get_window_data, window_index, load=load, nodescale=nodescale
  ;Returns a link to the data of window, or the data itself if keyword load is set
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 1 THEN BEGIN
    message, 'missing input, usage: get_window_data, window_index [, load=load, nodescale=nodescale]', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_window_index(window_index) THEN return, !NULL

  IF keyword_set(load) THEN BEGIN
    IF keyword_set(nodescale) THEN BEGIN
      data = (*(*self.window_data)[window_index])[0]
    ENDIF ELSE BEGIN
      data = self.descale_array((*(*self.window_data)[window_index])[0], window_index)
    ENDELSE
  ENDIF ELSE BEGIN
    data = *(*self.window_data)[window_index]
  ENDELSE
  return, data
END


;+
; Description:
;     Returns the data of the specified window and exposure index. If 'nodescale' keyword is set,
;     the function returns the data without applying 'descale_array' function.
;     The exposure index is in the first dimension of the 4D cube in case the study type is 'Raster',
;     and in the fourth dimension if study type is 'Sit-and-stare'.
;     The array is also transposed, so that it can be directly plotted, i.e.
;     array = [lambda, instrument-Y]
;
; INPUTS:
;     window_index : the index of the desired window
;     exposure_index : the index of the desired exposure
;
; KEYWORD PARAMETERS:
;     nodescale : if set, does not call descale_array
;
; OUTPUT:
;     returns the desired 2-dimensional image, as an array
;-
FUNCTION spice_data::get_one_image, window_index, exposure_index, nodescale=nodescale
  ;Returns a transposed 2D subset of the data from the specified window and exposure (array = [lambda, instrument-Y])
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: get_one_image, window_index, exposure_index [, nodescale=nodescale]', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_window_index(window_index) THEN return, !NULL
  IF self.get_sit_and_stare() THEN naxis = self.get_header_info('NAXIS4', window_index) $
  ELSE naxis = self.get_header_info('NAXIS1', window_index)
  IF exposure_index LT 0 || exposure_index GE naxis  THEN BEGIN
    print, 'exposure_index needs to be a scalar number between 0 and '+strtrim(string(naxis-1),2)
    return, !NULL
  ENDIF

  data = self.get_window_data(window_index, /load, nodescale=nodescale)
  IF self.get_sit_and_stare() THEN BEGIN
    data = reform(data[0,*,*,exposure_index])
  ENDIF ELSE BEGIN
    data = reform(data[exposure_index,*,*])
  ENDELSE
  data = transpose(data)
  return, data
END


;+
; Description:
;     Descales the array, using BSCALE and BZERO keywords in the header.
;     If you get the data from this object via get_window_data() without
;     setting the keyword 'load', you will have to call this method yourself.
;
; INPUTS:
;     array : a numeric array
;     window_index : the index of the window this array belongs to
;
; OUTPUT:
;     returns the descaled array (=array * bscale + bzero)
;-
FUNCTION spice_data::descale_array, array, window_index
  ;Descales the array, using BSCALE and BZERO keywords in the header
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: descale_array, array, window_index', /info
    return, !NULL
  ENDIF

  bscale = self.get_header_info('BSCALE', window_index)
  bzero = self.get_header_info('BZERO', window_index)
  return, array * bscale + bzero
END


;+
; Description:
;     Returns the specified keyword from the given window, if the keyword
;     does not exist 'missing_value' is returned if it is provided, !NULL otherwise.
;
; INPUTS:
;     keyword : string, the header keyword to be returned
;     window_index : the index of the window this keyword belongs to
;
; OPTIONAL INPUTS:
;     missing_value : the value that should be returned, if the keyword does not exist
;                     if this is not provided !NULL is returned
;
; OPTIONAL OUTPUT:
;     exists : boolean, True if keyword exists
;
; OUTPUT:
;     returns the keyword value, 'missing_value' or !NULL
;-
FUNCTION spice_data::get_header_info, keyword, window_index, missing_value, exists=exists
  ;Returns the specified keyword from the window, or 'missing_value' if provided, !NULL otherwise
  COMPILE_OPT IDL2

  IF N_PARAMS() LT 2 THEN BEGIN
    message, 'missing input, usage: get_header_info, keyword, window_index [, missing_value, exists=exists]', /info
    return, !NULL
  ENDIF ELSE IF N_ELEMENTS(keyword) NE 1 || SIZE(keyword, /TYPE) NE 7 THEN BEGIN
    message, 'keyword needs to be a scalar string', /info
    return, !NULL
  ENDIF ELSE IF ~self.check_window_index(window_index) THEN return, !NULL

  ; keywords with a '-' in the name, will be renamed when they are transformed into structures (in fitshead2struct),
  ; '-' becomes '_D$'
  temp = strsplit(keyword, '-', count=count, /extract)
  IF count GT 1 THEN keyword = strjoin(temp, '_D$')

  exists = TAG_EXIST(*(*self.window_headers)[window_index], keyword, index=index)
  IF exists THEN BEGIN
    return, (*(*self.window_headers)[window_index]).(index)
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(missing_value) EQ 0 THEN return, !NULL $
    ELSE return, missing_value
  ENDELSE

END


;+
; Description:
;     returns the number of windows this file/object contains
;
; OUTPUT:
;     number of windows
;-
FUNCTION spice_data::get_number_windows
  ;returns the number of windows this file contains
  COMPILE_OPT IDL2

  return, self.nwin
END


;+
; Description:
;     returns start date and time of observation in UTC format
;
; OUTPUT:
;     number of windows
;-
FUNCTION spice_data::get_start_time
  ;returns start date and time of observation in UTC format
  COMPILE_OPT IDL2

  start_time = self.get_header_info('DATE-BEG', 0)
  return, start_time
END


;+
; Description:
;     returns end date and time of observation in UTC format
;
; OUTPUT:
;     number of windows
;-
FUNCTION spice_data::get_end_time
  ;returns end date and time of observation in UTC format
  COMPILE_OPT IDL2

  end_time = self.get_header_info('DATE-END', 0)
  return, end_time
END


;+
; Description:
;     returns 1 if raster is a sit-and-stare, 0 otherwise
;
; OUTPUT:
;     boolean
;-
FUNCTION spice_data::get_sit_and_stare
  ;returns 1 if raster is a sit-and-stare, 0 otherwise
  COMPILE_OPT IDL2

  sit_and_stare = self.get_header_info('STUDYTYP', 0) EQ 'Sit-and-stare'
  return, sit_and_stare
END


;+
; Description:
;     returns the window ID
;
; INPUTS:
;     window_index : the index of the window the ID is asked for
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_window_id, window_index
  ;returns the window ID
  COMPILE_OPT IDL2

  IF n_params() EQ 0 THEN BEGIN
    window_id = strarr(self.get_number_windows())
    FOR i = 0, self.get_number_windows()-1 DO BEGIN
      window_id[i] = self.get_header_info('EXTNAME', i)
    ENDFOR
  ENDIF ELSE BEGIN
    window_id = self.get_header_info('EXTNAME', window_index)
  ENDELSE

  return, window_id
END


;+
; Description:
;     returns the number of exposures in the window
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_number_exposures, window_index
  ;returns the number of exposures in the window
  COMPILE_OPT IDL2

  IF self.get_sit_and_stare() then n_exp = self.get_header_info('NAXIS4', window_index) $
  ELSE n_exp = self.get_header_info('NAXIS1', window_index)
  return, n_exp
END


;+
; Description:
;     returns the exposure time of the given window per exposure
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_exposure_time, window_index
  ;returns the exposure time of the given window per exposure
  COMPILE_OPT IDL2

  exptime = self.get_header_info('XPOSURE', window_index)
  return, exptime
END


;+
; Description:
;     returns a vector containting the coordinate for each pixel in first dimension, instrument x-direction
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, coordinate in arcsec
;-
FUNCTION spice_data::get_instr_x_vector, window_index
  ;returns a vector containting the coordinate for each pixel in instrument x-direction
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval1', window_index)
  naxis = self.get_header_info('naxis1', window_index)
  crpix = self.get_header_info('crpix1', window_index)
  cdelt = self.get_header_info('cdelt1', window_index)
  pc1_1 = self.get_header_info('PC1_1', window_index)
  x_vector = crval + cdelt * pc1_1 * (findgen(naxis)+1.0-crpix)
  IF naxis EQ 1 THEN BEGIN
    naxis = self.get_header_info('naxis4', window_index)
    x_vector = replicate(x_vector, naxis)
  ENDIF
  return, x_vector
END


;+
; Description:
;     returns a vector containting the coordinate for each pixel in second dimension, instrument y-direction
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, coordinate in arcsec
;-
FUNCTION spice_data::get_instr_y_vector, window_index
  ;returns a vector containting the coordinate for each pixel in instrument y-direction
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval2', window_index)
  naxis = self.get_header_info('naxis2', window_index)
  crpix = self.get_header_info('crpix2', window_index)
  cdelt = self.get_header_info('cdelt2', window_index)
  pc2_2 = self.get_header_info('PC2_2', window_index)
  y_vector = crval + cdelt * pc2_2 * (findgen(naxis)+1.0-crpix)
  return, y_vector
END


;+
; Description:
;     returns a vector containting the wavelength for each pixel in third dimension
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, wavelength in nm
;-
FUNCTION spice_data::get_lambda_vector, window_index
  ;returns a vector containting the wavelength for each pixel in third dimension
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval3', window_index)
  naxis = self.get_header_info('naxis3', window_index)
  crpix = self.get_header_info('crpix3', window_index)
  cdelt = self.get_header_info('cdelt3', window_index)
  lambda_vector = crval + (findgen(naxis)+1.0-crpix) * cdelt
  return, lambda_vector
END


;+
; Description:
;     returns a vector containting the time for each pixel in fourth dimension
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, time in seconds
;-
FUNCTION spice_data::get_time_vector, window_index
  ;returns a vector containting the time for each pixel in fourth dimension
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval4', window_index)
  naxis = self.get_header_info('naxis4', window_index)
  IF naxis EQ 1  THEN BEGIN
    naxis = self.get_header_info('naxis1', window_index)
    crpix = self.get_header_info('crpix1', window_index)
    factor = self.get_header_info('PC4_1', window_index)
  ENDIF ELSE BEGIN
    crpix = self.get_header_info('crpix4', window_index)
    factor = 1
  ENDELSE
  cdelt = self.get_header_info('cdelt4', window_index)
  time_vector = crval + factor * (findgen(naxis)+1.0-crpix) * cdelt
  return, time_vector
END


;+
; Description:
;     returns a vector containting the resolution of each dimension, or a
;     scalar number respresenting the resolution of one dimension.
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
  ;returns a vector containting the resolution of each dimension, or a scalar if a keyword is set
  COMPILE_OPT IDL2

  cdelt1 = self.get_header_info('cdelt1', window_index)
  IF keyword_set(x) then return, cdelt1
  cdelt2 = self.get_header_info('cdelt2', window_index)
  IF keyword_set(y) then return, cdelt2
  cdelt3 = self.get_header_info('cdelt3', window_index)
  IF keyword_set(lambda) then return, cdelt3
  cdelt4 = self.get_header_info('cdelt4', window_index)
  IF keyword_set(time) then return, cdelt4
  return, [cdelt1, cdelt2, cdelt3, cdelt4]
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
    print, 'window_index needs to be a scalar number between 0 and '+strtrim(string(self.nwin-1),2)
    return, 0
  ENDIF ELSE return, 1

END



;---------------------------------------------------------
; dumbbell info
;---------------------------------------------------------


;+
; Description:
;     Returns 1 if data object contains one or two dumbbells
;
; OUTPUT:
;     boolean, True if input is a valid window index
;-
FUNCTION spice_data::has_dumbbells
  ;returns 1 if data object contains one or two dumbbells
  COMPILE_OPT IDL2

  return, self.dumbbells[0] ge 0 || self.dumbbells[1] ge 0
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
;     boolean, True if input is a valid window index
;-
FUNCTION spice_data::get_dumbbells_index, lower=lower, upper=upper
  ;returns the indices of the windows that contain the dumbbells
  COMPILE_OPT IDL2

  if keyword_set(lower) then return, self.dumbbells[0]
  if keyword_set(upper) then return, self.dumbbells[1]
  return, self.dumbbells
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
;
; KEYWORD PARAMETERS:
;     verbose : if set, the initiation of the object prints out some information (not used)
;-
PRO spice_data::read_file, file, verbose=verbose
  ;Reads a file, overwrites any existing data in this object.
  COMPILE_OPT IDL2

  IF n_elements(file) NE 1 || size(file, /TYPE) NE 7 THEN BEGIN
    message, 'spice_data->read_file, file [, verbose=verbose]', /info
    return
  ENDIF
  self.close
  IF keyword_set(verbose) THEN silent=1 ELSE silent=0
  message, 'reading file: ' + file, /info
  mreadfits_header, file, hdr, extension=0, only_tags='NWIN'
  self.nwin = hdr.nwin

  ; find location of line windows in fits file
  openr, file_lun, file, /swap_if_little_endian, /get_lun
  self.file_lun = file_lun
  position = iris_find_winpos(file_lun, self.nwin)
  assocs = ptrarr(self.nwin)
  headers = ptrarr(self.nwin)
  dumbbells = bytarr(self.nwin)
  FOR iwin = 0, self.nwin-1 DO BEGIN
    mreadfits_header, file, hdr, extension=iwin
    headers[iwin] = ptr_new(hdr)
    IF hdr.DUMBBELL EQ 1 THEN self.dumbbells[0] = iwin $
    ELSE IF hdr.DUMBBELL EQ 2 THEN self.dumbbells[1] = iwin

    CASE hdr.BITPIX OF
      16: assocs[iwin] = ptr_new(assoc(file_lun, intarr(hdr.NAXIS1, hdr.NAXIS2, hdr.NAXIS3, hdr.NAXIS4, /NOZERO), position[iwin]))
      -32: assocs[iwin] = ptr_new(assoc(file_lun, fltarr(hdr.NAXIS1, hdr.NAXIS2, hdr.NAXIS3, hdr.NAXIS4, /NOZERO), position[iwin]))
      ELSE: message,'unsupported datatype ' + strtrim(string(hdr.BITPIX), 2)
    ENDCASE

  ENDFOR ; iwin = 0, self.nwin-1
  self.window_data = ptr_new(assocs)
  self.window_headers = ptr_new(headers)

END


;+
; Description:
;     Class definition procedure
;-
PRO spice_data__define
  COMPILE_OPT IDL2

  struct = {spice_data, $
    title: '', $                ; instrument name
    nwin: 0, $                  ; number of windows
    window_data: ptr_new(), $   ; pointers to window data in the file using assoc (ptrarr)
    window_headers: ptr_new(), $; a pointer array, each pointing to a header structure of one window
    dumbbells: [-1, -1], $      ; contains the index of the window with [lower, upper] dumbbell
    file_lun: 0}                ; Logical Unit Number of the file
END
