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
;-
; $Id: 2020-11-25 13:33 CET $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise (not implemented)
;-
FUNCTION spice_data::init, file
  COMPILE_OPT IDL2

  self.title = 'SPICE'
  self.ccd_size = [1024, 1024]
  IF n_elements(file) EQ 1 THEN BEGIN
    self->read_file, file
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
    ptr_free, (*self.window_assoc)[i]
    ptr_free, (*self.window_headers)[i]
    ptr_free, (*self.window_headers_string)[i]
    ptr_free, (*self.window_wcs)[i]
    IF ptr_valid((*self.window_data)[i]) THEN ptr_free, (*self.window_data)[i]
  ENDFOR
  ptr_free, self.window_assoc
  ptr_free, self.window_headers
  ptr_free, self.window_headers_string
  ptr_free, self.window_wcs
  ptr_free, self.window_descaled
  ptr_free, self.window_data
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
    IF keyword_set(nodescale) THEN descaled=2 ELSE descaled=1
    IF (*self.window_descaled)[window_index] EQ descaled THEN BEGIN
      data = *(*self.window_data)[window_index]
    ENDIF ELSE BEGIN
      data = (*(*self.window_assoc)[window_index])[0]
      IF ~keyword_set(nodescale) THEN data = self.descale_array(data, window_index)
      (*self.window_descaled)[window_index] = descaled
      IF ptr_valid((*self.window_data)[window_index]) THEN ptr_free, (*self.window_data)[window_index]
      (*self.window_data)[window_index] = ptr_new(data)
    ENDELSE
  ENDIF ELSE BEGIN
    data = *(*self.window_assoc)[window_index]
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
;     debin : if set, the image will be expanded if binning is GT 1, and data values
;             will be divided by the binning value
;
; OUTPUT:
;     returns the desired 2-dimensional image, as an array
;-
FUNCTION spice_data::get_one_image, window_index, exposure_index, debin=debin, nodescale=nodescale
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

  bscale = self.get_header_info('BSCALE', window_index, 1)
  bzero = self.get_header_info('BZERO', window_index, 0)
  return, array * bscale + bzero
END


;+
; Description:
;     Returns window index of a given wavelength or window name
;
; INPUTS:
;     input : scalar or array of numbers or string
;             if input is one or more numbers, it is interpreted as wavelengths
;             and indices of windows including those wavelengths are returned
;             if input is one or more string, it is interpreted as the window ID
;             and indices of the corresponding windows are returned
;
; OUTPUT:
;     int array, with as many elements as input
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
;     Returns the position of the window on the CCD, starting with 0 if idl_coord is set, 1 otherwise
;     position is given as a 4-element vector, with [lambda0, lambda1, y0, y1].
;     Note: y0 > y1, but lambda0 < lambda1
;
; INPUTS:
;     window_index : the index of the window
;
; KEYWORD PARAMETERS:
;     idl_coord : if set, the coordinates start with zero, instead of with 1
;     reverse_y : y-coordinates are given as (CCD-size +1 - (original y-coords))
;     reverse_x : for dumbbells x-coordinates are flipped, if this keyword is set, the coordinates will
;                 be flipped again, i.e. values of PXBEG3 and PXEND3 will be swapped
;     no_warning: if set, warnings about x-flipping will be suppressed
;
; OUTPUT:
;     int array
;
; OPTIONAL OUTPUT:
;     detector : int, 1 or 2 to indicate on which detector the winodow is
;-
FUNCTION spice_data::get_window_position, window_index, detector=detector, $
  idl_coord=idl_coord, reverse_y=reverse_y, reverse_x=reverse_x, no_warning=no_warning
  ;Returns the position of the window on the CCD, starting with 0 if idl_coord is set, 1 otherwise
  COMPILE_OPT IDL2

  ccd_size = self.get_ccd_size()

  PXBEG3 = self.get_header_info('PXBEG3', window_index)
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

  PXEND3 = self.get_header_info('PXEND3', window_index)
  IF PXEND3 LT 0 THEN message, 'PXEND3 < 0: '+strtrim(string(PXEND3))+' < 0', /info
  IF PXEND3 LT PXBEG3 THEN BEGIN
    IF self.has_dumbbells(window_index) && keyword_set(reverse_x) THEN BEGIN
      beg_temp = PXEND3
      PXEND3 = PXBEG3
      PXBEG3 = beg_temp
    ENDIF ELSE BEGIN
      IF ~keyword_set(no_warning) THEN message, 'PXEND3 < PXBEG3: '+strtrim(string(PXEND3))+' < '+strtrim(string(PXBEG3)), /info
    ENDELSE
  ENDIF
  IF PXEND3 GT 2*ccd_size[0] THEN $
    message, 'PXEND3 > 2 * CCD-size: '+strtrim(string(PXEND3))+' > '+strtrim(string(2*ccd_size[0])), /info

  PXBEG2 = self.get_header_info('PXBEG2', window_index)
  IF PXBEG2 LT 0 THEN message, 'PXBEG2 < 0: '+strtrim(string(PXBEG2))+' < 0', /info
  IF PXBEG2 GT ccd_size[1] THEN $
    message, 'PXBEG2 > CCD-size: '+strtrim(string(PXBEG2))+' > '+strtrim(string(ccd_size[1])), /info

  PXEND2 = self.get_header_info('PXEND2', window_index)
  IF PXEND2 LT 0 THEN message, 'PXEND2 < 0: '+strtrim(string(PXEND2))+' < 0', /info
  IF PXEND2 GT PXBEG2 THEN message, 'PXEND2 > PXBEG2: '+strtrim(string(PXEND2))+' > '+strtrim(string(PXBEG2)), /info
  IF PXEND2 GT ccd_size[1] THEN $
    message, 'PXEND2 > CCD-size: '+strtrim(string(PXEND2))+' > '+strtrim(string(ccd_size[1])), /info

  position = [PXBEG3, PXEND3, PXBEG2, PXEND2]
  IF keyword_set(reverse_y) THEN position[2:3] = ccd_size[1] + 1 - position[2:3]
  IF keyword_set(idl_coord) THEN position = position - 1
  return, position
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
;     Returns the header of the given window
;
; INPUTS:
;     window_index : the index of the window this keyword belongs to
;
; KEYWORD PARAMETERS:
;     lower_dumbbell : if set, the header of the lower dumbbell will be returned
;     upper_dumbbell : if set, the header of the upper dumbbell will be returned
;     string : if set, the header will be returned as a string array instead of a structure
;
; OUTPUT:
;     returns the header as a structure or a string array
;-
FUNCTION spice_data::get_header, window_index, lower_dumbbell=lower_dumbbell, upper_dumbbell=upper_dumbbell, $
  string=string
  ;Returns the header of the given window as a structure or a string array
  COMPILE_OPT IDL2

  IF keyword_set(lower_dumbbell) THEN window_index=self.get_dumbbells_index(/lower)
  IF keyword_set(upper_dumbbell) THEN window_index=self.get_dumbbells_index(/upper)
  IF ~self.check_window_index(window_index) THEN return, !NULL
  IF keyword_set(string) then return, *(*self.window_headers_string)[window_index] $
  ELSE return, *(*self.window_headers)[window_index]
END


;+
; Description:
;     returns the number of windows this file/object contains
;
; OUTPUT:
;     int: number of windows
;-
FUNCTION spice_data::get_number_windows
  ;returns the number of windows this file contains
  COMPILE_OPT IDL2

  return, self.nwin
END


;+
; Description:
;     returns the title
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_title
  ;returns the title, i.e. 'SPICE'
  COMPILE_OPT IDL2

  return, self.title
END


;+
; Description:
;     returns SPICE OBS ID
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_obs_id
  ;returns SPICE OBS ID
  COMPILE_OPT IDL2

  obs_id = self.get_header_info('SPIOBSID', 0, -1)
  IF size(obs_id, /TYPE) NE 7 THEN obs_id = string(obs_id)
  return, strtrim(obs_id,2)
END


;+
; Description:
;     returns start date and time of observation in UTC format
;
; OUTPUT:
;     int: number of windows
;-
FUNCTION spice_data::get_start_time
  ;returns start date and time of observation in UTC format
  COMPILE_OPT IDL2

  start_time = self.get_header_info('DATE-BEG', 0, '')
  return, start_time
END


;+
; Description:
;     returns end date and time of observation in UTC format
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_end_time
  ;returns end date and time of observation in UTC format
  COMPILE_OPT IDL2

  end_time = self.get_header_info('DATE-END', 0, '')
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
;     returns the level of the file
;
; OUTPUT:
;     int: level number (0, 1 or 2)
;
; OPTIONAL OUTPUT:
;     low_latency: boolean, 1 if this file is a low latency file (i.e. LL0x)
;-
FUNCTION spice_data::get_level, low_latency=low_latency
  ;returns the level of the file
  COMPILE_OPT IDL2

  level_string = self.get_header_info('LEVEL', 0)
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
;     returns BUNIT, physical units of the data
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_variable_unit
  ;returns BUNIT, physical units of the data
  COMPILE_OPT IDL2

  bunit = self.get_header_info('BUNIT', 0, '')
  return, bunit
END


;+
; Description:
;     returns BTYPE, type of data in images
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_variable_type
  ;returns BTYPE, type of data in images
  COMPILE_OPT IDL2

  btype = self.get_header_info('BTYPE', 0, '')
  return, btype
END


;+
; Description:
;     returns S/C CCW roll relative to Solar north in degrees
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_satellite_rotation
  ;returns S/C CCW roll relative to Solar north in degrees
  COMPILE_OPT IDL2

  crota = self.get_header_info('CROTA', 0)
  return, crota
END


;+
; Description:
;     returns the value for missing pixels
;
; OUTPUT:
;     float
;-
FUNCTION spice_data::get_missing_value
  ;returns the value for missing pixels
  COMPILE_OPT IDL2

  missing = self.get_header_info('BLANK', 0)
  if N_ELEMENTS(missing) EQ 0 && self->get_level() EQ 2 THEN missing = !values.f_nan
  return, missing
END


;+
; Description:
;     returns the 2-element vector containing the CCD size
;
; OUTPUT:
;     int array
;-
FUNCTION spice_data::get_ccd_size
  ;returns the 2-element vector containing the CCD size
  COMPILE_OPT IDL2

  return, self.ccd_size
END


;+
; Description:
;     returns the window ID of one or more windows. window_index is optional
;     if not provided, window IDs of all windows are returned, if it is
;     scalar, the result will be a scalar string, and if window_index is
;     an array, the result will be a string array of same size.
;
; INPUTS:
;     window_index : the index of the window the ID is asked for
;                    scalar or 1D-int-array
;
; OUTPUT:
;     string or string array
;-
FUNCTION spice_data::get_window_id, window_index
  ;returns the window ID, as string or string array
  COMPILE_OPT IDL2

  IF n_params() EQ 0 THEN BEGIN
    window_id = strarr(self.get_number_windows())
    FOR i = 0, self.get_number_windows()-1 DO BEGIN
      window_id[i] = self.get_header_info('EXTNAME', i)
    ENDFOR
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(window_index) eq 1 THEN BEGIN
      window_id = self.get_header_info('EXTNAME', window_index[0])
    ENDIF ELSE BEGIN
      window_id = strarr(N_ELEMENTS(window_index))
      FOR i = 0,N_ELEMENTS(window_index)-1 DO BEGIN
        window_id[i] = self.get_header_info('EXTNAME', window_index[i])
      ENDFOR
    ENDELSE
  ENDELSE

  return, window_id
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
PRO spice_data::show_lines
  ;returns the window ID
  COMPILE_OPT IDL2

  window_id = self.get_window_id()
  FOR i=0,self.get_number_windows()-1 DO BEGIN
    print, i, ': ' + window_id[i]
  ENDFOR
END


;+
; Description:
;     returns the number of exposures in the window, or if window_index
;     is not provided, a vector containing the numbers of exposures
;     for each window
;
; OPTIONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     int or int-array
;-
FUNCTION spice_data::get_number_exposures, window_index
  ;returns the number of exposures in the window
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) EQ 0 THEN BEGIN
    n_exp = intarr(self.get_number_windows())
    FOR iwin=0,self.get_number_windows()-1 DO BEGIN
      IF self.get_sit_and_stare() THEN n_exp[iwin] = self.get_header_info('NAXIS4', iwin) $
      ELSE n_exp[iwin] = self.get_header_info('NAXIS1', iwin)
    ENDFOR
  ENDIF ELSE BEGIN
    IF self.get_sit_and_stare() THEN n_exp = self.get_header_info('NAXIS4', window_index) $
    ELSE n_exp = self.get_header_info('NAXIS1', window_index)
  ENDELSE
  return, n_exp
END


;+
; Description:
;     returns the number of pixels in y in the window, or if window_index
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
  ;returns the number of pixels in y in the window
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) EQ 0 THEN BEGIN
    n_slit = intarr(self.get_number_windows)
    FOR iwin=0,self.get_number_windows-1 DO BEGIN
      n_slit[iwin] = self.get_header_info('NAXIS2', iwin)
    ENDFOR
  ENDIF ELSE BEGIN
    n_slit = self.get_header_info('NAXIS2', window_index)
  ENDELSE
  return, n_slit
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
;     returns name of axis, if axis is not provided a string vector
;     will be returned that contains the names of all axes.
;     The name of the axis includes its unit in square brackets,
;     except if the pixels keyword is set, then it says pixels instead
;     of units, or if no_unit keyword is set.
;
; OPTIONAL INPUTS:
;     axis : the index of the axis, may be a vector
;
; KEYWORD PARAMETERS:
;     pixels : return 'pixels' as unit
;     no_unit : do not include units in axis name
;
; OUTPUT:
;     string or string array
;-
FUNCTION spice_data::get_axis_title, axis, pixels=pixels, no_unit=no_unit
  ;returns name of axis, if axis is not provided a string vector will be returned
  COMPILE_OPT IDL2

  axes = ['Solar X', 'Solar Y', 'Wavelength', 'Time']
  IF ~keyword_set(no_unit) THEN BEGIN
    IF keyword_set(pixels) THEN BEGIN
      axes = axes + ' [pixels]'
    ENDIF ELSE BEGIN
      axes[0] = axes[0] + ' [' + strtrim(self.get_header_info('CUNIT1', 0),2) + ']'
      axes[1] = axes[1] + ' [' + strtrim(self.get_header_info('CUNIT2', 0),2) + ']'
      axes[2] = axes[2] + ' [' + strtrim(self.get_header_info('CUNIT3', 0),2) + ']'
      axes[3] = axes[3] + ' [' + strtrim(self.get_header_info('CUNIT4', 0),2) + ']'
    ENDELSE
  ENDIF
  IF N_ELEMENTS(axis) eq 0 THEN return, axes
  return, axes[axis]
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
  ;returns a vector containing the coordinate for each pixel in instrument x-direction
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
;     for the selected window, or the full CCD this window belongs to
;
; INPUTS:
;     window_index : the index of the window
;
; OPTIONAL KEYWORDS:
;     full_ccd : if set, a vector of size CCD-size[1] is returned with coordinate values
;                for the whole detector
;
; OUTPUT:
;     float array, coordinate in arcsec
;-
FUNCTION spice_data::get_instr_y_vector, window_index, full_ccd=full_ccd
  ;returns a vector containing the coordinate for each pixel in instrument y-direction
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval2', window_index)
  crpix = self.get_header_info('crpix2', window_index)
  cdelt = self.get_header_info('cdelt2', window_index)
  pc2_2 = self.get_header_info('PC2_2', window_index)
  IF keyword_set(full_ccd) THEN BEGIN
    PXBEG3 = (self.get_window_position(window_index, /reverse_y, /no_warning))[2]
    cripx = crpix + PXBEG3
    naxis = (self.get_ccd_size())[1]
  ENDIF ELSE BEGIN
    naxis = self.get_header_info('naxis2', window_index)
  ENDELSE
  y_vector = crval + cdelt * pc2_2 * (findgen(naxis)+1.0-crpix)
  return, y_vector
END


;+
; Description:
;     returns a vector containting the wavelength for each pixel in third dimension for
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
  ;returns a vector containing the wavelength for each pixel in third dimension for window or full CCD
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval3', window_index)
  cdelt = self.get_header_info('cdelt3', window_index)
  crpix = self.get_header_info('crpix3', window_index)
  IF keyword_set(full_ccd) THEN BEGIN
    PXBEG3 = self.get_header_info('PXBEG3', window_index)
    cripx = crpix + PXBEG3
    naxis = (self.get_ccd_size())[0]
  ENDIF ELSE BEGIN
    naxis = self.get_header_info('naxis3', window_index)
  ENDELSE
  lambda_vector = crval + (findgen(naxis)+1.0-crpix) * cdelt
  return, lambda_vector
END


;+
; Description:
;     returns a vector containing the time for each pixel in fourth dimension
;
; INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float array, time in seconds
;-
FUNCTION spice_data::get_time_vector, window_index
  ;returns a vector containing the time for each pixel in fourth dimension
  COMPILE_OPT IDL2

  crval = self.get_header_info('crval4', window_index)
  naxis = self.get_header_info('naxis4', window_index)
  IF naxis EQ 1  THEN BEGIN
    naxis = self.get_header_info('naxis1', window_index)
    crpix = self.get_header_info('crpix1', window_index)
    factor = self.get_header_info('PC4_1', window_index, 1)
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
;     returns XCEN in archsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : xcen in arcseconds
;-
FUNCTION spice_data::get_xcen, window_index
  ;returns XCEN in archsec
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  crval = self.get_header_info('crval1', window_index)
  return, crval
END


;+
; Description:
;     returns YCEN in archsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : ycen in arcseconds
;-
FUNCTION spice_data::get_ycen, window_index
  ;returns YCEN in archsec
  COMPILE_OPT IDL2

  if N_ELEMENTS(window_index) eq 0 then begin
    window_index = 0
  endif else begin
    IF ~self.check_window_index(window_index) THEN return, !NULL
  endelse
  crval = self.get_header_info('crval2', window_index)
  return, crval
END


;+
; Description:
;     returns FOV in solar x direction, in arcsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : fovx in arcseconds
;-
FUNCTION spice_data::get_fovx, window_index
  ;returns FOV in solar x direction, in arcsec
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
;     returns FOV in solar y direction, in arcsec
;
; OPTONAL INPUTS:
;     window_index : the index of the window
;
; OUTPUT:
;     float : fovy in arcseconds
;-
FUNCTION spice_data::get_fovy, window_index
  ;returns FOV in solar y direction, in arcsec
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
;     returns the coordinate(s) of one or more specified pixels, or if
;     pixels is not provided, for all pixels. returns coordinate(s) either
;     for all dimensions or just the one specified.
;
; INPUTS:
;     window_index : the index of the window
;
; OPTIONAL INPUTS:
;     pixels : the pixel for which the coordinates should be returned. Values can be
;              outside of the actual data volume and can be floating point numbers.
;              Must be either a 4-element vector, or a 2D array of the form (4,n)
;              where n is the number of desired pixels.
;
; OPTIONAL KEYWORDS:
;     x : if set, only coordinates of first dimension (x-direction) or returned
;     y : if set, only coordinates of second dimension (y-direction) or returned
;     lambda : if set, only coordinates of third dimension (wavelength) or returned
;     time : if set, only coordinates of fourth dimension (time) or returned
;
; OUTPUT:
;     float array,
;         scalar: if one pixel is provided and one of the keywords is set
;         1D: - 1 pixel provided, no keywords set (4-element vector)
;             - several (n) pixels provided, one of the keywords set (n-element vector)
;         2D: several (n) pixels provided, no keywords set (4 x n array)
;         4D: no pixels provided, one of the keywords set (NAXIS1 x NAXIS2 x NAXIS3 x NAZIS4 array)
;         5D: no pixels provided, no keywords set (4 x NAXIS1 x NAXIS2 x NAXIS3 x NAZIS4 array)
;-
FUNCTION spice_data::get_wcs_coord, window_index, pixels, x=x, y=y, lambda=lambda, time=time
  ;returns the coordinate(s) of one or more specified pixels, or all if pixels not provided
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
      naxis1 = self.get_header_info('naxis1', window_index)
      naxis2 = self.get_header_info('naxis2', window_index)
      naxis3 = self.get_header_info('naxis3', window_index)
      naxis4 = self.get_header_info('naxis4', window_index)
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
;     returns a vector containing the resolution of each dimension, or a
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
  ;returns a vector containing the resolution of each dimension, or a scalar if a keyword is set
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
;     returns the binning factor in spatial y-direction.
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
  ;returns the binning factor in spatial y-direction (vector if window_index not provided)
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) eq 0 THEN window_index = indgen(self.get_number_windows())
  bin2 = intarr(N_ELEMENTS(window_index))
  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    bin2[i] = self.get_header_info('NBIN2', window_index[i])
  ENDFOR
  return, bin2
END


;+
; Description:
;     returns the binning factor in spectral direction.
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
  ;returns the binning factor in the spectral direction (vector if window_index not provided)
  COMPILE_OPT IDL2

  IF N_ELEMENTS(window_index) eq 0 THEN window_index = indgen(self.get_number_windows())
  bin3 = intarr(N_ELEMENTS(window_index))
  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    bin3[i] = self.get_header_info('NBIN3', window_index[i])
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
  ;returns 1 if data object contains one or two dumbbells, or if window_index is dumbbell
  COMPILE_OPT IDL2

  FOR i=0,N_ELEMENTS(window_index)-1 DO BEGIN
    IF self.dumbbells[0] EQ window_index[i] || self.dumbbells[1] EQ window_index[i] THEN BEGIN
      return, 1
    ENDIF
  ENDFOR
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
  mreadfits_header, file, hdr, extension=0, only_tags='NWIN'
  self.nwin = hdr.nwin

  ; find location of line windows in fits file
  openr, file_lun, file, /swap_if_little_endian, /get_lun
  self.file_lun = file_lun
  position = iris_find_winpos(file_lun, self.nwin-1)
  assocs = ptrarr(self.nwin)
  headers = ptrarr(self.nwin)
  headers_string = ptrarr(self.nwin)
  wcs = ptrarr(self.nwin)
  dumbbells = bytarr(self.nwin)
  FOR iwin = 0, self.nwin-1 DO BEGIN
    hdr = headfits(file, exten=iwin)
    headers_string[iwin] = ptr_new(hdr)
    hdr = fitshead2struct(hdr)
    headers[iwin] = ptr_new(hdr)
    wcs[iwin] = ptr_new(fitshead2wcs(hdr))
    IF hdr.DUMBBELL EQ 1 THEN self.dumbbells[0] = iwin $
    ELSE IF hdr.DUMBBELL EQ 2 THEN self.dumbbells[1] = iwin

    CASE hdr.BITPIX OF
      16: assocs[iwin] = ptr_new(assoc(file_lun, intarr(hdr.NAXIS1, hdr.NAXIS2, hdr.NAXIS3, hdr.NAXIS4, /NOZERO), position[iwin]))
      -32: assocs[iwin] = ptr_new(assoc(file_lun, fltarr(hdr.NAXIS1, hdr.NAXIS2, hdr.NAXIS3, hdr.NAXIS4, /NOZERO), position[iwin]))
      ELSE: message,'unsupported datatype ' + strtrim(string(hdr.BITPIX), 2)
    ENDCASE

  ENDFOR ; iwin = 0, self.nwin-1
  self.window_assoc = ptr_new(assocs)
  self.window_data = ptr_new(ptrarr(self.nwin))
  self.window_descaled = ptr_new(bytarr(self.nwin))
  self.window_headers = ptr_new(headers)
  self.window_headers_string = ptr_new(headers_string)
  self.window_wcs = ptr_new(wcs)
END


;+
; Description:
;     Returns the input filename
;
; OUTPUT:
;     string
;-
FUNCTION spice_data::get_filename
  ;returns the input filename
  COMPILE_OPT IDL2

  return, self.file
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
    window_assoc: ptr_new(), $  ; pointers to window data in the file using assoc (ptrarr)
    window_data: ptr_new(), $   ; loaded window data (ptrarr)
    window_descaled: ptr_new(), $; indicates for each window, whether data was loaded, 0:no, 1:yes, descaled, 2: yes, not descaled (bytarr)
    window_headers: ptr_new(), $; a pointer array, each pointing to a header structure of one window
    window_headers_string: ptr_new(), $; a pointer array, each pointing to a header string array of one window
    window_wcs: ptr_new(), $    ; pointers to wcs structure for each window
    dumbbells: [-1, -1], $      ; contains the index of the window with [lower, upper] dumbbell
    file_lun: 0}                ; Logical Unit Number of the file
END
