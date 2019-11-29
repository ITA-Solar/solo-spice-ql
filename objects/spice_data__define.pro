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
;     verbose : if set, the initiation of the object prints out some information
;
; OUTPUT:
;     Objects of type SPICE_DATA which describes and contains
;     a SPICE raster
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
  ptr_free, self.file_position
  IF self.file_lun GE 100 && self.file_lun LE 128 THEN free_lun, self.file_lun
  self.dumbbells = [-1, -1]
  self.nwin = 0
end


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
pro spice_data::help, description=description
  ;Prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  if arg_present(description) || keyword_set(description) then $
    obj_help, self, description=description $
  else $
    obj_help, self
end


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
  self.file_position = ptr_new(position)
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
      ELSE: message,'unsupported datatype '+self->getdatatype()
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
    file_lun: 0, $              ; Logical Unit Number of the file
    file_position: ptr_new()}   ; positions within the file where data block starts for each extension in bytes (lon64arr)
END
