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
; OUTPUTS:
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
;     verbose : if set, the initiation of the object prints out some information
;
; OUTPUTS:
;     1 (True) if initialization succeeded, 0 (False) otherwise (not implemented)
;-
FUNCTION spice_data::init, file, verbose=verbose
  COMPILE_OPT IDL2

  self.title='SPICE'
  IF n_elements(file) NE 0 THEN BEGIN
    self->read_file, file, verbose=verbose
  ENDIF
  return, 1
END


;+
; Description:
;     frees pointer to main data array "w" and closes all associated files.
;     used by cleanup and if object should be populated with new data
;-
pro spice_data::close
  COMPILE_OPT IDL2

  ;  for i=0,self.nwin-1 do begin
  ;    if ptr_valid(self.w[i]) then ptr_free,self.w[i]
  ;  endfor
  ;  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
  ;  for lwin=0,n_elements(self.lusji)-1 do begin
  ;    if self.lusji[lwin] ge 100 and self.lusji[lwin] le 128 then free_lun,self.lusji[lwin]
  ;  endfor
end


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro spice_data::cleanup
  COMPILE_OPT IDL2

  ;  if ptr_valid(self.aux) then begin
  ;    obj_destroy,*self.aux
  ;    ptr_free,self.aux
  ;  endif
  ;  if ptr_valid(self.cal) then begin
  ;    obj_destroy,*self.cal
  ;    ptr_free,self.cal
  ;  endif
  ;  for i=0,self.nwin do begin
  ;    ptr_free,self.hdr[i]
  ;  endfor
  ;  self->close
  ;  for i=0,self.nfiles-1 do begin
  ;  ptr_free,self.aux_info[i].time
  ;  ptr_free,self.aux_info[i].pztx
  ;  ptr_free,self.aux_info[i].pzty
  ;  ptr_free,self.aux_info[i].exptimef
  ;  ptr_free,self.aux_info[i].exptimen
  ;  ptr_free,self.aux_info[i].sumsptrf
  ;  ptr_free,self.aux_info[i].sumsptrn
  ;  ptr_free,self.aux_info[i].sumspatf
  ;  ptr_free,self.aux_info[i].sumspatn
  ;  ptr_free,self.aux_info[i].dsrcf
  ;  ptr_free,self.aux_info[i].dsrcn
  ;  ptr_free,self.aux_info[i].lutidf
  ;  ptr_free,self.aux_info[i].lutidn
  ;  ptr_free,self.aux_info[i].xcen
  ;  ptr_free,self.aux_info[i].ycen
  ;  ptr_free,self.aux_info[i].obs_vr
  ;  ptr_free,self.aux_info[i].ophase
  ;  ;
  ;  ptr_free,self.obs_info[i].frmid
  ;  ptr_free,self.obs_info[i].fdbidf
  ;  ptr_free,self.obs_info[i].fdbidn
  ;  ptr_free,self.obs_info[i].crsidf
  ;  ptr_free,self.obs_info[i].crsidn
  ;  ptr_free,self.obs_info[i].filef
  ;    ptr_free,self.obs_info[i].filen
  ;  endfor
  ;  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
end


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
;     window : the index of the desired window
;
; KEYWORD PARAMETERS:
;     load : if set, the data is read from the file and returned as an array
;     noscale : if set, does not call descale_array, ignored if 'load' is not set
;
; OUTPUTS:
;     returns either a link to the data, or the array itself
;-
FUNCTION spice_data::get_window_data, window, load=load, noscale=noscale
  ;Returns a link to the data of window, or the data itself if keyword load is set
  COMPILE_OPT IDL2

  IF keyword_set(load) THEN BEGIN
    IF keyword_set(noscale) THEN BEGIN
      data = (*(*self.window_data)[window])[0]
    ENDIF ELSE BEGIN
      data = self.descale_array((*(*self.window_data)[window])[0], window)
    ENDELSE
  ENDIF ELSE BEGIN
    data = *(*self.window_data)[window]        
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
;     window : the index of the window this array belongs to
;
; OUTPUTS:
;     returns the descaled array (=array * bscale + bzero)
;-
FUNCTION spice_data::descale_array, array, window
  ;Descales the array, using BSCALE and BZERO keywords in the header
  COMPILE_OPT IDL2

  bscale = (*(*self.window_headers)[window]).BSCALE
  bzero = (*(*self.window_headers)[window]).BZERO
  return, array * bscale + bzero
END


;+
; Description:
;     Returns the specified keyword from the extension 'window', if the keyword
;     does not exist 'missing_value' is returned if it is provided, !NULL otherwise.
;
; INPUTS:
;     keyword : string, the header keyword to be returned
;     window : the index of the window this keyword belongs to
;     
; OPTIONAL INPUTS:
;     missing_value : the value that should be returned, if the keyword does not exist
;                     if this is not provided !NULL is returned
;                     
; OPTIONAL OUTPUT:
;     exists : boolean, True if keyword exists
;
; OUTPUTS:
;     returns the descaled array (=array * bscale + bzero)
;-
FUNCTION spice_data::get_header_info, keyword, window, missing_value, exists=exists
  ;Returns the specified keyword from the window, or 'missing_value' if provided, !NULL otherwise
  COMPILE_OPT IDL2
  
  exists = TAG_EXIST(*(*self.window_headers)[window], keyword, index=index) 
  IF exists THEN BEGIN
    return, (*(*self.window_headers)[window]).(index)
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(missing_value) EQ 0 THEN return, missing_value $
    ELSE return, !NULL
  ENDELSE

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
;     verbose : if set, the initiation of the object prints out some information
;-
PRO spice_data::read_file, file, verbose=verbose
  ;Reads a file, overwrites any existing data in this object.
  COMPILE_OPT IDL2

  IF n_elements(file) EQ 0 THEN BEGIN
    message, 'spice_data->read_file, file [, verbose=verbose]', /info
    return
  ENDIF
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
