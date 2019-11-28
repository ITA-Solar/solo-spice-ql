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
  ;prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  if arg_present(description) || keyword_set(description) then $
    obj_help, self, description=description $
  else $
    obj_help, self
end


;---------------------------------------------------------
; I/O and related methods for loading data
;---------------------------------------------------------


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; KEYWORD PARAMETERS:
;     verbose : if set, the initiation of the object prints out some information
;-
PRO spice_data::read_file, file, verbose=verbose
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
stop
  FOR iwin = 0, self.nwin-1 DO BEGIN
    mrd_head, file, hdr, extension=iwin
      CASE fxpar(hdr,'BITPIX') OF
        16: self.w[iwin] = $
          ptr_new(assoc(file_lun, intarr(self->getxw(iwin), self->getyw(iwin)), position[iwin]))
        -32: self.w[iwin] = $
          ptr_new(assoc(file_lun, fltarr(self->getxw(iwin), self->getyw(iwin)), position[iwin]))
        ELSE: message,'unsupported datatype '+self->getdatatype()
      ENDCASE

  ENDFOR ; iwin = 0, self.nwin-1
  

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
    file_lun: 0, $              ; Logical Unit Number of the file
    file_position: ptr_new()}   ; positions within the file where data block starts for each extension in bytes (lon64arr)
END
