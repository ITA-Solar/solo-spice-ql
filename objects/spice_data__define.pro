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
; :Description:
;     Class definition procedure
;-
PRO spice_data__define
  struct = {spice_data, $
    title:'', $
    nwin:0}
END

FUNCTION iris_data::init, file, verbose=verbose
  ; initializes object, calls read if "file" parameter given
  self.title='SPICE'
  IF n_elements(file) NE 0 THEN BEGIN
    self->read, file, verbose=verbose
  ENDIF
  return, 1
END

pro iris_data::close
  ; frees pointer to main data array "w" and closes all associated files
  for i=0,self.nwin-1 do begin
    if ptr_valid(self.w[i]) then ptr_free,self.w[i]
  endfor
  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
  for lwin=0,n_elements(self.lusji)-1 do begin
    if self.lusji[lwin] ge 100 and self.lusji[lwin] le 128 then free_lun,self.lusji[lwin]
  endfor
end

pro iris_data::cleanup
  ; called by obj_destroy, frees all pointers and closes all associated files
  if ptr_valid(self.aux) then begin
    obj_destroy,*self.aux
    ptr_free,self.aux
  endif
  if ptr_valid(self.cal) then begin
    obj_destroy,*self.cal
    ptr_free,self.cal
  endif
  for i=0,self.nwin do begin
    ptr_free,self.hdr[i]
  endfor
  self->close
  for i=0,self.nfiles-1 do begin
    ptr_free,self.aux_info[i].time
    ptr_free,self.aux_info[i].pztx
    ptr_free,self.aux_info[i].pzty
    ptr_free,self.aux_info[i].exptimef
    ptr_free,self.aux_info[i].exptimen
    ptr_free,self.aux_info[i].sumsptrf
    ptr_free,self.aux_info[i].sumsptrn
    ptr_free,self.aux_info[i].sumspatf
    ptr_free,self.aux_info[i].sumspatn
    ptr_free,self.aux_info[i].dsrcf
    ptr_free,self.aux_info[i].dsrcn
    ptr_free,self.aux_info[i].lutidf
    ptr_free,self.aux_info[i].lutidn
    ptr_free,self.aux_info[i].xcen
    ptr_free,self.aux_info[i].ycen
    ptr_free,self.aux_info[i].obs_vr
    ptr_free,self.aux_info[i].ophase
    ;
    ptr_free,self.obs_info[i].frmid
    ptr_free,self.obs_info[i].fdbidf
    ptr_free,self.obs_info[i].fdbidn
    ptr_free,self.obs_info[i].crsidf
    ptr_free,self.obs_info[i].crsidn
    ptr_free,self.obs_info[i].filef
    ptr_free,self.obs_info[i].filen
  endfor
  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
  return
end

pro iris_data::help, description=description
  ;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end
