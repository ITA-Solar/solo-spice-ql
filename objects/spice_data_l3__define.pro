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
;-
; $Id: 2023-06-16 10:04 CEST $


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

  return, 1
END


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro spice_data_l3::cleanup
  COMPILE_OPT IDL2

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
    rasterno:-1 $         ; raster repetition number
  }
END
