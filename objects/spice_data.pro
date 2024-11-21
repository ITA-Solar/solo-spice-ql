;+
; NAME:
;     SPICE_DATA
;
; PURPOSE:
;     spice_data returns a 'spice_data' or a 'spice_data_l3' object.
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     obj = spice_data(file)
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUTS:
;     Object of type SPICE_DATA or SPICE_DATA_L3 which describes and contains
;     a SPICE raster.
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;     The function checks whether 'file' is a string ending with '.fits' and if it is a SPICE
;     file. If yes, it creates either a SPICE_DATA object, if it is a level 2 file, or a
;     SPICE_DATA_L3 object, if it is a level 3 file.
;
;     This function exists for the user's
;     convenience and simplifies the creation of a new object. See spice_data__define.pro and spice_data_l3__define.pro
;     for more information on the SPICE_DATA and the SPICE_DATA_L3 object.
;
; RESTRICTIONS:
;
; HISTORY:
;     27-Nov-2019: Martin Wiesmann
;-
; $Id: 2024-11-21 13:17 CET $

FUNCTION spice_data, file
  COMPILE_OPT IDL2

  IF n_elements(file) NE 1 || $
    size(file, /TYPE) NE 7 || $
    ~strmatch(file, '*.fits', /fold_case) THEN BEGIN
    print, 'File input must be a scalar string ending with .fits'
    return, 0
  ENDIF ELSE BEGIN
    file_info = spice_file2info(file)
    IF ~file_info.is_spice_file THEN BEGIN
      print, 'File is not a SPICE file: ' + file
      return, 0
    ENDIF
    CASE file_info.level OF
      2: return, obj_new('spice_data', file)
      3: return, obj_new('spice_data_l3', file)
      ELSE: BEGIN
        print, 'No IDL object defined for this data level: ' + string(file_info.level)
        return, 0
      END
    ENDCASE
  ENDELSE
END