;+
; NAME:
;     SPICE_CREATE_L3_PROGRESS__DEFINE
;
; PURPOSE:
;     SPICE_CREATE_L3_PROGRESS__DEFINE defines the class structure 'spice_create_l3_progress'.
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     The SPICE_CREATE_L3_PROGRESS__DEFINE procedure is not called directly. An
;     object of class SPICE_CREATE_L3_PROGRESS is created with the following
;     statement:
;                 spice_create_l3_progress = spice_create_l3_progress(input...)
;
; INPUTS:
;
; OUTPUT:
;     Object of type SPICE_CREATE_L3_PROGRESS which ...
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     06-Dec-2022: Martin Wiesmann, UIO, ITA.
;-
; $Id: 2022-12-06 14:56 CET $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise
;-
FUNCTION spice_create_l3_progress::init, n_files, i_file
  COMPILE_OPT IDL2

  prits_tools.parcheck, n_files, 1, "n_files", 'INTEGERS', 0, MINVAL=1

  self.n_files = n_files
  
  base = widget_base(/column, title='Progress of creation of SPICE level 3 files')
END


prits_tools.parcheck, i_file, 2, "i_file", 'INTEGERS', 0, MINVAL=0, MAXVAL=n_files-1


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro spice_create_l3_progress::cleanup
  COMPILE_OPT IDL2

  print, 'cleanup'
END


;+
; Description:
;     Class definition procedure
;-
PRO spice_create_l3_progress__define
  COMPILE_OPT IDL2

  struct = {spice_create_l3_progress, $
    n_files: 0, $                 ; number of files to be processed
    i_file: 0, $                ; index of current file beginning at zero
    filename: '', $          ; name of current file
    n_windows: 1, $                  ; number of windows in current file
    i_window: 0, $                  ; index of current window beginning at zero
    window_name: '' $   ; name of current window
    }
END
