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
;     n_files: The number of files to be processed. This number is ignored if 'FILES' is provided, but
;             required if 'FILES' is not provided.
;     files: An array of the names of the files to be processed. Preferably with the full path. 
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
; $Id: 2022-12-08 13:51 CET $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     n_files: (Possibly optional) The number of files to be processed. This number is ignored if 'FILES' is provided, but
;             required if 'FILES' is not provided.
;     files: Optional. An array of the names of the files to be processed. Preferably with the full path.
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise.
;-
FUNCTION spice_create_l3_progress::init, n_files, files=files
  COMPILE_OPT IDL2

  prits_tools.parcheck, files, 0, "files", 'string', [0, 1], /optional
  n_files_in = N_ELEMENTS(files)
  prits_tools.parcheck, n_files, 1, "n_files", 'INTEGERS', 0, MINVAL=1, optional=n_files_in

  IF n_files_in GT 0 THEN BEGIN
    self.n_files = n_files_in
    self.files_given = 1
  ENDIF ELSE BEGIN
    self.n_files = n_files
    self.files_given = 0
    files = strarr(n_files)
    for i=0,n_files-1 do files[i]=fns('File ######', i)
  ENDELSE
  self.files = ptr_new(files)
  temp = strlen(strtrim(self.n_files, 2))
  self.n_files_format = '(I' + strtrim(temp, 2) + ')'
  self.n_windows_format = '(I2)'
  self.i_file = -1

  self.base = widget_base(/column, title='Progress of creation of SPICE level 3 files')
  self.slider_total = widget_slider(self.base, xsize=400, minimum=0, maximum=100, $
    title='Percent done over all files')
  self.slider_file = widget_slider(self.base, xsize=400, minimum=0, maximum=100, $
    title='Percent done within the current file')

  self.label_current_file_num = widget_label(self.base, value='File '+string(1, format=self.n_files_format)+ $
    ' of '+string(1, format=self.n_files_format))
  self.label_current_path = cw_field(self.base, title='Current path  ', /NoEdit, xsize=75)
  self.label_current_file = cw_field(self.base, title='Current file  ', /NoEdit, xsize=75)
  label = widget_label(self.base, value=' ')

  self.label_current_window_num = widget_label(self.base, value='Window '+string(1, format=self.n_windows_format)+ $
    ' of '+string(1, format=self.n_windows_format))
  self.label_current_window = cw_field(self.base, title='Current window', /NoEdit, xsize=75)

  self.list_files = widget_list(self.base, value=file_basename(files), /frame, xsize=75, scr_ysize=200)
  self.stop_button = widget_button(self.base, value='Stop creation of level 3 files')
  xrealize, self.base

  return, 1
END


;+
; Description:
;     This procedure is called if the object is destroyed, it frees all pointers and kills all widgets.
;-
pro spice_create_l3_progress::cleanup
  COMPILE_OPT IDL2

  ptr_free, self.files
  xkill, self.base
END


;+
; Description:
;     This procedure sets the sliders and labels to the next file to be processed. The process first
;     checks whether the user clicked on the STOP button, and if yes, returns immediately without doing 
;     anything and setting 'halt' to 1.
;     If the last file has already been processed, the procedure issues a warning, but does otherwise nothing.
;
; INPUTS:
;     n_windows: The number of windows in the next file to be processed.
;     filename: Optional. The name and full path of the next file to be processed.
;               This input is ignored if a list of filenames has been provided when initialising the object.
;     window_name: Optional. The name of the first window of the next file.
;
; OUTPUT:
;     halt: Will be set to 1 if user clicked on 'STOP' button.
;-
pro spice_create_l3_progress::next_file, n_windows, filename=filename, window_name=window_name, halt=halt
  COMPILE_OPT IDL2

  prits_tools.parcheck, n_windows, 1, "n_windows", 'INTEGERS', 0, MINVAL=1
  prits_tools.parcheck, filename, 0, "filename", 'string', 0, /optional
  prits_tools.parcheck, window_name, 0, "window_name", 'string', 0, /optional

  event = widget_event(self.stop_button, /nowait)
  IF event.id NE 0L THEN BEGIN
    halt = 1
    return
  END
  halt = 0

  self.i_file = self.i_file + 1
  IF self.i_file EQ self.n_files THEN BEGIN
    message, 'All files processed already', /informational
    self.i_file = self.i_file - 1
    return
  ENDIF

  IF ~self.files_given && keyword_set(filename) THEN BEGIN
    (*self.files)[self.i_file] = filename
    widget_control, self.list_files, set_value=*self.files
  ENDIF
  self.filename = (*self.files)[self.i_file]

  self.i_window = 0
  IF ~keyword_set(window_name) THEN BEGIN
    self.window_name = fns('Window ##', self.i_window)
  ENDIF ELSE BEGIN
    self.window_name = window_name
  ENDELSE
  self.n_windows = n_windows

  widget_control, self.slider_total, set_value=float(self.i_file)/float(self.n_files)*100.0
  widget_control, self.slider_file, set_value=0
  widget_control, self.label_current_file_num, set_value='File '+string(self.i_file+1, format=self.n_files_format)+ $
    ' of '+string(self.n_files, format=self.n_files_format)
  widget_control, self.label_current_path, set_value=file_dirname(self.filename)
  widget_control, self.label_current_file, set_value=file_basename(self.filename)
  widget_control, self.label_current_window_num, set_value='Window '+string(self.i_window+1, format=self.n_windows_format)+ $
    ' of '+string(self.n_windows, format=self.n_windows_format)
  widget_control, self.label_current_window, set_value=self.window_name
end


;+
; Description:
;     This procedure sets the sliders and labels to the next window of the current file to be processed. The process first
;     checks whether the user clicked on the STOP button, and if yes, returns immediately without doing
;     anything and setting 'halt' to 1.
;     If the last window of the current file has already been processed, the procedure issues a warning, but does otherwise nothing.
;
; INPUTS:
;     window_name: Optional. The name of the first window of the next file.
;
; OUTPUT:
;     halt: Will be set to 1 if user clicked on 'STOP' button.
;-
pro spice_create_l3_progress::next_window, window_name=window_name, halt=halt
  COMPILE_OPT IDL2

  prits_tools.parcheck, window_name, 0, "window_name", 'string', 0, /optional

  event = widget_event(self.stop_button, /nowait)
  IF event.id NE 0L THEN BEGIN
    halt = 1
    return
  END
  halt = 0

  self.i_window = self.i_window + 1
  IF self.i_window EQ self.n_windows THEN BEGIN
    message, 'All windows of this file processed already', /informational
    self.i_window = self.i_window - 1
    return
  ENDIF

  IF ~keyword_set(window_name) THEN BEGIN
    self.window_name = fns('Window ##', self.i_window)
  ENDIF ELSE BEGIN
    self.window_name = window_name
  ENDELSE

  percent_file = float(self.i_file) / float(self.n_files) * 100.0
  percent_window = float(self.i_window) / float(self.n_windows)
  percent_total = percent_file + 1.0 / float(self.n_files) * percent_window * 100.0
  widget_control, self.slider_total, set_value=percent_total
  widget_control, self.slider_file, set_value=percent_window*100.0
  widget_control, self.label_current_window_num, set_value='Window '+string(self.i_window+1, format=self.n_windows_format)+ $
    ' of '+string(self.n_windows, format=self.n_windows_format)
  widget_control, self.label_current_window, set_value=self.window_name
end


;+
; Description:
;     Class definition procedure
;-
PRO spice_create_l3_progress__define
  COMPILE_OPT IDL2

  struct = {spice_create_l3_progress, $
    n_files: 1, $            ; number of files to be processed
    i_file: -1, $            ; index of current file beginning at zero
    files: ptr_new(), $      ; list of files to be processed
    files_given: 0b, $       ; indicates whether list of files was given at initialization
    n_files_format: '', $    ; format in which to display number of files
    filename: '', $          ; name of current file
    n_windows: 1, $          ; number of windows in current file
    i_window: -1, $          ; index of current window beginning at zero
    window_name: '', $       ; name of current window
    n_windows_format: '', $  ; format in which to display the number of windows
    base:0L, $
    slider_total:0L, $
    slider_file:0L, $
    label_current_file_num:0L, $
    label_current_path:0L, $
    label_current_file:0L, $
    label_current_window_num:0L, $
    label_current_window:0L, $
    list_files:0L, $
    stop_button:0L $
  }
END
