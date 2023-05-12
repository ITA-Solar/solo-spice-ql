
;+
; NAME:
;     WIDGET_POSITIONER
;
; PURPOSE:
;     pwidget_positioner__define
;
; CATEGORY:
;     PRITS - Tools.
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OUTPUT:
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
;     11-May-2023: Martin Wiesmann
;-
; $Id: 2023-05-12 10:58 CEST $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     widget : The ID of the widget to be positioned. Can also be set later.
;              The widget must be realised already.
;     parent : The ID of the parent widget to which the widget should get a certain position.
;              Can also be set later. The parent widget must be realised already.
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise
;-
FUNCTION widget_positioner::init, widget, parent
  COMPILE_OPT IDL2

  prits_tools.parcheck, widget, 1, "widget", ['integers'], 0, default=-1
  prits_tools.parcheck, parent, 2, "parent", ['integers'], 0, default=-1
  self.widget = widget
  self.parent = parent
  
  return, 1
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
pro widget_positioner::help, description=description, _extra=_extra
  ;Prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  IF arg_present(description) || keyword_set(description) THEN $
    obj_help, self, description=description, _extra=_extra $
  ELSE $
    obj_help, self, _extra=_extra
END


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
pro widget_positioner::cleanup
  COMPILE_OPT IDL2

END


;+
; Description:
;     Class definition procedure
;-
PRO widget_positioner__define
  COMPILE_OPT IDL2

  struct = {prits_tools__widget_positioner, $
    widget: -1, $     ; The ID of the widget to be positioned.
    parent: -1 $      ; The ID of the parent widget to which the widget should get a certain position.
    }
END

