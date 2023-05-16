;+
; NAME:
;     WIDGET_POSITIONER
;
; PURPOSE:
;     This object can be used to position a new widget relative to another widget,
;     or relative to the screen. This also works with multiple screens. The widget
;     is positioned and realised by this object.
;
; CATEGORY:
;     PRITS - Tools.
;
; CALLING SEQUENCE:
; parent = widget_base(/column, title='Parent Widget', xsize=300, ysize=400)
; widget_control, parent, /realize
;
; new_window = widget_base(/row, title='New Widget', xsize=400, ysize=300, group_leader=parent)
;
; wp = widget_positioner(new_window, parent=parent)
; wp->position, xoffset=xoffset, yoffset=yoffset
;
; widget_control, new_window, /realize  ; Alternatively the new widget may be also realized before calling widget_positioner->position
;                                       ; see also Restrictions
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
; If the widget_base is initialised with some x/yoffset values, then repositioning only works after
; the widget was realised.
;
; HISTORY:
;     11-May-2023: Martin Wiesmann
;-
; $Id: 2023-05-16 13:44 CEST $


;+
; Description:
;     Class initialisation function
;
; INPUTS:
;     widget : The ID of the widget to be positioned. Can also be set later with method 'set_widget'.
;     parent : The ID of the parent widget to which the widget should get a certain position.
;              Optional, but if provided then the parent widget must be realised already.
;              If not provided the widget will be positioned relative to upper left corner of display.
;              Can also be set later with method 'set_parent'.
;
; OUTPUT:
;     1 (True) if initialization succeeded, 0 (False) otherwise
;-
FUNCTION widget_positioner::init, widget, parent=parent
  COMPILE_OPT IDL2

  prits_tools.parcheck, widget, 1, "widget", ['integers'], 0, default=-1
  prits_tools.parcheck, parent, 0, "parent", ['integers'], 0, default=-1
  self.widget = widget
  self.parent = parent
  self.monitor =  obj_new('IDLsysMonitorInfo')

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
PRO widget_positioner::help, description=description, _extra=_extra
  ;Prints out this help, setting the 'description' keyword will also print the header info
  COMPILE_OPT IDL2

  IF arg_present(description) || keyword_set(description) THEN $
    obj_help, self, description=description, _extra=_extra $
  ELSE $
    obj_help, self, _extra=_extra
END


;+
; Description:
;     This procedure calculates where the new widget should be placed and checks, whether the
;     whole widget is within the display. If this is not the case it alters the new position,
;     so that it is fully, if possible, within the display. It also sets the new offset to the
;     widget and realises the widget then.
;
; INPUTS:
;     xoffset : Optional. The offset in x-direction relativ to the parent widget or the display.
;               Default=50 pixels.
;     yoffset : Optional. The offset in y-direction relativ to the parent widget or the display.
;               Default=50 pixels.
;-
PRO widget_positioner::position, xoffset=xoffset, yoffset=yoffset
  ;Positions the widget relative to parent or screen if no parent given
  COMPILE_OPT IDL2

  prits_tools.parcheck, xoffset, 0, "xoffset", ['numeric'], 0, default=50
  prits_tools.parcheck, yoffset, 0, "yoffset", ['numeric'], 0, default=50

  IF self.widget LT 0 THEN BEGIN
    message, 'No widget provided. Doing nothing.', /informational
    return
  ENDIF
  display_coord = self.get_display_coords(offset_parent=offset_parent, offset_widget=[xoffset, yoffset])

  geometry = widget_info(self.widget, /geometry)
  xsize = geometry.SCR_XSIZE + (2* geometry.MARGIN)
  IF xsize GT display_coord[2] THEN message, 'Widget is too wide for the screen', /informational
  ysize = geometry.SCR_YSIZE + (2* geometry.MARGIN)
  IF ysize GT display_coord[3] THEN message, 'Widget is too high for the screen', /informational

  xoffset_new = offset_parent[0] + xoffset
  IF xoffset_new LT display_coord[0] THEN xoffset_new = display_coord[0]
  x2_edge = xoffset_new + xsize
  IF x2_edge GT display_coord[2] THEN BEGIN
    move_dist = x2_edge - display_coord[2]
    IF move_dist GT xoffset_new-display_coord[0] THEN BEGIN
      xoffset_new = display_coord[0]
    ENDIF ELSE BEGIN
      xoffset_new = xoffset_new - move_dist
    ENDELSE
  ENDIF

  yoffset_new = offset_parent[1] + yoffset
  IF yoffset_new LT display_coord[1] THEN yoffset_new = display_coord[1]
  y2_edge = yoffset_new + ysize
  IF y2_edge GT display_coord[3] THEN BEGIN
    move_dist = y2_edge - display_coord[3]
    IF move_dist GT yoffset_new-display_coord[1] THEN BEGIN
      yoffset_new = display_coord[1]
    ENDIF ELSE BEGIN
      yoffset_new = yoffset_new - move_dist
    ENDELSE
  ENDIF
  widget_control, self.widget, map=0
  widget_control, self.widget, /realize
  widget_control, self.widget, TLB_SET_XOFFSET=xoffset_new, TLB_SET_YOFFSET=yoffset_new
  widget_control, self.widget, map=1
END


FUNCTION widget_positioner::get_display_coords, offset_parent=offset_parent, offset_widget=offset_widget
  COMPILE_OPT IDL2
  IF self.parent GE 0 THEN BEGIN
    widget_control, self.parent, TLB_GET_OFFSET=offset_parent
  ENDIF ELSE BEGIN
    offset_parent = offset_widget
  ENDELSE
  rectangles = self.monitor->GetRectangles()
  rectangles[2,*] = rectangles[0,*] + rectangles[2,*]
  rectangles[3,*] = rectangles[1,*] + rectangles[3,*]
  ind_display = where(rectangles[0,*] LE offset_parent[0] AND rectangles[2,*] GT offset_parent[0] AND $
    rectangles[1,*] LE offset_parent[1] AND rectangles[3,*] GT offset_parent[1])
  IF self.parent LT 0 THEN BEGIN
    offset_parent = [0, 0]
  ENDIF
  return, rectangles[*,ind_display]
END


PRO widget_positioner::set_widget, widget
  ;Sets a new widget that can be positioned.
  COMPILE_OPT IDL2

  prits_tools.parcheck, widget, 1, "widget", ['integers'], 0, minval=0
  self.widget = widget
END


FUNCTION widget_positioner::get_widget
  ;Gets the widget that can be positioned.
  COMPILE_OPT IDL2

  return, self.widget
END


PRO widget_positioner::set_parent, parent
  ;Sets a new parent widget to which the widget should be relatively positioned. Set to a negative value to remove parent.
  COMPILE_OPT IDL2

  prits_tools.parcheck, parent, 1, "parent", ['integers'], 0
  self.parent = parent
END


FUNCTION widget_positioner::get_parent
  ;Gets the parent widget to which the widget should be relatively positioned.
  COMPILE_OPT IDL2

  return, self.parent
END


;+
; Description:
;     called by obj_destroy, frees all pointers and closes all associated files
;-
PRO widget_positioner::cleanup
  COMPILE_OPT IDL2

END


;+
; Description:
;     Class definition procedure
;-
PRO widget_positioner__define
  COMPILE_OPT IDL2

  struct = {widget_positioner, $
    widget: -1, $         ; The ID of the widget to be positioned.
    parent: -1, $         ; The ID of the parent widget to which the widget should get a certain position.
    monitor: obj_new('IDLsysMonitorInfo') $   ; An object containing information about the displays.
  }
END

