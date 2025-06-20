;+
; NAME:
;     WIDGET_POSITION
;
; PURPOSE:
;     This procedure can be used to position a new widget relative to another widget,
;     or relative to the screen. This also works with multiple screens. The widget
;     is positioned and realised by this object.
;
;     This procedure calculates where the new widget should be placed and checks, whether the
;     whole widget is within the display. If this is not the case it alters the new position,
;     so that it is fully, if possible, within the display. It also sets the new offset to the
;     widget and realises the widget then.
;
;     Note, this is similar to the xrealize procedure, but more sophisticated.
;
; CATEGORY:
;     WIDGETS - Tools.
;
; CALLING SEQUENCE:
; parent = widget_base(/column, title='Parent Widget', xsize=300, ysize=400)
; widget_control, parent, /realize
; new_window = widget_base(/row, title='New Widget', xsize=400, ysize=300, group_leader=parent)
; widget_position, new_window, parent=parent, xoffset=xoffset, yoffset=yoffset
;
; INPUTS:
;     xoffset : Optional. The offset in x-direction relativ to the parent widget or the display.
;               Default=50 pixels. Overwritten if left_align or right_align is set.
;     yoffset : Optional. The offset in y-direction relativ to the parent widget or the display.
;               Default=50 pixels. Overwritten if top_align or bottom_align is set.
;     n_subplot: Optional. Number of child widgets of the same parent already shifted. Each widget
;               is positioned in a slightly different position.
;
; KEYWORDS:
;     left_align : If set, the widget will be positioned to the left of the parent, if there is
;               enough space.
;     right_align : If set, the widget will be positioned to the right of the parent, if there is
;               enough space. Ignored if left_align is set.
;     top_align : If set, the widget will be positioned to the top of the parent, if there is
;               enough space.
;     bottom_align : If set, the widget will be positioned to the bottom of the parent, if there is
;               enough space. Ignored if top_align is set.
;     center : If set, the widget will be positioned in the center of the parent, or of the screen
;               if no parent is provided.
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
;     10-Jun-2025: Martin Wiesmann, Refactored code to be a procedure instead of a class method
;
; $Id: 2025-06-19 09:35 CEST $
;
;-

FUNCTION get_display_coords, parent, offset_parent = offset_parent, offset_widget = offset_widget
  COMPILE_OPT IDL2
  IF parent GE 0 THEN BEGIN
    widget_control, parent, TLB_GET_OFFSET = offset_parent
  ENDIF ELSE BEGIN
    offset_parent = offset_widget
  ENDELSE
  monitor = obj_new('IDLsysMonitorInfo')
  rectangles = monitor.GetRectangles()
  rectangles[2, *] = rectangles[0, *] + rectangles[2, *]
  rectangles[3, *] = rectangles[1, *] + rectangles[3, *]
  ind_display = where(rectangles[0, *] LE offset_parent[0] AND rectangles[2, *] GT offset_parent[0] AND $
    rectangles[1, *] LE offset_parent[1] AND rectangles[3, *] GT offset_parent[1])
  IF parent LT 0 THEN BEGIN
    offset_parent = [0, 0]
  ENDIF
  return, rectangles[*, ind_display]
END

PRO widget_position, widget, parent = parent, xoffset = xoffset, yoffset = yoffset, $
  left_align = left_align, right_align = right_align, top_align = top_align, bottom_align = bottom_align, $
  center = center, $
  n_subplot = n_subplot
  ; Positions the widget relative to parent or screen if no parent given
  COMPILE_OPT IDL2

  If n_elements(widget) EQ 0 THEN widget = -1
  if n_elements(parent) EQ 0 THEN parent = -1
  if n_elements(xoffset) EQ 0 then xoffset = 50
  if n_elements(yoffset) EQ 0 then yoffset = 50
  if n_elements(n_subplot) EQ 0 then n_subplot = 0

  IF widget LT 0 THEN BEGIN
    message, 'No widget provided. Doing nothing.', /informational
    return
  ENDIF
  display_coord = get_display_coords(parent, offset_parent = offset_parent, offset_widget = [xoffset, yoffset])

  IF parent GE 0 THEN geometry_parent = widget_info(parent, /geometry)
  geometry = widget_info(widget, /geometry)
  xsize = geometry.SCR_XSIZE + (2 * geometry.MARGIN)
  IF xsize GT display_coord[2] THEN message, 'Widget is too wide for the screen', /informational
  IF keyword_set(left_align) THEN BEGIN
    xoffset = -xsize
  ENDIF ELSE IF keyword_set(right_align) THEN BEGIN
    IF parent GE 0 THEN BEGIN
      xoffset = geometry_parent.SCR_XSIZE + (2 * geometry_parent.MARGIN)
    ENDIF ELSE BEGIN
      xoffset = display_coord[2]
    ENDELSE
  ENDIF ELSE IF keyword_set(center) THEN BEGIN
    IF parent GE 0 THEN BEGIN
      xoffset = (geometry_parent.SCR_XSIZE - geometry.SCR_XSIZE) / 2
    ENDIF ELSE BEGIN
      xoffset = (display_coord[2] - geometry.SCR_XSIZE) / 2
    ENDELSE
  ENDIF

  ysize = geometry.SCR_YSIZE + (2 * geometry.MARGIN)
  IF ysize GT display_coord[3] THEN message, 'Widget is too high for the screen', /informational
  IF keyword_set(top_align) THEN BEGIN
    yoffset = -ysize
  ENDIF ELSE IF keyword_set(bottom_align) THEN BEGIN
    IF parent GE 0 THEN BEGIN
      yoffset = geometry_parent.SCR_YSIZE + (2 * geometry_parent.MARGIN)
    ENDIF ELSE BEGIN
      yoffset = display_coord[3]
    ENDELSE
  ENDIF ELSE IF keyword_set(center) THEN BEGIN
    IF parent GE 0 THEN BEGIN
      yoffset = (geometry_parent.SCR_YSIZE - geometry.SCR_YSIZE) / 2
    ENDIF ELSE BEGIN
      yoffset = (display_coord[3] - geometry.SCR_YSIZE) / 2
    ENDELSE
  ENDIF

  IF ~keyword_set(left_align) && ~keyword_set(right_align) THEN xoffset = xoffset + n_subplot * 20
  IF ~keyword_set(top_align) && ~keyword_set(bottom_align) THEN yoffset = yoffset + n_subplot * 20

  xoffset_new = offset_parent[0] + xoffset
  IF xoffset_new LT display_coord[0] THEN xoffset_new = display_coord[0]
  x2_edge = xoffset_new + xsize
  IF x2_edge GT display_coord[2] THEN BEGIN
    move_dist = x2_edge - display_coord[2]
    IF move_dist GT xoffset_new - display_coord[0] THEN BEGIN
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
    IF move_dist GT yoffset_new - display_coord[1] THEN BEGIN
      yoffset_new = display_coord[1]
    ENDIF ELSE BEGIN
      yoffset_new = yoffset_new - move_dist
    ENDELSE
  ENDIF
  widget_control, widget, map = 0
  widget_control, widget, /realize
  widget_control, widget, TLB_SET_XOFFSET = xoffset_new, TLB_SET_YOFFSET = yoffset_new
  widget_control, widget, map = 1
END
