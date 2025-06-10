;+
; NAME:
;     WIDGET_POSITIONER_TEST
;
; PURPOSE:
;     This procedure tests the class widget_positioner
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
; $Id: 2025-06-10 13:51 CEST $

PRO widget_position_test
  parent = widget_base(/column, title = 'Parent Widget', xsize = 300, ysize = 400)
  widget_control, parent, /realize

  new_window = widget_base(/row, title = 'New Widget', xsize = 400, ysize = 300, group_leader = parent)

  print, spice_get_screen_size()

  monitor = obj_new('IDLsysMonitorInfo')
  print, 'IDLsysMonitorInfo::GetMonitorNames   ', monitor.GetMonitorNames()
  print, 'IDLsysMonitorInfo::GetNumberOfMonitors   ', monitor.GetNumberOfMonitors()
  print, 'IDLsysMonitorInfo::GetPrimaryMonitorIndex   ', monitor.GetPrimaryMonitorIndex()
  print, 'IDLsysMonitorInfo::GetRectangles   ', monitor.GetRectangles()
  print, 'IDLsysMonitorInfo::GetResolutions   ', monitor.GetResolutions()
  print, 'IDLsysMonitorInfo::IsExtendedDesktop   ', monitor.IsExtendedDesktop()

  print, ''
  print, 'test 1'
  wait, 5
  widget_position, new_window, parent = parent
  widget_control, new_window, /realize
  wait, 3

  new_window = widget_base(/row, title = 'New Widget 2', xsize = 500, ysize = 500, group_leader = parent) ; , xoffset=50, yoffset=50)
  print, ''
  print, 'test 1 b'
  widget_position, new_window, parent = parent, xoffset = 200, yoffset = 300
  widget_control, new_window, /realize
  wait, 3
  print, 'test 1 c'
  widget_position, new_window, parent = parent, xoffset = 2000, yoffset = 800
  wait, 3

  print, ''
  print, 'test 2'
  widget_control, parent, TLB_SET_XOFFSET = 1000, TLB_SET_YOFFSET = 600
  widget_position, new_window, parent = parent
  wait, 3

  print, ''
  print, 'test 3'
  widget_control, parent, TLB_SET_XOFFSET = 3000, TLB_SET_YOFFSET = 600
  widget_position, new_window, parent = parent
  print, 'move parent window manually'
  wait, 6

  print, ''
  print, 'test 4'
  widget_position, new_window, parent = parent
  wait, 5

  print, ''
  print, 'test 5'
  widget_control, parent, TLB_SET_XOFFSET = 3900, TLB_SET_YOFFSET = 600
  widget_position, new_window, parent = parent, xoffset = -90, yoffset = -200
  wait, 3

  widget_control, parent, /destroy

  new_window = widget_base(/row, title = 'New Widget', xsize = 4000, ysize = 300)
  print, ''
  print, 'test 6'
  widget_position, new_window, yoffset = 200
  widget_control, new_window, /realize
  wait, 3

  widget_control, new_window, /destroy

  parent = widget_base(/column, title = 'Parent Widget', xsize = 300, ysize = 400)
  widget_control, parent, /realize

  new_window = widget_base(/row, title = 'New Widget', xsize = 400, ysize = 300, group_leader = parent)

  print, spice_get_screen_size()

  monitor = obj_new('IDLsysMonitorInfo')
  print, 'IDLsysMonitorInfo::GetMonitorNames   ', monitor.GetMonitorNames()
  print, 'IDLsysMonitorInfo::GetNumberOfMonitors   ', monitor.GetNumberOfMonitors()
  print, 'IDLsysMonitorInfo::GetPrimaryMonitorIndex   ', monitor.GetPrimaryMonitorIndex()
  print, 'IDLsysMonitorInfo::GetRectangles   ', monitor.GetRectangles()
  print, 'IDLsysMonitorInfo::GetResolutions   ', monitor.GetResolutions()
  print, 'IDLsysMonitorInfo::IsExtendedDesktop   ', monitor.IsExtendedDesktop()

  print, ''
  print, 'test 7'
  widget_position, new_window, parent = parent, xoffset = 2000
  widget_control, new_window, /realize

  wait, 3

  ; test alignment
  widget_control, parent, TLB_SET_XOFFSET = 1000, TLB_SET_YOFFSET = 600
  print, ''
  print, 'test 8  --  LEFT'
  widget_position, new_window, parent = parent, /left_align
  wait, 3

  print, ''
  print, 'test 9  --  RIGHT'
  widget_position, new_window, parent = parent, /right_align
  wait, 3

  print, ''
  print, 'test 10  --  TOP'
  widget_position, new_window, parent = parent, /top_align
  wait, 3

  print, ''
  print, 'test 11  --  BOTTOM'
  widget_position, new_window, parent = parent, /bottom_align
  wait, 3

  print, ''
  print, 'test 12  --  BOTTOM LEFT'
  widget_position, new_window, parent = parent, /bottom_align, /left_align
  wait, 3

  print, ''
  print, 'test 12  --  TOP RIGHT'
  widget_position, new_window, parent = parent, /top_align, /right_align
  wait, 3

  widget_control, parent, /destroy
END
