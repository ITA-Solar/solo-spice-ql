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
; $Id: 2023-05-15 11:32 CEST $


pro widget_positioner_test

  parent = widget_base(/column, title='Parent Widget', xsize=300, ysize=400)
  widget_control, parent, /realize
  
  new_window = widget_base(/row, title='New Widget', xsize=400, ysize=300, group_leader=parent)
  
  print,spice_get_screen_size()

  monitor =  obj_new('IDLsysMonitorInfo')
  print,'IDLsysMonitorInfo::GetMonitorNames   ' , monitor->GetMonitorNames()
  print,'IDLsysMonitorInfo::GetNumberOfMonitors   ' , monitor->GetNumberOfMonitors()
  print,'IDLsysMonitorInfo::GetPrimaryMonitorIndex   ' , monitor->GetPrimaryMonitorIndex()
  print,'IDLsysMonitorInfo::GetRectangles   ' , monitor->GetRectangles()
  print,'IDLsysMonitorInfo::GetResolutions   ' , monitor->GetResolutions()
  print,'IDLsysMonitorInfo::IsExtendedDesktop   ' , monitor->IsExtendedDesktop()
  
  wp = widget_positioner(new_window, parent=parent)
  
  print,''
  print,'test 1'
  wp->position
  widget_control, new_window, /realize
  wait,3
  

  new_window = widget_base(/row, title='New Widget 2', xsize=500, ysize=500, group_leader=parent);, xoffset=50, yoffset=50)
  wp = widget_positioner(new_window, parent=parent)
  print,''
  print,'test 1 b'
  wp->position, xoffset=200, yoffset=300
  widget_control, new_window, /realize
  wait,3
  print,'test 1 c'
  wp->position, xoffset=2000, yoffset=800
  wait,3



  print,''
  print,'test 2'
  widget_control, parent, TLB_SET_XOFFSET=1000, TLB_SET_YOFFSET=600
  wp->position
  wait,3
  
  print,''
  print,'test 3'
  widget_control, parent, TLB_SET_XOFFSET=3000, TLB_SET_YOFFSET=600
  wp->position
  print,'move parent window manually'
  wait,6

  print,''
  print,'test 4'
  wp->position
  wait,5

  print,''
  print,'test 5'
  widget_control, parent, TLB_SET_XOFFSET=3900, TLB_SET_YOFFSET=600
  wp->position, xoffset=-90, yoffset=-200
  wait,3

  widget_control, parent, /destroy




  new_window = widget_base(/row, title='New Widget', xsize=4000, ysize=300)
  wp->set_widget, new_window
  wp->set_parent, -1
  print,''
  print,'test 6'
  wp->position, yoffset=200
  widget_control, new_window, /realize
  wait,3

  widget_control, new_window, /destroy





  parent = widget_base(/column, title='Parent Widget', xsize=300, ysize=400)
  widget_control, parent, /realize

  new_window = widget_base(/row, title='New Widget', xsize=400, ysize=300, group_leader=parent)

  print,spice_get_screen_size()

  monitor =  obj_new('IDLsysMonitorInfo')
  print,'IDLsysMonitorInfo::GetMonitorNames   ' , monitor->GetMonitorNames()
  print,'IDLsysMonitorInfo::GetNumberOfMonitors   ' , monitor->GetNumberOfMonitors()
  print,'IDLsysMonitorInfo::GetPrimaryMonitorIndex   ' , monitor->GetPrimaryMonitorIndex()
  print,'IDLsysMonitorInfo::GetRectangles   ' , monitor->GetRectangles()
  print,'IDLsysMonitorInfo::GetResolutions   ' , monitor->GetResolutions()
  print,'IDLsysMonitorInfo::IsExtendedDesktop   ' , monitor->IsExtendedDesktop()

  wp = widget_positioner(new_window, parent=parent)

  print,''
  print,'test 7'
  wp->position, xoffset=2000
  widget_control, new_window, /realize

  wait,3
  widget_control, parent, /destroy
end
