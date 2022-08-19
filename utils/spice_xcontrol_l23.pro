;+
; NAME:
;       SPICE_XCONTROL_L23
;
; PURPOSE:
; SPICE_XCONTROL_L23 bla bla blaaa
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xcontrol_l23, file [, group_leader = group]
;
; INPUTS:
; file: A SPICE file either level 2 or level 3
;
; KEYWORD PARAMETERS:
; group_leader: Widget ID of parent widget
;
; OUTPUTS:
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;     18-Aug-2020: First version by Martin Wiesmann
;
; $Id: 2022-08-19 10:16 CEST $
;-
;
;
pro spice_xcontrol_l23_destroy, event
  print,'spice_xcontrol_l23_destroy'
  help,event
  pseudoevent = {WIDGET_KILL_REQUEST, $
    ID:event.id, $
    TOP:event.top, $
    HANDLER:event.handler}
  spice_xcontrol_event, pseudoevent
end

pro spice_xcontrol_l23_cleanup, tlb
  print,'spice_xcontrol_l23_cleanup'
  widget_control, tlb, get_uvalue = info
  ptr_free,info
end

pro spice_xcontrol_l23_event, event
  print,'spice_xcontrol_l23_event'
  help,event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, /destroy
  endif
end



pro spice_xcontrol_l23, file, group_leader = group_leader

  if n_params() lt 1 then begin
    message,'spice_xcontrol_l23, file [, group_leader = group_leader]',/cont
    return
  endif


  tlb = widget_base(/row, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)


  ; Define the info structure, used to send information around
  info= { $
    tlb:tlb $
  }
  info=ptr_new(info,/no_copy)


  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize

  xmanager, 'spice_xcontrol_l23', tlb, /no_block, $
    group_leader = group_leader, cleanup = 'spice_xcontrol_l23_cleanup'

end