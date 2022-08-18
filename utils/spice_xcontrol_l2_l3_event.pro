;+
; NAME:
;       SPICE_XCONTROL_L2_L3
;
; PURPOSE:
; SPICE_XCONTROL_L2_L3 bla bla blaaa
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xcontrol_l2_l3, file [, group_leader = group]
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
; $Id: 2022-08-18 15:36 CEST $
;-
;
;
pro spice_xcontrol_l2_l3_destroy, event
  pseudoevent = {WIDGET_KILL_REQUEST, $
    ID:event.id, $
    TOP:event.top, $
    HANDLER:event.handler}
  spice_xcontrol_event, pseudoevent
end

pro spice_xcontrol__l2_l3cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free,(*info).d
  ptr_free,info
end

pro spice_xcontrol_l2_l3_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, /destroy
  endif
end



pro spice_xcontrol_l2_l3, input_data, group_leader = group_leader

  if n_params() lt 1 then begin
    message,'spice_xcontrol_l2_l3, file [, group_leader = group_leader]',/cont
    return
  endif


  tlb = widget_base(/row, mbar=menubar, $
    title='SPICE_Xcontrol_L2_L3 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)


  ; Define the info structure, used to send information around
  info= { d:ptr_new(), $
    sdo:ptr_new(), $
    moment:ptr_new(), $
    lineselect:lineselect, $
    lineall_clear:lineall_clear, $
    data_textdump:'', $
    lines:intarr(nwin), $
    sjiref:-1, $
    pointing_size:pointing_size,$
    pointing_wid:-1,$
    pointing_phase:1,$
    tlb:tlb }
  info=ptr_new(info,/no_copy)


  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize

  xmanager, 'spice_xcontrol_l2_l3', tlb, /no_block, $
    group_leader = group_leader, cleanup = 'spice_xcontrol_l2_l3_cleanup'

end