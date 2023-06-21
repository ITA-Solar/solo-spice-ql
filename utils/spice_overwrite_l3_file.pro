;+
; NAME:
;       SPICE_OVERWRITE_L3_FILE
;
; PURPOSE:
; SPICE_OVERWRITE_L3_FILE is a modal widget that asks the user if he really wants to overwrite a given file.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       result = spice_overwrite_l3_file( l2file_object, group_leader [, allow_xcontrol_l23=allow_xcontrol_l23]
;
; INPUTS:
;   file: The file that is about to be overwritten.
;   group_leader: Widget ID of parent widget.
;
; KEYWORD PARAMETERS:
;   allow_xcontrol_l23: If set, a third button will show up, and give the user the option to
;                       open the file in spice_xcontrol_l23.
;
; OUTPUTS:
;     A string, containing the answer from the user. Either 'Yes', 'No' or 'Open'.
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
;     12-Sep-2022: First version by Martin Wiesmann
;
; $Id: 2023-06-13 11:50 CEST $
;-
;
;


pro spice_overwrite_l3_file_event, event
  widget_control, event.top, get_Uvalue=info
  widget_control, event.id, get_Uvalue=answer
  (*info.result) = answer
  widget_control, event.top, /destroy
end



; -----------------------------------------------------------------------
; MAIN program
; -----------------------------------------------------------------------

function spice_overwrite_l3_file, file, group_leader, allow_xcontrol_l23=allow_xcontrol_l23

  base = widget_base(title='File exists. Overwrite?', group_leader=group_leader, /column, /modal)
  label = widget_label(base, value='This file already exists.', /align_left)
  label = widget_label(base, value=file, /align_left)
  label = widget_label(base, value='Do you want to overwrite it?', /align_left)
  button_base = widget_base(base, /row)
  button_yes = widget_button(button_base, value='  Yes  ', uvalue='Yes')
  IF keyword_set(allow_xcontrol_l23) THEN button_xcontrol = widget_button(button_base, value='Open in XControl_L23', uvalue='Open')
  button_no = widget_button(button_base,  value='  No   ', uvalue='No')

  result = ptr_new('No')
  info = { $
    result:result $
  }

  ; Center the widget on display.
  widget_control, base, set_Uvalue=info, /No_Copy
  wp = widget_positioner(base, parent=group_leader)
  wp->position, /center
  xmanager, 'spice_overwrite_l3_file', base, event_handler='spice_overwrite_l3_file_event'

  res = *result
  ptr_free, result
  return, res
end
