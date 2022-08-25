;+
; NAME:
;       SPICE_CREATE_L3_WIDGET
;
; PURPOSE:
; SPICE_CREATE_L3_WIDGET bla bla blaaa
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
; $Id: 2022-08-25 15:16 CEST $
;-
;
;

pro spice_create_l3_widget_event, event
  widget_control, event.top, get_Uvalue=info, /No_Copy
  destroyed=0
  if ~destroyed then widget_control, event.top, set_Uvalue=info, /No_Copy
end


; User clicked on a line button
function spice_create_l3_widget_lineselect, event
  widget_control, event.top, get_uvalue = info
  widget_control, info.lineselect, get_value = lineselect
  widget_control, info.lineall_clear, get_value = lineall_clear
  case event.id of
    info.lineall_clear: begin
      if event.select eq 0 then return,0
      if event.value eq 0 then begin
        lineselect[*]=1
        lineall_clear=[1,0]
      endif else begin
        lineselect[*]=0
        lineall_clear=[0,1]
      endelse
      widget_control, info.lineselect, set_value = lineselect
    end
    info.lineselect: begin
      lineall_clear[*]=0
    end
    else: return,0
  endcase
  widget_control, info.lineall_clear, set_value = lineall_clear
  return,0
end


pro spice_create_l3_widget_changesdir, event
  widget_control, event.top, get_uvalue = info
  widget_control, info.dir_manual_field, get_value=sdir
  sfile=dialog_pickfile(path=sdir, title='Please select a directory', get_path=sdir)
  if sdir ne '' then begin
    widget_control, info.dir_manual_field, set_value=sdir
  endif
end




; MAIN program

function spice_create_l3_widget, l2_object, group_leader

  file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
  ;file = '/Users/mawiesma/data/spice/level2/2022/03/26/solo_L2_spice-n-ras_20220326T031318_V01_100663899-000.fits'
  l2_object = spice_data(file)
  
  top_dir_choice = 0
  
  ;IF N_PARAM() NE 2 THEN BEGIN
  ;  print, 'Usage: res = spice_create_l3_widget(l2_object, group_leader)'
  ;  return, -1
  ;ENDIF
  IF typename(l2_object) NE 'SPICE_DATA' THEN BEGIN
    print, 'l2_object needs to be a SPICE_DATA object'
    return, -1
  ENDIF

  base = widget_base(title='SPICE create level 3 data/file', group_leader=group_leader, /column);, /modal)

  file_l2 = l2_object->get_filename()
  line_id = l2_object->get_window_id()

  label = widget_label(base, value=(file_dirname(file_l2))[0], /align_center)
  label = widget_label(base, value=(file_basename(file_l2))[0], /align_center)
  column = fix(N_ELEMENTS(line_id)/3.0)+1
  if column gt 5 then column=5
  lineselect = cw_bgroup(base, /nonexclusive, line_id, column=column, event_func = 'spice_create_l3_widget_lineselect')
  lineall_clear = cw_bgroup(base, /nonexclusive, ['all','clear'], column=2, event_func = 'spice_create_l3_widget_lineselect')

  top_dir_base = widget_base(base, /row)
  top_dir_label1 = widget_label(top_dir_base, value='Top directory')
  top_dir_choice_bg = cw_bgroup(top_dir_base, ['Environment variable', 'Path'], set_value=top_dir_choice, /column, /exclusive)
  top_dir_path_base = widget_base(top_dir_base, /column)
  top_dir_env_var_base = widget_base(top_dir_path_base, /row)
  top_dir_env_var_field = cw_field(top_dir_env_var_base, title='', value = 'SPICE_DATA', /string, /return_events, xsize = 100, $
    /NOEDIT, ysize=0.7)
  dir_manual_base = widget_base(top_dir_path_base, /row)
  dir_manual_field = cw_field(dir_manual_base, title='', value = dir_manual, /string, /return_events, xsize = 100)
  dir_manual_button = widget_button(dir_manual_base, value='Change', event_pro='spice_create_l3_widget_changesdir')

  options_values = ['No masking of dumbbell'
  options = cw_bgroup(base, 



  result = ptr_new('asdf')

  info = { $
    result:result, $
    lineselect:lineselect, $
    lineall_clear:lineall_clear, $
    dir_manual_field:dir_manual_field $
    }
    
  widget_control, base, set_Uvalue=info, /No_Copy
  widget_control, base, /realize
  xmanager, 'spice_create_l3_widget', base, event_handler='spice_create_l3_widget_event'

  res = *result
  ptr_free, result
  return, res
end