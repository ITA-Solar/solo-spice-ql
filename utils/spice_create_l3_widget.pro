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
; $Id: 2022-08-26 14:44 CEST $
;-
;
;

pro spice_create_l3_widget_event, event
  widget_control, event.top, get_Uvalue=info
  case event.id of
    ;cancel, we do nothing, just destroy this widget
    ;the returned structure is the initial one
    ;the l3_file is 'Cancel'
    info.cancel: begin
      widget_control, event.top, /destroy
    end

    ;ok, use the current settings to create l3 data/file 
    ;and destroy the widget
    info.ok: begin
      widget_control, /hourglass
      widget_control, info.lineselect, get_value = lineselect
      help,lineselect
      print,lineselect
      window_indices = where(lineselect eq 1, count)
      print,window_indices
      if count eq 0 then begin
        box_message,'You need to select at least one window/line'
        return
      endif
      widget_control, info.options_bg, get_value = options
      no_masking = options[2]
      approximated_slit = options[3]
      no_fitting = options[0]
      no_widget = options[1]
      position = options[4]
      widget_control, info.fit_velocity_field, get_value = velocity
      widget_control, info.top_dir_choice_bg, get_value=top_dir_choice
      IF top_dir_choice EQ 1 THEN widget_control, info.dir_manual_field, get_value=top_dir
      widget_control, info.dir_user_bg, get_value=user_dir
      official_l3dir = user_dir[0] EQ 0
      widget_control, info.save_bg, get_value=save
      save_not = save[0] EQ 0
      ;l3_file = info.l2_object->create_l3_file(window_indices, no_masking=no_masking, approximated_slit=approximated_slit, $
      ;  no_fitting=no_fitting, no_widget=no_widget, position=position, velocity=velocity, $
      ;  official_l3dir=official_l3dir, top_dir=top_dir, save_not=save_not, $
      ;  all_ana=all_ana, all_result_headers=all_result_headers)
      (*info.result).l3_file = info.file_l3
      (*info.result).ana = ptr_new({a:0,b:'asdf'})
      widget_control, event.top, /destroy
    end
    else:
  endcase
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


; User changed top-dir of l3
function spice_create_l3_widget_change_topdir, event
  widget_control, event.top, get_uvalue = info
  spice_create_l3_widget_calc_l3_dir, info
  return,0
end


pro spice_create_l3_widget_changesdir, event
  widget_control, event.top, get_uvalue = info
  widget_control, info.dir_manual_field, get_value=sdir
  sfile=dialog_pickfile(path=sdir, title='Please select a directory', get_path=sdir)
  if sdir ne '' then begin
    widget_control, info.dir_manual_field, set_value=sdir
  endif
  spice_create_l3_widget_calc_l3_dir, info
end


pro spice_create_l3_widget_calc_l3_dir, info
  widget_control, info.top_dir_choice_bg, get_value=top_dir_choice
  IF top_dir_choice EQ 1 THEN widget_control, info.dir_manual_field, get_value=top_dir
  widget_control, info.dir_user_bg, get_value=user_dir
  user_dir = user_dir[0]
  file_l2 = info.l2_object->get_filename()
  spice_ingest, file_l2, user_dir=user_dir, top_dir=top_dir, /dry_run, /force, $
    destination=file_l3_calc
  file_l3_calc = file_l3_calc[0].replace('level2', 'level3')
  file_l3_calc = file_l3_calc.replace('_L2_','_L3_')
  info.file_l3 = file_l3_calc
  widget_control, info.file_l3_dir_label, set_value=(file_dirname(file_l3_calc))[0]
  widget_control, info.file_l3_name_label, set_value=(file_basename(file_l3_calc))[0]
end




; MAIN program

function spice_create_l3_widget, l2_object, group_leader

  file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
  ;file = '/Users/mawiesma/data/spice/level2/2022/03/26/solo_L2_spice-n-ras_20220326T031318_V01_100663899-000.fits'
  l2_object = spice_data(file)
  
  ;IF N_PARAM() NE 2 THEN BEGIN
  ;  print, 'Usage: res = spice_create_l3_widget(l2_object, group_leader)'
  ;  return, -1
  ;ENDIF
  IF typename(l2_object) NE 'SPICE_DATA' THEN BEGIN
    print, 'l2_object needs to be a SPICE_DATA object'
    return, -1
  ENDIF

  top_dir_choice = 0
  dir_user_choice = [0]
  option_choice = [0, 0, 0, 0, 0]
  velocity = 0.0
  save_choice = [0]
  if N_ELEMENTS(dir_manual) eq 0 then cd, current=dir_manual

  file_l2 = l2_object->get_filename()
  line_id = l2_object->get_window_id()

  base = widget_base(title='SPICE create level 3 data/file', group_leader=group_leader, /column);, /modal)

  label = widget_label(base, value=(file_dirname(file_l2))[0], /align_center)
  label = widget_label(base, value=(file_basename(file_l2))[0], /align_center)
  column = fix(N_ELEMENTS(line_id)/3.0)+1
  if column gt 5 then column=5
  lineselect = cw_bgroup(base, /nonexclusive, line_id, column=column, event_func = 'spice_create_l3_widget_lineselect')
  lineall_clear = cw_bgroup(base, /nonexclusive, ['all','clear'], column=2, event_func = 'spice_create_l3_widget_lineselect')

  top_dir_base = widget_base(base, /row, event_func='spice_create_l3_widget_change_topdir')
  top_dir_label1 = widget_label(top_dir_base, value='Top directory', /align_left)
  top_dir_choice_bg = cw_bgroup(top_dir_base, ['Environment variable', 'Path'], set_value=top_dir_choice, /column, /exclusive)
  top_dir_path_base = widget_base(top_dir_base, /column)
  ;top_dir_env_var_base = widget_base(top_dir_path_base, /row)
  top_dir_env_var_field = cw_field(top_dir_path_base, title='', value = 'SPICE_DATA', /string, /return_events, xsize = 15, $
    /NOEDIT, ysize=0.7)
  dir_manual_base = widget_base(top_dir_path_base, /row)
  dir_manual_field = cw_field(dir_manual_base, title='', value = dir_manual, /string, /return_events, xsize = 80)
  dir_manual_button = widget_button(dir_manual_base, value='Change', event_pro='spice_create_l3_widget_changesdir')
  dir_user_bg = cw_bgroup(base, ['Save in "user" subdirectory'], set_value=dir_user_choice, /nonexclusive, $
    event_func='spice_create_l3_widget_change_topdir')

  options_base = widget_base(base, /row)
  options_values = ['Do not run the fit routine', 'Do not open xcfit_block', $
    'No masking of dumbbell', 'Approximate dumbbell masking', 'Use position, i.e. fit lambda, not velocity']
  options_bg = cw_bgroup(options_base, options_values, set_value=option_choice, /nonexclusive, column=3)
  fit_velocity_field = cw_field(options_base, title='velocity', value = velocity, /float, xsize = 10)

  save_base = widget_base(base, /row)
  save_bg = cw_bgroup(save_base, ['Save level 3 FITS file to:'], set_value=save_choice, /nonexclusive)
  file_l3_base = widget_base(save_base, /column)
  file_l3_dir_label = widget_label(file_l3_base, value=(file_dirname('path/file_l3'))[0], /align_left, /DYNAMIC_RESIZE)
  file_l3_name_label = widget_label(file_l3_base, value=(file_basename('path/file_l3'))[0], /align_left, /DYNAMIC_RESIZE)
  
  button_base = widget_base(base, /row)
  button_ok = widget_button(button_base, value='OK')
  button_cancel = widget_button(button_base, value='Cancel')


  result = ptr_new({l3_file:'Cancel', ana:ptr_new(), result_headers:ptr_new(), file_saved:0b})

  info = { $
    l2_object:l2_object, $
    file_l3:'', $
    result:result, $
    lineselect:lineselect, $
    lineall_clear:lineall_clear, $
    top_dir_choice_bg:top_dir_choice_bg, $
    dir_manual_field:dir_manual_field, $
    dir_user_bg:dir_user_bg, $
    options_bg:options_bg, $
    fit_velocity_field:fit_velocity_field, $
    save_bg:save_bg, $
    file_l3_dir_label:file_l3_dir_label, $
    file_l3_name_label:file_l3_name_label, $
    ok:button_ok, $
    cancel:button_cancel $
    }
    
  spice_create_l3_widget_calc_l3_dir, info
  
  widget_control, base, set_Uvalue=info, /No_Copy
  widget_control, base, /realize
  xmanager, 'spice_create_l3_widget', base, event_handler='spice_create_l3_widget_event'

  res = *result
  ptr_free, result
  return, res
end