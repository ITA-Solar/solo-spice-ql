;+
; NAME:
;       SPICE_CREATE_L3_WIDGET
;
; PURPOSE:
; SPICE_CREATE_L3_WIDGET is the widget to the method SPICE_DATA::create_l3_file.
; It lets the user select all the possible options before creating the level 3 data and/or file.
; This widget is used by SPICE_XCONTROL and SPICE_XCONTROL_L23.
;
; CATEGORY:
;       Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;       result = spice_create_l3_widget( l2_object [, group_leader] [, window_index=window_index] $
;         [, /no_masking] [, /approximated_slit] $
;         [, /no_fitting] [, /no_widget] [, /position] [, velocity=velocity] $
;         [, /official_l3dir] [, top_dir=top_dir] [, /save_not] [, /block_save] )
;
; INPUTS:
;   l2_object: Either a SPICE_DATA object or a path to a level 2 SPICE FITS file.
;
; OPTIONAL INPUTS:
;     group_leader: Widget ID of parent widget.
;     window_index: One or more window indices that should be checked for processing.
;     top_dir: A path in which the level 3 file should be saved. If not provided the file will be saved
;                 into the $SPICE_DATA/user/ directory.
;     velocity: Set this equal to the initial velocity if you want the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength is taken from the supplied POSITION, i.e.,
;                 INT_POS_FWHM(1), which is calculated/estimated within the procedure 'generate_adef'.
;                 This input is ignored if /POSITION is set. Default is zero.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then SPICE_DATA::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The masking procedure is not called for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;     no_fitting: If set, fitting won't be computed. This can still be done manually in xcfit_block.
;     no_widget: If set, xcfit_block will not be called
;     position: If set, then the line position is NOT represented by the velocity
;                 relative to a lab wavelength, but as the wavelength.
;     official_l3dir: If set, the file will be moved to the directory $SPICE_DATA/level3, the directory
;                 for the official level 3 files.
;     save_not: If set, then the FITS file will not be saved. The output is otherwise the same as if
;                 this keyword has not been set.
;     block_save: If set, then the options concerning where and whether to save the FITS file are not sensitive.
;     allow_xcontrol_l23: Gives user option to open spice_xcontrol_l23, when trying to overwrite an existing file.
;     no_line_list: If set, then no predefined line list will be used to define gaussian fit components.
;                 By default, the list returned by the function spice_line_list() will be used.
;
;                 IMPORTANT NOTE: For now, this keyword is set by default. One has to set it explicitly to zero
;                 if one wants to use the predefined line list. This implementation may change in the future.
;
;                 Due to instrument temperature variations the wavelength scale changes significantly during
;                 the Solar Orbiter orbit, and this variation is not accounted for in L2 files. The wavelength shift is so large
;                 that using the line list when fitting fails in many cases.
;
; OUTPUTS:
;     A structure with tags:
;       l3_file: The path and name of the produced level 3 file. This tag will be set to 'Cancel' if
;                 the user clicks 'Cancel'.
;       ana: This is a pointer to a CFIT_ANALYSIS structure array. One ANA per processed window.
;       result_headers: This is a pointer to a pointer array, of which each element contains
;                 a string array, the level 3 header of the result extension of one window.
;       file_saved: A boolean, indicating whether the level 3 file has been saved.
;       user_dir: A boolean, indicating whether the output path points to 'user' subdirectory.
;       top_dir: A string, which points to the top directory in which the file has been saved.
;                 This is an empty string, if the file has been saved under $SPICE_DATA.
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
;     18-Aug-2022: First version by Martin Wiesmann
;
; $Id: 2023-06-14 10:55 CEST $
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
      window_index = where(lineselect eq 1, count)
      if count eq 0 then begin
        box_message,'You need to select at least one window/line'
        return
      endif
      widget_control, info.options_bg, get_value = options
      no_masking = options[2]
      approximated_slit = options[3]
      no_fitting = options[0]
      no_xcfit_block = options[1]
      position = options[5]
      no_line_list = options[4]
      widget_control, info.fit_velocity_field, get_value = velocity
      widget_control, info.top_dir_choice_bg, get_value=top_dir_choice
      IF top_dir_choice EQ 1 THEN widget_control, info.dir_manual_field, get_value=top_dir
      widget_control, info.dir_user_bg, get_value=user_dir
      official_l3dir = user_dir[0] EQ 0
      widget_control, info.save_bg, get_value=save
      save_not = save[0] EQ 0
      IF ~save_not && file_exist(info.file_l3) THEN BEGIN
        overwrite = spice_overwrite_l3_file(info.file_l3, event.top, allow_xcontrol_l23=info.allow_xcontrol_l23)
        IF overwrite EQ 'No' THEN return
        IF overwrite EQ 'Open' THEN BEGIN
          spice_xcontrol_l23, info.file_l3, group_leader=info.group_leader
          widget_control, event.top, /destroy
          return
        ENDIF
      ENDIF
      l3_file = info.l2_object->create_l3_file(window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
        no_fitting=no_fitting, no_xcfit_block=no_xcfit_block, position=position, velocity=velocity, no_line_list=no_line_list, $
        official_l3dir=official_l3dir, top_dir=top_dir, save_not=save_not, $
        all_ana=all_ana, all_result_headers=all_result_headers, group_leader=info.group_leader)
      (*info.result).l3_file = l3_file
      (*info.result).ana = ptr_new(all_ana)
      (*info.result).result_headers = ptr_new(all_result_headers)
      (*info.result).file_saved = save[0]
      (*info.result).user_dir = user_dir[0]
      IF top_dir_choice EQ 1 THEN (*info.result).top_dir = top_dir
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
  IF event.id EQ info.top_dir_choice_bg THEN BEGIN
    user_dir=0
    IF event.value EQ 0 && info.official_l3dir EQ 0 THEN user_dir=1
    widget_control, info.dir_user_bg, set_value=user_dir
  ENDIF
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
    destination=file_l3_calc, /quiet
  file_l3_calc = file_l3_calc[0].replace('level2', 'level3')
  file_l3_calc = file_l3_calc.replace('_L2_','_L3_')
  info.file_l3 = file_l3_calc
  widget_control, info.file_l3_dir_label, set_value=(file_dirname(file_l3_calc))[0]
  widget_control, info.file_l3_name_label, set_value=(file_basename(file_l3_calc))[0]
end



; -----------------------------------------------------------------------
; MAIN program
; -----------------------------------------------------------------------

function spice_create_l3_widget, l2_object, group_leader, window_index=window_index, $
  no_masking=no_masking, approximated_slit=approximated_slit, no_line_list=no_line_list, $
  no_fitting=no_fitting, no_widget=no_widget, position=position, velocity=velocity, $
  official_l3dir=official_l3dir, top_dir=top_dir, save_not=save_not, block_save=block_save, $
  allow_xcontrol_l23=allow_xcontrol_l23

  IF ~ARG_PRESENT(no_line_list) THEN no_line_list=1 ; See note for this keyword in documentation

  IF N_PARAMS() EQ 0 THEN BEGIN
    print, 'Usage: res = spice_create_l3_widget(l2_object [, group_leader] [, window_index=window_index] $'
      print, '  [, /no_masking] [, /approximated_slit] $'
      print, '  [, /no_fitting] [, /no_widget] [, /position] [, velocity=velocity] $'
      print, '  [, /official_l3dir] [, top_dir=top_dir] [, /save_not] )'
    return, -1
  ENDIF
  l2_object = spice_get_object(l2_object, is_spice=is_spice, object_created=object_created)
  if ~is_spice then return, -1

  official_l3dir = keyword_set(official_l3dir)
  top_dir_choice = keyword_set(top_dir)
  dir_user_choice = [~official_l3dir && ~top_dir_choice]
  option_choice = [keyword_set(no_fitting), keyword_set(no_widget), keyword_set(no_masking), $
    keyword_set(apporximated_slit), keyword_set(no_line_list), keyword_set(position)]
  if N_ELEMENTS(velocity) eq 0 then velocity = 0.0
  save_choice = [~keyword_set(save_not)]
  if N_ELEMENTS(top_dir) eq 0 then cd, current=dir_manual else dir_manual=top_dir

  file_l2 = l2_object->get_filename()
  line_id = l2_object->get_window_id()
  n_windows = N_ELEMENTS(line_id)
  window_select = intarr(n_windows)
  FOR i=0,N_ELEMENTS(window_index)-1 DO $
    IF window_index[i] GE 0 && window_index[i] LT n_windows THEN window_select[window_index[i]]=1

  base = widget_base(title='SPICE create level 3 data/file', group_leader=group_leader, /column);, modal=keyword_set(group_leader))
  ; If this widget is modal, the user can't stop the fitting calculations

  label = widget_label(base, value=(file_dirname(file_l2))[0], /align_center)
  label = widget_label(base, value=(file_basename(file_l2))[0], /align_center)
  column = fix(n_windows/3.0)+1
  if column gt 5 then column=5
  lineselect = cw_bgroup(base, /nonexclusive, line_id, column=column, set_value=window_select, $
    event_func = 'spice_create_l3_widget_lineselect')
  lineall_clear = cw_bgroup(base, /nonexclusive, ['all','clear'], column=2, event_func = 'spice_create_l3_widget_lineselect')

  output_path_base = widget_base(base, /column, sensitive=~keyword_set(block_save))
  top_dir_base = widget_base(output_path_base, /row, event_func='spice_create_l3_widget_change_topdir')
  top_dir_label1 = widget_label(top_dir_base, value='Top directory', /align_left)
  top_dir_choice_bg = cw_bgroup(top_dir_base, ['Environment variable', 'Path'], set_value=top_dir_choice, /column, /exclusive)
  top_dir_path_base = widget_base(top_dir_base, /column)
  top_dir_env_var_field = cw_field(top_dir_path_base, title='', value = 'SPICE_DATA', /string, /return_events, xsize = 15, $
    /NOEDIT, ysize=0.7)
  dir_manual_base = widget_base(top_dir_path_base, /row)
  dir_manual_field = cw_field(dir_manual_base, title='', value = dir_manual, /string, /return_events, xsize = 80)
  dir_manual_button = widget_button(dir_manual_base, value='Change', event_pro='spice_create_l3_widget_changesdir')
  dir_user_bg = cw_bgroup(output_path_base, ['Save in "user" subdirectory'], set_value=dir_user_choice, /nonexclusive, $
    event_func='spice_create_l3_widget_change_topdir')
  widget_control, dir_user_bg, sensitive=official_l3dir

  options_values = ['Do not run the fit routine', 'Do not open xcfit_block', $
    'No masking of dumbbell', 'Approximate dumbbell masking', 'Do not use line list', 'Use position, i.e. fit lambda, not velocity']
    print,options_values,option_choice
  options_bg = cw_bgroup(base, options_values, set_value=option_choice, /nonexclusive, column=3)
  fit_velocity_field = cw_field(base, title='Initial velocity', value = velocity, /float, xsize = 10)

  save_base = widget_base(base, /row, sensitive=~keyword_set(block_save))
  save_bg = cw_bgroup(save_base, ['Save level 3 FITS file to:'], set_value=save_choice, /nonexclusive)
  file_l3_base = widget_base(save_base, /column)
  file_l3_dir_label = widget_label(file_l3_base, value=(file_dirname('path/file_l3'))[0], /align_left, /DYNAMIC_RESIZE)
  file_l3_name_label = widget_label(file_l3_base, value=(file_basename('path/file_l3'))[0], /align_left, /DYNAMIC_RESIZE)

  button_base = widget_base(base, /row)
  button_ok = widget_button(button_base, value='OK')
  button_cancel = widget_button(button_base, value='Cancel')


  result = ptr_new({l3_file:'Cancel', ana:ptr_new(), result_headers:ptr_new(), file_saved:0b, user_dir:0b, top_dir:''})
  info = { $
    group_leader:group_leader, $
    l2_object:l2_object, $
    file_l3:'', $
    official_l3dir:official_l3dir, $
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
    cancel:button_cancel, $
    allow_xcontrol_l23:keyword_set(allow_xcontrol_l23) $
  }
  spice_create_l3_widget_calc_l3_dir, info

  widget_control, base, set_Uvalue=info, /No_Copy
  wp = widget_positioner(base, parent=group_leader)
  wp->position
  xmanager, 'spice_create_l3_widget', base, event_handler='spice_create_l3_widget_event'

  res = *result
  ptr_free, result
  return, res
end