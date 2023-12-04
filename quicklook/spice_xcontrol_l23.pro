;+
; NAME:
;       SPICE_XCONTROL_L23
;
; PURPOSE:
; SPICE_XCONTROL_L23 is a GUI utility to view, edit or create level 3 SPICE FITS files, along with the corresponding
; level 2 file. The tool takes as input a level 2 or a level 3 file, and then displays 3.
; Each column stands for one file, first the level 2 file, then the official level 3 file, i.e. the level 3 that is found
; in the official location, i.e. $SPICE_DATA/level3. The third column shows the level 3 file that the user can create
; with his/her own options and fitting components. There is also the possibility to have a level 3 file in a fully
; user-defined location, this would be shown in the 4th column. Each window can be viewed/edited or created separately
; and the FITS file can then be saved. Alternatively, with the upper most button, all or some windows can be (re)created
; in one go. This will overwrite all previous data, i.e. windows that had level 3 data, and that were not selected in this
; round, will get deleted.
; When viewing a level 3 window (in xcfit_block) the tool assumes that the window was edited, and will mark it as such, even
; though no changes were made. This is because the tool can not know whether the user actually made changes or not.
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
; SPICE_DATA, SPICE_CREATE_L3_WIDGET
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
; $Id: 2023-12-04 14:50 CET $
;-


pro spice_xcontrol_l23_destroy, event
  pseudoevent = {WIDGET_KILL_REQUEST, $
    ID:event.id, $
    TOP:event.top, $
    HANDLER:event.handler}
  spice_xcontrol_event, pseudoevent
end


pro spice_xcontrol_l23_cleanup, tlb
  widget_control, tlb, get_uvalue=info
  ptr_free,(*info).winno_l3_official
  ptr_free,(*info).winno_l3_user
  FOR i=0,N_ELEMENTS(*(*info).ana_l3_official)-1 DO delete_analysis, (*(*info).ana_l3_official)[i] 
  ptr_free,(*info).ana_l3_official
  FOR i=0,N_ELEMENTS(*(*info).ana_l3_user)-1 DO delete_analysis, (*(*info).ana_l3_user)[i]
  ptr_free,(*info).ana_l3_user
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_official)-1 DO ptr_free, (*(*info).hdr_l3_official)[i]
  ptr_free,(*info).hdr_l3_official
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_official_data)-1 DO ptr_free, (*(*info).hdr_l3_official_data)[i]
  ptr_free,(*info).hdr_l3_official_data
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_user)-1 DO ptr_free, (*(*info).hdr_l3_user)[i]
  ptr_free,(*info).hdr_l3_user
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_user_data)-1 DO ptr_free, (*(*info).hdr_l3_user_data)[i]
  ptr_free,(*info).hdr_l3_user_data
  FOR i=0,N_ELEMENTS(*(*info).proc_steps_user)-1 DO IF ptr_valid((*(*info).proc_steps_user)[i]) THEN ptr_free, (*(*info).proc_steps_user)[i]
  ptr_free,(*info).proc_steps_user
  FOR i=0,N_ELEMENTS(*(*info).proc_steps_official)-1 DO IF ptr_valid((*(*info).proc_steps_official)[i]) THEN ptr_free, (*(*info).proc_steps_official)[i]
  ptr_free,(*info).proc_steps_official
  ptr_free,info
end


pro spice_xcontrol_l23_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, get_uvalue=info
    IF total((*info).state_l3_user.edited) GT 0 THEN BEGIN
      answer = dialog_message(['WARNING: Possibly UNSAVED changes.', $
        'This warning also shows up, even if you only looked at level 3 data',$
        'without changing anything.', $
        'Do you really want to exit?'], $
        /question, /default_no, title='WARNING: Possibly UNSAVED changes.', $
        /center, dialog_parent=event.top)
      IF answer EQ 'No' THEN return
    ENDIF
    widget_control, event.top, /destroy
  endif
end


pro spice_xcontrol_l23_save_file, event
  widget_control, event.top, get_uvalue=info
  IF size((*info).object_l2, /type) NE 11 THEN BEGIN
    answer = dialog_message(['Saving of level 3 SPICE FITS files is not (yet) supported', $
      'when the corresponding level 2 file is not available.', $
      'Contact prits-group@astro.uio.no if you need this feature'], $
      title='WARNING: NOT supported.', $
      /center, dialog_parent=event.top)
    ; TODO
    return
  ENDIF
  nwin_l3 = (*info).nwin_l3_user
  winno_l3 = *(*info).winno_l3_user
  ana_l3 = *(*info).ana_l3_user
  file_l3 = (*info).file_l3_user
  IF file_exist(file_l3) THEN BEGIN
    answer = dialog_message(['This file already exists.',file_l3,'Do you want to overwrite it?'], $
      /question, /default_no, title='File exists. Overwrite?',/center)
    IF answer EQ 'No' THEN return
    file_old = 1
    file_move, file_l3, file_l3+'.old', /overwrite
  ENDIF ELSE file_old = 0
  IF ~FILE_TEST(file_dirname(file_l3)) THEN FILE_MKDIR, file_dirname(file_l3)

  all_result_headers = ptrarr(nwin_l3)
  all_data_headers = ptrarr(nwin_l3)
  l3_pr_steps_all = ptrarr(nwin_l3)
  FOR iwindow=0,nwin_l3-1 DO BEGIN

    original_data = (*info).object_l2->get_window_data(winno_l3[iwindow], no_masking=no_masking, approximated_slit=approximated_slit)
    
    PROC_STEPS = *(*(*info).proc_steps_user)[iwindow]
    if (*info).state_l3_user[iwindow].edited then begin
      proc_step_new =  [ $
        HASH('name','PRSTEP', 'value','MANUAL-LINE-FITTING', 'comment','Processing step type, step '), $
        HASH('name','PRPROC', 'value','spice_xcfit_block, spice_xcontrol_l23', 'comment','Name of procedure performing PRSTEP'), $
        HASH('name','PRLIB' , 'value','solarsoft/so/spice/idl/quicklook', 'comment','Software library containing PRPROC'), $
        HASH('name','PRPARA', 'value','POSSIBLE_MANUAL_EDITING = 1', 'comment','Parameters for PRPROC') $
        ]
      PROC_STEPS.add, proc_ste_new, /no_copy
    endif

    if iwindow gt 0 then IS_EXTENSION=1 else IS_EXTENSION=0
    ana2fits, ana_l3[iwindow], FILEPATH_OUT=file_l3, $
      N_WINDOWS=nwin_l3, WINNO=iwindow, $
      DATA_ID=DATA_ID, TYPE_XDIM1='WAVE', $
      IS_EXTENSION=IS_EXTENSION, LEVEL='L3', VERSION=number_version_l3, $
      PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
      PROGENITOR_DATA=original_data, HEADER_INPUT_DATA=(*info).object_l2->get_header(winno_l3[iwindow]), $
      SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
      SAVE_NOT=SAVE_NOT, $
      headers_results=headers_results, headers_data=headers_data

    all_result_headers[iwindow] = ptr_new(*headers_results[0])
    all_data_headers[iwindow] = ptr_new(*headers_data[0])
    l3_pr_steps_all[iwindow] = ptr_new(pr_steps)

  ENDFOR ; iwin=0,nwin_l3-1

  IF file_old THEN file_delete, file_l3+'.old'

  ptr_free, (*info).hdr_l3_user
  (*info).hdr_l3_user = ptr_new(all_result_headers)
  ptr_free, (*info).hdr_l3_user_data
  (*info).hdr_l3_user_data = ptr_new(all_data_headers)
  ptr_free, (*info).proc_steps_user
  (*info).proc_steps_user = ptr_new(l3_pr_steps_all)
  (*info).state_l3_user.edited = 0
  spice_xcontrol_l23_update_state_display, info
end


; Add or replace new results
; This happens if the user clicks on one of the '(Re)create window' buttons
pro spice_xcontrol_l23_update_state_add, info, result
  ana_l3 = *(*info).ana_l3_user
  hdr_l3 = *(*info).hdr_l3_user
  hdr_data_l3 = *(*info).hdr_l3_user_data
  proc_steps_user = *(*info).proc_steps_user
  state_l3 = (*info).state_l3_user
  winno_l3 = *(*info).winno_l3_user

  nwin_l3_result = N_ELEMENTS(*result.ana)
  winno_l3_result = intarr(nwin_l3_result)
  FOR iwin=0,nwin_l3_result-1 DO BEGIN
    winno_l3_result[iwin] = fxpar(*(*result.RESULT_HEADERS)[iwin], 'L2WINNO', missing=-1)
  ENDFOR

  nwin_l3 = 0
  FOR iwin=0,(*info).nwin-1 DO BEGIN
    ind_result = where(winno_l3_result eq iwin, count_result)
    ind_old = where(winno_l3 eq iwin, count_old)
    IF count_result GT 0 THEN BEGIN
      state_l3[iwin].l3_winno = nwin_l3
      state_l3[iwin].edited = ~result.file_saved
      spice_xcontrol_l23_add_window, ana_l3_new, (*result.ana)[ind_result[0]], $
        hdr_l3_new, *(*result.result_headers)[ind_result[0]], $
        hdr_data_l3_new, *(*result.data_headers)[ind_result[0]], $
        l3_pr_steps_new, *(*result.proc_steps)[ind_result[0]], $
        winno_l3_new, iwin
      nwin_l3++
    ENDIF ELSE IF count_old GT 0 THEN BEGIN
      state_l3[iwin].l3_winno = nwin_l3
      spice_xcontrol_l23_add_window, ana_l3_new, ana_l3[ind_old[0]], $
        hdr_l3_new, *hdr_l3[ind_old[0]], $
        hdr_data_l3_new,  *hdr_data_l3[ind_old[0]], $
        l3_pr_steps_new,  *proc_steps_user[ind_old[0]], $
        winno_l3_new, iwin
      nwin_l3++
    ENDIF ELSE BEGIN
      state_l3[iwin].l3_winno = -1
      state_l3[iwin].edited = 0
    ENDELSE
  ENDFOR ; iwin=0,(*info).nwin-1

  (*info).nwin_l3_user = nwin_l3
  ptr_free, (*info).ana_l3_user
  (*info).ana_l3_user = ptr_new(ana_l3_new)
  ptr_free, (*info).hdr_l3_user
  (*info).hdr_l3_user = ptr_new(hdr_l3_new)
  ptr_free, (*info).hdr_l3_user_data
  (*info).hdr_l3_user_data = ptr_new(hdr_data_l3_new)
  ptr_free, (*info).proc_steps_user
  (*info).proc_steps_user = ptr_new(l3_pr_steps_new)
  ptr_free, (*info).winno_l3_user
  (*info).winno_l3_user = ptr_new(winno_l3_new)
  (*info).state_l3_user = state_l3

  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_add_window, ana_l3_new, new_ana, hdr_l3_new, new_hdr, hdr_data_l3_new, new_hdr_data, l3_pr_steps_new, new_pr_step, winno_l3_new, new_winno
  IF N_ELEMENTS(ana_l3_new) EQ 0 THEN BEGIN
    ana_l3_new = new_ana
    hdr_l3_new = ptr_new(new_hdr)
    hdr_data_l3_new = ptr_new(new_hdr_data)
    l3_pr_steps_new = ptr_new(new_pr_step)
    winno_l3_new = new_winno
  ENDIF ELSE BEGIN
    ana_l3_new = [ana_l3_new, new_ana]
    hdr_l3_new = [hdr_l3_new, ptr_new(new_hdr)]
    hdr_data_l3_new = [hdr_data_l3_new, ptr_new(new_hdr_data)]
    l3_pr_steps_new = [l3_pr_steps_new, ptr_new(new_pr_step)]
    winno_l3_new = [winno_l3_new, new_winno]
  ENDELSE
end


; Replace previous results with the new ones.
; This happens if the user clicks on upper most '(Re)create file' button
pro spice_xcontrol_l23_update_state_replace, info, result

  state_l3 = (*info).state_l3_user
  nwin_l3_result = fxpar(*(*result.RESULT_HEADERS)[0], 'NWIN', missing=0)
  winno_l3_result = intarr(nwin_l3_result)
  FOR iwin=0,nwin_l3_result-1 DO BEGIN
    winno_l3_result[iwin] = fxpar(*(*result.RESULT_HEADERS)[iwin], 'L2WINNO', missing=-1)
  ENDFOR

  FOR iwin=0,(*info).nwin-1 DO BEGIN
    ind = where(winno_l3_result eq iwin, count)
    IF count GT 0 THEN BEGIN
      state_l3[iwin].l3_winno = ind[0]
      state_l3[iwin].edited = ~result.file_saved
    ENDIF ELSE BEGIN
      state_l3[iwin].l3_winno = -1
      state_l3[iwin].edited = 0
    ENDELSE
  ENDFOR

  (*info).file_l3_user = result.l3_file
  (*info).file_in_user_dir = result.user_dir
  IF ~result.user_dir THEN (*info).file_top_dir = result.top_dir
  (*info).nwin_l3_user = nwin_l3_result
  ptr_free, (*info).ana_l3_user
  (*info).ana_l3_user = ptr_new(*result.ana)
  ptr_free, (*info).hdr_l3_user
  (*info).hdr_l3_user = ptr_new(*result.result_headers)
  ptr_free, (*info).hdr_l3_user_data
  (*info).hdr_l3_user_data = ptr_new(*result.data_headers)
  ptr_free, (*info).proc_steps_user
  (*info).proc_steps_user = ptr_new(*result.proc_steps)
  ptr_free, (*info).winno_l3_user
  (*info).winno_l3_user = ptr_new(winno_l3_result)
  (*info).state_l3_user = state_l3

  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_update_state_display, info

  FOR icol=1,2 DO BEGIN
    CASE icol OF
      1: BEGIN
        file_l3 = (*info).file_l3_official
        hdr_l3 = *(*info).hdr_l3_official
        state_l3 = (*info).state_l3_official
        winno_l3 = *(*info).winno_l3_official
      END
      2: BEGIN
        file_l3 = (*info).file_l3_user
        hdr_l3 = *(*info).hdr_l3_user
        state_l3 = (*info).state_l3_user
        winno_l3 = *(*info).winno_l3_user
      END
    ENDCASE

    FOR iwin=0,(*info).nwin-1 DO BEGIN
      title = ' - '
      status = 'NOT CREATED'
      editable = 0
      IF state_l3[iwin].l3_winno GE 0 THEN BEGIN
        title = fxpar(*hdr_l3[state_l3[iwin].l3_winno], 'PGEXTNAM', missing='PGEXTNAM keyword empty/missing')
        status = 'CREATED'
        IF state_l3[iwin].edited THEN status = status + ' and EDITED'
        editable = 1
      ENDIF
      widget_control, state_l3[iwin].title_label, get_value=title_old
      IF title_old NE title THEN widget_control, state_l3[iwin].title_label, set_value=title
      widget_control, state_l3[iwin].status_label, get_value=status_old
      IF status_old NE status THEN widget_control, state_l3[iwin].status_label, set_value=status
      IF widget_info(state_l3[iwin].edit_button, /sensitive) NE editable THEN $
        widget_control, state_l3[iwin].edit_button, sensitive=editable
      IF icol EQ 2 THEN widget_control, state_l3[iwin].edit_button, set_uvalue={l3_type:2, winno:state_l3[iwin].l3_winno}
    ENDFOR ; iwin=0,(*info).nwin-1
    savable = total(state_l3.edited) GT 0
    IF icol eq 2 && widget_info((*info).save_button_user, /sensitive) NE savable THEN $
      widget_control, (*info).save_button_user, sensitive=savable
    widget_control, (*info).dir_labels[icol], get_value=dir_name_old
    dir_name = (file_dirname(file_l3))[0]
    IF dir_name_old NE dir_name THEN widget_control, (*info).dir_labels[icol], set_value=dir_name
    base_name = (file_basename(file_l3))[0]
    widget_control, (*info).file_labels[icol], get_value=base_name_old
    IF base_name_old NE base_name THEN widget_control, (*info).file_labels[icol], set_value=base_name

  ENDFOR ; icol=1,ncolumn

end


pro spice_xcontrol_l23_open_l2, event
  widget_control, event.top, get_uvalue=info
  spice_xcontrol, (*info).object_l2
end


pro spice_xcontrol_l23_open_l3, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=win_info
  case win_info.l3_type of
    1: BEGIN
      ana_l3 = *(*info).ana_l3_official
      hdr_l3 = *(*info).hdr_l3_official
      hdr_l3_data = *(*info).hdr_l3_official_data
      title = 'L3 - official - ' + fxpar(*hdr_l3[win_info.winno], 'PGEXTNAM', missing='PGEXTNAM keyword empty/missing')
      state_l3 = (*info).state_l3_official
    END
    2: BEGIN
      ana_l3 = *(*info).ana_l3_user
      hdr_l3 = *(*info).hdr_l3_user
      hdr_l3_data = *(*info).hdr_l3_user_data
      title = 'L3 - user - ' + fxpar(*hdr_l3[win_info.winno], 'PGEXTNAM', missing='PGEXTNAM keyword empty/missing')
      state_l3 = (*info).state_l3_user
    END
  endcase
  ana = ana_l3[win_info.winno]
  origin = [0,0,0]
  scale = [1,1,1]
  phys_scale = [0,0,0]
  spice_data_l3.get_plot_variables, *hdr_l3_data[win_info.winno], origin=origin, scale=scale, phys_scale=phys_scale
  spice_xcfit_block, ana=ana, title=title, origin=origin, scale=scale, phys_scale=phys_scale, group_leader=(*info).tlb
  ana_l3[win_info.winno] = ana

  ind = where(state_l3.l3_winno eq win_info.winno, count)
  if count NE 1 then begin
    print, 'This should not happen. Contact prits-group@astro.uio.no'
    stop
    return
  endif
  state_l3[ind[0]].edited = 1

  CASE win_info.l3_type OF
    1: BEGIN
      FOR i=0,N_ELEMENTS(*(*info).ana_l3_official)-1 DO delete_analysis, (*(*info).ana_l3_official)[i]
      ptr_free, (*info).ana_l3_official
      (*info).ana_l3_official = ptr_new(ana_l3)
      (*info).state_l3_official = state_l3
    END
    2: BEGIN
      FOR i=0,N_ELEMENTS(*(*info).ana_l3_user)-1 DO delete_analysis, (*(*info).ana_l3_user)[i]
      ptr_free, (*info).ana_l3_user
      (*info).ana_l3_user = ptr_new(ana_l3)
      (*info).state_l3_user = state_l3
    END
  ENDCASE
  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_create_l3, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=win_info
  IF ~(*info).file_in_user_dir THEN top_dir = (*info).file_top_dir
  IF N_ELEMENTS(win_info) GT 1 THEN all_windows=1 ELSE all_windows=0
  result = spice_create_l3_widget( (*info).object_L2, event.top, window_index=win_info, $
    no_widget=all_windows, top_dir=top_dir, save_not=~all_windows, block_save=~all_windows)

  IF result.l3_file EQ 'Cancel' THEN return
  IF all_windows THEN spice_xcontrol_l23_update_state_replace, info, result $
  ELSE spice_xcontrol_l23_update_state_add, info, result
end


pro spice_xcontrol_l23_copy_window, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=win_info
  ana_l3 = *(*info).ana_l3_official
  hdr_l3 = *(*info).hdr_l3_official
  hdr_new = ptrarr(1)
  hdr_new[0] = ptr_new(*hdr_l3[win_info])
  hdr_l3_data = *(*info).hdr_l3_official_data
  hdr_new_data = ptrarr(1)
  hdr_new_data[0] = ptr_new(*hdr_l3_data[win_info])
  proc_step_l3 = *(*info).proc_steps_official
  pr_step_new = ptrarr(1)
  pr_step_new[0] = ptr_new(*proc_step_l3[win_info])
  result = {l3_file:'', $
    ana:ptr_new(ana_l3[win_info]), $
    result_headers:ptr_new(hdr_new), $
    data_headers:ptr_new(hdr_new_data), $
    proc_steps:ptr_new(pr_step_new), $
    file_saved:0b, user_dir:0b, top_dir:''}

  spice_xcontrol_l23_update_state_add, info, result
end



; -----------------------------------------------------------------------
; MAIN PROGRAM
; -----------------------------------------------------------------------

pro spice_xcontrol_l23, file, group_leader=group_leader

  prits_tools.parcheck, file, 1, "file", 'string', 0
  prits_tools.parcheck, group_leader, 0, "group_leader", 'integers', 0, /optional

  if n_params() lt 1 then begin
    message,'Usage: spice_xcontrol_l23, file [, group_leader=group_leader]',/cont
    ;return
  endif

  file_in = (file_search(file, /fully_qualify_path))[0]
  IF file_in EQ '' THEN BEGIN
    print, 'Cannot find file : ' + file
    return
  ENDIF
  file_info = spice_file2info(file_in)
  IF ~file_info.is_spice_file THEN BEGIN
    print, 'File is not a SPICE FITS file : ' + file
    return
  ENDIF

  file_top_dir=''
  file_in_user_dir=0

  CASE file_info.level OF
    2: BEGIN
      file_l2 = file_in
      file_l3_official = spice_data_l3.find_l3_file_from_l2(file_l2, /latest)
      file_l3_user = spice_data_l3.find_l3_file_from_l2(file_l2, /user_dir, /latest)
    ENDCASE

    3: BEGIN
      l3_obj = spice_data(file_in)
      file_l2 = l3_obj->find_l2_file()
      IF file_l2 EQ '' THEN file_l2 = l3_obj->find_l2_file(/user_dir)

      file_l3_official = ''
      file_l3_official_all = spice_find_file(file_info.datetime, level=3, remove_duplicates=0, count_file=count_file_official)
      FOR ifile=0,count_file_official-1 DO BEGIN
        IF file_l3_official_all[ifile] EQ file_in THEN BEGIN
          file_l3_official = file_l3_official_all[ifile]
          break
        ENDIF
      ENDFOR

      file_l3_user = ''
      file_l3_user_all = spice_find_file(file_info.datetime, level=3, remove_duplicates=0, count_file=count_file_user, /user_dir)
      FOR ifile=0,count_file_user-1 DO BEGIN
        IF file_l3_user_all[ifile] EQ file_in THEN BEGIN
          file_l3_user = file_l3_user_all[ifile]
          file_in_user_dir=1
          break
        ENDIF
      ENDFOR

      IF file_l3_official EQ '' && file_l3_user EQ '' THEN BEGIN
        file_l3_user = file_in
        file_top_dir = file_dirname(file_l3_user)
        file_top_dir = file_top_dir.split(path_sep())
        IF N_ELEMENTS(file_top_dir) GT 6 THEN BEGIN
          file_top_dir = file_top_dir[0:-5]
        ENDIF
        file_top_dir = file_top_dir.join(path_sep())
      ENDIF

      IF file_l3_user EQ '' && count_file_user GT 0 THEN BEGIN
        file_info_l3_user = spice_file2info(file_l3_user_all)
        max_version = max(file_info_l3_user.version, max_ind)
        file_l3_user = file_l3_user_all[max_ind]
      ENDIF

      IF file_l3_official EQ '' && count_file_official GT 0 THEN BEGIN
        file_info_l3_official = spice_file2info(file_l3_official_all)
        max_version = max(file_info_l3_official.version, max_ind)
        file_l3_official = file_l3_official_all[max_ind]
      ENDIF
    ENDCASE

    ELSE: BEGIN
      print, 'SPICE FITS file needs to be either level 2 or level 3 : ' + file
      return
    ENDCASE
  ENDCASE

  exist_l2 = file_l2 NE ''
  exist_l3_official = file_l3_official NE ''
  exist_l3_user = file_l3_user NE ''
  nwin = 0

  IF exist_l2 THEN BEGIN
    object_l2 = spice_data(file_l2)
    nwin = object_l2->get_number_windows()
    file_l3_calc = file_l2.replace('level2', 'level3')
    file_l3_calc = file_l3_calc.replace('_L2_','_L3_')
  ENDIF ELSE object_l2=0

  IF exist_l3_official THEN BEGIN
    ana_l3_official = fits2ana(file_l3_official, headers_results=hdr_l3_official, headers_data=hdr_l3_official_data)
    nwin_l3_official = fxpar(*hdr_l3_official[0], 'NWIN', missing=0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_official_data[0], 'NWIN', missing=0)
    if nwin eq 0 then nwin = nwin_l3_official
    winno_l3_official = intarr(nwin_l3_official)
    FOR iwin=0,nwin_l3_official-1 DO BEGIN
      winno_l3_official[iwin] = fxpar(*hdr_l3_official_data[iwin], 'WINNO', missing=-1)
    ENDFOR
    l3_obj = spice_data(file_l3_official)
    proc_steps_official = l3_obj->get_l3_processing_steps()
  ENDIF ELSE BEGIN
    IF exist_l2 THEN BEGIN
      file_l3_official = file_l3_calc
    ENDIF ELSE IF exist_l3_user THEN BEGIN
      file_l3_official = file_l3_user.replace(path_sep()+'user'+path_sep(), path_sep())
    ENDIF
    nwin_l3_official = 0
    ana_l3_official = 0
    hdr_l3_official = ptr_new(0)
    hdr_l3_official_data = ptr_new(0)
    winno_l3_official = -1
    proc_steps_official = 0
  ENDELSE

  IF exist_l3_user THEN BEGIN
    ana_l3_user = fits2ana(file_l3_user, headers_results=hdr_l3_user, headers_data=hdr_l3_user_data)
    nwin_l3_user = fxpar(*hdr_l3_user[0], 'NWIN', missing=0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_user_data[0], 'NWIN', missing=0)
    if nwin eq 0 then nwin = nwin_l3_user
    winno_l3_user = intarr(nwin_l3_user)
    FOR iwin=0,nwin_l3_user-1 DO BEGIN
      winno_l3_user[iwin] = fxpar(*hdr_l3_user_data[iwin], 'WINNO', missing=-1)
    ENDFOR
    l3_obj = spice_data(file_l3_user)
    proc_steps_user = l3_obj->get_l3_processing_steps()
  ENDIF ELSE BEGIN
    old_path_part = path_sep()+'spice'+path_sep()+'level3'+path_sep()
    new_path_part = path_sep()+'spice'+path_sep()+'user'+path_sep()+'level3'+path_sep()
    IF exist_l2 THEN BEGIN
      file_l3_user = file_l3_calc.replace(old_path_part, new_path_part)
    ENDIF ELSE IF exist_l3_official THEN BEGIN
      file_l3_user = file_l3_user.replace(old_path_part, new_path_part)
    ENDIF
    nwin_l3_user = 0
    ana_l3_user = 0
    hdr_l3_user = ptr_new(0)
    hdr_l3_user_data = ptr_new(0)
    winno_l3_user = -1
    proc_steps_user = 0
  ENDELSE



  ; WIDGETS

  dir_labels = lonarr(4)
  file_labels = lonarr(4)

  tlb = widget_base(/column, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, group_leader=group_leader, /tlb_kill_request_events)

  win_base = widget_base(tlb, /grid_layout, column=3, /frame)


  ; Column 0 - Level 2 file

  base_l2 = widget_base(win_base, /column, /frame)
  label = widget_label(base_l2, value='LEVEL 2', /align_center)
  dir_labels[0] = widget_label(base_l2, value=(file_dirname(file_l2))[0], /DYNAMIC_RESIZE, /align_center)
  file_labels[0] = widget_label(base_l2, value=(file_basename(file_l2))[0], /DYNAMIC_RESIZE, /align_center)
  button = widget_button(base_l2, value='Open file', event_pro='spice_xcontrol_l23_open_l2', $
    sensitive=exist_l2)

  FOR iwin=0,nwin-1 DO BEGIN
    win_base_l2 = widget_base(win_base, /column, /frame)
    IF exist_l2 THEN BEGIN
      label = widget_label(win_base_l2, value=object_l2->get_window_id(iwin), /align_left)
    ENDIF
  ENDFOR ; iwin=0,nwin-1
  dummy_label = widget_label(win_base, value='')


  ; Column 1 - Level 3 - official file

  base_l3_official = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_official, value='LEVEL 3 - official', /align_center)
  dir_labels[1] = widget_label(base_l3_official, value=(file_dirname(file_l3_official))[0], /DYNAMIC_RESIZE, /align_center)
  file_labels[1] = widget_label(base_l3_official, value=(file_basename(file_l3_official))[0], /DYNAMIC_RESIZE, /align_center)

  state_l3_official = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  FOR iwin=0,nwin-1 DO BEGIN
    win_base_l3_official = widget_base(win_base, /column, /frame)
    win_created = 0
    title = ' - '
    status = 'NOT CREATED'
    IF exist_l3_official THEN BEGIN
      ind = where(winno_l3_official eq iwin, count)
      IF count GT 0 THEN BEGIN
        win_created = 1
        state_l3_official[iwin].l3_winno=ind[0]
        title = fxpar(*hdr_l3_official[ind[0]], 'PGEXTNAM', missing='PGEXTNAM keyword empty/missing')
        status = 'CREATED'
      ENDIF
    ENDIF
    state_l3_official[iwin].title_label = widget_label(win_base_l3_official, value=title, /DYNAMIC_RESIZE, /align_left)
    state_l3_official[iwin].status_label = widget_label(win_base_l3_official, value=status, /DYNAMIC_RESIZE, /align_left)
    button_base = widget_base(win_base_l3_official, /row)
    state_l3_official[iwin].edit_button = widget_button(button_base, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      sensitive=win_created, uvalue={l3_type:1, winno:state_l3_official[iwin].l3_winno})
    copy_button = widget_button(button_base, value='Copy window to user file', event_pro='spice_xcontrol_l23_copy_window', $
      sensitive=win_created, uvalue=state_l3_official[iwin].l3_winno)
  ENDFOR ; iwin=0,nwin-1
  dummy_label = widget_label(win_base, value='')


  ; Column 2 - Level 3 - user file

  base_l3_user = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_user, value='LEVEL 3 - user')
  dir_labels[2] = widget_label(base_l3_user, value=(file_dirname(file_l3_user))[0], /DYNAMIC_RESIZE, /align_center)
  file_labels[2] = widget_label(base_l3_user, value=(file_basename(file_l3_user))[0], /DYNAMIC_RESIZE, /align_center)
  button = widget_button(base_l3_user, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=exist_l2, uvalue=indgen(nwin))

  state_l3_user = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  FOR iwin=0,nwin-1 DO BEGIN
    win_base_l3_user = widget_base(win_base, /column, /frame)
    win_created = 0
    title = ' - '
    status = 'NOT CREATED'
    IF exist_l3_user THEN BEGIN
      ind = where(winno_l3_user eq iwin, count)
      IF count GT 0 THEN BEGIN
        win_created = 1
        state_l3_user[iwin].l3_winno=ind[0]
        title = fxpar(*hdr_l3_user[ind[0]], 'PGEXTNAM', missing='PGEXTNAM keyword empty/missing')
        status = 'CREATED'
      ENDIF
    ENDIF
    state_l3_user[iwin].title_label = widget_label(win_base_l3_user, value=title, /DYNAMIC_RESIZE, /align_left)
    state_l3_user[iwin].status_label = widget_label(win_base_l3_user, value=status, /DYNAMIC_RESIZE, /align_left)
    button_base = widget_base(win_base_l3_user, /row)
    state_l3_user[iwin].edit_button = widget_button(button_base, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      sensitive=win_created, uvalue={l3_type:2, winno:state_l3_user[iwin].l3_winno})
    create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue=iwin)
  ENDFOR ; iwin=0,nwin-1

  save_button_user = widget_button(win_base, value='Save File', sensitive=0, event_pro='spice_xcontrol_l23_save_file')


  ; Define the info structure, used to send information around
  info= { $
    tlb:tlb, $
    file_in:file_in, $
    dir_labels:dir_labels, $
    file_labels:file_labels, $
    exist_l2:exist_l2, $
    exist_l3_official:exist_l3_official, $
    exist_l3_user:exist_l3_user, $
    file_l2:file_l2, $
    file_l3_official:file_l3_official, $
    file_l3_user:file_l3_user, $
    file_in_user_dir:file_in_user_dir, $
    file_top_dir:file_top_dir, $
    object_l2:object_l2, $
    winno_l3_official:ptr_new(winno_l3_official), $
    winno_l3_user:ptr_new(winno_l3_user), $
    ana_l3_official:ptr_new(ana_l3_official), $
    ana_l3_user:ptr_new(ana_l3_user), $
    nwin:nwin, $
    nwin_l3_official:nwin_l3_official, $
    nwin_l3_user:nwin_l3_user, $
    hdr_l3_official:ptr_new(hdr_l3_official), $
    hdr_l3_official_data:ptr_new(hdr_l3_official_data), $
    hdr_l3_user:ptr_new(hdr_l3_user), $
    hdr_l3_user_data:ptr_new(hdr_l3_user_data), $
    proc_steps_official:ptr_new(proc_steps_official), $
    proc_steps_user:ptr_new(proc_steps_user), $
    state_l3_official:state_l3_official, $
    state_l3_user:state_l3_user, $
    save_button_user:save_button_user $
  }
  info=ptr_new(info,/no_copy)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  wp = widget_positioner(tlb, parent=group_leader)
  wp->position

  xmanager, 'spice_xcontrol_l23', tlb, /no_block, $
    group_leader=group_leader, cleanup='spice_xcontrol_l23_cleanup'

end
