;+
; NAME:
;       SPICE_XCONTROL_L23
;
; PURPOSE:
; SPICE_XCONTROL_L23 is a GUI utility to view, edit or create level 3 SPICE FITS files, along with the corresponding
; level 2 file. The tool takes as input a level 2 or a level 3 file, and then displays 3 or 4 columns (see show_other).
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
;       spice_xcontrol_l23, file [, group_leader = group] [, /show_other]
;
; INPUTS:
; file: A SPICE file either level 2 or level 3
;
; KEYWORD PARAMETERS:
; group_leader: Widget ID of parent widget
; show_other: If set, a fourth column will be displayed, in which level 3 files are shown
;             that are saved in a user-defined location, i.e. not in $SPICE_DATA/level3/ nor in
;             $SPICE_DATA/user/level3/. This column is shown automatically, if the input
;             file is of level 3 and in a user-defined location.
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
;     18-Aug-2020: First version by Martin Wiesmann
;
; $Id: 2022-09-05 14:15 CEST $
;-
;
;
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
  ptr_free,(*info).winno_l3_other
  ptr_free,(*info).ana_l3_official
  ptr_free,(*info).ana_l3_user
  ptr_free,(*info).ana_l3_other
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_official)-1 DO ptr_free, (*(*info).hdr_l3_official)[i]
  ptr_free,(*info).hdr_l3_official
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_user)-1 DO ptr_free, (*(*info).hdr_l3_user)[i]
  ptr_free,(*info).hdr_l3_user
  FOR i=0,N_ELEMENTS(*(*info).hdr_l3_other)-1 DO ptr_free, (*(*info).hdr_l3_other)[i]
  ptr_free,(*info).hdr_l3_other
  ptr_free,info
end

pro spice_xcontrol_l23_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, get_uvalue=info
    IF total((*info).state_l3_official.edited) GT 0 || $
      total((*info).state_l3_user.edited) GT 0 || $
      total((*info).state_l3_other.edited) GT 0 THEN BEGIN
      result = dialog_message(['WARNING: Possibly UNSAVED changes.', $
        'This warning also shows up, even if you only looked at level 3 data',$
        'without changing anything.', $
        'Do you really want to exit?'], /question, /default_no, title='WARNING: Possibly UNSAVED changes.', /center)
      IF result EQ 'No' THEN return
    ENDIF
    widget_control, event.top, /destroy
  endif
end


pro spice_xcontrol_l23_save_file, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=file_info
  IF size((*info).object_l2, /type) NE 11 THEN BEGIN
    result = dialog_message(['Saving of level 3 SPICE FITS files is not (yet) supported', $
      'when the corresponding level 2 file is not available.', $
      'Contact martin.wiesmann@astro.uio.no if you need this feature'])
    ; TODO
    return
  ENDIF
  case file_info.l3_type of
    1: BEGIN
      nwin_l3 = (*info).nwin_l3_official
      winno_l3 = *(*info).winno_l3_official
      ana_l3 = *(*info).ana_l3_official
      ;hdr_l3 = *(*info).hdr_l3_official
      file_l3 = (*info).file_l3_official
    END
    2: BEGIN
      nwin_l3 = (*info).nwin_l3_user
      winno_l3 = *(*info).winno_l3_user
      ana_l3 = *(*info).ana_l3_user
      ;hdr_l3 = *(*info).hdr_l3_user
      file_l3 = (*info).file_l3_user
    END
    3: BEGIN
      nwin_l3 = (*info).nwin_l3_other
      winno_l3 = *(*info).winno_l3_other
      ana_l3 = *(*info).ana_l3_other
      ;hdr_l3 = *(*info).hdr_l3_other
      file_l3 = (*info).file_l3_other
    END
  endcase
  IF file_exist(file_l3) THEN BEGIN
    result = dialog_message(['This file already exists.',file_l3,'Do you want to overwrite it?'], $
      /question, /default_no, title='File exists. Overwrite?',/center)
    IF result EQ 'No' THEN return
    file_old = 1
    file_move, file_l3, file_l3+'.old'
  ENDIF ELSE file_old = 0

  all_result_headers = ptrarr(nwin_l3)
  FOR iwindow=0,nwin_l3-1 DO BEGIN

    original_data = (*info).object_l2->get_window_data(winno_l3[iwindow], no_masking=no_masking, approximated_slit=approximated_slit)

    if iwindow gt 0 then extension=1 else extension=0
    headers = spice_ana2fitshdr(ana_l3[iwindow], header_l2=(*info).object_l2->get_header(winno_l3[iwindow]), $
      extension=extension, filename_l3=filename_l3, n_windows=nwin_l3, winno=iwindow, $
      HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
      FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
      CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
      DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, $
      original_data=original_data)

    writefits, file_l3, RESULT, *headers[0], append=extension
    writefits, file_l3, original_data, *headers[1], /append
    writefits, file_l3, LAMBDA, *headers[2], /append
    writefits, file_l3, RESIDUAL, *headers[3], /append
    writefits, file_l3, WEIGHTS, *headers[4], /append
    writefits, file_l3, INCLUDE, *headers[5], /append
    writefits, file_l3, CONST, *headers[6], /append

    all_result_headers[iwindow] = ptr_new(*headers[0])

  ENDFOR ; iwin=0,nwin_l3-1

  IF file_old THEN file_delete, file_l3+'.old'

  CASE file_info.l3_type OF
    1: BEGIN
      ptr_free, (*info).hdr_l3_official
      (*info).hdr_l3_official = ptr_new(all_result_headers)
      (*info).state_l3_official.edited = 0
    END
    2: BEGIN
      ptr_free, (*info).hdr_l3_user
      (*info).hdr_l3_user = ptr_new(all_result_headers)
      (*info).state_l3_user.edited = 0
    END
    3: BEGIN
      ptr_free, (*info).hdr_l3_other
      (*info).hdr_l3_other = ptr_new(all_result_headers)
      (*info).state_l3_other.edited = 0
    END
  ENDCASE
  spice_xcontrol_l23_update_state_display, info
end


; Add or replace new results
; This happens if the user clicks on one of the '(Re)create window' buttons
pro spice_xcontrol_l23_update_state_add, info, result, all_windows=all_windows
  IF result.top_dir EQ '' THEN BEGIN
    IF result.user_dir THEN BEGIN
      win_type = 2
      ana_l3 = *(*info).ana_l3_user
      hdr_l3 = *(*info).hdr_l3_user
      state_l3 = (*info).state_l3_user
      winno_l3 = *(*info).winno_l3_user
    ENDIF ELSE BEGIN
      win_type = 1
      ana_l3 = *(*info).ana_l3_official
      hdr_l3 = *(*info).hdr_l3_official
      state_l3 = (*info).state_l3_official
      winno_l3 = *(*info).winno_l3_official
    ENDELSE
  ENDIF ELSE BEGIN
    win_type = 3
    ana_l3 = *(*info).ana_l3_other
    hdr_l3 = *(*info).hdr_l3_other
    state_l3 = (*info).state_l3_other
    winno_l3 = *(*info).winno_l3_other
  ENDELSE

  nwin_l3_result = N_ELEMENTS(*result.ana)
  winno_l3_result = intarr(nwin_l3_result)
  FOR iwin=0,nwin_l3_result-1 DO BEGIN
    winno_l3_result[iwin] = fxpar(*(*result.RESULT_HEADERS)[iwin], 'L2WINNO', -1)
  ENDFOR

  nwin_l3 = 0
  FOR iwin=0,(*info).nwin-1 DO BEGIN
    ind_result = where(winno_l3_result eq iwin, count_result)
    ind_old = where(winno_l3 eq iwin, count_old)
    IF count_result GT 0 THEN BEGIN
      state_l3[iwin].l3_winno = ind_result[0]
      state_l3[iwin].edited = ~result.file_saved
      spice_xcontrol_l23_add_window, ana_l3_new, (*result.ana)[ind_result[0]], $
        hdr_l3_new, *(*result.result_headers)[ind_result[0]], winno_l3_new, iwin
      nwin_l3++
    ENDIF ELSE IF count_old GT 0 THEN BEGIN
      state_l3[iwin].l3_winno = ind_old[0]
      spice_xcontrol_l23_add_window, ana_l3_new, ana_l3[ind_old[0]], $
        hdr_l3_new, *hdr_l3[ind_old[0]], winno_l3_new, iwin
      nwin_l3++
    ENDIF ELSE BEGIN
      state_l3[iwin].l3_winno = -1
      state_l3[iwin].edited = 0
    ENDELSE
  ENDFOR ; iwin=0,(*info).nwin-1

  CASE win_type OF
    1: BEGIN
      (*info).nwin_l3_official = nwin_l3
      ptr_free, (*info).ana_l3_official
      (*info).ana_l3_official = ptr_new(ana_l3_new)
      ptr_free, (*info).hdr_l3_official
      (*info).hdr_l3_official = ptr_new(hdr_l3_new)
      ptr_free, (*info).winno_l3_official
      (*info).winno_l3_official = ptr_new(winno_l3_new)
      (*info).state_l3_official = state_l3
    END
    2: BEGIN
      (*info).nwin_l3_user = nwin_l3
      ptr_free, (*info).ana_l3_user
      (*info).ana_l3_user = ptr_new(ana_l3_new)
      ptr_free, (*info).hdr_l3_user
      (*info).hdr_l3_user = ptr_new(hdr_l3_new)
      ptr_free, (*info).winno_l3_user
      (*info).winno_l3_user = ptr_new(winno_l3_new)
      (*info).state_l3_user = state_l3
    END
    3: BEGIN
      (*info).nwin_l3_other = nwin_l3
      ptr_free, (*info).ana_l3_other
      (*info).ana_l3_other = ptr_new(ana_l3_new)
      ptr_free, (*info).hdr_l3_other
      (*info).hdr_l3_other = ptr_new(hdr_l3_new)
      ptr_free, (*info).winno_l3_other
      (*info).winno_l3_other = ptr_new(winno_l3_new)
      (*info).state_l3_other = state_l3
    END
  ENDCASE

  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_add_window, ana_l3_new, new_ana, hdr_l3_new, new_hdr, winno_l3_new, new_winno
  IF N_ELEMENTS(ana_l3_new) EQ 0 THEN BEGIN
    ana_l3_new = new_ana
    hdr_l3_new = ptr_new(new_hdr)
    winno_l3_new = new_winno
  ENDIF ELSE BEGIN
    ana_l3_new = [ana_l3_new, new_ana]
    hdr_l3_new = [hdr_l3_new, ptr_new(new_hdr)]
    winno_l3_new = [winno_l3_new, new_winno]
  ENDELSE
end


; Replace previous results with the new ones.
; This happens if the user clicks on upper most '(Re)create file' button
pro spice_xcontrol_l23_update_state_replace, info, result

  IF result.top_dir EQ '' THEN BEGIN
    IF result.user_dir THEN BEGIN
      win_type = 2
      state_l3 = (*info).state_l3_user
    ENDIF ELSE BEGIN
      win_type = 1
      state_l3 = (*info).state_l3_official
    ENDELSE
  ENDIF ELSE BEGIN
    win_type = 3
    state_l3 = (*info).state_l3_other
  ENDELSE

  nwin_l3_result = fxpar(*(*result.RESULT_HEADERS)[0], 'NWIN', 0)
  winno_l3_result = intarr(nwin_l3_result)
  FOR iwin=0,nwin_l3_result-1 DO BEGIN
    winno_l3_result[iwin] = fxpar(*(*result.RESULT_HEADERS)[iwin], 'L2WINNO', -1)
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

  CASE win_type OF
    1: BEGIN
      (*info).file_l3_official = result.l3_file
      (*info).nwin_l3_official = nwin_l3_result
      ptr_free, (*info).ana_l3_official
      (*info).ana_l3_official = ptr_new(*result.ana)
      ptr_free, (*info).hdr_l3_official
      (*info).hdr_l3_official = ptr_new(*result.result_headers)
      ptr_free, (*info).winno_l3_official
      (*info).winno_l3_official = ptr_new(winno_l3_result)
      (*info).state_l3_official = state_l3
    END
    2: BEGIN
      (*info).file_l3_user = result.l3_file
      (*info).nwin_l3_user = nwin_l3_result
      ptr_free, (*info).ana_l3_user
      (*info).ana_l3_user = ptr_new(*result.ana)
      ptr_free, (*info).hdr_l3_user
      (*info).hdr_l3_user = ptr_new(*result.result_headers)
      ptr_free, (*info).winno_l3_user
      (*info).winno_l3_user = ptr_new(winno_l3_result)
      (*info).state_l3_user = state_l3
    END
    3: BEGIN
      (*info).file_l3_other = result.l3_file
      (*info).nwin_l3_other = nwin_l3_result
      ptr_free, (*info).ana_l3_other
      (*info).ana_l3_other = ptr_new(*result.ana)
      ptr_free, (*info).hdr_l3_other
      (*info).hdr_l3_other = ptr_new(*result.result_headers)
      ptr_free, (*info).winno_l3_other
      (*info).winno_l3_other = ptr_new(winno_l3_result)
      (*info).state_l3_other = state_l3
    END
  ENDCASE

  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_update_state_display, info

  IF (*info).save_button_other EQ 0 THEN ncolumn=2 ELSE ncomlumn=3
  FOR icol=1,ncolumn DO BEGIN
    CASE icol OF
      1: BEGIN
        file_l3 = (*info).file_l3_official
        hdr_l3 = *(*info).hdr_l3_official
        state_l3 = (*info).state_l3_official
        winno_l3 = *(*info).winno_l3_official
        save_button = (*info).save_button_official
      END
      2: BEGIN
        hdr_l3 = *(*info).hdr_l3_user
        state_l3 = (*info).state_l3_user
        winno_l3 = *(*info).winno_l3_user
        save_button = (*info).save_button_user
      END
      3: BEGIN
        hdr_l3 = *(*info).hdr_l3_other
        state_l3 = (*info).state_l3_other
        winno_l3 = *(*info).winno_l3_other
        save_button = (*info).save_button_other
      END
    ENDCASE

    FOR iwin=0,(*info).nwin-1 DO BEGIN
      title = ' - '
      status = 'NOT CREATED'
      editable = 0
      IF state_l3[iwin].l3_winno GE 0 THEN BEGIN
        title = fxpar(*hdr_l3[state_l3[iwin].l3_winno], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
        status = 'CREATED'
        IF state_l3[iwin].edited THEN status = status + ' and EDITED'
        editable = 1
      ENDIF
      widget_control, state_l3[iwin].title_label, set_value=title
      widget_control, state_l3[iwin].status_label, set_value=status
      widget_control, state_l3[iwin].edit_button, sensitive=editable
    ENDFOR ; iwin=0,(*info).nwin-1
    widget_control, save_button, sensitive=total(state_l3.edited) GT 0
    widget_control, (*info).dir_labels[icol], set_value=(file_dirname(file_l3))[0]
    widget_control, (*info).file_labels[icol], set_value=(file_basename(file_l3))[0]

  ENDFOR ; icol=1,ncolumn

end


pro spice_xcontrol_l23_open_l2, event
  widget_control, event.top, get_uvalue=info
  spice_xcontrol, (*info).object_l2
end


pro spice_xcontrol_l23_open_l3, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=file_info
  case file_info.l3_type of
    1: BEGIN
      ana_l3 = *(*info).ana_l3_official
      hdr_l3 = *(*info).hdr_l3_official
      title = 'L3 - official - ' + fxpar(*hdr_l3[file_info.winno], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
      state_l3 = (*info).state_l3_official
    END
    2: BEGIN
      ana_l3 = *(*info).ana_l3_user
      hdr_l3 = *(*info).hdr_l3_user
      title = 'L3 - user - ' + fxpar(*hdr_l3[file_info.winno], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
      state_l3 = (*info).state_l3_user
    END
    3: BEGIN
      ana_l3 = *(*info).ana_l3_other
      hdr_l3 = *(*info).hdr_l3_other
      title = 'L3 - other - ' + fxpar(*hdr_l3[file_info.winno], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
      state_l3 = (*info).state_l3_other
    END
  endcase
  ana = ana_l3[file_info.winno]
  xcfit_block, ana=ana, title=title
  ana_l3[file_info.winno] = ana

  ind = where(state_l3.l3_winno eq file_info.winno, count)
  if count NE 1 then begin
    print, 'This should not happen. Contact martin.wiesmann@astro.uio.no'
    stop
    return
  endif
  state_l3[ind[0]].edited = 1

  CASE file_info.l3_type OF
    1: BEGIN
      ptr_free, (*info).ana_l3_official
      (*info).ana_l3_official = ptr_new(ana_l3)
      (*info).state_l3_official = state_l3
    END
    2: BEGIN
      ptr_free, (*info).ana_l3_user
      (*info).ana_l3_user = ptr_new(ana_l3)
      (*info).state_l3_user = state_l3
    END
    3: BEGIN
      ptr_free, (*info).ana_l3_other
      (*info).ana_l3_other = ptr_new(ana_l3a)
      (*info).state_l3_other = state_l3
    END
  ENDCASE
  spice_xcontrol_l23_update_state_display, info
end


pro spice_xcontrol_l23_create_l3, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=win_info
  case win_info.l3_type of
    1: official_l3dir = 1
    2:
    3: top_dir = (file_dirname((*info).file_l3_other))[0]
  endcase

  IF N_ELEMENTS(win_info.winno) GT 1 THEN all_windows=1 ELSE all_windows=0
  result = spice_create_l3_widget( (*info).object_L2, event.top, window_index=win_info.winno, $
    no_widget=all_windows, official_l3dir=official_l3dir, top_dir=top_dir, save_not=~all_windows, block_save=~all_windows)

  IF result.l3_file EQ 'Cancel' THEN return
  IF all_windows THEN spice_xcontrol_l23_update_state_replace, info, result $
  ELSE spice_xcontrol_l23_update_state_add, info, result
end



; -----------------------------------------------------------------------
; MAIN PROGRAM
; -----------------------------------------------------------------------

pro spice_xcontrol_l23, file, group_leader=group_leader, show_other=show_other

  if n_params() lt 1 then begin
    message,'spice_xcontrol_l23, file [, group_leader=group_leader]',/cont
    file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
    ;file = '/Users/mawiesma/data/spice/level2/2022/03/26/solo_L2_spice-n-ras_20220326T031318_V01_100663899-000.fits'
    ;  return
  endif

  file_in = (file_search(file, /fully_qualify_path))[0]
  IF file_in EQ '' THEN BEGIN
    print, 'Cannot find file ' + file
    return
  ENDIF
  file_info = spice_file2info(file_in)
  IF ~file_info.is_spice_file THEN return

  help, file_info
  file_l2 = spice_find_file(file_info.datetime)
  IF file_l2 NE '' THEN BEGIN
    file_l2_info = spice_file2info(file_l2)
    IF file_l2_info.version NE file_info.version || $
      file_l2_info.spiobsid NE file_info.spiobsid || $
      file_l2_info.rasterno NE file_info.rasterno THEN file_l2 = ''
  ENDIF
  print,'file_l2           ',file_l2
  file_l3_official = spice_find_file(file_info.datetime, level=3)
  IF file_l3_official NE '' THEN BEGIN
    file_l3_official_info = spice_file2info(file_l3_official)
    IF file_l3_official_info.version NE file_info.version || $
      file_l3_official_info.spiobsid NE file_info.spiobsid || $
      file_l3_official_info.rasterno NE file_info.rasterno THEN file_l3_official = ''
  ENDIF
  print,'file_l3_official  ',file_l3_official
  file_l3_user = spice_find_file(file_info.datetime, /user, level=3)
  IF file_l3_user NE '' THEN BEGIN
    file_l3_user_info = spice_file2info(file_l3_user)
    IF file_l3_user_info.version NE file_info.version || $
      file_l3_user_info.spiobsid NE file_info.spiobsid || $
      file_l3_user_info.rasterno NE file_info.rasterno THEN file_l3_user = ''
  ENDIF
  print,'file_l3_user      ',file_l3_user
  file_l3_other = ''
  CASE file_info.level OF
    2: BEGIN
      file_l2 = file_in
    END
    3: BEGIN
      IF ((file_l3_officical NE '' && file_in NE file_l3_official) $
        && (file_l3_user NE '' && file_in NE file_l3_user)) $
        || (file_l3_officical EQ '' && file_l3_user EQ '') THEN file_l3_other = file_in
    END
    ELSE: BEGIN
      print, 'SPICE FITS file needs to be either level 2 or level 3.'
      return
    END
  ENDCASE
  print,'file_l3_other      ',file_l3_other

  exist_l2 = file_l2 NE ''
  exist_l3_official = file_l3_official NE ''
  exist_l3_user = file_l3_user NE ''
  exist_l3_other = file_l3_other NE ''
  nwin = 0

  IF exist_l2 THEN BEGIN
    object_l2 = spice_data(file_l2)
    nwin = object_l2->get_number_windows()
    file_l3_calc = file_l2.replace('level2', 'level3')
    file_l3_calc = file_l3_calc.replace('_L2_','_L3_')
  ENDIF ELSE object_l2=0

  IF exist_l3_official THEN BEGIN
    ana_l3_official = fits2ana(file_l3_official, headers_results=hdr_l3_official)
    nwin_l3_official = fxpar(*hdr_l3_official[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_official[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_official
    winno_l3_official = intarr(nwin_l3_official)
    FOR iwin=0,nwin_l3_official-1 DO BEGIN
      winno_l3_official[iwin] = fxpar(*hdr_l3_official[iwin], 'L2WINNO', -1)
    ENDFOR
  ENDIF ELSE BEGIN
    IF exist_l2 THEN BEGIN
      file_l3_official = file_l3_calc
    ENDIF ELSE IF exist_l3_user THEN BEGIN
      file_l3_official = file_l3_user.replace(path_sep()+'user'+path_sep(), path_sep())
    ENDIF
    nwin_l3_official = 0
    ana_l3_official = 0
    hdr_l3_official = ptr_new(0)
    winno_l3_official = -1
  ENDELSE

  IF exist_l3_user THEN BEGIN
    ana_l3_user = fits2ana(file_l3_user, headers_results=hdr_l3_user)
    nwin_l3_user = fxpar(*hdr_l3_user[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_user[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_user
    winno_l3_user = intarr(nwin_l3_user)
    FOR iwin=0,nwin_l3_user-1 DO BEGIN
      winno_l3_user[iwin] = fxpar(*hdr_l3_user[iwin], 'L2WINNO', -1)
    ENDFOR
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
    winno_l3_user = -1
  ENDELSE

  IF exist_l3_other THEN BEGIN
    ana_l3_other = fits2ana(file_l3_other, headers_results=hdr_l3_other)
    nwin_l3_other = fxpar(*hdr_l3_other[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_other[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_other
    winno_l3_other = intarr(nwin_l3_other)
    FOR iwin=0,nwin_l3_other-1 DO BEGIN
      winno_l3_other[iwin] = fxpar(*hdr_l3_other[iwin], 'L2WINNO', -1)
    ENDFOR
  ENDIF ELSE BEGIN
    cd, current=new_path
    IF exist_l2 THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_calc))[0]) $
    ELSE IF exist_l3_official THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_official))[0]) $
    ELSE IF exist_l3_user THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_user))[0])
    nwin_l3_other = 0
    ana_l3_other = 0
    hdr_l3_other = ptr_new(0)
    winno_l3_other = -1
  ENDELSE



  ; WIDGETS

  dir_labels = lonarr(4)
  file_labels = lonarr(4)

  tlb = widget_base(/column, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)

  IF keyword_set(show_other) || exist_l3_other THEN column=4 ELSE column=3
  win_base = widget_base(tlb, /grid_layout, column=column, /frame)


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

  label = widget_label(win_base, value='')


  ; Column 1 - Level 3 - official file

  base_l3_official = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_official, value='LEVEL 3 - official', /align_center)
  dir_labels[1] = widget_label(base_l3_official, value=(file_dirname(file_l3_official))[0], /DYNAMIC_RESIZE, /align_center)
  file_labels[1] = widget_label(base_l3_official, value=(file_basename(file_l3_official))[0], /DYNAMIC_RESIZE, /align_center)
  button = widget_button(base_l3_official, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=exist_l2, uvalue={l3_type:1, winno:indgen(nwin)})

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
        title = fxpar(*hdr_l3_official[ind[0]], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
        status = 'CREATED'
      ENDIF
    ENDIF
    state_l3_official[iwin].title_label = widget_label(win_base_l3_official, value=title, /DYNAMIC_RESIZE, /align_left)
    state_l3_official[iwin].status_label = widget_label(win_base_l3_official, value=status, /DYNAMIC_RESIZE, /align_left)
    button_base = widget_base(win_base_l3_official, /row)
    state_l3_official[iwin].edit_button = widget_button(button_base, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      sensitive=win_created, uvalue={l3_type:1, winno:state_l3_official[iwin].l3_winno})
    create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:1, winno:iwin})
  ENDFOR ; iwin=0,nwin-1

  save_button_official = widget_button(win_base, value='Save File', sensitive=0, event_pro='spice_xcontrol_l23_save_file', $
    uvalue={l3_type:1})


  ; Column 2 - Level 3 - user file

  base_l3_user = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_user, value='LEVEL 3 - user')
  dir_labels[2] = widget_label(base_l3_user, value=(file_dirname(file_l3_user))[0], /DYNAMIC_RESIZE, /align_center)
  file_labels[2] = widget_label(base_l3_user, value=(file_basename(file_l3_user))[0], /DYNAMIC_RESIZE, /align_center)
  button = widget_button(base_l3_user, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=exist_l2, uvalue={l3_type:2, winno:indgen(nwin)})

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
        title = fxpar(*hdr_l3_user[ind[0]], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
        status = 'CREATED'
      ENDIF
    ENDIF
    state_l3_user[iwin].title_label = widget_label(win_base_l3_user, value=title, /DYNAMIC_RESIZE, /align_left)
    state_l3_user[iwin].status_label = widget_label(win_base_l3_user, value=status, /DYNAMIC_RESIZE, /align_left)
    button_base = widget_base(win_base_l3_user, /row)
    state_l3_user[iwin].edit_button = widget_button(button_base, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      sensitive=win_created, uvalue={l3_type:2, winno:state_l3_user[iwin].l3_winno})
    create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:2, winno:iwin})
  ENDFOR ; iwin=0,nwin-1

  save_button_user = widget_button(win_base, value='Save File', sensitive=0, event_pro='spice_xcontrol_l23_save_file', $
    uvalue={l3_type:2})


  ; Column 3 - Level 3 - other file

  state_l3_other = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  IF keyword_set(show_other) || exist_l3_other THEN BEGIN

    base_l3_other = widget_base(win_base, /column, /frame)
    label = widget_label(base_l3_other, value='LEVEL 3 - other')
    dir_labels[3] = widget_label(base_l3_other, value=(file_dirname(file_l3_other))[0], /DYNAMIC_RESIZE, /align_center)
    file_labels[3] = widget_label(base_l3_other, value=(file_basename(file_l3_other))[0], /DYNAMIC_RESIZE, /align_center)
    button = widget_button(base_l3_other, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:3, winno:indgen(nwin)})

    FOR iwin=0,nwin-1 DO BEGIN
      win_base_l3_other = widget_base(win_base, /column, /frame)
      win_created = 0
      title = ' - '
      status = 'NOT CREATED'
      IF exist_l3_other THEN BEGIN
        ind = where(winno_l3_other eq iwin, count)
        IF count GT 0 THEN BEGIN
          win_created = 1
          state_l3_other[iwin].l3_winno=ind[0]
          title = fxpar(*hdr_l3_other[ind[0]], 'L2EXTNAM', 'L2EXTNAM keyword empty/missing')
          status = 'CREATED'
        ENDIF
      ENDIF
      state_l3_other[iwin].title_label = widget_label(win_base_l3_other, value=title, /DYNAMIC_RESIZE, /align_left)
      state_l3_other[iwin].status_label = widget_label(win_base_l3_other, value=status, /DYNAMIC_RESIZE, /align_left)
      button_base = widget_base(win_base_l3_other, /row)
      state_l3_other[iwin].edit_button = widget_button(button_base, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
        sensitive=win_created, uvalue={l3_type:3, winno:state_l3_other[iwin].l3_winno})
      create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
        sensitive=exist_l2, uvalue={l3_type:3, winno:iwin})
    ENDFOR ; iwin=0,nwin-1

    save_button_other = widget_button(win_base, value='Save File', sensitive=0, event_pro='spice_xcontrol_l23_save_file', $
      uvalue={l3_type:3})

  ENDIF ELSE BEGIN ; keyword_set(show_other) || exist_l3_other
    save_button_other = 0
  ENDELSE


  ; Define the info structure, used to send information around
  info= { $
    tlb:tlb, $
    file_in:file_in, $
    dir_labels:dir_labels, $
    file_labels:file_labels, $
    exist_l2:exist_l2, $
    exist_l3_official:exist_l3_official, $
    exist_l3_user:exist_l3_user, $
    exist_l3_other:exist_l3_other, $
    file_l2:file_l2, $
    file_l3_official:file_l3_official, $
    file_l3_user:file_l3_user, $
    file_l3_other:file_l3_other, $
    object_l2:object_l2, $
    winno_l3_official:ptr_new(winno_l3_official), $
    winno_l3_user:ptr_new(winno_l3_user), $
    winno_l3_other:ptr_new(winno_l3_other), $
    ana_l3_official:ptr_new(ana_l3_official), $
    ana_l3_user:ptr_new(ana_l3_user), $
    ana_l3_other:ptr_new(ana_l3_other), $
    nwin:nwin, $
    nwin_l3_official:nwin_l3_official, $
    nwin_l3_user:nwin_l3_user, $
    nwin_l3_other:nwin_l3_other, $
    hdr_l3_official:ptr_new(hdr_l3_official), $
    hdr_l3_user:ptr_new(hdr_l3_user), $
    hdr_l3_other:ptr_new(hdr_l3_other), $
    state_l3_official:state_l3_official, $
    state_l3_user:state_l3_user, $
    state_l3_other:state_l3_other, $
    save_button_official:save_button_official, $
    save_button_user:save_button_user, $
    save_button_other:save_button_other $
  }
  info=ptr_new(info,/no_copy)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize

  xmanager, 'spice_xcontrol_l23', tlb, /no_block, $
    group_leader=group_leader, cleanup='spice_xcontrol_l23_cleanup'

end
