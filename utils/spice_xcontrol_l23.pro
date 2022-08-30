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
; $Id: 2022-08-30 15:39 CEST $
;-
;
;
pro spice_xcontrol_l23_destroy, event
  print,'spice_xcontrol_l23_destroy'
  pseudoevent = {WIDGET_KILL_REQUEST, $
    ID:event.id, $
    TOP:event.top, $
    HANDLER:event.handler}
  spice_xcontrol_event, pseudoevent
end

pro spice_xcontrol_l23_cleanup, tlb
  widget_control, tlb, get_uvalue=info
  ; FREE....
  ptr_free,info
end

pro spice_xcontrol_l23_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, /destroy
  endif
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
      ana = ana_l3_official[file_info.winno]
    END
    2: print,2
    3: print,3
    else: print,'else'
  endcase
  xcfit_block, ana=ana[iana], title=strtrim(fxpar(*headers_results[iana], 'EXTNAML2', missing=''), 2)
end


pro spice_xcontrol_l23_create_l3, event
  widget_control, event.top, get_uvalue=info
  widget_control, event.id, get_uvalue=win_info
  case win_info.l3_type of
    1: begin
      ;file_l3 = info.file_l3_official
      ;nwin_l3 = info.nwin_l3_official
      ;ana_l3 = info.ana_l3_official
      ;hdr_l3 = info.hdr_l3_official
      ;state_l3 = info.state_l3_official
      official_l3dir = 1
    end
    2: begin
      ;file_l3 = info.file_l3_user
      ;nwin_l3 = info.nwin_l3_user
      ;ana_l3 = info.ana_l3_user
      ;hdr_l3 = info.hdr_l3_user
      ;state_l3 = info.state_l3_user
    end
    3: begin
      ;file_l3 = info.file_l3_other
      ;nwin_l3 = info.nwin_l3_other
      ;ana_l3 = info.ana_l3_other
      ;hdr_l3 = info.hdr_l3_other
      ;state_l3 = info.state_l3_other
      top_dir = (file_dirname((*info).file_l3_other))[0]
    end    
  endcase

  IF N_ELEMENTS(win_info.winno) GT 1 THEN all_windows=1 ELSE all_windows=0
  spice_create_l3_widget, (*info).object_L2, event.top, window_index=win_info.winno, $
    no_widget=all_windows, $
    official_l3dir=official_l3dir, top_dir=top_dir, save_not=~all_windows
end



; -----------------------------------------------------------------------
; MAIN PROGRAM
; -----------------------------------------------------------------------

pro spice_xcontrol_l23, file, group_leader=group_leader, show_other=show_other

  file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
  ;file = '/Users/mawiesma/data/spice/level2/2022/03/26/solo_L2_spice-n-ras_20220326T031318_V01_100663899-000.fits'

  if n_params() lt 1 then begin
    message,'spice_xcontrol_l23, file [, group_leader=group_leader]',/cont
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
  print,'file_l2           ',file_l2
  file_l3_official = spice_find_file(file_info.datetime, level=3)
  print,'file_l3_official  ',file_l3_official
  file_l3_user = spice_find_file(file_info.datetime, /user, level=3)
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
    hdr_l3_official = 0
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
    hdr_l3_user = 0
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
    hdr_l3_other = 0
  ENDELSE



  ; WIDGETS

  tlb = widget_base(/column, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)

  IF keyword_set(show_other) || exist_l3_other THEN column=4 ELSE column=3
  win_base = widget_base(tlb, /grid_layout, column=column, /frame)


  ; Column Level 2 file

  base_l2 = widget_base(win_base, /column, /frame)
  label = widget_label(base_l2, value='LEVEL 2', /align_center)
  label = widget_label(base_l2, value=(file_dirname(file_l2))[0], /align_center)
  label = widget_label(base_l2, value=(file_basename(file_l2))[0], /align_center)
  button = widget_button(base_l2, value='Open file', event_pro='spice_xcontrol_l23_open_l2', $
    sensitive=exist_l2)

  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l2 = widget_base(win_base, /column, /frame)
    IF exist_l2 THEN BEGIN
      label = widget_label(win_base_l2, value=object_l2->get_window_id(iwin), /align_left)
    ENDIF

  ENDFOR ; iwin=0,nwin-1


  ; Column Level 3 - official file

  base_l3_official = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_official, value='LEVEL 3 - official', /align_center)
  label = widget_label(base_l3_official, value=(file_dirname(file_l3_official))[0], /align_center)
  label = widget_label(base_l3_official, value=(file_basename(file_l3_official))[0], /align_center)
  button = widget_button(base_l3_official, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=exist_l2, uvalue={l3_type:1, winno:indgen(nwin)})

  state_l3_official = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l3_official = widget_base(win_base, /column, /frame)
    win_created = 0
    title = ''
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
      sensitive=win_created, uvalue={l3_type:1, winno:iwin})
    create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:1, winno:iwin})

  ENDFOR ; iwin=0,nwin-1


  ; Column Level 3 - user file

  base_l3_user = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_user, value='LEVEL 3 - user')
  label = widget_label(base_l3_user, value=(file_dirname(file_l3_user))[0])
  label = widget_label(base_l3_user, value=(file_basename(file_l3_user))[0])
  button = widget_button(base_l3_user, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=exist_l2, uvalue={l3_type:2, winno:indgen(nwin)})

  state_l3_user = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l3_user = widget_base(win_base, /column, /frame)
    win_created = 0
    title = ''
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
      sensitive=win_created, uvalue={l3_type:2, winno:iwin})
    create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:2, winno:iwin})

  ENDFOR ; iwin=0,nwin-1


  ; Column Level 3 - other file

  state_l3_other = make_array(nwin, value={l3_winno:-1, edited:0b, title_label:0L, status_label:0L, edit_button:0L})
  IF keyword_set(show_other) || exist_l3_other THEN BEGIN

    base_l3_other = widget_base(win_base, /column, /frame)
    label = widget_label(base_l3_other, value='LEVEL 3 - other')
    label = widget_label(base_l3_other, value=(file_dirname(file_l3_other))[0])
    label = widget_label(base_l3_other, value=(file_basename(file_l3_other))[0])
    button = widget_button(base_l3_other, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
      sensitive=exist_l2, uvalue={l3_type:3, winno:indgen(nwin)})

    FOR iwin=0,nwin-1 DO BEGIN

      win_base_l3_other = widget_base(win_base, /column, /frame)
      win_created = 0
      title = ''
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
        sensitive=win_created, uvalue={l3_type:2, winno:iwin})
      create_button = widget_button(button_base, value='(Re)create window', event_pro='spice_xcontrol_l23_create_l3', $
        sensitive=exist_l2, uvalue={l3_type:2, winno:iwin})

    ENDFOR ; iwin=0,nwin-1

  ENDIF ; keyword_set(show_other) || exist_l3_other


  ; Define the info structure, used to send information around
  info= { $
    tlb:tlb, $
    file_in:file_in, $
    exist_l2:exist_l2, $
    exist_l3_official:exist_l3_official, $
    exist_l3_user:exist_l3_user, $
    exist_l3_other:exist_l3_other, $
    file_l2:file_l2, $
    file_l3_official:file_l3_official, $
    file_l3_user:file_l3_user, $
    file_l3_other:file_l3_other, $
    object_l2:object_l2, $
    ana_l3_official:ana_l3_official, $
    ana_l3_user:ana_l3_user, $
    ana_l3_other:ana_l3_other, $
    nwin:nwin, $
    nwin_l3_official:nwin_l3_official, $
    nwin_l3_user:nwin_l3_user, $
    nwin_l3_other:nwin_l3_other, $
    hdr_l3_official:hdr_l3_official, $
    hdr_l3_user:hdr_l3_user, $
    hdr_l3_other:hdr_l3_other, $
    state_l3_official:state_l3_official, $
    state_l3_user:state_l3_user, $
    state_l3_other:state_l3_other $
  }
  info=ptr_new(info,/no_copy)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize

  xmanager, 'spice_xcontrol_l23', tlb, /no_block, $
    group_leader=group_leader, cleanup='spice_xcontrol_l23_cleanup'

end
