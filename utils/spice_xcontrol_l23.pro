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
; $Id: 2022-08-24 10:59 CEST $
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
  spice_xcontrol, (*info).l2_object
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


pro spice_xcontrol_l23, file, group_leader=group_leader

  file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
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
  
  l2_exist = file_l2 NE ''
  l3_official_exist = file_l3_official NE ''
  l3_user_exist = file_l3_user NE ''
  l3_other_exist = file_l3_other NE ''
  nwin = 0
  
  IF l2_exist THEN BEGIN
    l2_object = spice_data(file_l2)
    nwin = l2_object->get_number_windows()
    file_l3_calc = file_l2.replace('level2', 'level3')
    file_l3_calc = file_l3_calc.replace('_L2_','_L3_')
  ENDIF ELSE l2_object=0
  
  IF l3_official_exist THEN BEGIN
    ana_l3_official = fits2ana(file_l3_official, headers_results=hdr_l3_official)
    nwin_l3_official = fxpar(*hdr_l3_official[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_official[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_official
  ENDIF ELSE BEGIN
    IF l2_exist THEN BEGIN
      file_l3_official = file_l3_calc
    ENDIF ELSE IF l3_user_exist THEN BEGIN
      file_l3_official = file_l3_user.replace(path_sep()+'user'+path_sep(), path_sep())
    ENDIF
    nwin_l3_official = 0
    ana_l3_official = 0
    hdr_l3_official = 0
  ENDELSE
  
  IF l3_user_exist THEN BEGIN
    ana_l3_user = fits2ana(file_l3_user, headers_results=hdr_l3_user)
    nwin_l3_user = fxpar(*hdr_l3_user[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_user[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_user
  ENDIF ELSE BEGIN
    old_path_part = path_sep()+'spice'+path_sep()+'level3'+path_sep()
    new_path_part = path_sep()+'spice'+path_sep()+'user'+path_sep()+'level3'+path_sep()
    IF l2_exist THEN BEGIN
      file_l3_user = file_l3_calc.replace(old_path_part, new_path_part)
    ENDIF ELSE IF l3_official_exist THEN BEGIN
      file_l3_user = file_l3_user.replace(old_path_part, new_path_part)
    ENDIF
    nwin_l3_user = 0
    ana_l3_user = 0
    hdr_l3_user = 0
  ENDELSE
  
  IF l3_other_exist THEN BEGIN
    ana_l3_other = fits2ana(file_l3_other, headers_results=hdr_l3_other)
    nwin_l3_other = fxpar(*hdr_l3_other[0], 'NWIN', 0)
    if nwin eq 0 then nwin = fxpar(*hdr_l3_other[0], 'L2NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_other
  ENDIF ELSE BEGIN
    new_path = path_sep()+'some'+path_sep()+'path'
    IF l2_exist THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_calc))[0]) $
    ELSE IF l3_official_exist THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_official))[0]) $
    ELSE IF l3_user_exist THEN file_l3_other = concat_dir(new_path, (file_basename(file_l3_user))[0])
    nwin_l3_other = 0
    ana_l3_other = 0
    hdr_l3_other = 0
  ENDELSE
  
  
  
  ; WIDGETS

  tlb = widget_base(/column, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)

  win_base = widget_base(tlb, /grid_layout, column=4, /frame)


  ; Column Level 2 file

  base_l2 = widget_base(win_base, /column, /frame)
  label = widget_label(base_l2, value='LEVEL 2')
  label = widget_label(base_l2, value=(file_dirname(file_l2))[0])
  label = widget_label(base_l2, value=(file_basename(file_l2))[0])
  button = widget_button(base_l2, value='Open file', event_pro='spice_xcontrol_l23_open_l2', $
    sensitive=l2_exist)

  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l2 = widget_base(win_base, /column, /frame)
    IF l2_exist THEN BEGIN
      label = widget_label(win_base_l2, value=l2_object->get_window_id(iwin))
    ENDIF

  ENDFOR


  ; Column Level 3 - official file

  base_l3_official = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_official, value='LEVEL 3 - official')
  label = widget_label(base_l3_official, value=(file_dirname(file_l3_official))[0])
  label = widget_label(base_l3_official, value=(file_basename(file_l3_official))[0])
  button = widget_button(base_l3_official, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=l2_exist, uvalue={l3_type:1})

  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l3_official = widget_base(win_base, /column, /frame)
    IF l3_official_exist THEN BEGIN
    ENDIF
    label = widget_label(win_base_l3_official, value='win_base_l3_official')
    button = widget_button(win_base_l3_official, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      uvalue={l3_type:1, winno:iwin})

  ENDFOR


  ; Column Level 3 - user file

  base_l3_user = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_user, value='LEVEL 3 - user')
  label = widget_label(base_l3_user, value=(file_dirname(file_l3_user))[0])
  label = widget_label(base_l3_user, value=(file_basename(file_l3_user))[0])
  button = widget_button(base_l3_user, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=l2_exist, uvalue={l3_type:2})

  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l3_user = widget_base(win_base, /column, /frame)
    IF l3_user_exist THEN BEGIN
    ENDIF
    label = widget_label(win_base_l3_user, value='win_base_l3_user')
    button = widget_button(win_base_l3_user, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      uvalue={l3_type:2, winno:iwin})

  ENDFOR


  ; Column Level 3 - other file

  base_l3_other = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_other, value='LEVEL 3 - other')
  label = widget_label(base_l3_other, value=(file_dirname(file_l3_other))[0])
  label = widget_label(base_l3_other, value=(file_basename(file_l3_other))[0])
  button = widget_button(base_l3_other, value='(Re)create file', event_pro='spice_xcontrol_l23_create_l3', $
    sensitive=l2_exist, uvalue={l3_type:3})

  FOR iwin=0,nwin-1 DO BEGIN

    IF l3_other_exist THEN BEGIN
    ENDIF
    win_base_l3_other = widget_base(win_base, /column, /frame)
    label = widget_label(win_base_l3_other, value='win_base_l3_other')
    button = widget_button(win_base_l3_other, value='View/Edit window', event_pro='spice_xcontrol_l23_open_l3', $
      uvalue={l3_type:3, winno:iwin})
  ENDFOR


  ; Define the info structure, used to send information around
  info= { $
    tlb:tlb, $
    file_in:file_in, $
    l2_exist:l2_exist, $
    l3_official_exist:l3_official_exist, $
    l3_user_exist:l3_user_exist, $
    l3_other_exist:l3_other_exist, $
    file_l2:file_l2, $
    file_l3_official:file_l3_official, $
    file_l3_user:file_l3_user, $
    file_l3_other:file_l3_other, $
    l2_object:l2_object, $
    ana_l3_official:ana_l3_official, $
    ana_l3_user:ana_l3_user, $
    ana_l3_other:ana_l3_other, $
    nwin:nwin, $
    nwin_l3_official:nwin_l3_official, $
    nwin_l3_user:nwin_l3_user, $
    nwin_l3_other:nwin_l3_other, $
    hdr_l3_official:hdr_l3_official, $
    hdr_l3_user:hdr_l3_user, $
    hdr_l3_other:hdr_l3_other $
  }
  info=ptr_new(info,/no_copy)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue=info

  ; realize the top level base widget
  widget_control, tlb, /realize

  xmanager, 'spice_xcontrol_l23', tlb, /no_block, $
    group_leader=group_leader, cleanup='spice_xcontrol_l23_cleanup'

end
