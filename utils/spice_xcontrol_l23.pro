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
; $Id: 2022-08-19 15:11 CEST $
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
  widget_control, tlb, get_uvalue = info
  ptr_free,info
end

pro spice_xcontrol_l23_event, event
  if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    widget_control, event.top, /destroy
  endif
end



pro spice_xcontrol_l23, file, group_leader = group_leader

  file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
  if n_params() lt 1 then begin
    message,'spice_xcontrol_l23, file [, group_leader = group_leader]',/cont
    ;  return
  endif

  file_in = file_search(file, /fully_qualify_path)
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
  ENDIF
  IF l3_official_exist THEN BEGIN
    hdr = headfits(file_l3_official, exten=0)
    nwin_l3_official = fxpar(hdr, 'NWIN', 0)
    if nwin eq 0 then nwin = nwin_l3_official
    hdr_l3_official = ptrarr(nwin_l3_official)
    hdr_l3_official[0] = ptr_new(hdr)
    FOR iwin=1,nwin_l3_official-1 DO BEGIN
      hdr = headfits(file_l3_official, exten=iwin*7)
      hdr_l3_official[iwin] = ptr_new(hdr)
    ENDFOR
  ENDIF
  IF l3_user_exist THEN BEGIN
    hdr = headfits(file_l3_user, exten=0)
    nwin_l3_user = fxpar(hdr, 'NWIN', 0)
    if nwin lt nwin_l3_user then nwin = nwin_l3_user
    hdr_l3_user = ptrarr(nwin_l3_user)
    hdr_l3_user[0] = ptr_new(hdr)
    FOR iwin=1,nwin_l3_user-1 DO BEGIN
      hdr = headfits(file_l3_user, exten=iwin*7)
      hdr_l3_user[iwin] = ptr_new(hdr)
    ENDFOR
  ENDIF
  IF l3_other_exist THEN BEGIN
    hdr = headfits(file_l3_other, exten=0)
    nwin_l3_other = fxpar(hdr, 'NWIN', 0)
    if nwin lt nwin_l3_other then nwin = nwin_l3_other
    hdr_l3_other = ptrarr(nwin_l3_other)
    hdr_l3_other[0] = ptr_new(hdr)
    FOR iwin=1,nwin_l3_other-1 DO BEGIN
      hdr = headfits(file_l3_other, exten=iwin*7)
      hdr_l3_other[iwin] = ptr_new(hdr)
    ENDFOR
  ENDIF

  tlb = widget_base(/column, mbar=menubar, $
    title='SPICE_Xcontrol_L23 - '+file, $
    xoffset=50, yoffset=50, group_leader=group_leader, /tlb_kill_request_events)

  win_base = widget_base(tlb, /grid_layout, row=nwin+1, /frame)
  
  base_l2 = widget_base(win_base, /column, /frame)
  label = widget_label(base_l2, value='LEVEL 2')
  label = widget_label(base_l2, value=(file_dirname(file_l2))[0])
  label = widget_label(base_l2, value=(file_basename(file_l2))[0])

  base_l3_official = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_official, value='LEVEL 3 - official')
  label = widget_label(base_l3_official, value=(file_dirname(file_l3_official))[0])
  label = widget_label(base_l3_official, value=(file_basename(file_l3_official))[0])

  base_l3_user = widget_base(win_base, /column, /frame)
  label = widget_label(base_l3_user, value='LEVEL 3 - user')
  label = widget_label(base_l3_user, value=(file_dirname(file_l3_user))[0])
  label = widget_label(base_l3_user, value=(file_basename(file_l3_user))[0])

  IF l3_other_exist THEN BEGIN
    base_l3_other = widget_base(win_base, /column, /frame)
    label = widget_label(base_l3_other, value='LEVEL 3 - other')
    label = widget_label(base_l3_other, value=(file_dirname(file_l3_other))[0])
    label = widget_label(base_l3_other, value=(file_basename(file_l3_other))[0])
  ENDIF

  FOR iwin=0,nwin-1 DO BEGIN

    win_base_l2 = widget_base(win_base, /column, /frame)
    IF l2_exist THEN BEGIN
      label = widget_label(win_base_l2, value=l2_object->get_window_id(iwin))
    ENDIF

    win_base_l3_official = widget_base(win_base, /column, /frame)
    IF l3_official_exist THEN BEGIN
      label = widget_label(win_base_l3_official, value='win_base_l3_official')
    ENDIF

    win_base_l3_user = widget_base(win_base, /column, /frame)
    IF l3_user_exist THEN BEGIN
      label = widget_label(win_base_l3_user, value='win_base_l3_user')
    ENDIF

    IF l3_other_exist THEN BEGIN
      win_base_l3_other = widget_base(win_base, /column, /frame)
      label = widget_label(win_base_l3_other, value='win_base_l3_other')
    ENDIF
  ENDFOR


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
