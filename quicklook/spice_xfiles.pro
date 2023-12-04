;+
; NAME:
;       SPICE_XFILES
;
; PURPOSE:
;
;       SPICE_XFILES is used to select data files from data bases.
;       SPICE_XFILES defines the data objects, header objects and
;       auxiliary objects and sends them to XDISPLAY or
;       IRIS_XDISPLAY. The XDISPLAY window is opened when the
;       user selects a data file in SPICE_XFILES.
;
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xfiles
;
; INPUTS:
;       none
;
; KEYWORD PARAMETERS:
;       none
;
;
; OUTPUTS:
;       Opens the XDISPLAY widget
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
; PROCEDURE:
;       SPICE_XFILES searches through data bases (directories) for data
;       files. Which data base and directory can be selected from the
;       various data sources.
;
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       2001: Oivind Wikstol. Gradually developed through the year.
;       19-Apr-2004: Oivind Wikstol - Cleaned up.
;       06-May-2004: Oivind Wikstol. Changed call to xcontrol for
;                    ccsds type.
;       18-Nov-2006: Viggo H. Cleaned up. Made fits default file type,
;                    activated date filter, added filename filter.
;       29-Sep-2007: Alessandro Gardini. Added the Confirmation button. Set
;                    the image device to Pixmap. Freed (*info).filelist
;                    each time it is redefined, and at the end. Renamed the
;                    various Row# in the widget according to their order.
;                    The function "findfile" was replaced by "file_search"
;                    already on 19-Jun-2007.
;       18-Mar-2008: A. Gardini. Check on level 2 FITS files, and call of
;                    xmap instead of xcontrol.
;       24-May-2013: Viggo H. IRIS version
;       2014-2016:   Martin Wiesmann, added new features, e.g. showing
;                    OBS and corresponding files separately, made it faster
;       Aug/Sep 2020:Martin Wiesmann, adapted it to SPICE and renamed it to
;                    spice_xfiles
;
; $Id: 2023-12-04 14:49 CET $
;-


; xfiles exit:
pro spice_xfiles_exit, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_save_params, info
  widget_control, event.top, /destroy
end


; spice_xfiles cleanup
pro spice_xfiles_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).filelistall
  ptr_free, (*info).filelist
  ptr_free, (*info).file2obsmap
  ptr_free, info
end


; save parameters into a hidden file
pro spice_xfiles_save_params, info, valid_times=valid_times
  widget_control, (*info).tstart, get_value=tstartval
  (*info).tstartval=tstartval
  widget_control, (*info).tstop, get_value=tstopval
  (*info).tstopval=tstopval
  valid_times = valid_time(tstartval) && valid_time(tstopval)
  if valid_times then begin
    widget_control, (*info).top_dir_choice_bg, get_value=top_dir_choice
    widget_control, (*info).top_dir_env_var_field, get_value=top_dir_env_var
    widget_control, (*info).dir_manual_field, get_value=dir_manual
    level = widget_info((*info).level_choice_droplist, /droplist_select)
    widget_control, (*info).use_path_prefix_bg, get_value=use_path_prefix
    ignoretime=(*info).ignoretime

    ;update recent time window list
    if ~ignoretime || use_path_prefix[1] then begin
      (*info).recentwindows->newsearch, tstartval, tstopval
      widget_control, (*info).recentdroplist, set_value=(*info).recentwindows->getwindows()
    endif
    (*info).recentwindows->gettimes, starttimes, endtimes
    
    ;save also position of window on screen
    widget_control, (*info).tlb, TLB_GET_OFFSET=offset_widget

    save, tstartval, tstopval, ignoretime, starttimes, endtimes, $
      top_dir_choice, top_dir_env_var, dir_manual, level, use_path_prefix, $
      offset_widget, $
      filename=SPICE_xfiles_appReadme()+'/spice_xfiles_searches.sav'

  endif
end


pro spice_xfiles_startsearch, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_save_params, info, valid_times=valid_times
  if valid_times then begin
    spice_xfiles_searchdir, info
  endif else box_message,'invalid time format(s)'
end


;this procedure searches a directory(-tree) for files using the filter and the start- and stoptimes
pro spice_xfiles_searchdir, info
  widget_control, /hourglass
  dirsep = path_sep()

  ;we have to make sure that we have the correct dates
  widget_control, (*info).tstart, get_value=tstartval
  (*info).tstartval=tstartval
  widget_control, (*info).tstop, get_value=tstopval
  (*info).tstopval=tstopval

  if ~valid_time(tstartval) || ~valid_time(tstopval) then begin
    box_message,'invalid time format(s)'
    return
  endif

  widget_control, (*info).top_dir_choice_bg, get_value=top_dir_choice
  dirsep = path_sep()
  case top_dir_choice of
    0: begin
      widget_control, (*info).top_dir_env_var_field, get_value=top_dir_env_var
      top_dir = getenv(top_dir_env_var)
      if top_dir eq '' then begin
        box_message,[top_dir_env_var + ' is not defined', 'using current directory']
        top_dir = '.' + dirsep
      endif
    end
    1: begin
      widget_control, (*info).dir_manual_field, get_value=dir_manual
      top_dir = dir_manual
    end
  endcase
  if strmid(top_dir, 0,1, /reverse_offset) ne dirsep then top_dir = top_dir+dirsep
  level = widget_info((*info).level_choice_droplist, /droplist_select)
  level = strtrim(string(level), 2)
  widget_control, (*info).use_path_prefix_bg, get_value=use_path_prefix
  no_level = ~use_path_prefix[0]
  no_tree_struct = ~use_path_prefix[1]
  search_subdir = use_path_prefix[2]
  user_dir = use_path_prefix[3]

  files = spice_find_file(tstartval, time_end=tstopval, top_dir=top_dir, $
    no_tree_struct=no_tree_struct, search_subdir=search_subdir, level=level, user_dir=user_dir, $
    no_level=no_level, ignore_time=(*info).ignoretime, /sequence)
  if size(files, /type) ne 7 then files = files.ToArray(dimension=1)

  ptr_free, (*info).filelistall
  (*info).filelistall = ptr_new(files)
  spice_xfiles_display_results, info, /newfiles
end


; displays the found files and sequences
pro spice_xfiles_display_results, info, newfiles=newfiles
  ;now we search the headers for different runs of OBS to display
  widget_control, /hourglass
  OBSdesc=''
  file2obsmap=0
  purpose = ['All']
  studytyp = ['All']
  slit_wid = [0, 10000]
  files = *(*info).filelistall
  if N_ELEMENTS(files) gt 0 && files[0] ne '' then begin
    if keyword_set(newfiles) then begin
      file_info = spice_file2info(files)
      uniqin = UNIQ(file_info.spiobsid, sort(file_info.spiobsid))
      template={SEQ_BEG:'', SPIOBSID:0L, STUDYTYP:'', STUDYDES:'', PURPOSE:'', SLIT_WID:0, DSUN_AU:0.0, CROTA:0.0, CRVAL1:0.0, CRVAL2:0.0}
      for fit=0,N_ELEMENTS(uniqin)-1 do begin
        ind = where(file_info.spiobsid eq file_info[uniqin[fit]].spiobsid, count)
        if count gt 0 then begin
          mreadfits_header, files[ind[0]], hdrtemp, only_tags='SEQ_BEG,SPIOBSID,STUDYTYP,STUDYDES,PURPOSE,SLIT_WID,DSUN_AU,CROTA,CRVAL1,CRVAL2', template=template
          IF hdrtemp.seq_beg EQ '' THEN BEGIN
            fits_open, files[ind[0]], fits_content
            fits_close, fits_content
            ind_data = where(fits_content.extname.Contains(' data'), count)
            IF count EQ 0 THEN BEGIN
              message, 'File does not contain keyword SEQ_BEG in main header and no extension with name "* data". Cannot display it.',/info
            ENDIF ELSE BEGIN
              mreadfits_header, files[ind[0]], hdrtemp, only_tags='SEQ_BEG,SPIOBSID,STUDYTYP,STUDYDES,PURPOSE,SLIT_WID,DSUN_AU,CROTA,CRVAL1,CRVAL2', template=template, $
                ext=ind_data[0]
            ENDELSE
          ENDIF
          if N_ELEMENTS(hdr) eq 0 then hdr=hdrtemp $
          else hdr=[hdr,hdrtemp]
        endif
      endfor ; fit=0,N_ELEMENTS(uniqin)-1
      ptr_free, (*info).filehdr
      (*info).filehdr = ptr_new(hdr)
      spiobsids = file_info.spiobsid
      ptr_free, (*info).file_spiobsids
      (*info).file_spiobsids = ptr_new(spiobsids)

      ; set all possible filter values (only used when this method is called from spice_xfiles_searchdir
      purpose = [purpose, hdr[uniq(hdr.purpose, sort(hdr.purpose))].purpose]
      studytyp = [studytyp, hdr[uniq(hdr.studytyp, sort(hdr.studytyp))].studytyp]
      slit_wid_min = min(hdr.slit_wid, max=slit_wid_max)
      slit_wid = [slit_wid_min, slit_wid_max]
      widget_control, (*info).display_filter_purpose, set_value=purpose
      widget_control, (*info).display_filter_studytyp, set_value=studytyp
      widget_control, (*info).display_filter_slitwid_min, set_value=slit_wid[0]
      widget_control, (*info).display_filter_slitwid_max, set_value=slit_wid[1]
    endif else begin ; keyword_set(newfiles)
      hdr = *(*info).filehdr
      spiobsids = *(*info).file_spiobsids
    endelse ; keyword_set(newfiles)
    hdrall = hdr
    file2obsmap = make_array(N_ELEMENTS(files), value=-1L)

    ; apply display filter
    widget_control, (*info).display_filter_purpose, get_value=purpose_values
    purpose_select = widget_info((*info).display_filter_purpose, /droplist_select)
    purpose_select = purpose_values[purpose_select]
    widget_control, (*info).display_filter_studytyp, get_value=studytyp_values
    studytyp_select = widget_info((*info).display_filter_studytyp, /droplist_select)
    studytyp_select = studytyp_values[studytyp_select]
    widget_control, (*info).display_filter_slitwid_min, get_value=slit_wid_min
    widget_control, (*info).display_filter_slitwid_max, get_value=slit_wid_max
    countp = N_ELEMENTS(hdr)
    IF purpose_select NE 'All' then begin
      ind = where(hdr.purpose eq purpose_select, countp)
      hdr = hdr[ind]
    ENDIF
    IF countp gt 0 THEN BEGIN
      counts = N_ELEMENTS(hdr)
      IF studytyp_select NE 'All' then begin
        ind = where(hdr.studytyp eq studytyp_select, counts)
        hdr = hdr[ind]
      ENDIF
      if counts gt 0 then begin
        ind = where(hdr.slit_wid GE slit_wid_min AND hdr.slit_wid LE slit_wid_max, countsw)
        if countsw gt 0 then begin
          hdr = hdr[ind]
          for ihdr=0,countsw-1 do begin
            ind = where(spiobsids eq hdr[ihdr].spiobsid, countobs)
            if countobs gt 0 then file2obsmap[ind]=ihdr+1
          endfor ; ihdr=0,countsw-1

          OBSdesc = get_infox(hdr, 'SEQ_BEG, SPIOBSID, PURPOSE, STUDYTYP, DSUN_AU, SLIT_WID, CROTA, CRVAL1, CRVAL2, STUDYDES', header=header, $
            format='a,(I12),a,a,(f7.3),(I8),(f7.1),(f7.1),(f7.1),a')
          OBSdesc = [header, OBSdesc]
        endif ; countsw gt 0
      endif ; counts gt 0
    ENDIF ; countp gt 0
  endif else files='' ; N_ELEMENTS(files) gt 0 && files[0] ne ''
  ptr_free, (*info).file2obsmap
  (*info).file2obsmap = ptr_new(file2obsmap)
  widget_control, (*info).foundOBS, set_value = OBSdesc
  widget_control, (*info).foundOBS, set_list_select = 1
  ind=where(file2obsmap eq 1, count)
  if count gt 0 then displayfiles=files[ind] $
  else displayfiles=''
  ptr_free, (*info).filelist
  (*info).filelist = ptr_new(displayfiles)
  widget_control, (*info).foundfiles, set_value = displayfiles
end


;function spice_xfiles_stopsearch, event
;  return, {widget_stopsearch, id:1L, top:0L, handler:0L}
;end

pro spice_xfiles_event, event
  ;this is just here for the stop button, because apparently I can't define an event_func and event_pro at the same time
  ;when there is an event_func defined, it ignores event_pro and searches for spice_xfiles_event
end


;opens new window with the catalog, if user selected files, then those will be displayed
pro spice_xfiles_use_catalog, event
  widget_control, event.top, get_uvalue = info
  files = spice_cat()
  if files eq !NULL then files='' $
  else begin
    topdir = getenv("SPICE_DATA")
    files_path=[]
    for i=0,N_ELEMENTS(files)-1 do begin
      file = file_search(topdir, files[i], count=count)
      if count gt 0 then begin
        files_path = [files_path, file[0]]
      endif
    endfor
    if N_ELEMENTS(files_path) eq 0 then files_path=''
    files=files_path
  endelse
  ptr_free, (*info).filelistall
  (*info).filelistall = ptr_new(files)
  spice_xfiles_display_results, info, /newfiles
end


; filters files according to date
pro spice_xfiles_date, event
  widget_control, event.top, get_uvalue = info
  case event.id of
    (*info).tstart: begin
      if valid_time(event.value) then begin
        (*info).tstartval=event.value
      endif else begin
        box_message,'invalid time format in start time'
        return
      endelse
    end
    (*info).tstop: begin
      if valid_time(event.value) then begin
        (*info).tstopval=event.value
      endif else begin
        box_message,'invalid time format in stop time'
        return
      endelse
    end
    (*info).ignoredatebg: begin
      widget_control, (*info).ignoredatebg, get_value=ignoretime
      (*info).ignoretime=ignoretime[0]
    end
    (*info).recentdroplist:begin
      recentind = event.index
      (*info).recentwindows->gettimes, starttimes, endtimes, index=recentind
      widget_control, (*info).tstart, set_value=starttimes
      (*info).tstartval = starttimes
      widget_control, (*info).tstop, set_value=endtimes
      (*info).tstopval = endtimes
    end
  endcase
end

pro spice_xfiles_currentdate, event
  widget_control, event.top, get_uvalue = info
  GET_UTC, tstopval, /stime, /truncate
  widget_control, (*info).tstop, set_value = tstopval
  if event.id eq (*info).getlast5days then begin
    tstartval = str2utc(tstopval)
    tstartval.mjd = tstartval.mjd-5
    tstartval = utc2str(tstartval, /STIME, /truncate)
    widget_control, (*info).tstart, set_value = tstartval
  endif else widget_control, (*info).tstart, get_value = tstartval
end

; list files in directory
pro spice_xfiles_dir, event
  dirsep = path_sep()
  widget_control, event.top, get_uvalue = info
  sdir = strtrim(event.value, 2)
  if strmid(sdir, 0,1, /reverse_offset) ne dirsep then sdir = sdir+dirsep
  (*info).sdir=sdir
  widget_control, (*info).searchdir, set_value=sdir
  widget_control, (*info).searchdroplist, set_droplist_select = 0
end

pro spice_xfiles_changesdir, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).dir_manual_field, get_value=sdir
  sfile=dialog_pickfile(path=sdir, title='Please select a directory', get_path=sdir)
  if sdir ne '' then begin
    widget_control, (*info).dir_manual_field, set_value=sdir
    spice_xfiles_search_dir, info
  endif
end


;user selected an OBS, we have to display to files which go with it
pro spice_xfiles_selectOBS, event
  widget_control, event.top, get_uvalue = info
  ind=where(*(*info).file2obsmap eq event.index, count)
  if count gt 0 then displayfiles=(*(*info).filelistall)[ind] $
  else displayfiles=''
  ptr_free, (*info).filelist
  (*info).filelist = ptr_new(displayfiles)
  widget_control, (*info).foundfiles, set_value = displayfiles
end


; print filename to console
pro spice_xfiles_printfilename, event
  widget_control, event.top, get_uvalue = info
  print,(*info).fileselect
end


; save the selected file
pro spice_xfiles_select, event
  widget_control, event.top, get_uvalue = info
  ; first check if this is the second click of a double click
  ; ...if so call spice_xfiles_read
  if event.clicks eq 2 then begin
    pseudoevent={widget_button,id:0L, $
      top:event.top, handler:0l, select:1}
    spice_xfiles_read,pseudoevent
    return
  endif
  ; first click, so figure out file and/or directory required...
  findx = event.index
  flist = *(*info).filelist
  sdir=(*info).sdir

  ; add full path to filenames in subdirectories, so that these
  ; files can be selected directly
  ; first check if the first entry is a subdirectory (it ends with a ':')

  last_char=strmid(flist(0),0,/reverse_offset)

  ; find the indexes of the rest of the subdirectories
  subdirindx=where(flist eq '',count)+1
  nsub = n_elements(subdirindx) ; number of subdirectories
  if count eq 0 then nsub=-1
  ;check if first entry in flist also is subdirectory
  ;(special case since it is then not lead by an empty entry)
  if last_char eq ':' then begin
    subdir=flist[0]
    slen=strlen(subdir)  ; length of string
    ;take out ':' at the end and add dirsep
    subdir=strmid(subdir,0,slen-1)+(*info).dirsep
    flist[0]=subdir
    start=1
    stop=subdirindx[0]-2
    if stop gt start then flist(start:stop)=subdir+flist[start:stop]
  endif
  ; then add path to the rest of the files in subdirectories
  for i=0,nsub-1 do begin
    subdir=flist[subdirindx[i]]
    slen=strlen(subdir)  ; length of string
    ;take out ':' at the end and add dirsep
    subdir=strmid(subdir,0,slen-1)+(*info).dirsep
    flist[subdirindx[i]]=subdir
    start = subdirindx[i]+1
    if i eq nsub-1 then stop=n_elements(flist)-1 else $
      stop=subdirindx[i+1]-2
    if stop gt start then flist[start:stop]=subdir+flist[start:stop]
  endfor
  (*info).fileselect = flist[findx]      ; selected file
  ;  if the file is a directory change sdir and return
  if (file_info((*info).fileselect)).directory then begin
    dirsep = path_sep()
    sdir=(*info).fileselect
    if strmid(sdir, 0,1, /reverse_offset) ne dirsep then sdir = sdir+dirsep
    (*info).sdir = sdir
    sstr = (*info).sdir + (*info).filter
    filelist = file_search(sstr, count = fcount)
    if fcount ne 0 then begin
      ptr_free, (*info).filelist
      (*info).filelist = ptr_new(strarr(fcount))
      *(*info).filelist = filelist
    endif else filelist=' '
    widget_control, (*info).searchdir, set_value = sdir
    widget_control, (*info).foundfiles, set_value = filelist
    return
  end
end


; event handler for search directory input fields
pro spice_xfiles_change_search, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_search_dir, info
end


; event handler for display filter input fields
pro spice_xfiles_change_display_filter, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_display_results, info
end


; calculate current search direcrory
pro spice_xfiles_search_dir, info
  widget_control, (*info).top_dir_choice_bg, get_value=top_dir_choice
  dirsep = path_sep()
  case top_dir_choice of
    0: begin
      widget_control, (*info).top_dir_env_var_field, get_value=top_dir_env_var
      top_dir = getenv(top_dir_env_var)
      if top_dir eq '' then begin
        box_message,[top_dir_env_var + ' is not defined', 'using current directory']
        top_dir = '.' + dirsep
      endif
    end
    1: begin
      widget_control, (*info).dir_manual_field, get_value=dir_manual
      top_dir = dir_manual
    end
  endcase
  if strmid(top_dir, 0,1, /reverse_offset) ne dirsep then top_dir = top_dir+dirsep
  level = widget_info((*info).level_choice_droplist, /droplist_select)
  level = strtrim(string(level), 2)
  widget_control, (*info).use_path_prefix_bg, get_value=use_path_prefix
  if use_path_prefix[3] then top_dir = top_dir + 'user' + dirsep
  if use_path_prefix[0] then top_dir = top_dir + 'level' + level + dirsep
  (*info).sdir = top_dir
  if use_path_prefix[1] then top_dir = top_dir + 'yyyy' + dirsep + 'mm' + dirsep + 'dd' + dirsep
  (*info).filter = 'solo_L' + strtrim(string(level),2) + '_spice-*.fits(.gz)'
  top_dir = top_dir + (*info).filter
  if use_path_prefix[2] then top_dir = top_dir + ' -r'
  widget_control, (*info).searchdir, set_value=top_dir
end


; call spice_xcontrol with the selected file
pro spice_xfiles_read, event
  widget_control, event.top, get_uvalue = info
  IF event.id NE 0 THEN widget_control, event.id, get_uvalue = xcontrol_l23 $
  ELSE xcontrol_l23 = 0
  file = ((*info).fileselect)
  if file eq '' then begin
    box_message,'You need to select a file first'
  endif else begin
    file_info = spice_file2info(file)
    if file_info.level eq 3 || xcontrol_l23 then begin
      spice_xcontrol_l23, file, group_leader=event.top
    endif else begin
      spice_xcontrol, file, group_leader=(*info).tlb
    endelse
  endelse
end


pro spice_xfiles

  sdirfile=SPICE_xfiles_appReadme()+'/spice_xfiles_searches.sav'
  if file_test(sdirfile) then begin
    restore,sdirfile
    ;    save, tstartval, tstopval, ignoretime, starttimes, endtimes, $
    ;      top_dir_choice, top_dir_env_var, dir_manual, level, use_path_prefix, $
    ;      offset_widget, $
    ;      filename=SPICE_xfiles_appReadme()+'/spice_xfiles_searches.sav'
    ; TO BE ADDED
    ; filter_purpose, filter_studytyp, filter_slitwid = [min,max]
  endif

  ; initialize variables, if they don't exist yet
  if ~valid_time(tstartval) || ~valid_time(tstopval) then begin
    GET_UTC, tstopval, /stime, /truncate
    tstartval = str2utc(tstopval)
    tstartval.mjd = tstartval.mjd-5
    tstartval = utc2str(tstartval, /STIME, /truncate)
  endif
  if n_elements(ignoretime) eq 0 then ignoretime=0
  if (N_ELEMENTS(starttimes) eq 0) || (N_ELEMENTS(endtimes) eq 0) then begin
    starttimes = tstartval
    endtimes = tstopval
  endif
  recentwindows = OBJ_NEW('IRIS_recent_timewindows', starttimes, endtimes)

  if N_ELEMENTS(top_dir_choice) eq 0 then top_dir_choice=0
  if N_ELEMENTS(top_dir_env_var) eq 0 then top_dir_env_var='SPICE_DATA'
  if N_ELEMENTS(dir_manual) eq 0 then dir_manual='./'
  if N_ELEMENTS(level) eq 0 then level=2
  if N_ELEMENTS(use_path_prefix) ne 4 then use_path_prefix=[1, 1, 1, 0]
  if N_ELEMENTS(filter_purpose) eq 0 then filter_purpose='All'
  if N_ELEMENTS(filter_studytyp) eq 0 then filter_studytyp='All'
  if N_ELEMENTS(filter_slitwid) eq 0 then filter_slitwid=[0,10000]
  if N_ELEMENTS(offset_widget) eq 0 then offset_widget=[200,200]

  sfilter = 'solo_L' + strtrim(string(level),2) + '_spice-*.fits(.gz)'
  dirsep = path_sep()


  ; top level base widget:
  tlb = widget_base(/column, title='SPICE_Xfiles - QL Control Window', event_pro='spice_xfiles_event')

  ; first row contains exit button
  exitbase = widget_base(tlb, /row, /frame)
  exitb = widget_button(exitbase, value = 'Exit', event_pro = 'spice_xfiles_exit')

  eis_icon_base=widget_base(exitbase, /col, /align_right)
  eis_icon = widget_draw(eis_icon_base, retain = 2, $
    XSize = 120, YSize = 60, frame = 1)

  iris_icon_size=120
  iris_icon_aspect=146./200.
  iris_icon_base=widget_base(exitbase, /col, /align_right)
  iris_icon = widget_draw(iris_icon_base, retain = 2, $
    XSize = iris_icon_size, YSize =iris_icon_size*iris_icon_aspect , frame = 1)

  spice_icon_base=widget_base(exitbase, /col, /align_right)
  spice_icon = widget_draw(spice_icon_base, retain = 2, $
    XSize = 120, YSize =120 , frame = 1)

  ; date/time fields
  row3=widget_base(tlb, /row, /frame)
  tlabelfield = widget_base(row3,/column)
  tls = 'Start/Stop for file search. Time Units: [D]D-MON-[YR]YR HH:MM:SS[.MS]'
  tlabel = widget_label(tlabelfield, value=tls, /align_left)
  tfield=widget_base(tlabelfield, /row, event_pro='spice_xfiles_date')
  tstart=cw_field(tfield, Title='Start Time:  ', value=tstartval, /string ,/return_events)
  tstop =cw_field(tfield, Title='Stop Time:   ', value=tstopval, /string ,/return_events)
  tfieldbuttons = widget_base(row3, /Column, event_pro='spice_xfiles_currentdate')
  getlast5days = widget_button(tfieldbuttons, value='Last 5 days')
  getcurrentdate = widget_button(tfieldbuttons, value='Up until now')
  tfield2 = widget_base(row3, /column, event_pro='spice_xfiles_date')
  recentdroplist = widget_droplist(tfield2, value=recentwindows->getwindows(), title='Recent time-windows')
  ignoredatebg = cw_bgroup(tfield2, ['Ignore times (only if no tree structure)'], set_value=[ignoretime], /column, /nonexclusive)

  ; search filter
  row4=widget_base(tlb, /column, /frame, event_pro='spice_xfiles_change_search')
  top_dir_base = widget_base(row4, /row)
  top_dir_label1 = widget_label(top_dir_base, value='Top directory')
  top_dir_choice_bg = cw_bgroup(top_dir_base, ['Environment variable', 'Path'], set_value=top_dir_choice, /column, /exclusive)
  top_dir_path_base = widget_base(top_dir_base, /column)
  top_dir_env_var_base = widget_base(top_dir_path_base, /row)
  top_dir_env_var_field = cw_field(top_dir_env_var_base, title='', value = top_dir_env_var, /string, /return_events, xsize = 100, ysize=0.7)
  dir_manual_base = widget_base(top_dir_path_base, /row)
  dir_manual_field = cw_field(dir_manual_base, title='', value = dir_manual, /string, /return_events, xsize = 100)
  dir_manual_button = widget_button(dir_manual_base, value='Change', event_pro='spice_xfiles_changesdir')
  level_base = widget_base(row4, /row)
  level_choice_droplist = widget_droplist(level_base, value=['Level 0', 'Level 1', 'Level 2', 'Level 3'], title='Data Level')
  widget_control, level_choice_droplist, set_droplist_select=level
  use_path_prefix_bg = cw_bgroup(level_base, ['Use levelx in path', 'Use Date-tree-structure in path', 'Search subdirectories', 'Search "user dir"'], $
    set_value=use_path_prefix, /row, /nonexclusive)
  search_path_base = widget_base(row4, /row)
  searchdir = cw_field(search_path_base, title='Search Directory  ', value = 'blablabladkjfa/adflkja/dlkfja/', /string, xsize = 100, /noedit)
  label = widget_label(search_path_base, value='     ')
  searchstartbutton = widget_button(search_path_base, value='Start Search', event_pro='spice_xfiles_startsearch')
  label = widget_label(search_path_base, value='     ')
  ;searchstopbutton = widget_button(search_path_base, value='Stop Search', event_func='spice_xfiles_stopsearch')
  use_catalog_button = widget_button(search_path_base, value='Use catalog', event_pro='spice_xfiles_use_catalog')

  ; display filter
  display_filter_base = widget_base(row4, /row, event_pro='spice_xfiles_change_display_filter')
  display_filter_label = widget_label(display_filter_base, value='Filter displayed OBS: ')
  display_filter_purpose = widget_droplist(display_filter_base, value=['All'], title='Purpose', xsize=230)
  display_filter_studytyp = widget_droplist(display_filter_base, value=['All'], title='Study Type', xsize=200)
  display_filter_slitwid_label = widget_label(display_filter_base, value='Slit width:')
  display_filter_slitwid_min = cw_field(display_filter_base, title='min', value = 0, /integer, /return_events, xsize = 6)
  display_filter_slitwid_max = cw_field(display_filter_base, title='max', value = 10000, /integer, /return_events, xsize = 6)

  ; display results
  foundOBS=widget_list(row4, value='', /frame, xsize = 150 $
    , scr_ysize = 0, units = 2, $
    event_pro = 'spice_xfiles_selectOBS')
  foundfiles=widget_list(row4, value='', /frame, xsize = 150 $
    , scr_ysize = 0, units = 2 $
    , event_pro = 'spice_xfiles_select')
  confbase = widget_base(row4, /row, /align_left)
  confb = widget_button(confbase, value = 'Confirm selection' $
    , event_pro = 'spice_xfiles_read', uvalue=0)
  label = widget_label(confbase, value='                 ')
  confb = widget_button(confbase, value = 'Open file in XControl_L23' $
    , event_pro = 'spice_xfiles_read', uvalue=1)
  label = widget_label(confbase, value='                 ')
  printfile = widget_button(confbase, value = 'Print filename to console' $
    , event_pro = 'spice_xfiles_printfilename')

  geometry = widget_info(tlb,/geometry)
  screen = spice_get_screen_size()
  space = float(screen[1]) - float(geometry.scr_ysize) - 80
  if space lt 1100 then widget_control,tlb,yoffset=0
  if space gt 900 then space = 900
  space = space / 5.0
  widget_control,foundOBS,scr_ysize=space*2
  widget_control,foundfiles,scr_ysize=space*3

  ; realize the top level base widget
  wp = widget_positioner(tlb)
  wp->position, xoffset=offset_widget[0], yoffset=offset_widget[1]
  ;widget_control, tlb, /realize
  

  ; Define the info structure, used to send information around
  info= { tlb:tlb, $
    tstart:tstart, $
    tstop:tstop, $
    tstartval:tstartval, $
    tstopval:tstopval, $
    ignoretime:ignoretime, $
    ignoredatebg:ignoredatebg, $
    getlast5days:getlast5days, $
    filter:sfilter, $
    dirsep:dirsep, $
    top_dir_choice_bg:top_dir_choice_bg, $
    top_dir_env_var_field:top_dir_env_var_field, $
    dir_manual_field:dir_manual_field, $
    level_choice_droplist:level_choice_droplist, $
    use_path_prefix_bg:use_path_prefix_bg, $
    searchdir:searchdir, $
    display_filter_purpose:display_filter_purpose, $
    display_filter_studytyp:display_filter_studytyp, $
    display_filter_slitwid_min:display_filter_slitwid_min, $
    display_filter_slitwid_max:display_filter_slitwid_max, $
    sdir:'', $
    filelist:ptr_new(), $
    filelistall:ptr_new(), $
    file2obsmap:ptr_new(), $
    filehdr:ptr_new(), $
    file_spiobsids:ptr_new(), $
    fileselect:'', $
    foundOBS:foundOBS, $
    foundfiles:foundfiles, $
    ;searchstopbutton:searchstopbutton, $
    recentdroplist:recentdroplist, $
    recentwindows:recentwindows}
  info=ptr_new(info,/no_copy)


  ; Set the info ptr to be the user value of the tlb widget
  widget_control,tlb, set_uvalue=info

  widget_control, eis_icon , get_value = drawID
  wset,drawID
  fileName = concat_dir(GETENV('ancillary') , 'eis_logo_sarah_small.jpg')
  if (file_info(fileName)).exists then begin
    read_jpeg , filename , icon
    icon_resized = CONGRID(icon,3,120,60)
    tvscl,icon_resized,true = 1
  endif else begin
    xyouts,0.5,0.5,'EIS',chars=chars,/normal,alignment=0.5
  endelse
  ;
  widget_control, iris_icon , get_value = drawID1
  wset,drawID1
  if getenv('IRIS_ANCILLARY') eq '' then $
    set_logenv,'IRIS_ANCILLARY',concat_dir(getenv('SSW'),'iris/idl/uio/ancillary/')
  fileName = concat_dir(getenv('IRIS_ANCILLARY'),'iris_logo.jpg')
  if (file_info(fileName)).exists then begin
    read_jpeg,filename,icon
    icon_resized = congrid(icon,3,iris_icon_size,iris_icon_size*iris_icon_aspect)
    tvscl,icon_resized,true=1
  endif else begin
    xyouts,0.5,0.5,'IRIS',chars=chars,/normal,alignment=0.5
  endelse
  ;
  widget_control, spice_icon , get_value = drawID2
  wset,drawID2
  have_con=have_proc('spice_xfiles',out=fname)
  if have_con then begin
    fileName = concat_dir(file_dirname(file_dirname(fname)), 'ancillary/spice-logo---colour.jpg')
    if (file_info(fileName)).exists then begin
      read_jpeg,filename,icon
      icon_resized = congrid(icon,3,120,120)
      tvscl,icon_resized,true=1
    endif else begin
      xyouts,0.5,0.5,'SPICE',chars=chars,/normal,alignment=0.5
    endelse
  endif else begin
    xyouts,0.5,0.5,'SPICE',chars=chars,/normal,alignment=0.5
  endelse

  spice_xfiles_search_dir, info

  xmanager, 'spice_xfiles', tlb, /no_block, $
    group_leader = group, cleanup = 'spice_xfiles_cleanup', event_handler='spice_xfiles_event';, /catch, no_block=0

end

