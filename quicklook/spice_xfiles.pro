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
; $Id: 2024-11-22 15:20 CET $
;-

; xfiles exit:
PRO spice_xfiles_exit, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_save_params, info
  widget_control, event.top, /destroy
END

; spice_xfiles cleanup
PRO spice_xfiles_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).filelistall
  ptr_free, (*info).filelist
  ptr_free, (*info).file2obsmap
  ptr_free, info
END

; save parameters into a hidden file
PRO spice_xfiles_save_params, info, valid_times = valid_times
  widget_control, (*info).tstart, get_value = tstartval
  (*info).tstartval = tstartval
  widget_control, (*info).tstop, get_value = tstopval
  (*info).tstopval = tstopval
  valid_times = valid_time(tstartval) && valid_time(tstopval)
  IF valid_times THEN BEGIN
    widget_control, (*info).top_dir_choice_bg, get_value = top_dir_choice
    widget_control, (*info).top_dir_env_var_field, get_value = top_dir_env_var
    widget_control, (*info).dir_manual_field, get_value = dir_manual
    level = widget_info((*info).level_choice_droplist, /droplist_select)
    widget_control, (*info).use_path_prefix_bg, get_value = use_path_prefix
    ignoretime = (*info).ignoretime

    ; update recent time window list
    IF ~ignoretime || use_path_prefix[1] THEN BEGIN
      (*info).recentwindows.newsearch, tstartval, tstopval
      widget_control, (*info).recentdroplist, set_value = (*info).recentwindows.getwindows()
    ENDIF
    (*info).recentwindows.gettimes, starttimes, endtimes

    ; save also position of window on screen
    widget_control, (*info).tlb, TLB_GET_OFFSET = offset_widget

    save, tstartval, tstopval, ignoretime, starttimes, endtimes, $
      top_dir_choice, top_dir_env_var, dir_manual, level, use_path_prefix, $
      offset_widget, $
      filename = spice_xfiles_appreadme() + '/spice_xfiles_searches.sav'
  ENDIF
END

PRO spice_xfiles_startsearch, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_save_params, info, valid_times = valid_times
  IF valid_times THEN BEGIN
    spice_xfiles_searchdir, info
  ENDIF ELSE box_message, 'invalid time format(s)'
END

; this procedure searches a directory(-tree) for files using the filter and the start- and stoptimes
PRO spice_xfiles_searchdir, info
  widget_control, /hourglass
  dirsep = path_sep()

  ; we have to make sure that we have the correct dates
  widget_control, (*info).tstart, get_value = tstartval
  (*info).tstartval = tstartval
  widget_control, (*info).tstop, get_value = tstopval
  (*info).tstopval = tstopval

  IF ~valid_time(tstartval) || ~valid_time(tstopval) THEN BEGIN
    box_message, 'invalid time format(s)'
    return
  ENDIF

  widget_control, (*info).top_dir_choice_bg, get_value = top_dir_choice
  dirsep = path_sep()
  CASE top_dir_choice OF
    0: BEGIN
      widget_control, (*info).top_dir_env_var_field, get_value = top_dir_env_var
      top_dir = getenv(top_dir_env_var)
      IF top_dir EQ '' THEN BEGIN
        box_message, [top_dir_env_var + ' is not defined', 'using current directory']
        top_dir = '.' + dirsep
      ENDIF
    END
    1: BEGIN
      widget_control, (*info).dir_manual_field, get_value = dir_manual
      top_dir = dir_manual
    END
  ENDCASE
  IF strmid(top_dir, 0, 1, /reverse_offset) NE dirsep THEN top_dir = top_dir + dirsep
  level = widget_info((*info).level_choice_droplist, /droplist_select)
  level = strtrim(string(level), 2)
  widget_control, (*info).use_path_prefix_bg, get_value = use_path_prefix
  no_level = ~use_path_prefix[0]
  no_tree_struct = ~use_path_prefix[1]
  search_subdir = use_path_prefix[2]
  user_dir = use_path_prefix[3]

  files = spice_find_file(tstartval, time_end = tstopval, top_dir = top_dir, $
    no_tree_struct = no_tree_struct, search_subdir = search_subdir, level = level, user_dir = user_dir, $
    no_level = no_level, ignore_time = (*info).ignoretime, /sequence)
  IF size(files, /type) NE 7 THEN files = files.ToArray(dimension = 1)

  ptr_free, (*info).filelistall
  (*info).filelistall = ptr_new(files)
  spice_xfiles_display_results, info, /newfiles
END

; displays the found files and sequences
PRO spice_xfiles_display_results, info, newfiles = newfiles
  ; now we search the headers for different runs of OBS to display
  widget_control, /hourglass
  OBSdesc = ''
  file2obsmap = 0
  purpose = ['All']
  studytyp = ['All']
  slit_wid = [0, 10000]
  files = *(*info).filelistall
  IF n_elements(files) GT 0 && files[0] NE '' THEN BEGIN
    IF keyword_set(newfiles) THEN BEGIN
      file_info = spice_file2info(files)
      uniqin = uniq(file_info.spiobsid, sort(file_info.spiobsid))
      template = {SEQ_BEG: '', SPIOBSID: 0l, STUDYTYP: '', STUDYDES: '', PURPOSE: '', SLIT_WID: 0, DSUN_AU: 0.0, CROTA: 0.0, CRVAL1: 0.0, CRVAL2: 0.0}
      FOR fit = 0, n_elements(uniqin) - 1 DO BEGIN
        ind = where(file_info.spiobsid EQ file_info[uniqin[fit]].spiobsid, count_spiobs)
        IF count_spiobs GT 0 THEN BEGIN
          mreadfits_header, files[ind[0]], hdrtemp, only_tags = 'SEQ_BEG,SPIOBSID,STUDYTYP,STUDYDES,PURPOSE,SLIT_WID,DSUN_AU,CROTA,CRVAL1,CRVAL2', template = template
          IF hdrtemp.seq_beg EQ '' THEN BEGIN
            fits_open, files[ind[0]], fits_content
            fits_close, fits_content
            hdr_load = headfits(files[ind[0]])
            dataext = fxpar(hdr_load, 'DATAEXT', missing = '')
            dataext = strsplit(dataext, ';', /extract)
            dataext = dataext[-1]
            ind_data = where(fits_content.extname EQ dataext, count_ext)
            IF count_ext EQ 0 THEN BEGIN
              message, 'File does not contain keyword SEQ_BEG in main header and no data extension found. Cannot display it.', /info
            ENDIF ELSE BEGIN
              mreadfits_header, files[ind[0]], hdrtemp, only_tags = 'SEQ_BEG,SPIOBSID,STUDYTYP,STUDYDES,PURPOSE,SLIT_WID,DSUN_AU,CROTA,CRVAL1,CRVAL2', template = template, $
                ext = ind_data[0]
            ENDELSE
          ENDIF
          IF n_elements(hdr) EQ 0 THEN hdr = hdrtemp $
          ELSE hdr = [hdr, hdrtemp]
        ENDIF
      ENDFOR ; fit=0,N_ELEMENTS(uniqin)-1
      ptr_free, (*info).filehdr
      (*info).filehdr = ptr_new(hdr)
      spiobsids = file_info.spiobsid
      ptr_free, (*info).file_spiobsids
      (*info).file_spiobsids = ptr_new(spiobsids)

      ; set all possible filter values (only used when this method is called from spice_xfiles_searchdir
      purpose = [purpose, hdr[uniq(hdr.purpose, sort(hdr.purpose))].purpose]
      studytyp = [studytyp, hdr[uniq(hdr.studytyp, sort(hdr.studytyp))].studytyp]
      slit_wid_min = min(hdr.slit_wid, max = slit_wid_max)
      slit_wid = [slit_wid_min, slit_wid_max]
      widget_control, (*info).display_filter_purpose, set_value = purpose
      widget_control, (*info).display_filter_studytyp, set_value = studytyp
      widget_control, (*info).display_filter_slitwid_min, set_value = slit_wid[0]
      widget_control, (*info).display_filter_slitwid_max, set_value = slit_wid[1]
    ENDIF ELSE BEGIN ; keyword_set(newfiles)
      hdr = *(*info).filehdr
      spiobsids = *(*info).file_spiobsids
    ENDELSE ; keyword_set(newfiles)
    file2obsmap = make_array(n_elements(files), value = -1l)

    ; apply display filter
    widget_control, (*info).display_filter_purpose, get_value = purpose_values
    purpose_select = widget_info((*info).display_filter_purpose, /droplist_select)
    purpose_select = purpose_values[purpose_select]
    widget_control, (*info).display_filter_studytyp, get_value = studytyp_values
    studytyp_select = widget_info((*info).display_filter_studytyp, /droplist_select)
    studytyp_select = studytyp_values[studytyp_select]
    widget_control, (*info).display_filter_slitwid_min, get_value = slit_wid_min
    widget_control, (*info).display_filter_slitwid_max, get_value = slit_wid_max
    countp = n_elements(hdr)
    IF purpose_select NE 'All' THEN BEGIN
      ind = where(hdr.purpose EQ purpose_select, countp)
      hdr = hdr[ind]
    ENDIF
    IF countp GT 0 THEN BEGIN
      counts = n_elements(hdr)
      IF studytyp_select NE 'All' THEN BEGIN
        ind = where(hdr.studytyp EQ studytyp_select, counts)
        hdr = hdr[ind]
      ENDIF
      IF counts GT 0 THEN BEGIN
        ind = where(hdr.slit_wid GE slit_wid_min AND hdr.slit_wid LE slit_wid_max, countsw)
        IF countsw GT 0 THEN BEGIN
          hdr = hdr[ind]
          FOR ihdr = 0, countsw - 1 DO BEGIN
            ind = where(spiobsids EQ hdr[ihdr].spiobsid, countobs)
            IF countobs GT 0 THEN file2obsmap[ind] = ihdr + 1
          ENDFOR ; ihdr=0,countsw-1

          OBSdesc = get_infox(hdr, 'SEQ_BEG, SPIOBSID, PURPOSE, STUDYTYP, DSUN_AU, SLIT_WID, CROTA, CRVAL1, CRVAL2, STUDYDES', header = header, $
            format = 'a,(I12),a,a,(f7.3),(I8),(f7.1),(f7.1),(f7.1),a')
          OBSdesc = [header, OBSdesc]
        ENDIF ; countsw gt 0
      ENDIF ; counts gt 0
    ENDIF ; countp gt 0
  ENDIF ELSE files = '' ; N_ELEMENTS(files) gt 0 && files[0] ne ''
  ptr_free, (*info).file2obsmap
  (*info).file2obsmap = ptr_new(file2obsmap)
  widget_control, (*info).foundOBS, set_value = OBSdesc
  widget_control, (*info).foundOBS, set_list_select = 1
  ind = where(file2obsmap EQ 1, count)
  IF count GT 0 THEN displayfiles = files[ind] $
  ELSE displayfiles = ''
  ptr_free, (*info).filelist
  (*info).filelist = ptr_new(displayfiles)
  widget_control, (*info).foundfiles, set_value = displayfiles
END

; function spice_xfiles_stopsearch, event
; return, {widget_stopsearch, id:1L, top:0L, handler:0L}
; end

; idl-disable-next-line unused-var
PRO spice_xfiles_event, event
  ; this is just here for the stop button, because apparently I can't define an event_func and event_pro at the same time
  ; when there is an event_func defined, it ignores event_pro and searches for spice_xfiles_event
END

; opens new window with the catalog, if user selected files, then those will be displayed
PRO spice_xfiles_use_catalog, event
  widget_control, event.top, get_uvalue = info
  files = spice_cat()
  IF files EQ !NULL THEN files = '' $
  ELSE BEGIN
    topdir = getenv("SPICE_DATA")
    files_path = []
    FOR i = 0, n_elements(files) - 1 DO BEGIN
      file = file_search(topdir, files[i], count = count)
      IF count GT 0 THEN BEGIN
        files_path = [files_path, file[0]]
      ENDIF
    ENDFOR
    IF n_elements(files_path) EQ 0 THEN files_path = ''
    files = files_path
  ENDELSE
  ptr_free, (*info).filelistall
  (*info).filelistall = ptr_new(files)
  spice_xfiles_display_results, info, /newfiles
END

; filters files according to date
PRO spice_xfiles_date, event
  widget_control, event.top, get_uvalue = info
  CASE event.id OF
    (*info).tstart: BEGIN
      IF valid_time(event.value) THEN BEGIN
        (*info).tstartval = event.value
      ENDIF ELSE BEGIN
        box_message, 'invalid time format in start time'
        return
      ENDELSE
    END
    (*info).tstop: BEGIN
      IF valid_time(event.value) THEN BEGIN
        (*info).tstopval = event.value
      ENDIF ELSE BEGIN
        box_message, 'invalid time format in stop time'
        return
      ENDELSE
    END
    (*info).ignoredatebg: BEGIN
      widget_control, (*info).ignoredatebg, get_value = ignoretime
      (*info).ignoretime = ignoretime[0]
    END
    (*info).recentdroplist: BEGIN
      recentind = event.index
      (*info).recentwindows.gettimes, starttimes, endtimes, index = recentind
      widget_control, (*info).tstart, set_value = starttimes
      (*info).tstartval = starttimes
      widget_control, (*info).tstop, set_value = endtimes
      (*info).tstopval = endtimes
    END
  ENDCASE
END

PRO spice_xfiles_currentdate, event
  widget_control, event.top, get_uvalue = info
  get_utc, tstopval, /stime, /truncate
  widget_control, (*info).tstop, set_value = tstopval
  IF event.id EQ (*info).getlast5days THEN BEGIN
    tstartval = str2utc(tstopval)
    tstartval.mjd = tstartval.mjd - 5
    tstartval = utc2str(tstartval, /STIME, /truncate)
    widget_control, (*info).tstart, set_value = tstartval
  ENDIF ELSE widget_control, (*info).tstart, get_value = tstartval
END

; list files in directory
PRO spice_xfiles_dir, event
  dirsep = path_sep()
  widget_control, event.top, get_uvalue = info
  sdir = strtrim(event.value, 2)
  IF strmid(sdir, 0, 1, /reverse_offset) NE dirsep THEN sdir = sdir + dirsep
  (*info).sdir = sdir
  widget_control, (*info).searchdir, set_value = sdir
  widget_control, (*info).searchdroplist, set_droplist_select = 0
END

PRO spice_xfiles_changesdir, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).dir_manual_field, get_value = sdir
  ; idl-disable-next-line unused-var, illegal-arrow
  sfile = dialog_pickfile(path = sdir, title = 'Please select a directory', get_path = sdir)
  IF sdir NE '' THEN BEGIN
    widget_control, (*info).dir_manual_field, set_value = sdir
    spice_xfiles_search_dir, info
  ENDIF
END

; user selected an OBS, we have to display to files which go with it
PRO spice_xfiles_selectOBS, event
  widget_control, event.top, get_uvalue = info
  ind = where(*(*info).file2obsmap EQ event.index, count)
  IF count GT 0 THEN displayfiles = (*(*info).filelistall)[ind] $
  ELSE displayfiles = ''
  ptr_free, (*info).filelist
  (*info).filelist = ptr_new(displayfiles)
  widget_control, (*info).foundfiles, set_value = displayfiles
END

; print filename to console
PRO spice_xfiles_printfilename, event
  widget_control, event.top, get_uvalue = info
  print, (*info).fileselect
END

; save the selected file
PRO spice_xfiles_select, event
  widget_control, event.top, get_uvalue = info
  ; first check if this is the second click of a double click
  ; ...if so call spice_xfiles_read
  IF event.clicks EQ 2 THEN BEGIN
    ; idl-disable-next-line unknown-structure
    pseudoevent = {widget_button, id: 0l, $
      top: event.top, handler: 0l, select: 1}
    spice_xfiles_read, pseudoevent
    return
  ENDIF
  ; first click, so figure out file and/or directory required...
  findx = event.index
  flist = *(*info).filelist
  sdir = (*info).sdir

  ; add full path to filenames in subdirectories, so that these
  ; files can be selected directly
  ; first check if the first entry is a subdirectory (it ends with a ':')

  last_char = strmid(flist[0], 0, /reverse_offset)

  ; find the indexes of the rest of the subdirectories
  subdirindx = where(flist EQ '', count) + 1
  nsub = n_elements(subdirindx) ; number of subdirectories
  IF count EQ 0 THEN nsub = -1
  ; check if first entry in flist also is subdirectory
  ; (special case since it is then not lead by an empty entry)
  IF last_char EQ ':' THEN BEGIN
    subdir = flist[0]
    slen = strlen(subdir) ; length of string
    ; take out ':' at the end and add dirsep
    subdir = strmid(subdir, 0, slen - 1) + (*info).dirsep
    flist[0] = subdir
    start = 1
    stop = subdirindx[0] - 2
    IF stop GT start THEN flist[start : stop] = subdir + flist[start : stop]
  ENDIF
  ; then add path to the rest of the files in subdirectories
  FOR i = 0, nsub - 1 DO BEGIN
    subdir = flist[subdirindx[i]]
    slen = strlen(subdir) ; length of string
    ; take out ':' at the end and add dirsep
    subdir = strmid(subdir, 0, slen - 1) + (*info).dirsep
    flist[subdirindx[i]] = subdir
    start = subdirindx[i] + 1
    IF i EQ nsub - 1 THEN stop = n_elements(flist) - 1 ELSE $
      stop = subdirindx[i + 1] - 2
    IF stop GT start THEN flist[start : stop] = subdir + flist[start : stop]
  ENDFOR
  (*info).fileselect = flist[findx] ; selected file
  ; if the file is a directory change sdir and return
  IF (file_info((*info).fileselect)).directory THEN BEGIN
    dirsep = path_sep()
    sdir = (*info).fileselect
    IF strmid(sdir, 0, 1, /reverse_offset) NE dirsep THEN sdir = sdir + dirsep
    (*info).sdir = sdir
    sstr = (*info).sdir + (*info).filter
    filelist = file_search(sstr, count = fcount)
    IF fcount NE 0 THEN BEGIN
      ptr_free, (*info).filelist
      (*info).filelist = ptr_new(filelist)
    ENDIF ELSE filelist = ' '
    widget_control, (*info).searchdir, set_value = sdir
    widget_control, (*info).foundfiles, set_value = filelist
    return
  END
END

; event handler for search directory input fields
PRO spice_xfiles_change_search, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_search_dir, info
END

; event handler for display filter input fields
PRO spice_xfiles_change_display_filter, event
  widget_control, event.top, get_uvalue = info
  spice_xfiles_display_results, info
END

; calculate current search direcrory
PRO spice_xfiles_search_dir, info
  widget_control, (*info).top_dir_choice_bg, get_value = top_dir_choice
  dirsep = path_sep()
  CASE top_dir_choice OF
    0: BEGIN
      widget_control, (*info).top_dir_env_var_field, get_value = top_dir_env_var
      top_dir = getenv(top_dir_env_var)
      IF top_dir EQ '' THEN BEGIN
        box_message, [top_dir_env_var + ' is not defined', 'using current directory']
        top_dir = '.' + dirsep
      ENDIF
    END
    1: BEGIN
      widget_control, (*info).dir_manual_field, get_value = dir_manual
      top_dir = dir_manual
    END
  ENDCASE
  IF strmid(top_dir, 0, 1, /reverse_offset) NE dirsep THEN top_dir = top_dir + dirsep
  level = widget_info((*info).level_choice_droplist, /droplist_select)
  level = strtrim(string(level), 2)
  widget_control, (*info).use_path_prefix_bg, get_value = use_path_prefix
  IF use_path_prefix[3] THEN top_dir = top_dir + 'user' + dirsep
  IF use_path_prefix[0] THEN top_dir = top_dir + 'level' + level + dirsep
  (*info).sdir = top_dir
  IF use_path_prefix[1] THEN top_dir = top_dir + 'yyyy' + dirsep + 'mm' + dirsep + 'dd' + dirsep
  (*info).filter = 'solo_L' + strtrim(string(level), 2) + '_spice-*.fits(.gz)'
  top_dir = top_dir + (*info).filter
  IF use_path_prefix[2] THEN top_dir = top_dir + ' -r'
  widget_control, (*info).searchdir, set_value = top_dir
END

; call spice_xcontrol with the selected file
PRO spice_xfiles_read, event
  widget_control, event.top, get_uvalue = info
  IF event.id NE 0 THEN widget_control, event.id, get_uvalue = xcontrol_l23 $
  ELSE xcontrol_l23 = 0
  file = ((*info).fileselect)
  IF file EQ '' THEN BEGIN
    box_message, 'You need to select a file first'
  ENDIF ELSE BEGIN
    file_info = spice_file2info(file)
    IF file_info.level EQ 3 || xcontrol_l23 THEN BEGIN
      spice_xcontrol_l23, file, group_leader = event.top
    ENDIF ELSE BEGIN
      spice_xcontrol, file, group_leader = (*info).tlb
    ENDELSE
  ENDELSE
END

PRO spice_xfiles
  sdirfile = spice_xfiles_appreadme() + '/spice_xfiles_searches.sav'
  IF file_test(sdirfile) THEN BEGIN
    restore, sdirfile
    ; save, tstartval, tstopval, ignoretime, starttimes, endtimes, $
    ; top_dir_choice, top_dir_env_var, dir_manual, level, use_path_prefix, $
    ; offset_widget, $
    ; filename=SPICE_xfiles_appReadme()+'/spice_xfiles_searches.sav'
    ; TO BE ADDED
    ; filter_purpose, filter_studytyp, filter_slitwid = [min,max]
  ENDIF

  ; initialize variables, if they don't exist yet
  IF ~valid_time(tstartval) || ~valid_time(tstopval) THEN BEGIN
    get_utc, tstopval, /stime, /truncate
    tstartval = str2utc(tstopval)
    tstartval.mjd = tstartval.mjd - 5
    tstartval = utc2str(tstartval, /STIME, /truncate)
  ENDIF
  IF n_elements(ignoretime) EQ 0 THEN ignoretime = 0
  IF (n_elements(starttimes) EQ 0) || (n_elements(endtimes) EQ 0) THEN BEGIN
    starttimes = tstartval
    endtimes = tstopval
  ENDIF
  recentwindows = obj_new('IRIS_recent_timewindows', starttimes, endtimes)

  IF n_elements(top_dir_choice) EQ 0 THEN top_dir_choice = 0
  IF n_elements(top_dir_env_var) EQ 0 THEN top_dir_env_var = 'SPICE_DATA'
  IF n_elements(dir_manual) EQ 0 THEN dir_manual = './'
  IF n_elements(level) EQ 0 THEN level = 2
  IF n_elements(use_path_prefix) NE 4 THEN use_path_prefix = [1, 1, 1, 0]
  IF n_elements(filter_purpose) EQ 0 THEN filter_purpose = 'All'
  IF n_elements(filter_studytyp) EQ 0 THEN filter_studytyp = 'All'
  IF n_elements(filter_slitwid) EQ 0 THEN filter_slitwid = [0, 10000]
  IF n_elements(offset_widget) EQ 0 THEN offset_widget = [200, 200]

  sfilter = 'solo_L' + strtrim(string(level), 2) + '_spice-*.fits(.gz)'
  dirsep = path_sep()

  ; top level base widget:
  tlb = widget_base(/column, title = 'SPICE_Xfiles - QL Control Window', event_pro = 'spice_xfiles_event')

  ; first row contains exit button
  exitbase = widget_base(tlb, /row, /frame)
  exitb = widget_button(exitbase, value = 'Exit', event_pro = 'spice_xfiles_exit') ; idl-disable-line unused-var

  eis_icon_base = widget_base(exitbase, /col, /align_right)
  eis_icon = widget_draw(eis_icon_base, retain = 2, $
    XSize = 120, YSize = 60, frame = 1)

  iris_icon_size = 120
  iris_icon_aspect = 146. / 200.
  iris_icon_base = widget_base(exitbase, /col, /align_right)
  iris_icon = widget_draw(iris_icon_base, retain = 2, $
    XSize = iris_icon_size, YSize = iris_icon_size * iris_icon_aspect, frame = 1)

  spice_icon_base = widget_base(exitbase, /col, /align_right)
  spice_icon = widget_draw(spice_icon_base, retain = 2, $
    XSize = 120, YSize = 120, frame = 1)

  ; date/time fields
  row3 = widget_base(tlb, /row, /frame)
  tlabelfield = widget_base(row3, /column)
  tls = 'Start/Stop for file search. Time Units: [D]D-MON-[YR]YR HH:MM:SS[.MS]'
  tlabel = widget_label(tlabelfield, value = tls, /align_left) ; idl-disable-line unused-var
  tfield = widget_base(tlabelfield, /row, event_pro = 'spice_xfiles_date')
  tstart = cw_field(tfield, Title = 'Start Time:  ', value = tstartval, /string, /return_events)
  tstop = cw_field(tfield, Title = 'Stop Time:   ', value = tstopval, /string, /return_events)
  tfieldbuttons = widget_base(row3, /Column, event_pro = 'spice_xfiles_currentdate')
  getlast5days = widget_button(tfieldbuttons, value = 'Last 5 days')
  getcurrentdate = widget_button(tfieldbuttons, value = 'Up until now') ; idl-disable-line unused-var
  tfield2 = widget_base(row3, /column, event_pro = 'spice_xfiles_date')
  recentdroplist = widget_droplist(tfield2, value = recentwindows.getwindows(), title = 'Recent time-windows')
  ignoredatebg = cw_bgroup(tfield2, ['Ignore times (only if no tree structure)'], set_value = [ignoretime], /column, /nonexclusive)

  ; search filter
  row4 = widget_base(tlb, /column, /frame, event_pro = 'spice_xfiles_change_search')
  top_dir_base = widget_base(row4, /row)
  top_dir_label1 = widget_label(top_dir_base, value = 'Top directory') ; idl-disable-line unused-var
  top_dir_choice_bg = cw_bgroup(top_dir_base, ['Environment variable', 'Path'], set_value = top_dir_choice, /column, /exclusive)
  top_dir_path_base = widget_base(top_dir_base, /column)
  top_dir_env_var_base = widget_base(top_dir_path_base, /row)
  top_dir_env_var_field = cw_field(top_dir_env_var_base, title = '', value = top_dir_env_var, /string, /return_events, xsize = 100, ysize = 0.7)
  dir_manual_base = widget_base(top_dir_path_base, /row)
  dir_manual_field = cw_field(dir_manual_base, title = '', value = dir_manual, /string, /return_events, xsize = 100)
  dir_manual_button = widget_button(dir_manual_base, value = 'Change', event_pro = 'spice_xfiles_changesdir') ; idl-disable-line unused-var
  level_base = widget_base(row4, /row)
  level_choice_droplist = widget_droplist(level_base, value = ['Level 0', 'Level 1', 'Level 2', 'Level 3'], title = 'Data Level')
  widget_control, level_choice_droplist, set_droplist_select = level
  use_path_prefix_bg = cw_bgroup(level_base, ['Use levelx in path', 'Use Date-tree-structure in path', 'Search subdirectories', 'Search "user dir"'], $
    set_value = use_path_prefix, /row, /nonexclusive)
  search_path_base = widget_base(row4, /row)
  searchdir = cw_field(search_path_base, title = 'Search Directory  ', value = 'blablabladkjfa/adflkja/dlkfja/', /string, xsize = 100, /noedit)
  label = widget_label(search_path_base, value = '     ')
  searchstartbutton = widget_button(search_path_base, value = 'Start Search', event_pro = 'spice_xfiles_startsearch') ; idl-disable-line unused-var
  label = widget_label(search_path_base, value = '     ')
  ; searchstopbutton = widget_button(search_path_base, value='Stop Search', event_func='spice_xfiles_stopsearch')
  use_catalog_button = widget_button(search_path_base, value = 'Use catalog', event_pro = 'spice_xfiles_use_catalog') ; idl-disable-line unused-var

  ; display filter
  display_filter_base = widget_base(row4, /row, event_pro = 'spice_xfiles_change_display_filter')
  display_filter_label = widget_label(display_filter_base, value = 'Filter displayed OBS: ') ; idl-disable-line unused-var
  display_filter_purpose = widget_droplist(display_filter_base, value = ['All'], title = 'Purpose', xsize = 230)
  display_filter_studytyp = widget_droplist(display_filter_base, value = ['All'], title = 'Study Type', xsize = 200)
  display_filter_slitwid_label = widget_label(display_filter_base, value = 'Slit width:') ; idl-disable-line unused-var
  display_filter_slitwid_min = cw_field(display_filter_base, title = 'min', value = 0, /integer, /return_events, xsize = 6)
  display_filter_slitwid_max = cw_field(display_filter_base, title = 'max', value = 10000, /integer, /return_events, xsize = 6)

  ; display results
  foundOBS = widget_list(row4, value = '', /frame, xsize = 150, scr_ysize = 0, units = 2, event_pro = 'spice_xfiles_selectOBS')
  foundfiles = widget_list(row4, value = '', /frame, xsize = 150, scr_ysize = 0, units = 2, event_pro = 'spice_xfiles_select')
  confbase = widget_base(row4, /row, /align_left)
  confb = widget_button(confbase, value = 'Confirm selection', event_pro = 'spice_xfiles_read', uvalue = 0)
  label = widget_label(confbase, value = '                 ')
  confb = widget_button(confbase, value = 'Open file in XControl_L23', event_pro = 'spice_xfiles_read', uvalue = 1)
  label = widget_label(confbase, value = '                 ')
  printfile = widget_button(confbase, value = 'Print filename to console', event_pro = 'spice_xfiles_printfilename') ; idl-disable-line unused-var

  geometry = widget_info(tlb, /geometry)
  screen = spice_get_screen_size()
  space = float(screen[1]) - float(geometry.scr_ysize) - 80
  IF space LT 1100 THEN widget_control, tlb, yoffset = 0
  IF space GT 900 THEN space = 900
  space = space / 5.0
  widget_control, foundOBS, scr_ysize = space * 2
  widget_control, foundfiles, scr_ysize = space * 3

  ; realize the top level base widget
  wp = widget_positioner(tlb)
  wp.position, xoffset = offset_widget[0], yoffset = offset_widget[1]
  ; widget_control, tlb, /realize

  ; Define the info structure, used to send information around
  info = {tlb: tlb, $
    tstart: tstart, $
    tstop: tstop, $
    tstartval: tstartval, $
    tstopval: tstopval, $
    ignoretime: ignoretime, $
    ignoredatebg: ignoredatebg, $
    getlast5days: getlast5days, $
    filter: sfilter, $
    dirsep: dirsep, $
    top_dir_choice_bg: top_dir_choice_bg, $
    top_dir_env_var_field: top_dir_env_var_field, $
    dir_manual_field: dir_manual_field, $
    level_choice_droplist: level_choice_droplist, $
    use_path_prefix_bg: use_path_prefix_bg, $
    searchdir: searchdir, $
    display_filter_purpose: display_filter_purpose, $
    display_filter_studytyp: display_filter_studytyp, $
    display_filter_slitwid_min: display_filter_slitwid_min, $
    display_filter_slitwid_max: display_filter_slitwid_max, $
    sdir: '', $
    filelist: ptr_new(), $
    filelistall: ptr_new(), $
    file2obsmap: ptr_new(), $
    filehdr: ptr_new(), $
    file_spiobsids: ptr_new(), $
    fileselect: '', $
    foundOBS: foundOBS, $
    foundfiles: foundfiles, $
    ; searchstopbutton:searchstopbutton, $
    recentdroplist: recentdroplist, $
    recentwindows: recentwindows}
  info = ptr_new(info, /no_copy)

  ; Set the info ptr to be the user value of the tlb widget
  widget_control, tlb, set_uvalue = info

  widget_control, eis_icon, get_value = drawID
  wset, drawID
  fileName = concat_dir(getenv('ancillary'), 'eis_logo_sarah_small.jpg')
  IF (file_info(fileName)).exists THEN BEGIN
    read_jpeg, fileName, icon
    icon_resized = congrid(icon, 3, 120, 60)
    tvscl, icon_resized, true = 1
  ENDIF ELSE BEGIN
    xyouts, 0.5, 0.5, 'EIS', chars = chars, /normal, alignment = 0.5
  ENDELSE
  ;
  widget_control, iris_icon, get_value = drawID1
  wset, drawID1
  IF getenv('IRIS_ANCILLARY') EQ '' THEN $
    set_logenv, 'IRIS_ANCILLARY', concat_dir(getenv('SSW'), 'iris/idl/uio/ancillary/')
  fileName = concat_dir(getenv('IRIS_ANCILLARY'), 'iris_logo.jpg')
  IF (file_info(fileName)).exists THEN BEGIN
    read_jpeg, fileName, icon
    icon_resized = congrid(icon, 3, iris_icon_size, iris_icon_size * iris_icon_aspect)
    tvscl, icon_resized, true = 1
  ENDIF ELSE BEGIN
    xyouts, 0.5, 0.5, 'IRIS', chars = chars, /normal, alignment = 0.5
  ENDELSE
  ;
  widget_control, spice_icon, get_value = drawID2
  wset, drawID2
  have_con = have_proc('spice_xfiles', out = fname)
  IF have_con THEN BEGIN
    fileName = concat_dir(file_dirname(file_dirname(fname)), 'ancillary/spice-logo---colour.jpg')
    IF (file_info(fileName)).exists THEN BEGIN
      read_jpeg, fileName, icon
      icon_resized = congrid(icon, 3, 120, 120)
      tvscl, icon_resized, true = 1
    ENDIF ELSE BEGIN
      xyouts, 0.5, 0.5, 'SPICE', chars = chars, /normal, alignment = 0.5
    ENDELSE
  ENDIF ELSE BEGIN
    xyouts, 0.5, 0.5, 'SPICE', chars = chars, /normal, alignment = 0.5
  ENDELSE

  spice_xfiles_search_dir, info

  xmanager, 'spice_xfiles', tlb, /no_block, $
    cleanup = 'spice_xfiles_cleanup', event_handler = 'spice_xfiles_event' ; , /catch, no_block=0
END