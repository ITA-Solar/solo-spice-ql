;+
; NAME:
;     WRITE_IMAGE_REAL_SIZE
;
; PURPOSE:
;     This routine returns the path(s) and name(s) of SPICE raster files that correspond
;     to the specified time or lie within a given time window. The logic for what filename(s) is returned is as follows:
;
;     If only the start time is given, then the file closest to that time is returned or, if SEQUENCE keyword is set,
;     all files with the same SPIOBSID as the file closest to that time are returned.
;
;     If both, the end and start time are given, all files within the time window are returned or,
;     if SEQUENCE keyword is set, all SPICE observations that have at least one file within the time window are returned.
;
; CATEGORY:
;      IMAGES -- writing image files.
;
; CALLING SEQUENCE:
;     Result = SPICE_FIND_FILE(time_start [, time_end=time_end, level=level, $
;       top_dir=top_dir, path_index=path_index, count_file=count_file, count_seq=count_seq, $
;       /SEQUENCE, /ALL, /NO_LEVEL, /NO_TREE_STRUCT, /USER_DIR, /SEARCH_SUBDIR, /IGNORE_TIME ] )
;
; INPUTS:
;     TIME_START: This can be in any format accepted by the ANYTIM suite of
;                 routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;                 either the time for which the file closest to it is returned,
;                 or the start of the time window to be searched, in case 'time_end'
;                 is also provided.
;
; OPTIONAL INPUT:
;     TIME_END: This can be in any format accepted by the ANYTIM suite of
;               routines. For example, '1-jan-2010', '2010-01-01 05:00'. This is
;               the end of the time window to be searched.
;     LEVEL:    Desired level of the data (0, 1, 2 (default) or 3)
;     TOP_DIR:  Top directory in which the SPICE data lies. If not provided
;               the path given in $SPICE_DATA is searched.
;     PATH_INDEX: If $SPICE_DATA or TOP_DIR contains multiple paths, then this
;               keyword allows you to specify which path should be searched. Default is 0.
;
; KEYWORD PARAMETERS:
;     SEQUENCE: If set, then all files of the sequence that the found files belong to will
;               be returned, i.e. the time window to be searched is expanded to include files
;               outside of the given time window, but only sequences (= Spice observations)
;               that have at least one file in the given time window are returned.
;               If set and 'time_end' is provided, the returned value will be a LIST
;               in which each element is a string or string array with paths to SPICE FITS files
;               that belong to the same sequence.
;     ALL:      If set, then all filenames for the specified day will be returned.
;               Ignored if TIME_END is provided or if NO_TREE_STRUCT or SEQUENCE is set.
;     NO_LEVEL: If set, then the level part of the default path is omitted
;               (e.g. $SPICE_DATA/2020/06/21/ instead of $SPICE_DATA/level2/2020/06/21/)
;     NO_TREE_STRUCT: If set, then the tree structure won't be appended to TOP_DIR
;               (e.g. TOP_DIR/level2/ instead of TOP_DIR/level2/2020/06/21/)
;     USER_DIR: If set, the procedure searches in TOP_DIR/user/ instead of TOP_DIR/.
;     SEARCH_SUBDIR: If set then the program looks for spice files recurrently,
;               i.e. in all subdirectories
;     IGNORE_TIME: If set, TIME_START and TIME_END are ignored, and all files will be
;               returned. Ignored if NO_TREE_STRUCT is not set.
;               This keyword is used by spice_xfiles, it makes it possible to return
;               all files that are stored locally
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
;
; CALLS:
;
; MODIFICATION HISTORY:
;     Ver.1, 18-Oct-2022, Martin Wiesmann
;
;-
; $Id: 2022-10-18 15:39 CEST $


PRO prits_tools::write_image_real_size, image_data, filename, colortable=colortable, format=format, $
  xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
  xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
  title=title, $
  background=background, color=color, rgb_background=rgb_background, rgb_color=rgb_color, $
  show_plot=show_plot

  compile_opt idl2, static
  
  xs = 100
  ys = 700
  image_data=fltarr(xs,ys)
  for i=0,xs-1 do begin
    for j=0,ys-1 do begin
      image_data[i,j] = ((i+j) mod 2) * randomn(seed)*3
    endfor
  endfor
  filename = '~/temp/test_pt.png'


  prits_tools.parcheck, image_data, 1, "image_data", 'NUMERIC', 2
  prits_tools.parcheck, filename, 2, "filename", 'STRING', 0
  prits_tools.parcheck, colortable, 0, "colortable", 'INTEGERS', 0, minval=0, default=0
  prits_tools.parcheck, format, 0, "format", 'STRING', 0, default='JPEG'
  prits_tools.parcheck, xrange1, 0, "xrange1", ['NUMERIC', 'undefined'], 1, valid_nelements=2
  prits_tools.parcheck, xrange2, 0, "xrange2", ['NUMERIC', 'undefined'], 1, valid_nelements=2
  prits_tools.parcheck, yrange1, 0, "yrange1", ['NUMERIC', 'undefined'], 1, valid_nelements=2
  prits_tools.parcheck, yrange2, 0, "yrange2", ['NUMERIC', 'undefined'], 1, valid_nelements=2
  prits_tools.parcheck, xtitle1, 0, "xtitle1", ['STRING', 'undefined'], 0
  prits_tools.parcheck, xtitle2, 0, "xtitle2", ['STRING', 'undefined'], 0
  prits_tools.parcheck, ytitle1, 0, "ytitle1", ['STRING', 'undefined'], 0
  prits_tools.parcheck, ytitle2, 0, "ytitle2", ['STRING', 'undefined'], 0
  prits_tools.parcheck, title, 0, "title", ['STRING', 'undefined'], 0
  prits_tools.parcheck, background, 0, "background", 'INTEGERS', 0, minval=0, maxval=255, default=0
  prits_tools.parcheck, color, 0, "color", 'INTEGERS', 0, minval=0, maxval=255, default=255
  prits_tools.parcheck, rgb_background, 0, "rgb_background", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[255, 255, 255]
  prits_tools.parcheck, rgb_color, 0, "rgb_color", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[0, 0, 0]

  ; Get the current colortable to restore it at the end
  TVLCT, Red_old, Green_old, Blue_old, /GET

  ; Install the new colortable and set the background and text color
  loadct, colortable
  tvlct,r,g,b,/get
  ;background color
  r[0]=rgb_background[0]
  g[0]=rgb_background[1]
  b[0]=rgb_background[2]
  ;text color
  r[255]=rgb_color[0]
  g[255]=rgb_color[1]
  b[255]=rgb_color[2]
  tvlct,r,g,b
  
  saveplot = ~keyword_set(show_plot)

  margin_left = 5
  IF keyword_set(xtitle1) THEN margin_left += 10
  IF keyword_set(yrange1) THEN BEGIN
    n_digits_y1 = max(strlen(trim(string(yrange1))))
    if saveplot then begin
      margin_left += 7 * n_digits_y1
    endif else begin
      margin_left += 5 * n_digits_y1
    endelse
  ENDIF

  margin_right = 5
  IF keyword_set(xtitle2) THEN margin_right += 10
  IF keyword_set(yrange2) THEN BEGIN
    n_digits_y2 = max(strlen(trim(string(yrange2))))
    if saveplot then begin
      margin_right += 6 * n_digits_y2
    endif else begin
      margin_right += 5 * n_digits_y2
    endelse
  ENDIF

  margin_top = 5
  IF keyword_set(ytitle2) THEN margin_top += 10
  IF keyword_set(xrange2) THEN margin_top += 10

  margin_bottom = 5
  IF keyword_set(ytitle1) THEN margin_bottom += 10
  IF keyword_set(xrange1) THEN margin_bottom += 10


  WINsize = [xs+margin_left+margin_right, ys+margin_top+margin_bottom]
  print,WINSize
  Win_position = [double(margin_left)/WINsize[0], double(margin_bottom)/WINsize[1], $
    (double(xs+margin_left))/WINsize[0], (double(ys+margin_bottom))/WINsize[1]]

  if saveplot then set_plot,'z' else set_plot,'x'
  ;!p.background=253
  ;!p.color=0
  ;!p.ticklen=-.02
  ;!p.charsize=1  ;those 2 are defined above
  ;!p.charthick=1
  if saveplot then device,set_res=WINsize $
  else window, 16, xs=WINsize[0], ys=WINsize[1]


  pih, image_data, 0.01, position=Win_position, $
    xstyle=5, ystyle=5, top=253, $
    background=255, color=254, title='adsfas'

  if saveplot then charsize=1 else charsize=1.15
  axis, xaxis=0, xrange=xrange1, xtit='Solar X [arcsec]', xstyle=1, color=254, charsize=charsize, xticks=2
  axis, xaxis=1, xrange=xrange2, xstyle=1, color=254, charsize=charsize, xticks=2
  axis, yaxis=0, yrange=yrange1, ytit='Solar Y [arcsec]', ystyle=1, color=254, charsize=charsize
  axis, yaxis=1, yrange=yrange2, ystyle=1, color=254, charsize=charsize


  write_image,filename,'JPEG',tvrd(),r,g,b


;
;
;  ; save image
;  if saveplot then begin
;    file=filename+'_'+trim(string(ys))+'_tv.png'
;    write_png,file,tvrd(),r,g,b
;
;    file=filename+'_'+trim(string(ys))+'_tv_2.png'
;    write_image,file,'PNG',tvrd(),r,g,b
;
;    help,tvrd()
;    help,tvrd(/true)
;    image_raw = tvrd()
;    size_raw = size(image_raw)
;    image_true = bytarr(3, size_raw[1], size_raw[2])
;    for i=0,size_raw[1]-1 do begin
;      for j=0,size_raw[2]-1 do begin
;        image_true[0,i,j] = r[image_raw[i,j]]
;        image_true[1,i,j] = g[image_raw[i,j]]
;        image_true[2,i,j] = b[image_raw[i,j]]
;      endfor
;    endfor
;    file=filename+'_'+trim(string(ys))+'_tv.jpg'
;    write_jpeg,file,image_true,/true
;
;    file=filename+'_'+trim(string(ys))+'_tv_2.jpg'
;    write_image,file,'JPEG',tvrd(),r,g,b
;
;
;

    TVLCT, Red_old, Green_old, Blue_old


  END
