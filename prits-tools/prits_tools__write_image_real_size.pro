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
; RESTRICTIONS:
; Setting TITLE plus upper axis XTITLE2 and/or XRANGE2 results in a messy output!
;
; MODIFICATION HISTORY:
;     Ver.1, 18-Oct-2022, Martin Wiesmann
;
;-
; $Id: 2022-10-19 11:58 CEST $


PRO prits_tools::write_image_real_size, image_data, filename, colortable=colortable, format=format, $
  xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
  xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
  title=title, $
  background=background, text=text, rgb_background=rgb_background, rgb_text=rgb_text, $
  show_plot=show_plot

  compile_opt idl2, static

  prits_tools.parcheck, image_data, 1, "image_data", 'NUMERIC', 2
  prits_tools.parcheck, filename, 2, "filename", 'STRING', 0, default=''
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
  prits_tools.parcheck, text, 0, "text", 'INTEGERS', 0, minval=0, maxval=255, default=255
  prits_tools.parcheck, rgb_background, 0, "rgb_background", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[255, 255, 255]
  prits_tools.parcheck, rgb_text, 0, "rgb_text", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[0, 0, 0]

  DEVICE, DECOMPOSED = 0

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
  r[255]=rgb_text[0]
  g[255]=rgb_text[1]
  b[255]=rgb_text[2]
  tvlct,r,g,b

  show_plot = keyword_set(show_plot)
  IF ~show_plot && filename EQ '' THEN BEGIN
    filename = 'image.'+format
    message, 'No filename provided. Saving image in '+filename+' in current directory', /info
  ENDIF

  margin_left = 5
  IF keyword_set(ytitle1) THEN margin_left += 30
  IF keyword_set(yrange1) THEN BEGIN
    n_digits_y1 = max(strlen(trim(string(yrange1))))
    margin_left += 7
    margin_left += 8 * n_digits_y1
  ENDIF

  margin_right = 5
  IF keyword_set(ytitle2) THEN margin_right += 30
  IF keyword_set(yrange2) THEN BEGIN
    n_digits_y2 = max(strlen(trim(string(yrange2))))
    margin_right += 7
    margin_right += 8 * n_digits_y2
  ENDIF

  margin_bottom = 5
  IF keyword_set(xtitle1) && keyword_set(xrange1) THEN BEGIN
    margin_bottom += 40
  ENDIF ELSE IF keyword_set(xtitle1) THEN BEGIN
    margin_bottom += 35
  ENDIF ELSE IF keyword_set(xrange1) THEN BEGIN
    margin_bottom += 20
  ENDIF

  margin_top = 5
  IF keyword_set(title) THEN BEGIN
    margin_top += 20
    IF keyword_set(xtitle2) || keyword_set(xrange2) THEN BEGIN
      message, 'Setting TITLE plus upper axis XTITLE2 and/or XRANGE2 results in a messy output!', /info
    ENDIF
  ENDIF
  IF keyword_set(xtitle2) && keyword_set(xrange2) THEN BEGIN
    margin_top += 30
  ENDIF ELSE IF keyword_set(xtitle2) THEN BEGIN
    margin_top += 30
  ENDIF ELSE IF keyword_set(xrange2) THEN BEGIN
    margin_top += 18
  ENDIF

  size_image = size(image_data)
  xs = size_image[1]
  ys = size_image[2]
  WINsize = [xs+margin_left+margin_right, ys+margin_top+margin_bottom]
  Win_position = [double(margin_left)/WINsize[0], double(margin_bottom)/WINsize[1], $
    (double(xs+margin_left))/WINsize[0], (double(ys+margin_bottom))/WINsize[1]]

  if show_plot then begin
    set_plot, 'x'
    window, 16, xs=WINsize[0], ys=WINsize[1]
  endif else begin
    set_plot, 'z'
    device, set_res=WINsize
  endelse


  pih, image_data, 0.01, position=Win_position, $
    xstyle=5, ystyle=5, top=254, bottom=1, $
    background=0, color=255, title=title

  if show_plot then charsize=1.15 else charsize=1

  IF keyword_set(xrange1) THEN BEGIN
    xticks=0
    xtickname=''
  ENDIF ELSE BEGIN
    xticks=1
    xtickname=REPLICATE(' ', 2)
  ENDELSE
  axis, xaxis=0, xrange=xrange1, xtitle=xtitle1, xstyle=1, color=255, charsize=charsize, xticks=xticks, xtickname=xtickname

  IF keyword_set(xrange2) THEN BEGIN
    xticks=0
    xtickname=''
  ENDIF ELSE BEGIN
    xticks=1
    xtickname=REPLICATE(' ', 2)
  ENDELSE
  axis, xaxis=1, xrange=xrange2, xtitle=xtitle2, xstyle=1, color=255, charsize=charsize, xticks=xticks, xtickname=xtickname

  IF keyword_set(yrange1) THEN BEGIN
    yticks=0
    ytickname=''
  ENDIF ELSE BEGIN
    yticks=1
    ytickname=REPLICATE(' ', 2)
  ENDELSE
  axis, yaxis=0, yrange=yrange1, ytitle=ytitle1, ystyle=1, color=255, charsize=charsize, yticks=yticks, ytickname=ytickname

  IF keyword_set(yrange2) THEN BEGIN
    yticks=0
    ytickname=''
  ENDIF ELSE BEGIN
    yticks=1
    ytickname=REPLICATE(' ', 2)
  ENDELSE
  axis, yaxis=1, yrange=yrange2, ytitle=ytitle2, ystyle=1, color=255, charsize=charsize, yticks=yticks, ytickname=ytickname

  IF ~show_plot THEN write_image, filename, format, tvrd(), r, g, b

  ; Set previous colortable again
  TVLCT, Red_old, Green_old, Blue_old
END





PRO prits_tools::write_image_real_size_test
  compile_opt static
  print,''
  print,'write_image_real_size_test'
  print,''

  format='PNG'
  filename = '~/temp/test_pt.png'

  ;show_plot = 1
  colortable = 72

  ;title='My Sun'

  xtitle1='Solar X'
  xrange1=[1,9]

  ;xtitle2='Solar Z'
  xrange2=[1,9]

  ytitle1='Solar Y'
  yrange1=[-1111,111]

  ;ytitle2='Solarplex'
  yrange2=[-1111,111]

  xs = 100
  ys = 700
  image_data=fltarr(xs,ys)
  for i=0,xs-1 do begin
    for j=0,ys-1 do begin
      image_data[i,j] = ((i+j) mod 2) * (randomn(seed)*30 + i +j)
    endfor
  endfor


  prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
    xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
    xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
    title=title, $
    background=background, text=text, rgb_background=rgb_background, rgb_text=rgb_text, $
    show_plot=show_plot

END



IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
  IF getenv("USER") EQ "steinhh" THEN add_path, "$HOME/idl/solo-spice-ql", /expand
  prits_tools.write_image_real_size_test
ENDIF

END
