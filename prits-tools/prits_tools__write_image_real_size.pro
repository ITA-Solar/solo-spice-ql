;+
; NAME:
;     WRITE_IMAGE_REAL_SIZE
;
; PURPOSE:
;     This routine creates an image of the data including optional axis titles and ranges. The focus of this
;     procedure is the size of the data image. The size of the image is calculated so that the data image
;     itself has the exact desired size, by default the same size as the input data array, so that each pixel
;     in the data array is represented by one pixel in the output file.
;     The size of the image can be manipulated with the keywords SCALE_FACTOR, HEIGHT and WIDTH.
;     The image can be saved in different file formats.
;
; CATEGORY:
;      IMAGES -- writing image files.
;
; CALLING SEQUENCE:
;     prits_tools.write_image_real_size, IMAGE_DATA [, FILENAME]
;     [,REMOVE_TRENDS=REMOVE_TRENDS] [,SMOOTH=SMOOTH] [, COLORTABLE=COLORTABLE] [, FORMAT=FORMAT] $
;       [, XRANGE1=XRANGE1] [, XRANGE2=XRANGE2] [, YRANGE1=YRANGE1] [, YRANGE2=YRANGE2] $
;       [, XTITLE1=XTITLE1] [, XTITLE2=XTITLE2] [, YTITLE1=YTITLE1] [, YTITLE2=YTITLE2] $
;       [, TITLE=TITLE] $
;       [, BACKGROUND_COLOR=BACKGROUND_COLOR] [, TEXT_COLOR=TEXT_COLOR] $
;       [, BORDER=BORDER] [, SCALE_FACTOR=SCALE_FACTOR] [, HEIGHT=HEIGHT] [, WIDTH=WIDTH] $
;       [, /SCALE_TO_RANGE] [, /NO_AXIS] $
;       [, CUTOFF_THRESHOLD=CUTOFF_THRESHOLD] [, COLOR_CENTER_VALUE=COLOR_CENTER_VALUE] $
;       [, JPEG_QUALITY=JPEG_QUALITY] $
;       [, /SHOW_PLOT] [, /REVERSE_COLORTABLE] $
;       [, _EXTRA=_EXTRA]
;
; INPUTS:
;     IMAGE_DATA: A 2-dimensional numeric array. This is the data/image to be plotted.
;     FILENAME: A string. The filename (and path) for the output file, in which the image should be saved.
;               Default is 'image.xxx' (where xxx is the chosen file format) in the current directory.
;
; OPTIONAL INPUT:
;     SMOOTH: An integer. The width of the boxcar used when smoothing the
;             image using the smooth function. If not set no smoothing is performed.
;     COLORTABLE: An integer. The number of the colortable to be used. See here for a list of colortables:
;                 https://www.l3harrisgeospatial.com/docs/loadingdefaultcolortables.html . Setting this keyword 
;                 to 100 (a color table that doesn't exist) signals that the 
;                 input image_data is a velocity image that needs special
;                 treatment, among other things using the eis_colors,/velocity
;                 red-blue color table.
;     FORMAT:   A string, indicating the file format in which the image should be saved to.
;               Possible values: BMP, GIF, JPEG, PNG, PPM, SRF, TIFF, JPEG2000 (=JP2). Default is JPEG.
;     XRANGE1:  A 2-element numeric vector, indicating the data range displayed on the lower axis.
;               If not provided, no tick marks will be shown on the lower axis.
;     XRANGE2:  A 2-element numeric vector, indicating the data range displayed on the upper axis.
;               If not provided, no tick marks will be shown on the upper axis.
;     YRANGE1:  A 2-element numeric vector, indicating the data range displayed on the left axis.
;               If not provided, no tick marks will be shown on the left axis.
;     YRANGE2:  A 2-element numeric vector, indicating the data range displayed on the right axis.
;               If not provided, no tick marks will be shown on the right axis.
;     XTITLE1:  A string, that will be used as the title for the lower axis.
;     XTITLE2:  A string, that will be used as the title for the upper axis.
;     YTITLE1:  A string, that will be used as the title for the left axis.
;     YTITLE2:  A string, that will be used as the title for the right axis.
;     TITLE:    A string, that will be used as the title of the image. Note that providing both TITLE
;               and XTITLE2 and/or XRANGE2 will result in a messy output.
;     BACKGROUND_COLOR: A 3-element byte vector, giving the color for the background of the image.
;               Default is [255,255,255] (white).
;     TEXT_COLOR: A 3-element byte vector, giving the color of the axis and text in the image.
;               Default is [0,0,0] (black).
;     BORDER:   An integer giving the number of pixels that should be added around the image.
;               Default is 5. If this is set to zero and none of the TITLE and RANGE inputs are provided,
;               then the image is plotted with suppressed axis.
;     SCALE_FACTOR: A number. The data size is expanded by this factor. Default is 1.0. This is ignored
;               if SCALE_TO_RANGE is set.
;     HEIGHT:   An integer giving the desired height of the data image. WIDTH is calculated if not provided.
;               This is ignored if SCALE_FACTOR is provided.
;     WIDTH:    An integer giving the desired width of the data image. HEIGHT is calculated if not provided.
;               This is ignored if SCALE_FACTOR is provided.
;     CUTOFF_THRESHOLD: A fraction to be used in histo_opt to scale the image. Default is 0.02.
;               Set to zero to suppress calling histo_opt.
;     COLOR_CENTER_VALUE: If provided, then this value will be set as the center of the colortable. Useful
;               for e.g. displaying velocity with a blue/red colortable.
;     JPEG_QUALITY: This keyword specifies the quality index, in the range of 0 (terrible) to 100 (excellent)
;               for the JPEG file. The default value is 75, which corresponds to very good quality. Lower values
;               of QUALITY produce higher compression ratios and smaller files.
;               This input is ignored if the file format is not JPEG.
;     _EXTRA:   Extra keywords sent to PIH and through it to PLOT and to WRITE_IMAGE and through it to
;               WRITE_BMP, WRITE_GIF, WRITE_JPEG, WRITE_PNG, WRITE_PPM, WRITE_SRF, WRITE_TIFF and WRITE_JPEG2000.
;
; KEYWORD PARAMETERS:
;     INTERPOLATION: If set, then the image is expanded with bilinear interpolation.
;               This keyword should not be set, if SMOOTH input is provided.
;     REMOVE_TRENDS: If set, remove horizontal and vertical trends in the image
;     SCALE_TO_RANGE: If set, then the width/height ratio of the image will be adjusted to the given
;               XRANGE1 and YRANGE1. If neither HEIGHT nor WIDTH is provided, then the width of the 
;               image will be adjusted. 
;               This keyword is ignored if XRANGE1 and YRANGE1 are not provided, or if both HEIGHT and WIDTH
;               are provided.
;     NO_AXIS: If set, then no axis will be plotted, eventhough XRANGEn and/or YRANGEn is provided.
;               Useful if you want to provide XRANGE1 and YRANGE1 to be able to set SCALE_TO_RANGE.
;     SHOW_PLOT: If set, then the image is shown on the screen and not saved into a file.
;     REVERSE_COLORTABLE: If set, then the colors of the given colortable are reversed. Useful for e.g.
;               ColorBrewer Schemes.
;
; OUTPUTS:
; This routine saves the image into a file.
;
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
; See prits_tools::write_image_real_size_test
;
; CALLS:
; PIH, EIS_COLORS
;
; RESTRICTIONS:
; Setting TITLE plus upper axis XTITLE2 and/or XRANGE2 results in a messy output!
;
; MODIFICATION HISTORY:
;     Ver.1, 18-Oct-2022, Martin Wiesmann
;     Ver.2, 22-Jan-2023, Terje Fredvik - added special treatment of velocity
;     images
;     Ver.3, 08-Feb-2024, Terje Fredvik - added keyword remove_trend, if set
;     remove horizontal and vertical trends in the image. Added keyword
;     smooth, can be set to the width of the boxcar used by smooth
;
;-
; $Id: 2024-02-13 12:57 CET $



PRO prits_tools::write_image_real_size, image_data, filename, remove_trends = remove_trends, smooth = smooth, $
  colortable=colortable, format=format, interpolation=interpolation, $
  xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
  xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
  title=title, $
  background_color=background_color, text_color=text_color, $
  border=border, scale_factor=scale_factor, height=height, width=width, $ 
  SCALE_TO_RANGE=SCALE_TO_RANGE, no_axis=no_axis, $
  cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, $
  jpeg_quality=jpeg_quality, $
  show_plot=show_plot, reverse_colortable=reverse_colortable, $
  _extra=_extra

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
  prits_tools.parcheck, background_color, 0, "background_color", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[255, 255, 255]
  prits_tools.parcheck, text_color, 0, "text_color", 'INTEGERS', 1, valid_nelements=3, $
    minval=0, maxval=255, default=[0, 0, 0]
  prits_tools.parcheck, border, 0, "border", 'INTEGERS', 0, minval=0, default=5
  prits_tools.parcheck, scale_factor, 0, "scale_factor", 'numeric', 0, minval=1e-6, /optional
  prits_tools.parcheck, height, 0, "height", 'INTEGERS', 0, minval=2, /optional
  prits_tools.parcheck, width, 0, "width", 'INTEGERS', 0, minval=2, /optional
  prits_tools.parcheck, cutoff_threshold, 0, "cutoff_threshold", 'NUMERIC', 0, minval=0, maxval=1, default=0.02
  prits_tools.parcheck, color_center_value, 0, "color_center_value", 'NUMERIC', 0, /optional
  prits_tools.parcheck, jpeg_quality, 0, "jpeg_quality", 'numeric', 0, minval=0, maxval=100, default=75
  prits_tools.parcheck, smooth, 0, "smooth", 'numeric', 0, minval=0, /optional
  
  show_plot = keyword_set(show_plot)
  
  IF show_plot THEN set_plot,'x' ELSE set_plot,'z'
  
  DEVICE, DECOMPOSED = 0  

  ; Get the current colortable to restore it at the end
  TVLCT, Red_old, Green_old, Blue_old, /GET

                                ; Install the new colortable and set the background and text color
  cutoff_threshold_old = cutoff_threshold
  velocity = colortable EQ 100
  IF velocity THEN BEGIN 
     eis_colors, /velocity
     cutoff_threshold = 0
     value_max = 50             
     value_min = -50
  ENDIF ELSE loadct, colortable
  
  tvlct,r,g,b,/get
  
  IF keyword_set(reverse_colortable) THEN BEGIN
    r = reverse(r)
    g = reverse(g)
    b = reverse(b)
 ENDIF
  
  ;background color
 
  r[0]=background_color[0]
  g[0]=background_color[1]
  b[0]=background_color[2]

     
  ;text color
  r[255]=text_color[0]
  g[255]=text_color[1]
  b[255]=text_color[2]
  tvlct,r,g,b

  
  IF ~show_plot && filename EQ '' THEN BEGIN
    filename = 'image.'+format
    message, 'No filename provided. Saving image in '+filename+' in current directory', /info
  ENDIF

  margin_left = border
  IF keyword_set(ytitle1) THEN margin_left += 30
  IF keyword_set(yrange1) && ~keyword_set(no_axis) THEN BEGIN
    n_digits_y1 = max(strlen(trim(string(ceil(yrange1)))))
    margin_left += 7
    margin_left += 8 * n_digits_y1
  ENDIF

  margin_right = border
  IF keyword_set(ytitle2) THEN margin_right += 30
  IF keyword_set(yrange2) && ~keyword_set(no_axis) THEN BEGIN
    n_digits_y2 = max(strlen(trim(string(ceil(yrange2)))))
    margin_right += 7
    margin_right += 8 * n_digits_y2
  ENDIF

  margin_bottom = border
  IF keyword_set(xtitle1) && keyword_set(xrange1) && ~keyword_set(no_axis) THEN BEGIN
    margin_bottom += 40
  ENDIF ELSE IF keyword_set(xtitle1) THEN BEGIN
    margin_bottom += 35
  ENDIF ELSE IF keyword_set(xrange1) && ~keyword_set(no_axis) THEN BEGIN
    margin_bottom += 20
  ENDIF

  margin_top = border
  IF keyword_set(title) THEN BEGIN
    margin_top += 20
    IF keyword_set(xtitle2) || (keyword_set(xrange2) && ~keyword_set(no_axis)) THEN BEGIN
      message, 'Setting TITLE plus upper axis XTITLE2 and/or XRANGE2 results in a messy output!', /info
    ENDIF
  ENDIF
  IF keyword_set(xtitle2) && keyword_set(xrange2) && ~keyword_set(no_axis) THEN BEGIN
    margin_top += 30
  ENDIF ELSE IF keyword_set(xtitle2) THEN BEGIN
    margin_top += 30
  ENDIF ELSE IF keyword_set(xrange2) && ~keyword_set(no_axis) THEN BEGIN
    margin_top += 18
  ENDIF

  size_image = size(image_data)
  xs = size_image[1]
  ys = size_image[2]
  IF keyword_set(SCALE_TO_RANGE) && keyword_set(xrange1) && keyword_set(yrange1) && $
    ~(keyword_set(HEIGHT) && keyword_set(WIDTH)) THEN BEGIN
    
    xyratio = (yrange1[1] - yrange1[0]) / (xrange1[1] - xrange1[0])
    IF keyword_set(height) THEN BEGIN
      ys = height
      xs = height / xyratio
    ENDIF ELSE IF keyword_set(width) THEN BEGIN
      xs = width
      ys = width * xyratio
    ENDIF ELSE BEGIN
      xs = ys / xyratio
    ENDELSE

  ENDIF ELSE IF keyword_set(scale_factor) THEN BEGIN
    xs = round(double(xs)*scale_factor)
    ys = round(double(ys)*scale_factor)
  ENDIF ELSE BEGIN
    IF keyword_set(height) && keyword_set(width) THEN BEGIN
      xs = width
      ys = height
    ENDIF ELSE IF keyword_set(height) THEN BEGIN
      scale_factor = double(height) / double(ys)
      xs = round(double(xs)*scale_factor)
      ys = height
    ENDIF ELSE IF keyword_set(width) THEN BEGIN
      scale_factor = double(width) / double(xs)
      ys = round(double(ys)*scale_factor)
      xs = width
    ENDIF
  ENDELSE
  WINsize = [xs+margin_left+margin_right, ys+margin_top+margin_bottom]
  Win_position = [double(margin_left)/WINsize[0], double(margin_bottom)/WINsize[1], $
    (double(xs+margin_left))/WINsize[0], (double(ys+margin_bottom))/WINsize[1]]

  if show_plot then begin
    window, 16, xs=WINsize[0], ys=WINsize[1]
  endif else begin
    device, set_res=WINsize
  endelse
  
  IF keyword_set(smooth)        THEN image_data = smooth(image_data, smooth)
  IF keyword_set(remove_trends) THEN image_data = prits_tools.remove_trends(image_data, value_min=value_min, value_max=value_max)
     
  image_data_use = (cutoff_threshold GT 0) ? HISTO_OPT(image_data, cutoff_threshold) : image_data

 IF N_ELEMENTS(color_center_value) EQ 1 THEN BEGIN
    max_image = max(abs(image_data_use-color_center_value))
    min_image = -1 * max_image + color_center_value
    max_image += color_center_value
  ENDIF

  pih, image_data_use, position=Win_position, $
    xstyle=5, ystyle=5, top=254, bottom=1, $
    background=0, color=255, title=title, $
    min=min_image, max=max_image, smooth=interpolation, $
    _extra=_extra

  if show_plot then charsize=1.15 else charsize=1

  IF border GT 0 || keyword_set(title) || $
    keyword_set(xtitle1) || keyword_set(xrange1) || $
    keyword_set(xtitle2) || keyword_set(xrange2) || $
    keyword_set(ytitle1) || keyword_set(yrange1) || $
    keyword_set(ytitle2) || keyword_set(yrange2) THEN BEGIN

    IF keyword_set(xrange1) && ~keyword_set(no_axis) THEN BEGIN
      n_digits_x1 = max(strlen(trim(string(ceil(xrange1)))))
      xticks = floor(double(xs) / 15.0d / n_digits_x1)
      IF xticks EQ 0 THEN xticks=1
      IF xticks GT 3 THEN xticks=0
      xtickname=''
    ENDIF ELSE BEGIN
      xticks=1
      xtickname=REPLICATE(' ', 2)
    ENDELSE
    axis, xaxis=0, xrange=xrange1, xtitle=xtitle1, xstyle=1, color=255, charsize=charsize, $
      xticks=xticks, xtickname=xtickname, xtickformat='(I)'

    IF keyword_set(xrange2) && ~keyword_set(no_axis) THEN BEGIN
      n_digits_x2 = max(strlen(trim(string(ceil(xrange2)))))
      xticks = floor(double(xs) / 15.0d / n_digits_x2)
      IF xticks EQ 0 THEN xticks=1
      IF xticks GT 3 THEN xticks=0
      xtickname=''
    ENDIF ELSE BEGIN
      xticks=1
      xtickname=REPLICATE(' ', 2)
    ENDELSE
    axis, xaxis=1, xrange=xrange2, xtitle=xtitle2, xstyle=1, color=255, charsize=charsize, $
      xticks=xticks, xtickname=xtickname, xtickformat='(I)'

    IF keyword_set(yrange1) && ~keyword_set(no_axis) THEN BEGIN
      yticks=0
      ytickname=''
    ENDIF ELSE BEGIN
      yticks=1
      ytickname=REPLICATE(' ', 2)
    ENDELSE
    axis, yaxis=0, yrange=yrange1, ytitle=ytitle1, ystyle=1, color=255, charsize=charsize, $
      yticks=yticks, ytickname=ytickname, ytickformat='(I)'

    IF keyword_set(yrange2) && ~keyword_set(no_axis) THEN BEGIN
      yticks=0
      ytickname=''
    ENDIF ELSE BEGIN
      yticks=1
      ytickname=REPLICATE(' ', 2)
    ENDELSE
    axis, yaxis=1, yrange=yrange2, ytitle=ytitle2, ystyle=1, color=255, charsize=charsize, $
      yticks=yticks, ytickname=ytickname, ytickformat='(I)'

  ENDIF

  IF ~show_plot THEN write_image, filename, format, tvrd(), r, g, b, quality=jpeg_quality, _extra=_extra

  ; Set previous colortable again
  TVLCT, Red_old, Green_old, Blue_old
  cutoff_threshold = cutoff_threshold_old
  set_plot, 'x'
END





PRO prits_tools::write_image_real_size_test
  compile_opt static
  print,''
  print,'write_image_real_size_test'
  print,''

  format='PNG'
  filename = '~/temp/test_pt.png'

  ;format='JPEG'
  ;filename = '~/temp/test_pt.jpg'
  ;jpeg_quality = 30

  ;show_plot = 1
  colortable = 70
  ;border = 3
  ;scale_factor = 0.2
  ;height = 400
  ;width = 40

  ;title='My Sun'

  xtitle1='Solar X'
  xrange1=[100,900]

  ;xtitle2='Solar Z'
  xrange2=[1,9]

  ytitle1='Solar Y'
  yrange1=[-1111,111]

  ;ytitle2='Solarplex'
  yrange2=[-1111,111]

  ;background_color = [255,0,0]
  ;text_color = [88,233,23]

  xs = 100
  ys = 700
  image_data=fltarr(xs,ys)
  for i=0,xs-1 do begin
    for j=0,ys-1 do begin
      image_data[i,j] = ((i+j) mod 2) * (randomn(seed)*5 + i +j)
    endfor
  endfor


  prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
    xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
    xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
    title=title, $
    background_color=background_color, text_color=text_color, $
    border=border, scale_factor=scale_factor, height=height, width=width, $
    jpeg_quality=jpeg_quality, $
    show_plot=show_plot

END



IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
  IF getenv("USER") EQ "steinhh" THEN add_path, "$HOME/idl/solo-spice-ql", /expand
  prits_tools.write_image_real_size_test
ENDIF

END
