;+
; NAME:
;     WRITE_JPG
;; PURPOSE:
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
;     prits_tools.write_image_real_size, IMAGE_DATA [, FILENAME] $
;       [, /REMOVE_TRENDS] [,SMOOTH=SMOOTH] [, COLORTABLE=COLORTABLE] [, FORMAT=FORMAT] $
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
;     SMOOTH:   An integer. The width of the boxcar used when smoothing the
;               image using the smooth function. If not set no smoothing is performed.
;     COLORTABLE: An integer. The number of the colortable to be used. See here for a list of colortables:
;               https://www.l3harrisgeospatial.com/docs/loadingdefaultcolortables.html . Setting this keyword 
;               to 100 (a color table that doesn't exist) signals that the 
;               input image_data is a velocity image that needs special
;               treatment, among other things using the eis_colors,/velocity
;               red-blue color table.
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
;     REMOVE_HORIZONTAL_TREND: If set, remove horizontal trend in the image
;     REMOVE_VERTICAL_TREND: If set, remove vertical trend in the image
;     SCALE_TO_RANGE: If set, then the width/height ratio of the image will be adjusted to the given
;               XRANGE1 and YRANGE1. If neither HEIGHT nor WIDTH is provided, then the width of the 
;               image will be adjusted. 
;               This keyword is ignored if XRANGE1 and YRANGE1 are not provided, or if both HEIGHT and WIDTH
;               are provided.
;     NO_AXIS:  If set, then no axis will be plotted, eventhough XRANGEn and/or YRANGEn is provided.
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
;     Ver. 4, 14-May-2024, Terje Fredvik - replaced remove_trend keyword with
;     remove_vertical_trend and remove_horizontal_trend
;     Ver. 5, 03-Jun-2024, Terje Fredvik - New keyword fit_trend. Remove min value from line width
;     images. 
;     Ver. 6, 03-Jul-2024, Terje Fredvik - New keywords value_max and
;     value_min. Currently only used as upper and lower limits for velocity images
;
;-
; $Id: 2024-10-08 09:26 CEST $
FUNCTION spice_get_units, parameter
  CASE parameter OF 
     'int': units = '$W/m^2/sr/nm$' 
     'vel': units = 'km/s'
     'wid': units = 'nm'
  ENDCASE 
  return, units
END

PRO spice_plot_texts, l2_header, font_size=font_size
  font_size = font_size+2
  
  date_beg = (fxpar(l2_header,'DATE-BEG')).substring(0,18)
  time = '           '+date_beg.extract('[0-9]+:[0-9]+')+'   '
  
  
  tDate = text(0.01,0.98,date_beg,/NORMAL,color='black', font_name='courier', font_size=font_size)
  tTime = text(0.01,0.98,time,    /NORMAL,color='red',   font_name='courier', font_size=font_size)
  
  id = (fxpar(l2_header,'filename')).extract('([0-9]+)-([0-9]+)',/subex)
  spiobsid_rasterno = id[0]
  spiobsid = id[1]
  rasterno = id[2]
  format = '(A'+ trim(spiobsid.strlen()+1) +')'
  
  tSpiobsid =text(0.01,0.96,spiobsid_rasterno,/NORMAL,color='dim gray',font_name='courier', font_size=font_size)
  tSpiobsid =text(0.01,0.96,string('',format=format)+rasterno,/NORMAL,color='blue',font_name='courier', font_size=font_size)
; tCrota  = text(0.01, 0.01, 'S/C roll: '+string(fxpar(l2_header,'CROTA'),format='(f6.3)')+'$\deg$', color='Green')
  
  f = 'courier'
  c = 'Green'
  fs = font_size-3
  tDsunAu = text(0.01, 0.041, 'S/C-Sun distance: '+string(fxpar(l2_header,'DSUN_AU'),  format='(f8.3)')+' AU',    font_name=f, color=c, font_size=fs)
  tLat    = text(0.01, 0.023, 'S/C latitude:     '+string(fxpar(l2_header,'HGLT_OBS'), format='(f8.2)')+'$\deg$', font_name=f, color=c, font_size=fs)
  tLon    = text(0.01, 0.005, 'S/C longitude:    '+string(fxpar(l2_header,'HGLN_OBS'), format='(f8.2)')+'$\deg$', font_name=f, color=c, font_size=fs)
  
END

FUNCTION spice_get_title, filename, parameter
  hLines = spice_line_list()
  lambda = filename.extract('([0-9]+)nm([0-9]+)',/subexp)
  lambda = lambda[1]+'.'+lambda[2]
  ion = hLines[float(lambda)]
  
  CASE parameter OF 
     'int': param_txt = ' Intensity'
     'vel': param_txt = ' Velocity'
     'wid': param_txt = ' Width'
  ENDCASE 
  
  title = [ion + ' '+lambda + ' nm', param_txt] 
  
  return, title
END

PRO spice_init_graphics_window, sz_padded, filename, buffer=buffer, $
                                font_size=font_size
  
  winsize_padded = [sz_padded[1]+150, sz_padded[2]+320]
  
  font_size = 12
  position = [0.55,0.5]
  
  name = (filename.extract('([0-9]+)-[0-9]+',/subexp))[1]
  
  win = getwindows(name)
  win_exists = obj_valid(win)
  
  create_window = (~win_exists || ~array_equal(win.dimensions, winsize_padded))
  
  IF create_window THEN win = window(dimensions=winsize_padded, font_size=font_size, name=name, buffer=buffer)
  
  win.setCurrent
  win.erase
 
END

FUNCTION spice_get_xyaxis, sRange, sz_padded
  xaxis = sRange.x[0] + dindgen(sz_padded[1])
  yaxis = sRange.y[0] + dindgen(sz_padded[2])
  
  return, {x:xaxis, y:yaxis}
END


FUNCTION spice_get_padded_data, aImage, sRangePadded,  sz_padded=sz_padded, sit_and_stare=sit_and_stare, reverse_colortable=reverse_colortable
  
  default, sit_and_stare, 0
  
  IF sit_and_stare THEN BEGIN 
     aPaddedImage = aImage 
     sz_padded = size(aPaddedImage)
  ENDIF ELSE BEGIN 
     xrange = sRangePadded.x
     yrange = sRangePadded.y
     
     aPaddedImage = bytarr(ceil(xrange[1]-xrange[0]), ceil(yrange[1]-yrange[0]))
     aPaddedImage[*] = (reverse_colortable) ? 0 : 255
          
     sz_padded = size(aPaddedImage)
     sz = size(aImage)
     
     xix = [sz_padded[1]/2-sz[1]/2. , sz_padded[1]/2+sz[1]/2.-1]
     yix = [sz_padded[2]/2-sz[2]/2. , sz_padded[2]/2+sz[2]/2.-1]
     aPaddedImage[xix[0]:xix[1], yix[0]:yix[1]] = aImage
  ENDELSE
  
  return, aPaddedImage
END


FUNCTION spice_get_xyrange_padded, xrange1, xrange2, yrange1, yrange2
  xrange=[min(xrange1 < xrange2), max(xrange1 >  xrange2)]
  yrange=[min(yrange1 < yrange2), max(yrange1 >  yrange2)]
  
  return, {x:xrange, y:yrange}
END


FUNCTION spice_congrid_data, aImageIn, startrow, endrow, l2_header, l3_header, sz=sz
  crval1 = fxpar(l2_header,'CRVAL1')
  cdelt1 = fxpar(l2_header,'CDELT1')
  crpix1 = fxpar(l2_header,'CRPIX1')
  crval2 = fxpar(l2_header,'CRVAL2')
  cdelt2 = fxpar(l2_header,'CDELT2')
  crpix2 = fxpar(l2_header,'CRPIX2')
 
  slit_wid = fxpar(l2_header,'SLIT_WID')
  
  sz = size(aImageIn)

  x_unrot = crval1 + cdelt1*(indgen(sz[1])+1 - crpix1)
  x_min_unrot = min(x_unrot)
  x_max_unrot = max(x_unrot)
  xsz_unrot = x_max_unrot-x_min_unrot
; xsz_unrot_congrid = xsz_unrot + (slit_wid-1)
 
 naxis2_l2 = fxpar(l3_header,'NAXIS3')
 y_unrot = crval2 +cdelt2*(indgen(naxis2_l2)+1 - crpix2)
 y_min_unrot = y_unrot[startrow]
 y_max_unrot = y_unrot[endrow]
 
 ;y_min_unrot = crval2 - cdelt2*naxis2_l2/2.
 ;y_max_unrot = crval2 + cdelt2*naxis2_l2/2.
; 
; hpixhalf = (endrow-startrow+1)/2.
; y_min_unrot = crval2 - cdelt2*hpixhalf
; y_max_unrot = crval2 + cdelt2*hpixhalf
 ysz_unrot =  y_max_unrot-y_min_unrot
;; ysz_unrot_edge_to_edge = ysz_unrot + cdelt2
;; ysz_unrot_congrid = round(ysz_unrot_edge_to_edge - 1)
 
 
; crota = fxpar(l2_header,'CROTA')
; rad = crota*!pi/180
 
 aImage = bytscl(congrid(aImageIn, xsz_unrot, ysz_unrot))
 aImage[where(aImage EQ max(aImage))] = 254
 aImage[where(aImage EQ min(aImage))] = 1
 
 return, aImage
END


FUNCTION spice_remove_trends, image, value_min=value_min, value_max=value_max, $
                         remove_horizontal_trend=remove_horizontal_trend, remove_vertical_trend=remove_vertical_trend, fit_trend=fit_trend
  
  sz = size(image)
  trend_image = fltarr(sz[1],sz[2])
  
  IF keyword_set(remove_horizontal_trend) THEN BEGIN 
     trend_median_x = median(image,dimension=2)
     
     IF keyword_set(fit_trend) THEN BEGIN 
        x = indgen(sz[1])
        r = linfit(x, trend_median_x)
        corr = r[0]+x*r[1]
        corr_im = rebin(corr, sz[1], sz[2])
        image -= corr_im
     END ELSE BEGIN 
        FOR y=0,sz[2]-1 DO trend_image[*,y] = trend_median_x
        image -= trend_image 
     ENDELSE 
  ENDIF
  
  IF keyword_set(remove_vertical_trend) THEN BEGIN 
     trend_median_y = median(image,dimension=1)
     
     IF keyword_set(fit_trend) THEN BEGIN 
        y = indgen(sz[2])
        r = linfit(y, trend_median_y)
        corr = r[0]+y*r[1]
        corr_im = transpose(rebin(corr, sz[2], sz[1]))
        image -= corr_im
     ENDIF ELSE BEGIN 
        FOR x=0,sz[1]-1 DO trend_image[x,*] = trend_median_y
        image -= trend_image
     ENDELSE 
  ENDIF 
  
  nanix = where(image NE image,n_nanix)
  
  IF keyword_set(value_min) OR keyword_set(value_max) THEN BEGIN 
     minval = (keyword_set(value_min)) ? value_min : min(image)
     maxval = (keyword_set(value_max)) ? value_max : max(image)
  
     image = image > (value_min) < value_max
  ENDIF
  
  
  IF n_nanix GT 0 THEN image[nanix] = 0
  
  return, image
END


PRO spice_write_jpg, aData, filename, $
                     remove_horizontal_trend=remove_horizontal_trend, remove_vertical_trend=remove_vertical_trend, fit_trend=fit_trend, $
                     value_max=value_max, value_min=value_min, $
                     colortable=colortable, $
                     xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
                     title=title, xtitle=xtitle, ytitle=ytitle, $
                     show_plot=show_plot, reverse_colortable=reverse_colortable, startrow=startrow, endrow=endrow, l3_header=l3_header, l2_header=l2_header
  
  default, show_plot, 0
  TVLCT, old_palette,/get
  
  parameter = (filename.extract('([a-z]+).jpg',/subex))[1]
  
  sit_and_stare = filename.contains('sit')
  
  IF parameter EQ 'vel' THEN BEGIN 
     eis_colors, /velocity
     default, value_max, 50             
     default, value_min, -50
  ENDIF ELSE loadct, colortable
  
  tvlct, palette,/get
  IF parameter EQ 'wid' THEN palette[255,*] = 255
  IF parameter EQ 'int' THEN reverse_colortable = 1
  IF keyword_set(reverse_colortable) THEN palette = reverse(palette)
  tvlct, palette
  
  IF parameter EQ 'vel' THEN aData -= median(aData)
  IF parameter EQ 'wid' THEN BEGIN 
   ;  aData -= min(aData)
  ENDIF
  
  aData = spice_remove_trends(aData, value_min=value_min, value_max=value_max, $
                                         remove_horizontal_trend=remove_horizontal_trend, remove_vertical_trend=remove_vertical_trend, fit_trend = fit_trend)
     
  aDataClipped = (parameter EQ 'vel') ? aData : sigrange(aData)
  
  aDataCongrid = spice_congrid_data(aDataClipped, startrow, endrow, l2_header, l3_header, sz=sz)
  
  sRangePadded = spice_get_xyrange_padded(xrange1, xrange2, yrange1, yrange2)
  aDataPadded = spice_get_padded_data(aDataCongrid, sRangePadded, sz_padded=sz_padded, sit_and_stare=sit_and_stare, reverse_colortable=reverse_colortable)
  
  sAxisPadded = spice_get_xyaxis(sRangePadded, sz_padded)

  spice_init_graphics_window, sz_padded, filename, buffer=~show_plot, font_size=font_size
  
  plot_position = [0.55,0.5]
  imPadded = image(aDataPadded, sAxisPadded.x, sAxisPadded.y, rgb_table=palette, axis_style=4,xtitle='Solar X [arcsec]', ytitle='Solar Y [arcsec]', title=title,/current)
  imPadded.position = plot_position
  
  IF ~sit_and_stare THEN imPadded.rotate, fxpar(l2_header,'CROTA')
  
  title = spice_get_title(filename, parameter)
  
  IF sz_padded[1] LT 150 THEN BEGIN 
     xtickinterval = 50
     text_font_size = font_size-2
  ENDIF ELSE BEGIN 
     xtickinterval = 100
     text_font_size = font_size
  ENDELSE 
  
  imCoordinates = image(aDataPadded, sAxisPadded.x, sAxisPadded.y, transparency=100, /current, axis_style=2, xtickinterval=xtickinterval, $
                        xtitle=xtitle, ytitle=ytitle, title=title, font_size = font_size)
  imCoordinates.position = plot_position
  
  units = spice_get_units(parameter)
  cColorbar = colorbar(range=[min(aDataClipped), max(aDataClipped)], position=[0.1,0.085,0.9,0.096], border=1, orientation=0, textpos=0, title=units)
  
  spice_plot_texts, l2_header, font_size = text_font_size
  
  imPadded.save, filename, width = 1024
  
  TVLCT, old_palette
END


