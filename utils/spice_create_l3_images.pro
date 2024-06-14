;+
; NAME:
;      SPICE_CREATE_L3_IMAGES
;
; PURPOSE:
;      This procedure creates images from level 3 data. The filename is constructed with this formula:
;      filename = l3_filename(but replace 'spice' with 'spice-ql' and the l3 fileversionnumber with the new fileversionnumber) + 
;        '_' + fns('##',hdr.winno) + '_' + fns('##',icomp+1) + '_' + param.name + $
;        '_' + image_type(see list below) + file-suffix
;
;      It will create these images per fit parameter of each fit component for each window:
;        - 1 JPG image where data area has the original size (i.e. 1 pixel of data is 1 pixel in the image). (image_type='')
;        - 1 PNG image without any axis of height 64 pixels. (image_type='thumb')
;
; CATEGORY:
;      Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;      spice_create_l3_images, l3_file, out_dir [, smooth=smooth] [, /interpolation] $
;        [, version=version] [, /remove_trends] [, /no_background_images] $
;        [, /NO_TREE_STRUCT] [, /show_plot]
;
; INPUTS:
;      l3_file: The full path to the level 3 SPICE FITS file.
;      out_dir: The directory in which the images should be saved to.
;
; OPTIONAL INPUTS:
;     SMOOTH: An integer. The width of the boxcar used when smoothing the
;             image using the smooth function. If not set no smoothing is performed.
;     VERSION: A string giving the version number of the file. Default is '01'.
;
; KEYWORDS:
;     INTERPOLATION: If set, then the image is expanded with bilinear interpolation.
;               This keyword should not be set, if SMOOTH input is provided.
;     NO_TREE_STRUCT: If set, then the date tree structure won't be appended to OUT_DIR
;               (e.g. OUT_DIR/ instead of OUT_DIR/2020/06/21/)
;     SHOW_PLOT: If set, then the image is shown on the screen and not saved into a file.
;     REMOVE_TRENDS: If set, remove horizontal and vertical trends in the image
;     no_background_images: If set, then the images for the background component will not 
;               be created.
;
; OUTPUTS:
;      Writes jpeg and png files with images into out_dir.
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;      fits2ana, fitshead2struct, fxpar, fitshead2wcs, wcs_get_coord,
;      prits_tools.write_image_real_size
;
; HISTORY:
;      Ver. 1,   23-Jun-2022, Martin Wiesmann
;      Ver. 1.1, 19-Jan-2024, Terje Fredvik - extract FITS keyword winno from
;                             header (instead of l2winno which no longer
;                             exists)
;      Ver. 1.2, 22-Jan-2024, Terje Fredvik - New keyword show_plot handed over
;      to prits_tools__write_image_real_size. Set colortable keyword to 100
;      for velocity images to signal that special eis_colors,/velocity color
;      table should be restored. Added "ql" in the filename. 
;      Ver. 2, 24-Jan-2024, TF - New keyword VERSION, to set the version
;      number of L3ql files. If not set, the version will be 'V01'. Removed
;      "original' from full size jpgs.
;      Ver. 3, 12-Feb-2024, TF - call delete_analysis when done with calls to
;      handle_value 
;      Ver. 4., 10-May-2024, TF - use result array to determine startrow and
;      endrow. Modified filename to adher to the Metadata standard.
;      Ver. 5., 14-May-2024, TF - replaced remove_trend keyword with
;      remove_vertical_trend and remove_horizontal_trend
;      Ver. 6., 03-Jun-2024, TF - Modified filename to adhere to the SoLO
;      Metadata standard. Ensure that trends are only removed for velocity
;      images (provided that one or more of the remove_*_trend keywords are
;      set). New keyword fit_trend, if set together with one or both
;      remove_*_trend, remove a linear fit of the velocity trend instead of
;      removing the mean of each row and/or column.   
;
;
;-
; $Id: 2024-06-14 11:43 CEST $


PRO spice_create_l3_images, l3_file, out_dir, smooth=smooth, interpolation=interpolation, $
                            version=version, remove_horizontal_trend=remove_horizontal_trend, remove_vertical_trend=remove_vertical_trend, fit_trend=fit_trend, $ 
                            no_background_images=no_background_images, $
                            NO_TREE_STRUCT=NO_TREE_STRUCT, show_plot=show_plot

  prits_tools.parcheck, l3_file, 1, "l3_file", 'STRing', 0
  prits_tools.parcheck, out_dir, 2, "out_dir", 'STRing', 0
  prits_tools.parcheck, version, 0, "version", 'STRing', 0, default='01'
  prits_tools.parcheck, smooth, 0, "smooth", 'numeric', 0, minval=0, /optional
     
  l3_filename = file_basename(l3_file)
 
  l3_filename = strsplit(l3_filename, '.', /extract)
  l3_filename = l3_filename[0]
  base_dir = out_dir
  if ~keyword_set(NO_TREE_STRUCT) THEN BEGIN
    date_dirs = file_dirname(l3_file)
    date_dirs = strsplit(date_dirs, path_sep(), /extract)
    date_dirs = strjoin(date_dirs[-3:-1], path_sep())
    base_dir += date_dirs
  ENDIF
  l3ql_filename = l3_filename.replace('spice','spice-ql')
  IF version THEN l3ql_filename = l3ql_filename.replace(l3ql_filename.extract('V[0-9]{2}'), 'V'+version)
  
  filename_base = base_dir + path_sep() + l3ql_filename + '_'
  if ~file_test(base_dir, /directory) then file_mkdir, base_dir

  ana = fits2ana(l3_file, headers_results=headers_results)

  for iana=0,N_ELEMENTS(ana)-1 do begin

    handle_value,ana[iana].data_h,data;,/no_copy
    handle_value,ana[iana].result_h,result;,/no_copy
    handle_value,ana[iana].fit_h,fit;,/no_copy
    
    delete_analysis, ana[iana]
    
    hdr = fitshead2struct(*headers_results[iana])
   
    ; check that there is more than one exposures
    naxis2 = fxpar(*headers_results[iana], 'NAXIS2', missing=1)
    naxis4 = fxpar(*headers_results[iana], 'NAXIS4', missing=1)
    IF naxis2+naxis4 LE 2 THEN BEGIN
      message, ['This is a single exposure window, cannot create images from it', $
        l3_file, $
        'window: ' + trim(fxpar(*headers_results[iana], 'WINNO', missing=-1))], /info
      continue
    ENDIF
    wcs = fitshead2wcs(hdr)
    coords = wcs_get_coord(wcs)
    
    raster = l3_file.contains('ras')
    sz = size(result)
    result_along_x = (raster) ? reform(result[0,*,sz[3]/2.]) : reform(result[0,*,sz[3]/2.,*])
    goodx = where(result_along_x EQ result_along_x)
    result_along_y = (raster) ? reform(result[0, goodx[0], *]) : reform(result[0, *, *,goodx[0]])
    ok_result_along_y = where(result_along_y EQ result_along_y)
    startrow = ok_result_along_y[0]
    endrow   = ok_result_along_y[-1]

    n_components = N_TAGS(fit)
    ipartotal = 0
    for icomp=0,n_components-1 do begin
      fit_cur = fit.(icomp)
      n_params = N_ELEMENTS(fit_cur.param)
      include_component = (keyword_set(no_background_images)) ? fit_cur.name NE 'Background' : 1
      IF include_component THEN for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        name = (fit_cur.name.compress()).toLower()
        ion = name.extract('[a-z]+')
        ion = string(ion+'--------', format='(A-8)')
        lam = name.extract('[0-9]+.[0-9]+')
        lam = lam.replace('.','nm')  
        
        filename_base2 = filename_base.replace('ql','ql-'+ion+'-'+lam+'-'+param.name.substring(0,2))+fns('##',hdr.winno)+'_'+fns('##',icomp+1)+'_'+param.name.substring(0,2)
        ; crop image so that lines with invalid data is not shown
        image_data = reform(result[ipartotal,*,startrow:endrow,*])
        help,image_data

        IF naxis4 GT 1 THEN BEGIN
          ; sit-and-stare
          xtitle1 = 'Time [sec]'
          xrange1 = [coords[3,0,0,startrow,0], coords[3,0,0,startrow,-1]]
          xrange2 = [coords[3,0,0,endrow,0], coords[3,0,0,endrow,-1]]
          yrange1 = [coords[2,0,0,startrow,0], coords[2,0,0,endrow,0]]
          yrange2 = [coords[2,0,0,startrow,-1], coords[2,0,0,endrow,-1]]
          SCALE_TO_RANGE = 0
          image_data = transpose(image_data)
        ENDIF ELSE BEGIN
          ; raster
          xtitle1 = 'Solar X [arcsec]'
          xrange1 = [coords[1,0,0,startrow], coords[1,0,-1,startrow]]
          xrange2 = [coords[1,0,0,endrow], coords[1,0,-1,endrow]]
          yrange1 = [coords[2,0,0,startrow], coords[2,0,0,endrow]]
          yrange2 = [coords[2,0,-1,startrow], coords[2,0,-1,endrow]]
          SCALE_TO_RANGE = 1
        ENDELSE
        ytitle1 = 'Solar Y [arcsec]'

        case param.name of
          'velocity': begin
            color_center_value = 0

            ; Option A
            ;colortable = 33
            ;reverse_colortable = 0

            ; Option B
            ;colortable = 72
            ;reverse_colortable = 1
            
            ; Option C - triggers call to eis_colors
            colortable = 100
            reverse_colortable = 0
            background_color = 0
          end
          'width' : begin
            colortable = 4
            reverse_colortable = 0
            color_center_value = !NULL
          end
          else: begin
            color_center_value = !NULL

            ; Option A
            colortable = 3
            reverse_colortable = 0
            
            ; Option B
            ;colortable = 56
            ;reverse_colortable = 1
          end
        endcase
        
        this_remove_horizontal_trend = (param.name EQ 'velocity') ? remove_horizontal_trend : 0
        this_remove_vertical_trend   = (param.name EQ 'velocity') ? remove_vertical_trend   : 0   
        
        filename = filename_base2 + '.jpg'
        format = 'JPEG'
        prits_tools.write_image_real_size, image_data, filename, $
           remove_horizontal_trend=this_remove_horizontal_trend, remove_vertical_trend=this_remove_vertical_trend, fit_trend = fit_trend, smooth = smooth, $
           colortable=colortable, format=format, interpolation=interpolation, $
           xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
           xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
           SCALE_TO_RANGE=SCALE_TO_RANGE, $
           cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, $
           reverse_colortable=reverse_colortable, show_plot=show_plot

        filename = filename_base2 + '_thumb.png'
        format = 'PNG'
        prits_tools.write_image_real_size, image_data, filename, $
           remove_horizontal_trend=this_remove_horizontal_trend, remove_vertical_trend=this_remove_vertical_trend, fit_trend = fit_trend, smooth = smooth, $
           colortable=colortable, format=format, interpolation=interpolation, $
           height=64, border=0, reverse_colortable=reverse_colortable, $
           xrange1=xrange1, yrange1=yrange1, SCALE_TO_RANGE=SCALE_TO_RANGE, /no_axis, $
           cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, show_plot=show_plot
        
        ipartotal++

      endfor ; ipar0,n_params-1
    endfor ; icomp=0,n_components-1

  endfor ; iana=0,N_ELEMENTS(ana)-1 do begin

END
