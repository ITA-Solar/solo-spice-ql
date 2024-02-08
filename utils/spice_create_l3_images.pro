;+
; NAME:
;      SPICE_CREATE_L3_IMAGES
;
; PURPOSE:
;      This procedure creates images from level 3 data. The filename is constructed with this formula:
;      filename = l3_filename + '_' + fns('##',hdr.l2winno) + '_' + fns('##',icomp+1) + '_' + param.name + $
;        '_' + image_type(see list below) + file-suffix
;
;      It will create these images per fit parameter of each fit component for each window:
;        - 1 JPG image where data area has the original size (i.e. 1 pixel of data is 1 pixel in the image). (image_type='original')
;        - 1 PNG image without any axis of height 64 pixels. (image_type='64')
;
; CATEGORY:
;      Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;      spice_create_l3_images, l3_file, out_dir
;
; INPUTS:
;      l3_file: The full path to the level 3 SPICE FITS file.
;      out_dir: The directory in which the images should be saved to.
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;     NO_TREE_STRUCT: If set, then the date tree structure won't be appended to OUT_DIR
;               (e.g. OUT_DIR/ instead of OUT_DIR/2020/06/21/)
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
;
;-
; $Id: 2024-02-08 12:00 CET $


PRO spice_create_l3_images, l3_file, out_dir, NO_TREE_STRUCT=NO_TREE_STRUCT, show_plot=show_plot, version=version, remove_trends = remove_trends, smooth = smooth, $
                            no_background_images=no_background_images

  prits_tools.parcheck, l3_file, 1, "l3_file", 'STRing', 0
  prits_tools.parcheck, out_dir, 2, "out_dir", 'STRing', 0
  
  default, version, '01'

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

    handle_value,ana[iana].result_h,result;,/no_copy
    handle_value,ana[iana].fit_h,fit;,/no_copy

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

    n_components = N_TAGS(fit)
    ipartotal = 0
    for icomp=0,n_components-1 do begin
      ;for icomp=0,0 do begin
      fit_cur = fit.(icomp)
      n_params = N_ELEMENTS(fit_cur.param)
      include_component = (keyword_set(no_background_images)) ? fit_cur.name NE 'Background' : 1
      IF include_component THEN for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        filename_base2 = filename_base+fns('##',hdr.winno)+'_'+fns('##',icomp+1)+'_'+param.name
        ; crop image so that lines with invalid data is not shown
        IF naxis4 GT 1 THEN BEGIN
          image_data = reform(result[ipartotal,*,*, *])
        ENDIF ELSE BEGIN
          image_data = reform(result[ipartotal,*,*])
        ENDELSE
        size_image = size(image_data)
        startrow = 0
        for i=0,size_image[2]/2-1 do begin
          ind = where(image_data[*,i] EQ image_data[*,i], count)
          if count gt 0 then begin
            startrow = i
            break
          endif
        endfor
        endrow = size_image[2]-1
        for i=size_image[2]-1,size_image[2]/2,-1 do begin
          ind = where(image_data[*,i] EQ image_data[*,i], count)
          if count gt 0 then begin
            endrow = i
            break
          endif
        endfor
        image_data = image_data[*,startrow:endrow]


        xrange1 = [coords[1,0,0,startrow], coords[1,0,-1,startrow]]
        xrange2 = [coords[1,0,0,endrow], coords[1,0,-1,endrow]]
        yrange1 = [coords[2,0,0,startrow], coords[2,0,0,endrow]]
        yrange2 = [coords[2,0,-1,startrow], coords[2,0,-1,endrow]]

        xtitle1='Solar X [arcsec]'
        ytitle1='Solar Y [arcsec]'

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

        filename = filename_base2 + '.jpg'
        format = 'JPEG'
        prits_tools.write_image_real_size, image_data, filename, remove_trends = remove_trends, smooth = smooth, $
          colortable=colortable, format=format, $
          xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
          xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
          /SCALE_TO_RANGE, $
          cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, $
          reverse_colortable=reverse_colortable, show_plot=show_plot

        filename = filename_base2 + '_thumb.png'
        format = 'PNG'
        prits_tools.write_image_real_size, image_data, filename, remove_trends = remove_trends, smooth = smooth, $
          colortable=colortable, format=format, $
          height=64, border=0, reverse_colortable=reverse_colortable, $
          xrange1=xrange1, yrange1=yrange1, /SCALE_TO_RANGE, /no_axis, $
          cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, show_plot=show_plot

        ipartotal++

      endfor ; ipar0,n_params-1
    endfor ; icomp=0,n_components-1

  endfor ; iana=0,N_ELEMENTS(ana)-1 do begin

END
