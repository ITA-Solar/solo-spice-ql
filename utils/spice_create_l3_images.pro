;+
; NAME:
;      SPICE_CREATE_L3_IMAGES
;
; PURPOSE:
;      This function creates images from level 3 data.
;      It will create 3 images per fit parameter of each fit component for each
;      window. 2 JPG images with height 512 and 1024 and 1 PNG image with height 64 pixels.
;
; CATEGORY:
;      SPICE -- utility
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
;
; HISTORY:
;      Ver. 1, 23-Jun-2022, Martin Wiesmann
;
;-
; $Id: 2022-10-21 14:46 CEST $


PRO spice_create_l3_images, l3_file, out_dir, NO_TREE_STRUCT=NO_TREE_STRUCT

  prits_tools.parcheck, l3_file, 1, "l3_file", 'STRing', 0
  prits_tools.parcheck, out_dir, 2, "out_dir", 'STRing', 0

  ;  ; Red Temperature (intensity/default)
  ;  c = colortable(3)
  ;  c = colortable(62) ; CB-Reds
  ;  ct1_r=reform(c[*,0])
  ;  ct1_g=reform(c[*,1])
  ;  ct1_b=reform(c[*,2])
  ;
  ;  ; Blue-Red (velocity)
  ;  c = colortable(33)
  ;  c = colortable(72) ; CB-RdYlBu
  ;  ct2_r=reform(c[*,0])
  ;  ct2_g=reform(c[*,1])
  ;  ct2_b=reform(c[*,2])
  ;
  ;  ; Blue/Green/Red/Yellow (width)
  ;  c = colortable(4)
  ;  ct3_r=reform(c[*,0])
  ;  ct3_g=reform(c[*,1])
  ;  ct3_b=reform(c[*,2])

  l3_filename = file_basename(l3_file)
  l3_filename = strsplit(l3_filename, '.', /extract)
  l3_filename = l3_filename[0]
  base_dir = out_dir
  if ~keyword_set(NO_TREE_STRUCT) THEN BEGIN
    date_dirs = file_dirname(l3_file)
    date_dirs = strsplit(date_dirs, path_sep(), /extract)
    date_dirs = strjoin(date_dirs[-3:-1], path_sep())
    base_dir += date_dirs
  endif
  filename_base = base_dir + path_sep() + l3_filename + '_'
  if ~file_test(base_dir, /directory) then file_mkdir, base_dir

  ana = fits2ana(l3_file, headers_results=headers_results)
  for iana=0,N_ELEMENTS(ana)-1 do begin

    handle_value,ana[iana].result_h,result;,/no_copy
    handle_value,ana[iana].fit_h,fit;,/no_copy

    hdr = fitshead2struct(*headers_results[iana])

    ; check that there there are more than one exposures
    naxis2 = fxpar(*headers_results[iana], 'NAXIS2', missing=1)
    naxis4 = fxpar(*headers_results[iana], 'NAXIS4', missing=1)
    IF naxis2+naxis4 LE 2 THEN BEGIN
      message, 'This is a single exposure window, cannot create images from it', $
        l3_file, $
        'window: ' + trim(fxpar(*headers_results[iana], 'WINNO', missing=-1))
      continue
    ENDIF
    wcs = fitshead2wcs(hdr)
    coords = wcs_get_coord(wcs)
    ;    size_result = size(result)
    ;    xrange = [coords[1,0,0,0], coords[1,0,-1,0]]
    ;    xcoord_transform = [xrange[0], (xrange[1]-xrange[0])/size_result[2]]
    ;    xrange2 = [coords[1,0,0,-1], coords[1,0,-1,-1]]
    ;    xcoord_transform2 = [xrange2[0], (xrange2[1]-xrange2[0])/size_result[2]]
    ;    yrange = [coords[2,0,0,0], coords[2,0,0,-1]]
    ;    ycoord_transform = [yrange[0], (yrange[1]-yrange[0])/size_result[3]]
    ;    yrange2 = [coords[2,0,-1,0], coords[2,0,-1,-1]]
    ;    ycoord_transform2 = [yrange2[0], (yrange2[1]-yrange2[0])/size_result[3]]

    n_components = N_TAGS(fit)
    ipartotal = 0
    ;for itag=0,n_components-1 do begin
    for itag=0,0 do begin
      fit_cur = fit.(itag)
      n_params = N_ELEMENTS(fit_cur.param)
      for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        filename_base2 = filename_base+fns('##',hdr.l2winno)+'_'+fns('##',itag+1)+'_'+param.name

        ; crop image so that lines with invalid data is not shown
        IF naxis4 GT 1 THEN BEGIN
          image_data = reform(result[ipartotal,*,*, *])
        ENDIF ELSE BEGIN
          image_data = reform(result[ipartotal,*,*])
        ENDELSE
        size_image = size(image_data)
        startrow = 0
        for i=0,size_image[2]/2-1 do begin
          ind = where(image_data[*,i] lt -999.9 AND image_data[*,i] gt -1000.1, count, ncomplement=ncomplement)
          if ncomplement gt 0 then begin
            startrow = i
            break
          endif
        endfor
        endrow = size_image[2]-1
        for i=size_image[2]-1,size_image[2]/2,-1 do begin
          ind = where(image_data[*,i] lt -999.9 AND image_data[*,i] gt -1000.1, count, ncomplement=ncomplement)
          if ncomplement gt 0 then begin
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
            ;colortable = 70
            colortable = 33
          end
          'width' : begin
            colortable = 4
          end
          else: begin
            ;colortable = 62
            colortable = 3
          end
        endcase

        filename = filename_base2 + '_original.jpg'
        format = 'JPEG'
        prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
          xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
          xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
          border=0

        ;        filename = filename_base2 + '_512.jpg'
        ;        format = 'JPEG'
        ;        prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
        ;          xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
        ;          xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
        ;          height=512

        filename = filename_base2 + '_64.png'
        format = 'PNG'
        prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
          height=64, border=0



        ;
        ;        size_image = size(image_data)
        ;        xrange = [coords[1,0,0,startrow], coords[1,0,-1,startrow]]
        ;        xcoord_transform = [xrange[0], (xrange[1]-xrange[0])/size_image[1]]
        ;        xrange2 = [coords[1,0,0,endrow], coords[1,0,-1,endrow]]
        ;        xcoord_transform2 = [xrange2[0], (xrange2[1]-xrange2[0])/size_image[1]]
        ;        yrange = [coords[2,0,0,startrow], coords[2,0,0,endrow]]
        ;        ycoord_transform = [yrange[0], (yrange[1]-yrange[0])/size_image[2]]
        ;        yrange2 = [coords[2,0,-1,startrow], coords[2,0,-1,endrow]]
        ;        ycoord_transform2 = [yrange2[0], (yrange2[1]-yrange2[0])/size_image[2]]
        ;
        ;        ind = where(image_data lt -999.9 AND image_data gt -1000.1, count, complement=gooddata)
        ;        image_min = min(image_data[gooddata], max=image_max)
        ;        image_min = image_min - (image_max-image_min)/10
        ;        if count gt 0 then image_data[ind]=!Values.F_NAN
        ;
        ;
        ;        im=image(image_data, axis_style=2, rgb_table=colortab, $ ;min_value=-10, $
        ;          xtitle='Solar X [arcsec]', ytitle='Solar Y [arcsec]')
        ;        a = im.AXES
        ;        a[0].coord_transform=xcoord_transform
        ;        a[0].major=2
        ;
        ;        a[1].coord_transform=ycoord_transform
        ;
        ;        a[2].coord_transform=xcoord_transform2
        ;        a[2].title = ''
        ;        a[2].tickfont_name = a[0].tickfont_name
        ;        a[2].tickfont_size = a[0].tickfont_size
        ;        a[2].major=2
        ;        a[2].hide=0
        ;        a[2].showtext=1
        ;
        ;        a[3].coord_transform=ycoord_transform2
        ;        a[3].title = ''
        ;        a[3].tickfont_name = a[0].tickfont_name
        ;        a[3].tickfont_size = a[0].tickfont_size
        ;        a[3].hide=0
        ;        a[3].showtext=1
        ;
        ;        im.save,filename+'_512.jpg', height=512, border=5
        ;        im.save,filename+'_1024.jpg', height=1024, border=5
        ;        im.close
        ;
        ;        ; 64px png image
        ;        size_image = size(image_data)
        ;        magnification = 64d/size_image[2]
        ;        image_small = rot(image_data, 0, magnification, /interp)
        ;        ind = where(image_small lt -999.9 AND image_small gt -1000.1, count, complement=gooddata)
        ;        image_min = min(image_small[gooddata], max=image_max)
        ;        image_min = image_min - (image_max-image_min)/20
        ;        if count gt 0 then image_small[ind]=image_min
        ;        factor = 255d/(image_max-image_min)
        ;        image_small = (image_small - image_min) * factor
        ;        image_small = UINT(image_small)
        ;        dx = size_image[1]*magnification/2
        ;        x0 = floor(size_image[1]/2.0-dx)
        ;        x1 = ceil(size_image[1]/2.0+dx)
        ;        y0 = floor(size_image[2]/2.0-32)
        ;        y1 = ceil(size_image[2]/2.0+32)-1
        ;        image_small = image_small[x0:x1, y0:y1]
        ;
        ;        write_png, filename+'_64.png', image_small, ct_r, ct_g, ct_b

        ipartotal++

      endfor ; ipar0,n_params-1
    endfor ; itag=0,N_TAGS(fit)-1

  endfor ; iana=0,N_ELEMENTS(ana)-1 do begin

END
