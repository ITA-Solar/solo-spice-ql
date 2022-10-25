;+
; NAME:
;      SPICE_CREATE_L3_IMAGES
;
; PURPOSE:
;      This function creates images from level 3 data. The filename is constructed with this formula:
;      filename = l3_filename + '_' + fns('##',hdr.l2winno) + '_' + fns('##',icomp+1) + '_' + param.name + $
;        '_' + image_type(see list below) + file-suffix
;
;      It will create these images per fit parameter of each fit component for each window:
;        - 1 JPG image where data area has the original size (i.e. 1 pixel of data is 1 pixel in the image). (image_type='original')
;        - 1 PNG image without any axis of height 64 pixels. (image_type='64')
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
;      fits2ana, fitshead2struct, fxpar, fitshead2wcs, wcs_get_coord,
;      prits_tools.write_image_real_size
;
; HISTORY:
;      Ver. 1, 23-Jun-2022, Martin Wiesmann
;
;-
; $Id: 2022-10-25 13:05 CEST $


PRO spice_create_l3_images, l3_file, out_dir, NO_TREE_STRUCT=NO_TREE_STRUCT

  prits_tools.parcheck, l3_file, 1, "l3_file", 'STRing', 0
  prits_tools.parcheck, out_dir, 2, "out_dir", 'STRing', 0

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
      for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        filename_base2 = filename_base+fns('##',hdr.l2winno)+'_'+fns('##',icomp+1)+'_'+param.name

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
            color_center_value = 0

            ; Option A
            ;colortable = 33
            ;reverse_colortable = 0

            ; Option B
            colortable = 72
            reverse_colortable = 1
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

        filename = filename_base2 + '_original.jpg'
        format = 'JPEG'
        prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
          xrange1=xrange1, xrange2=xrange2, yrange1=yrange1, yrange2=yrange2, $
          xtitle1=xtitle1, xtitle2=xtitle2, ytitle1=ytitle1, ytitle2=ytitle2, $
          cutoff_threshold=cutoff_threshold, color_center_value=color_center_value, $
          reverse_colortable=reverse_colortable

        filename = filename_base2 + '_64.png'
        format = 'PNG'
        prits_tools.write_image_real_size, image_data, filename, colortable=colortable, format=format, $
          height=64, border=0, reverse_colortable=reverse_colortable, $
          cutoff_threshold=cutoff_threshold, color_center_value=color_center_value

        ipartotal++

      endfor ; ipar0,n_params-1
    endfor ; icomp=0,n_components-1

  endfor ; iana=0,N_ELEMENTS(ana)-1 do begin

END
