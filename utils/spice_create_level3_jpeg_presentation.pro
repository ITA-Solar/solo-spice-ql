;+
; NAME:
;      SPICE_CREATE_LEVEL3_JPEG
;
; PURPOSE:
;      This function creates images out of level 3 data.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      spice_create_level3_jpeg
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;
; HISTORY:
;      Ver. 1, 13-Jun-2022, Martin Wiesmann
;
;-
; $Id: 2022-06-17 15:24 CEST $


PRO spice_create_level3_jpeg_presentation, meta_data

  root_dir = '/mn/stornext/u3/mawiesma/spice/l3_images/'
  meta_data_file = '/Users/mawiesma/Documents/spice/generate_l3_meta_data.sav'
  meta_data_template = { $
    file:'', $
    winno:0, $
    extname:'', $
    category:0, $
    l3_created:0b, $
    l3_file:'', $
    image_small_created:0b, $
    image_large_created:0b $
  }

  if N_ELEMENTS(meta_data) EQ 0 then restore, meta_data_file
  ind = where(meta_data.l3_created, ndata)
  
  c = colortable(3)
  ct_r=reform(c[*,0])
  ct_g=reform(c[*,1])
  ct_b=reform(c[*,2])

  
  ;for idata=0,ndata-1 do begin
  for idata=0,0 do begin
    ana = fits2ana(meta_data[ind[idata]].l3_file, titles=titles)
    ;handle_value,ana.data_h,data,/no_copy
    handle_value,ana.result_h,result,/no_copy
    handle_value,ana.fit_h,fit,/no_copy
    print,meta_data[ind[idata]].l3_file
    l3_filename = file_basename(meta_data[ind[idata]].l3_file)
    l3_filename = strsplit(l3_filename, '.', /extract)
    l3_filename = l3_filename[0]
    date_dirs = file_dirname(meta_data[ind[idata]].l3_file)
    date_dirs = strsplit(date_dirs, '/', /extract)
    date_dirs = strjoin(date_dirs[-3:-1], '/')
    filename = root_dir + date_dirs + '/' + l3_filename + '_'
    file_mkdir, root_dir + date_dirs
    ;help,data
    ;help,result
    ;help,fit
    hdr0 = headfits(meta_data[ind[idata]].l3_file, exten=0) ; for real exten must be (winno-1)*7 or so...
    hdr = headfits(meta_data[ind[idata]].l3_file, exten=fxpar(hdr0,'dataext'))
    hdr = fitshead2struct(hdr)
    wcs = fitshead2wcs(hdr)
    coords = wcs_get_coord(wcs)
    size_result = size(result)
    xrange = [coords[1,0,0,0], coords[1,0,-1,0]]
    xcoord_transform = [xrange[0], (xrange[1]-xrange[0])/size_result[2]]
    xrange2 = [coords[1,0,0,-1], coords[1,0,-1,-1]]
    xcoord_transform2 = [xrange2[0], (xrange2[1]-xrange2[0])/size_result[2]]
    yrange = [coords[2,0,0,0], coords[2,0,0,-1]]
    ycoord_transform = [yrange[0], (yrange[1]-yrange[0])/size_result[3]]
    yrange2 = [coords[2,0,-1,0], coords[2,0,-1,-1]]
    ycoord_transform2 = [yrange2[0], (yrange2[1]-yrange2[0])/size_result[3]]
    ;print,xrange,yrange
    ;print,xcoord_transform,ycoord_transform
    ;print,xrange2,yrange2
    ;print,xcoord_transform2,ycoord_transform2
    
    n_components = N_TAGS(fit)
    ipartotal=0
    ;for itag=0,n_components-1 do begin
    for itag=0,0 do begin
      fit_cur = fit.(itag)
      n_params = N_ELEMENTS(fit_cur.param)
      for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        
        image_data = reform(result[ipartotal,*,*])
        ;im=image(reform(result[ipartotal,*,*]), axis_style=0, rgb_table=3, min_value=-10)
        ;im.save,filename+fns('##',itag+1)+'_'+param.name+'_64.png', height=64, border=0
        ;im.save,filename+param.name+'_64.png', height=64, border=0
        ;im.close
        
        im=image(image_data, axis_style=2, rgb_table=3, min_value=-10, $
          xtitle='Solar X [arcsec]', ytitle='Solar Y [arcsec]')
        a = im.AXES
        a[0].coord_transform=xcoord_transform
        a[0].major=2

        a[1].coord_transform=ycoord_transform

        a[2].coord_transform=xcoord_transform2
        a[2].title = ''
        a[2].tickfont_name = a[0].tickfont_name
        a[2].tickfont_size = a[0].tickfont_size
        a[2].major=2
        a[2].hide=0
        a[2].showtext=1

        a[3].coord_transform=ycoord_transform2
        a[3].title = ''
        a[3].tickfont_name = a[0].tickfont_name
        a[3].tickfont_size = a[0].tickfont_size
        a[3].hide=0
        a[3].showtext=1
        
        ;im.save,filename+fns('##',itag+1)+'_'+param.name+'_512.jpg', height=512, border=5
        ;im.save,filename+fns('##',itag+1)+'_'+param.name+'_1024.jpg', height=1024, border=5
        im.save,filename+param.name+'_512.jpg', height=512, border=5
        im.save,filename+param.name+'_1024.jpg', height=1024, border=5
        im.close
        
        ; 64px png image
        ind = where(image_data lt -999.9 AND image_data gt -1000.1, count, complement=gooddata)
        image_min = min(image_data[gooddata], max=image_max)
        image_min = image_min - (image_max-image_min)/10
        if count gt 0 then image_data[ind]=image_min
        size_image = size(image_data)
        magnification = 64d/size_image[2]
        image_small = rot(image_data, 0, magnification, /interp)
        factor = 255d/(image_max-image_min)
        image_small = (image_small - image_min) * factor
        image_small = UINT(image_small)
        dx = size_image[1]*magnification/2
        x0 = floor(size_image[1]/2.0-dx)
        x1 = ceil(size_image[1]/2.0+dx)
        y0 = floor(size_image[2]/2.0-32)
        y1 = ceil(size_image[2]/2.0+32)-1
        image_small = image_small[x0:x1, y0:y1]

        write_png, filename+param.name+'_64.png', image_small, ct_r, ct_g, ct_b

        
        ipartotal++
      endfor ; ipar0,n_params-1
    endfor ; itag=0,N_TAGS(fit)-1

    ;stop
  endfor ; idata=0,ndata-1
END
