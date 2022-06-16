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
; $Id: 2022-06-16 14:45 CEST $


PRO spice_create_level3_jpeg_presentation

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

  restore, meta_data_file
  ind = where(meta_data.l3_created, ndata)
  
  ;for idata=0,ndata-1 do begin
  for idata=0,1 do begin
    ana = fits2ana(meta_data[ind[idata]].l3_file, titles=titles)
    handle_value,ana.data_h,data,/no_copy
    handle_value,ana.result_h,result,/no_copy
    handle_value,ana.fit_h,fit,/no_copy
    print,meta_data[ind[idata]].l3_file
    help,data
    help,result
    help,fit
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
    print,xrange,yrange
    print,xcoord_transform,ycoord_transform
    print,xrange2,yrange2
    print,xcoord_transform2,ycoord_transform2
    
    n_components = N_TAGS(fit)
    ipartotal=0
    for itag=0,n_components-1 do begin
      fit_cur = fit.(itag)
      n_params = N_ELEMENTS(fit_cur.param)
      for ipar=0,n_params-1 do begin
        param = fit_cur.param[ipar]
        
        im=image(reform(result[ipartotal,*,*]), axis_style=0, rgb_table=3, min_value=-10)
        im.save,'~/Desktop/spice_L3_blabla_'+fns('##',itag+1)+'_'+param.name+'_64.png', height=64
        ;im.close
        
        im=image(reform(result[ipartotal,*,*]), axis_style=2, rgb_table=3, min_value=-10, $
          ;xrange=xrange, yrange=yrange, $
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
        
        im.save,'~/Desktop/spice_L3_blabla_'+fns('##',itag+1)+'_'+param.name+'_512.jpg', height=512
        ;im.close
        ;a[2].title='New see here'
        ;a[2].axis_range=[10,90]
        help,a
        stop
        ipartotal++
      endfor ; ipar0,n_params-1
    endfor ; itag=0,N_TAGS(fit)-1

    stop
  endfor ; idata=0,ndata-1
END
