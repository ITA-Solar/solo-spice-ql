pro test_image

  filename = '~/temp/test'

  ; Red Temperature (intensity/default)
  c = colortable(3)
  ct1_r=reform(c[*,0])
  ct1_g=reform(c[*,1])
  ct1_b=reform(c[*,2])
  colortab = 3
  ct_r = ct1_r
  ct_g = ct1_g
  ct_b = ct1_b


  colortab = 38



  image_data = findgen(90,700);/150.d
  min_image = min(image_data, max=max_image)
  help,min_image,max_image

  loadct, 38
  window,0
  pih,image_data
  ;stop

  size_image = size(image_data)
  xrange = [10d,20d]
  xcoord_transform = [xrange[0], (xrange[1]-xrange[0])/size_image[1]]
  xrange2 = [120d,180d]
  xcoord_transform2 = [xrange2[0], (xrange2[1]-xrange2[0])/size_image[1]]
  yrange = [0d,9d]
  ycoord_transform = [yrange[0], (yrange[1]-yrange[0])/size_image[2]]
  yrange2 = [13d,18d]
  ycoord_transform2 = [yrange2[0], (yrange2[1]-yrange2[0])/size_image[2]]




  im=image(image_data, axis_style=2, rgb_table=colortab, $ ;min_value=-10, $
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
  
  height = size_image[2] *18/16.d *500/426.d
  help,height

  im.save,filename+'_orig.jpg'
  im.save,filename+'_orig_border.jpg', border=5
  im.save,filename+'_512.jpg', height=512, border=5
  im.save,filename+'_1024.jpg', height=1024, border=5
  im.save,filename+'_1024.jpg', height=1024, border=5
  im.save,filename+'_'+trim(string(size_image[2]))+'_0.jpg', height=height, border=0
  im.save,filename+'_'+trim(string(size_image[2]))+'_5.jpg', height=height, border=5
  im.save,filename+'_'+trim(string(size_image[1]))+'_5_width.jpg', width=size_image[1], border=5
  im.save,filename+'_'+trim(string(size_image[2]))+'_5.svg', height=height, border=5
  im.save,filename+'_'+trim(string(size_image[2]))+'_5.bmp', height=height, border=5
  im.save,filename+'_'+trim(string(size_image[2]))+'_5.png', height=height, border=5
  
  
  im.generatecode, filename+'_code.pro'

  im.close




end
