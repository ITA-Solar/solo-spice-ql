pro test_image_tv

  filename = '~/temp/test'

  saveplot=1
  loadct, 3
  tvlct,r,g,b,/get
  ;background color
  r[255]=255
  g[255]=255
  b[255]=255
  ;text color
  r[254]=0
  g[254]=0
  b[254]=0
  tvlct,r,g,b


  xs = 100
  ys = 700
  image_data=fltarr(xs,ys)
  for i=0,xs-1 do begin
    for j=0,ys-1 do begin
      image_data[i,j] = ((i+j) mod 2) * randomn(seed)*3
    endfor
  endfor

  ;window,0
  ;pih,image_data,0.1
  ;print,image_data
  ;return

  xrange1 = [-1111,50]
  xrange2 = [2,6]
  yrange1 = [-1,15]
  yrange2 = [-12,16]

  n_digits_y1 = max(strlen(trim(string(yrange1))))
  n_digits_y2 = max(strlen(trim(string(yrange2))))

  ; 3 stellig
  margin_left = 55
  margin_right = 35

  if saveplot then begin
    margin_left = 40 + 7 * n_digits_y1
    margin_right = 20 + 6 * n_digits_y2
  endif else begin
    margin_left = 40 + 5 * n_digits_y1
    margin_right = 20 + 5 * n_digits_y2
  endelse
  margin_top = 20
  margin_bottom = 40
  WINsize = [xs+margin_left+margin_right, ys+margin_top+margin_bottom]
  print,WINSize
  Win_position = [double(margin_left)/WINsize[0], double(margin_bottom)/WINsize[1], $
    (double(xs+margin_left))/WINsize[0], (double(ys+margin_bottom))/WINsize[1]]

  if saveplot then set_plot,'z' else set_plot,'x'
  ;!p.background=253
  ;!p.color=0
  ;!p.ticklen=-.02
  ;!p.charsize=1  ;those 2 are defined above
  ;!p.charthick=1
  if saveplot then device,set_res=WINsize $
  else window, 16, xs=WINsize[0], ys=WINsize[1]


  pih, image_data, 0.01, position=Win_position, $
    xstyle=5, ystyle=5, top=253, $
    background=255, color=254, title='adsfas'

  if saveplot then charsize=1 else charsize=1.15
  axis, xaxis=0, xrange=xrange1, xtit='Solar X [arcsec]', xstyle=1, color=254, charsize=charsize, xticks=2
  axis, xaxis=1, xrange=xrange2, xstyle=1, color=254, charsize=charsize, xticks=2
  axis, yaxis=0, yrange=yrange1, ytit='Solar Y [arcsec]', ystyle=1, color=254, charsize=charsize
  axis, yaxis=1, yrange=yrange2, ystyle=1, color=254, charsize=charsize


  ; save image
  if saveplot then begin
    file=filename+'_'+trim(string(ys))+'_tv.png'
    write_png,file,tvrd(),r,g,b

    file=filename+'_'+trim(string(ys))+'_tv_2.png'
    write_image,file,'PNG',tvrd(),r,g,b
    
    help,tvrd()
    help,tvrd(/true)
    image_raw = tvrd()
    size_raw = size(image_raw)
    image_true = bytarr(3, size_raw[1], size_raw[2])
    for i=0,size_raw[1]-1 do begin
      for j=0,size_raw[2]-1 do begin
        image_true[0,i,j] = r[image_raw[i,j]]
        image_true[1,i,j] = g[image_raw[i,j]]
        image_true[2,i,j] = b[image_raw[i,j]]
      endfor
    endfor
    file=filename+'_'+trim(string(ys))+'_tv.jpg'
    write_jpeg,file,image_true,/true

    file=filename+'_'+trim(string(ys))+'_tv_2.jpg'
    write_image,file,'JPEG',tvrd(),r,g,b
    
    
    
    
  endif




;  ; Save device settings and tell IDL to use a color table
;  DEVICE, GET_DECOMPOSED=old_decomposed
;  DEVICE, DECOMPOSED=0
;  LOADCT, 14
;  ; Create an image and display it
;  IMAGE1 = DIST(300)
;  WINDOW, 1, XSIZE=300, YSIZE=300
;  TV, IMAGE1
;  ; Write a bitmap file to the temporary directory
;  ; Note the use of the TRUE keywords to TVRD and WRITE_JPEG
;  file = filename+'_test.jpg'
;  WRITE_JPEG, file, TVRD(/TRUE), /TRUE


end
