pro test_image_tv

  saveplot=0
  
  xs = 30
  ys = 500
  image_data=fltarr(xs,ys)
  for i=0,xs-1 do begin
    for j=0,ys-1 do begin
      image_data[i,j] = (i+j) mod 2
    endfor
  endfor
  
  window,0
  pih,image_data,0.1
  ;print,image_data
  ;return
  
  margin_left = 100
  margin_right = 40
  margin_top = 40
  margin_bottom = 100
  WINsize = [xs+margin_left+margin_right, ys+margin_top+margin_bottom]
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
    ;origin=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[2]+1], $
    ;min=SJIData[i].min_show, max=maxshow, $
    xstyle=5, ystyle=5, top=253
  ;axis, xaxis=1, xrange=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[4]+1], xtit='Solar X [Px]', xstyle=1
  ;axis, xaxis=0, xrange=[(-SJIData[i].Boxcoords[1])/2/6.0, (SJIData[i].Boxcoords[1])/2/6.0], xtit='Solar X [arcsec]', xstyle=1
  ;axis, yaxis=1, yrange=[SJIData[i].Boxcoords[2]+1, SJIData[i].Boxcoords[5]+1], ytit='Solar Y [Px]', ystyle=1
  ;axis, yaxis=0, yrange=[(-SJIData[i].Boxcoords[3])/2/6.0, (SJIData[i].Boxcoords[3])/2/6.0], ytit='Solar Y [arcsec]', ystyle=1


  ; save image
  if saveplot then begin
    file=datestamp + fns(OBSList.ID+'_pic_####.png',step)
    write_png,file,tvrd(),r,g,b
  endif



end
