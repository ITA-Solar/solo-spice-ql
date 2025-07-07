pro load_gen_dlms_func_test
  x=findgen(100)/20 - 2.5

  
  cf_g_p0_,x,[1,0,1,0.5],y
  window,0
  plot,x,y, title='cf_g_p0_(1,0,1,0.5)', xstyle=3, ystyle=2

  comp_poly,x,[1,1,1,1],y
  window,1
  plot,x,y, title='comp_poly(1,1,1,1)', xstyle=3, ystyle=3

  comp_gauss,x,[1.,0,1],y  
  window,2
  plot,x,y, title='comp_gauss(1.,0,1)', xstyle=3, ystyle=3

  a = !null
  a = [a, 1, -2, 0.2]
  a = [a, 1, -1, 0.2]
  a = [a, 1, 0, 0.2]
  a = [a, 1, 1, 0.2]
  a = [a, 1, 2, 0.2]
  a = [a, 1,1]
  cf_g_g_g_g_g_p1_,x,a,y
  window,3
  plot,x,y, title='cf_g_g_g_g_g_p1_', xstyle=3, ystyle=3
end
message,/info,"Can't run test here, dlms must be loaded before compilation"
message,/info,"or else cf_g_g_g_g_g_p1_ will not be defined, huh???"
end