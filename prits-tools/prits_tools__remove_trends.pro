FUNCTION prits_tools::remove_trends, image, value_min=value_min, value_max=value_max
  compile_opt idl2, static
  
  sz = size(image)
  trend_image = fltarr(sz[1],sz[2])
  
  ;; Remove vertical velocity trend
  trend_median_x = median(image,dimension=2)
  FOR y=0,sz[2]-1 DO trend_image[*,y] = trend_median_x
  image -= trend_image
  
  ;; Remove horizontal velocity trend
  trend_median_y = median(image,dimension=1)
  FOR x=0,sz[1]-1 DO trend_image[x,*] = trend_median_y
  image -= trend_image
       
  nanix = where(image NE image,n_nanix)
  
  IF keyword_set(value_min) OR keyword_set(value_max) THEN BEGIN 
     minval = (keyword_set(value_min)) ? value_min : min(image)
     maxval = (keyword_set(value_max)) ? value_max : max(image)
  
     image = image > (value_min) < value_max
  ENDIF
  
  
  IF n_nanix GT 0 THEN image[nanix] = 0
  
  return, image
END
