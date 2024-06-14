FUNCTION prits_tools::remove_trends, image, value_min=value_min, value_max=value_max, $
                                     remove_horizontal_trend=remove_horizontal_trend, remove_vertical_trend=remove_vertical_trend, fit_trend=fit_trend
  compile_opt idl2, static
  
  sz = size(image)
  trend_image = fltarr(sz[1],sz[2])
  
  IF keyword_set(remove_horizontal_trend) THEN BEGIN 
     trend_median_x = median(image,dimension=2)
     
     IF keyword_set(fit_trend) THEN BEGIN 
        x = indgen(sz[1])
        r = linfit(x, trend_median_x)
        corr = r[0]+x*r[1]
        corr_im = rebin(corr, sz[1], sz[2])
        image -= corr_im
     END ELSE BEGIN 
        FOR y=0,sz[2]-1 DO trend_image[*,y] = trend_median_x
        image -= trend_image 
     ENDELSE 
  ENDIF
  
  IF keyword_set(remove_vertical_trend) THEN BEGIN 
     trend_median_y = median(image,dimension=1)
     
     IF keyword_set(fit_trend) THEN BEGIN 
        y = indgen(sz[2])
        r = linfit(y, trend_median_y)
        corr = r[0]+y*r[1]
        corr_im = transpose(rebin(corr, sz[2], sz[1]))
        image -= corr_im
     ENDIF ELSE BEGIN 
        FOR x=0,sz[1]-1 DO trend_image[x,*] = trend_median_y
        image -= trend_image
     ENDELSE 
  ENDIF 
  
  nanix = where(image NE image,n_nanix)
  
  IF keyword_set(value_min) OR keyword_set(value_max) THEN BEGIN 
     minval = (keyword_set(value_min)) ? value_min : min(image)
     maxval = (keyword_set(value_max)) ? value_max : max(image)
  
     image = image > (value_min) < value_max
  ENDIF
  
  
  IF n_nanix GT 0 THEN image[nanix] = 0
  
  return, image
END
