PRO image_test
  COMPILE_OPT IDL2
  ; Create a test image
  data = fltarr(100, 100)
  FOR i = 0, 99 DO BEGIN
    data[i, *] = i
  ENDFOR
  data = findgen(100, 100)
  ; Display the image
  ; Test 1
  x = findgen(100) * 0.1
  y = findgen(100)
  ; im = image(data, x, y, axis_style = 2)

  ; Test 2
  ; im = image(data, x, y, axis_style = 2, aspect_ratio = 0.10)

  ; Test 3
  ; im = image(data, x, y, axis_style = 2, aspect_ratio = 0.10, scale_factor = 2)

  ; Test 3
  size_data = size(data)
  xrange = [60, 61]
  yrange = [-10, 10]
  im = image(data, axis_style = 2) ; , aspect_ratio = 0.01, scale_factor = 2)

  xa = double(min(xrange))
  xb = (max(xrange) - xa) / (size_data[1] - 1)
  ya = double(min(yrange))
  yb = (max(yrange) - ya) / (size_data[2] - 1)

  ax = im.axes
  ax[0].coord_transform = [xa, xb]
  ax[0].major = 3
  ax[1].coord_transform = [ya, yb]
  ; ax[0].yrange = y
END
