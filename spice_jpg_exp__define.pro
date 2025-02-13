PRO spice_jpg_exp::_set_filename_related_parameters
  self.d.sit_and_stare = 1
  self.d.parameter = 'int'
  self.d.units = '$W/m^2/sr/nm$'
END

FUNCTION spice_jpg_exp::_get_plot_dimensions_ix
  return, [2, 1]
END

PRO spice_jpg_exp::_set_xyrange_padded_data
  ; ; We want the x/y ranges of the rebinned-to-1" resolution array, we need to
  ; ; calculate the coordinates at the centre of edge pixels when rebinned to 1"
  ; ; The pixel index offset from the centre of a pixel to the centre of the
  ; ; first rebinned-to-1"-resolution pixel. For sit-and-stare the image is not rebinned.
  ix = self._get_corner_pixels_wcs_coord_ix()
  aDimIx = self._get_plot_dimensions_ix()

  IF self.d.sit_and_stare THEN BEGIN
    lower_left = (wcs_get_coord(self.d.wcs, [0, ix.lower.left.x, ix.lower.left.y, ix.lower.left.t]))[aDimIx]
    lower_right = (wcs_get_coord(self.d.wcs, [0, ix.lower.right.x, ix.lower.right.y, ix.lower.right.t]))[aDimIx]
    upper_left = (wcs_get_coord(self.d.wcs, [0, ix.upper.left.x, ix.upper.left.y, ix.upper.left.t]))[aDimIx]
    upper_right = (wcs_get_coord(self.d.wcs, [0, ix.upper.right.x, ix.upper.right.y, ix.upper.right.t]))[aDimIx] ; typo in original code
  ENDIF ELSE BEGIN
    lower_left = (wcs_get_coord(self.d.wcs, [0, ix.lower.left.x, ix.lower.left.y]))[aDimIx]
    lower_right = (wcs_get_coord(self.d.wcs, [0, ix.lower.right.x, ix.lower.right.y]))[aDimIx]
    upper_left = (wcs_get_coord(self.d.wcs, [0, ix.upper.left.x, ix.upper.left.y]))[aDimIx]
    upper_right = (wcs_get_coord(self.d.wcs, [0, ix.upper.right.x, ix.upper.right.y]))[aDimIx]
  ENDELSE

  xrange1 = [lower_left[0], lower_right[0]]
  xrange2 = [upper_left[0], upper_right[0]]
  yrange1 = [lower_left[1], upper_left[1]]
  yrange2 = [lower_right[1], upper_right[1]]

  xrange = [min(xrange1 < xrange2), max(xrange1 > xrange2)]
  yrange = [min(yrange1 < yrange2), max(yrange1 > yrange2)]

  self.d.sRangePadded = {x: xrange, y: yrange}
END

PRO spice_jpg_exp::_set_congrid_data
  aData = sigrange(self.d.aData)
  self.d.aData = aData

  crval1 = fxpar(self.d.l2_header, 'CRVAL1')
  cdelt1 = fxpar(self.d.l2_header, 'CDELT1')
  crpix1 = fxpar(self.d.l2_header, 'CRPIX1')
  crval2 = fxpar(self.d.l2_header, 'CRVAL2')
  cdelt2 = fxpar(self.d.l2_header, 'CDELT2')
  crpix2 = fxpar(self.d.l2_header, 'CRPIX2')

  x_unrot = crval1 + cdelt1 * (indgen(self.d.sz[1]) + 1 - crpix1)

  xadd = (fxpar(self.d.l2_header, 'SLIT_WID') - 1) / 2.
  yadd = (fxpar(self.d.l2_header, 'CDELT2') - 1) / 2.

  x_min_unrot = min(x_unrot) - xadd
  x_max_unrot = max(x_unrot) + xadd
  self.d.xrange_congrid = [x_min_unrot, x_max_unrot]

  naxis2_l2 = fxpar(self.d.l3_header, 'NAXIS3')
  y_unrot = crval2 + cdelt2 * (indgen(naxis2_l2) + 1 - crpix2)
  y_min_unrot = y_unrot[self.d.startrow] - yadd
  y_max_unrot = y_unrot[self.d.endrow] + yadd
  self.d.yrange_congrid = [y_min_unrot, y_max_unrot]

  self.d.sz = size(aData)
  self.d.aDataCongrid = aData
END

PRO spice_jpg_exp::_set_title
  IF self.d.hLines EQ !NULL THEN self.d.hLines = spice_line_list()
  self.d.title = ['Single Exposure', ' Intensity']
END

PRO spice_jpg_exp::_plot_data
  size_image = size(self.d.aData)
  sAxisx = findgen(size_image[1])
  sAxisy = findgen(size_image[2])
  imPadded = image(self.d.aData, sAxisx, sAxisy, /current, $
    title = self.d.title, xtitle = self.d.xtitle, ytitle = self.d.ytitle, $
    axis_style = 2, font_size = self.d.font_size, $
    rgb_table = self.d.palette)
  imPadded.position = self.d.plot_position

  xrange = self.d.srangepadded.x
  xa = double(min(xrange))
  xb = (max(xrange) - xa) / (size_image[1] - 1)
  yrange = self.d.srangepadded.y
  ya = double(min(yrange))
  yb = (max(yrange) - ya) / (size_image[2] - 1)

  axes = imPadded.axes
  axes[0].coord_transform = [xa, xb]
  axes[0].major = self.d.xtickmajor
  axes[1].coord_transform = [ya, yb]

  self.d.imPadded = imPadded

  self.d.imCoordinatesPosition = imPadded.position
  self.d.plot_top_position = self.d.imCoordinatesPosition[3] * self.d.winsize_padded[1]
END

PRO spice_jpg_exp::_set_plot_keywords_based_on_padded_data_size
  self.d.xtickinterval = 0
  self.d.xtickmajor = -1
  self.d.colorbar_major = -1
  self.d.font_size = 12
  self.d.text_font_size = 10

  self.d.clock_size = 1
  self.d.clock_position_offset = [0, 0]

  szx = self.d.sz[1]
  IF szx LT 160 THEN BEGIN
    self.d.xtickmajor = 2
    self.d.colorbar_major = (self.d.parameter EQ 'vel') ? 3 : 2
  ENDIF

  IF szx LT 100 THEN BEGIN
    self.d.clock_size = 0.7
    self.d.clock_position_offset = [-10, -10]
  ENDIF

  IF szx LT 80 THEN BEGIN
    self.d.text_font_size -= 2
  ENDIF

  IF szx LT 50 THEN BEGIN
    self.d.xtickmajor = 1
    self.d.text_font_size -= 1
    self.d.clock_size = 0.5
    self.d.clock_position_offset = [25, -5]
  ENDIF
END

PRO spice_jpg_exp::plot, clock = clock
  tic
  self._plot_data
  print, 'self._plot_data'
  toc
  ; self._plot_coordinate_system
  print, 'self._plot_coordinate_system'
  toc
  self._plot_colorbar
  print, 'self._plot_colorbar'
  toc
  self._plot_texts
  print, 'self._plot_texts'
  toc
  self._plot_compass
  print, 'self._plot_compass'
  toc
  IF keyword_set(clock) THEN self._plot_clock
  print, 'self._plot_clock'
  toc
END

PRO spice_jpg_exp__define
  COMPILE_OPT IDL2
  ; Define the path to the spice_jpg_exp directory
  !NULL = {spice_jpg_exp, INHERITS spice_jpg}
END
