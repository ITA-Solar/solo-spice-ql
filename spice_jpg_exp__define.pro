PRO spice_jpg_exp::_set_filename_related_parameters
  self.d.sit_and_stare = 1
  self.d.parameter = 'int'
  self.d.units = '$W/m^2/sr/nm$'
END

FUNCTION spice_jpg_exp::_get_plot_dimensions_ix
  return, [2, 1]
END

FUNCTION spice_jpg_exp::_get_tix
  return, self.d.sit_and_stare ? [0, self.d.wcs.naxis[3] - 1] : [0, 0]
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
    upper_right = (wcs_get_coord(self.d.wcs, [0, ix.upper.right.x, ix.upper.right.y, ix.upper.right.t]))[aDimIx]
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
  help, xrange1
  print, xrange1
  help, xrange2
  print, xrange2
  help, xrange
  print, xrange
  help, yrange1
  print, yrange1
  help, yrange2
  print, yrange2
  help, yrange
  print, yrange

  help, xrange, yrange
  ; stop
  self.d.sRangePadded = {x: xrange, y: yrange}
END

PRO spice_jpg_exp::_set_congrid_data
  aData = self.d.aData

  IF self.d.parameter NE 'vel' THEN BEGIN
    aData = sigrange(aData)
    self.d.aData = aData
  ENDIF

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

  self.d.sz = size(self.d.aData)
  self.d.aDataCongrid = self.d.aData
END

PRO spice_jpg_exp::_set_title
  IF self.d.hLines EQ !NULL THEN self.d.hLines = spice_line_list()
  self.d.title = ['Single Exposure', ' Intensity']
END

PRO spice_jpg_exp::_plot_coordinate_system
  self._set_coordinate_data
  help, self.d.aDataCoordinates, self.d.sCoordinateAxis.x, self.d.sCoordinateAxis.y
  ; stop
  imCoordinates = image(self.d.aData, /current, $ ; self.d.sCoordinateAxis.x, self.d.sCoordinateAxis.y, /current, $
    transparency = 90, axis_style = 2, xtickinterval = self.d.xtickinterval, $
    xtitle = self.d.xtitle, ytitle = self.d.ytitle, title = self.d.title, $
    font_size = self.d.font_size)

  imCoordinates.position = self.d.plot_position

  self.d.imCoordinatesPosition = imCoordinates.position
  self.d.plot_top_position = self.d.imCoordinatesPosition[3] * self.d.winsize_padded[1]
END

PRO spice_jpg_exp::_plot_data
  help, self.d.aData, self.d.sAxisPadded.x, self.d.sAxisPadded.y
  ; stop
  imPadded = image(self.d.aData, $ ; self.d.sAxis.x, self.d.sAxis.y, $
    rgb_table = self.d.palette, axis_style = 4, font_size = self.d.font_size, xtitle = self.d.xtitle, ytitle = self.d.ytitle, /current)
  imPadded.position = self.d.plot_position

  self.d.imPadded = imPadded
END

PRO spice_jpg_exp__define
  COMPILE_OPT IDL2
  ; Define the path to the spice_jpg_exp directory
  !NULL = {spice_jpg_exp, INHERITS spice_jpg}
END
