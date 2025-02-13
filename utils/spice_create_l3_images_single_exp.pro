PRO spice_create_l3_images_single_exp, ana, headers_data, filename_base, l2_header, $
  show_plot = show_plot, filename = filename
  oJpg = spice_jpg_exp()

  handle_value, ana.data_h, data

  wcs = fitshead2wcs(headers_data)

  ; crop image so that lines with invalid data is not shown
  image_data = reform(data)

  ion = 'single'
  lam = '00nm00'
  ion = string(ion + '--------', format = '(A-8)')
  IF lam.strlen() EQ 6 THEN lam = '-' + lam

  winno = fxpar(headers_data, 'WINNO')
  filename_base2 = filename_base.replace('ql', 'ql-' + ion + lam + '-' + 'int') + fns('#', winno) + '-' + '1-int'
  filename = filename_base2 + '.jpg'

  colortable = 3
  color_center_value = !NULL
  reverse_colortable = 0
  startrow = 0
  endrow = (size(image_data))[1] - 1

  xtitle1 = 'Wavelength [nm]'
  ytitle1 = 'Solar Y [arcsec]'

  IF 1 THEN BEGIN
    oJpg.update, filename, image_data, wcs, remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, $
      fit_trend = fit_trend, value_max = value_max, value_min = value_min, colortable = colortable, reverse_colortable = reverse_colortable, $
      xtitle = xtitle1, ytitle = ytitle1, $
      startrow = startrow, endrow = endrow, l2_header = l2_header, l3_header = headers_data, show_plot = show_plot
    oJpg.plot, /clock
    oJpg.save
  ENDIF

  filename = filename_base2 + '-thumb.png'
  format = 'PNG'
  prits_tools.write_image_real_size, image_data, filename, $
    remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, fit_trend = fit_trend, smooth_width = smooth_width, $
    value_max = value_max, value_min = value_min, colortable = colortable, format = format, interpolation = interpolation, $
    height = 64, border = 0, reverse_colortable = reverse_colortable, $
    xrange1 = xrange1, yrange1 = yrange1, SCALE_TO_RANGE = SCALE_TO_RANGE, /no_axis, $
    color_center_value = color_center_value, show_plot = show_plot
END
