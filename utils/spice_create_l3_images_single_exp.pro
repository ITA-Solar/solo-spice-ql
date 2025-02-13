PRO spice_create_l3_images_single_exp, l3_file, ana, headers_data, filename_base, l2_header, $
  show_plot = show_plot, filename = filename
  help, headers_data
  ; print, headers_data

  oJpg = spice_jpg_exp()

  handle_value, ana.data_h, data

  wcs = fitshead2wcs(headers_data)
  coords = wcs_get_coord(wcs)

  ; crop image so that lines with invalid data is not shown
  image_data = reform(data)
  help, image_data

  ion = 'single'
  lam = '00nm00'
  ; ion = name.extract('[a-z]+')
  ion = string(ion + '--------', format = '(A-8)')
  IF lam.strlen() EQ 6 THEN lam = '-' + lam

  winno = fxpar(headers_data, 'WINNO')
  ; filename_base2 = filename_base.replace('ql', 'ql-' + ion + lam + '-' + param.name.substring(0, 2)) + fns('#', hdr.winno) + '-' + fns('#', icomp + 1) + '-' + param.name.substring(0, 2)
  filename_base2 = filename_base.replace('ql', 'ql-' + ion + lam + '-' + 'int') + fns('#', winno) + '-' + '1-int'
  filename = filename_base2 + '.jpg'
  print, filename_base
  print, filename_base2
  print, filename

  colortable = 3
  reverse_colortable = 0
  startrow = 0
  endrow = (size(image_data))[1] - 1
  help, startrow, endrow

  xtitle1 = 'Wavelength [nm]'
  ytitle1 = 'Solar Y [arcsec]'

  IF 1 THEN BEGIN
    oJpg.update, filename, image_data, wcs, remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, $
      fit_trend = fit_trend, value_max = value_max, value_min = value_min, colortable = colortable, reverse_colortable = reverse_colortable, $
      xtitle = xtitle1, ytitle = ytitle1, $
      startrow = startrow, endrow = endrow, l2_header = l2_header, l3_header = headers_data, show_plot = show_plot
    ; stop
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

  return

  FOR iana = 0, n_elements(ana) - 1 DO BEGIN
    wcs = fitshead2wcs(hdr)
    coords = wcs_get_coord(wcs)

    spice_get_slit_region, l3_filename, result, startrow = startrow, endrow = endrow

    IF keyword_set(strongest_lines) THEN lLines = spice_line_list(/strongest_lines)

    n_components = n_tags(fit)
    ipartotal = 0
    FOR icomp = 0, n_components - 1 DO BEGIN
      stop
      fit_cur = fit.(icomp)
      n_params = n_elements(fit_cur.param)
      include_component = (keyword_set(no_background_images)) ? fit_cur.name NE 'Background' : 1

      lam = (fit_cur.name).extract('[0-9]+.[0-9]+')

      IF keyword_set(strongest_lines) THEN include_component = lLines.hasKey(float(lam))

      IF include_component THEN FOR ipar = 0, n_params - 1 DO BEGIN
        param = fit_cur.param[ipar]
        name = (fit_cur.name.compress()).toLower()
        ion = name.extract('[a-z]+')
        ion = string(ion + '--------', format = '(A-8)')
        lam = lam.replace('.', 'nm')
        IF lam.strlen() EQ 6 THEN lam = '-' + lam

        filename_base2 = filename_base.replace('ql', 'ql-' + ion + lam + '-' + param.name.substring(0, 2)) + fns('#', hdr.winno) + '-' + fns('#', icomp + 1) + '-' + param.name.substring(0, 2)
        ; crop image so that lines with invalid data is not shown
        image_data = reform(result[ipartotal, *, startrow : endrow, *])
        help, image_data

        IF naxis4 GT 1 THEN BEGIN
          ; sit-and-stare
          xtitle1 = 'Time [s]'
          xrange1 = [coords[3, 0, 0, startrow, 0], coords[3, 0, 0, startrow, -1]]
          xrange2 = [coords[3, 0, 0, endrow, 0], coords[3, 0, 0, endrow, -1]]
          yrange1 = [coords[2, 0, 0, startrow, 0], coords[2, 0, 0, endrow, 0]]
          yrange2 = [coords[2, 0, 0, startrow, -1], coords[2, 0, 0, endrow, -1]]
          SCALE_TO_RANGE = 0
          image_data = transpose(image_data)
        ENDIF ELSE BEGIN
          ; raster
          xtitle1 = 'Solar X [arcsec]'
          xrange1 = [coords[1, 0, 0, startrow], coords[1, 0, -1, startrow]]
          xrange2 = [coords[1, 0, 0, endrow], coords[1, 0, -1, endrow]]
          yrange1 = [coords[2, 0, 0, startrow], coords[2, 0, 0, endrow]]
          yrange2 = [coords[2, 0, -1, startrow], coords[2, 0, -1, endrow]]
          SCALE_TO_RANGE = 1
        ENDELSE
        ytitle1 = 'Solar Y [arcsec]'

        CASE param.name OF
          'velocity': BEGIN
            color_center_value = 0

            ; triggers call to eis_colors
            colortable = 100
          END
          'width': BEGIN
            colortable = 68 ; 4
            color_center_value = !NULL
          END
          ELSE: BEGIN
            color_center_value = !NULL
            colortable = 3
          END
        ENDCASE

        this_remove_horizontal_trend = (param.name EQ 'velocity') ? remove_horizontal_trend : 0
        this_remove_vertical_trend = (param.name EQ 'velocity') ? remove_vertical_trend : 0

        filename = filename_base2 + '.jpg'

        oJpg.update, filename, image_data, wcs, remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, $
          fit_trend = fit_trend, value_max = value_max, value_min = value_min, colortable = colortable, reverse_colortable = reverse_colortable, $
          xtitle = xtitle1, ytitle = ytitle1, $
          startrow = startrow, endrow = endrow, l2_header = l2_header, l3_header = *headers_results[iana], show_plot = show_plot
        oJpg.plot, /clock
        oJpg.save

        filename = filename_base2 + '-thumb.png'
        format = 'PNG'
        prits_tools.write_image_real_size, image_data, filename, $
          remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, fit_trend = fit_trend, smooth_width = smooth_width, $
          value_max = value_max, value_min = value_min, colortable = colortable, format = format, interpolation = interpolation, $
          height = 64, border = 0, reverse_colortable = reverse_colortable, $
          xrange1 = xrange1, yrange1 = yrange1, SCALE_TO_RANGE = SCALE_TO_RANGE, /no_axis, $
          color_center_value = color_center_value, show_plot = show_plot

        ipartotal++
      ENDFOR ; ipar0,n_params-1
    ENDFOR ; icomp=0,n_components-1
  ENDFOR ; iana=0,N_ELEMENTS(ana)-1 do begin

  jpg_window_name = (filename.extract('([0-9]+)-[0-9]+', /subexp))[1]
  win = getwindows(jpg_window_name)
  win_exists = obj_valid(win)
  IF win_exists THEN win.close
END
