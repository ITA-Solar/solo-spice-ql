PRO test_get_distorted_coord
  create_l3 = 0 ; create L3 files
  use_l2 = 1 ; use L2 files

  l2_files = ['/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits', $ ; raster
    '/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-sit_20231028T032925_V22_218104192-000.fits', $ ; sit-and-stare
    '/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T151432_V02_167772346-000.fits', $ ; single exposure, small window
    '/Users/mawiesma/data/spice/level2/2024/01/01/solo_L2_spice-n-exp_20240101T180040_V02_234881025-000.fits' $ ; single exposure, whole detector
    ] ; list of L2 files
  file = l2_files[1] ; select one file

  obj = spice_object(file)
  x0 = obj.get_instr_x_vector(0)
  print, x0[0 : 9]

  IF 0 THEN BEGIN
    print, ''

    data = readfits(file, header, ext = 0)
    wcs = fitshead2wcs(header, filename = file)
    corrected_coordinates = wcs_get_coord(wcs)
    sc = size(corrected_coordinates)
    help, corrected_coordinates
    print, sc
    x1 = reform(corrected_coordinates[0, *, floor(sc[3] / 2.), floor(sc[4] / 2.), floor(sc[5] / 2.)])
    help, x1
    n_elementsx = n_elements(x1)
    nx = (n_elementsx GT 10) ? 9 : n_elementsx - 1
    print, x1[0 : nx]

    print, ''

    x2 = obj.get_wcs_coord(0, /x)
    sc2 = size(x2)
    help, x2
    print, sc2
    x2b = reform(x2[*, floor(sc2[2] / 2.), floor(sc2[3] / 2.), floor(sc2[4] / 2.)])
    n_elementsx = n_elements(x2b)
    nx = (n_elementsx GT 10) ? 9 : n_elementsx - 1
    print, x2b[0 : nx]
  ENDIF

  print, ''

  naxis = obj.get_header_keyword('naxis*', 0)
  print, naxis

  print, ''

  naxis = obj.get_header_keyword('naxis*', 0)
  npix = obj.get_sit_and_stare() ? naxis[3] : naxis[0]
  pixels = lonarr(4, npix)
  pixels[0, *] = obj.get_sit_and_stare() ? 0 : indgen(npix)
  pixels[1, *] = floor(naxis[1] / 2.)
  pixels[2, *] = floor(naxis[2] / 2.)
  pixels[3, *] = obj.get_sit_and_stare() ? indgen(npix) : 0

  x_coords = obj.get_wcs_coord(0, pixels, /x)
  help, x_coords
  print, x_coords[0 : 9]
  ; plot, x_coords

  print, ''

  y_coord_start = (obj.get_window_position(0, /idl_coord, /debin))[2]
  print, y_coord_start

  npix += 30
  pixels = lonarr(4, npix)
  pixels[0, *] = obj.get_sit_and_stare() ? 0 : indgen(npix) - 10
  pixels[1, *] = floor(naxis[1] / 2.)
  pixels[2, *] = floor(naxis[2] / 2.)
  pixels[3, *] = obj.get_sit_and_stare() ? indgen(npix) - 10 : 0

  x_coords = obj.get_wcs_coord(0, pixels, /x)
  help, x_coords
  print, x_coords[8 : 19]

  print, ''

  y0 = obj.get_instr_y_vector(0)
  help, y0
  print, y0[0 : 9]
  y1 = obj.get_instr_y_vector(0, /full_ccd)
  help, y1
  print, y1[0 : 9]
  plot, y1
  oplot, y0
  ; stop

  FOREACH l2_file, l2_files DO BEGIN
    print, ''
    print, l2_file
    print, ''
    obj = spice_object(l2_file)
    x = obj.get_instr_x_vector(0)
    y0 = obj.get_instr_y_vector(0)
    y1 = obj.get_instr_y_vector(0, /full_ccd)
    lambda0 = obj.get_lambda_vector(0)
    lambda1 = obj.get_lambda_vector(0, /full_ccd)
    time = obj.get_time_vector(0)

    help, x
    help, y0
    help, y1
    help, lambda0
    help, lambda1
    help, time

    nbin = obj.get_spatial_binning(0)
    y_coord_start = (obj.get_window_position(0, /idl_coord, /debin))[2]
    npix = (obj.get_ccd_size())[1]
    pixels = fltarr(4, npix)
    pixels[1, *] = findgen(npix) / nbin - y_coord_start

    plot, pixels[1, *], y1, psym = 4
    oplot, indgen(n_elements(y0)), y0, psym = 7, symsize = 2
    IF n_elements(x) GT 1 THEN oplot, x, psym = 2
    ; stop

    nbin = obj.get_spectral_binning(0)
    lambda_coord_start = (obj.get_window_position(0, /idl_coord, /debin))[0]
    npix = (obj.get_ccd_size())[0]
    pixels = fltarr(4, npix)
    pixels[2, *] = findgen(npix) / nbin - lambda_coord_start

    plot, pixels[2, *], lambda1, psym = 4
    oplot, indgen(n_elements(lambda0)), lambda0, psym = 7, symsize = 2

    IF n_elements(time) GT 1 THEN BEGIN
      plot, time, psym = 4
      stop
    ENDIF ELSE BEGIN
      print, 'No time vector'
    ENDELSE

    print, '------------'
    ; stop
  ENDFOREACH
END
