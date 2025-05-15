PRO ana2fits_test
  create_l3 = 1

  IF create_l3 EQ 1 THEN BEGIN
    files = []
    files = [files, "$SPICE_DATA/level2/2020/04/21/solo_L2_spice-n-exp_20200421T130201_V22_12583397-000.fits"] ; single exposure
    files = [files, "$SPICE_DATA/level2/2020/11/18/solo_L2_spice-n-ras_20201118T103132_V22_33554583-000.fits"] ; raster
    files = [files, "$SPICE_DATA/level2/2020/11/19/solo_L2_spice-n-sit_20201119T102559_V22_33554593-000.fits"] ; sit-and-stare

    file = spice_find_file(files[1])
    print, file

    obj = spice_data(file[0])
    file_l3 = obj.create_l3_file(/no_xcfit_block, /no_fitting, /no_widget) ; , /PRINT_HEADERS, /SAVE_RESIDUALS) ;
    ; stop
    print, 'Level 3 file created: ', file_l3
  ENDIF ELSE BEGIN
    file_l3 = 'solo_L3_spice-n-ras_20201118T103132_V06_33554583-000.fits'
    file_l3 = spice_find_file(file_l3, /user, level = 3)
    print, file_l3
    file_l3 = file_l3[0]
    print, 'Level 3 file found: ', file_l3
  ENDELSE

  ana = fits2ana(file_l3, headers_results = headers_results, headers_data = headers_data) ; Pass the created Level 3 file to fits2ana
  FOR i = 0, n_elements(ana) - 1 DO BEGIN
    print, '--------- ana', i
    ; help, ana[i]
    hdr0 = *headers_results[i]
    print, '------ results', i
    print, 'PARENT : ', fxpar(hdr0, 'PARENT', missing = '')
    print, 'PARENTXT : ', fxpar(hdr0, 'PARENTXT', missing = '')
    print, 'EXTNAME : ', fxpar(hdr0, 'EXTNAME', missing = '')
    print, 'RESEXT : ', fxpar(hdr0, 'RESEXT', missing = '')
    print, 'DATAEXT : ', fxpar(hdr0, 'DATAEXT', missing = '')
    print, 'WGTEXT : ', fxpar(hdr0, 'WGTEXT', missing = '')
    print, 'INCLEXT : ', fxpar(hdr0, 'INCLEXT', missing = '')
    print, 'CONSTEXT : ', fxpar(hdr0, 'CONSTEXT', missing = '')
    print, 'RESIDEXT : ', fxpar(hdr0, 'RESIDEXT', missing = '')
    ; print, hdr0
    ; stop

    hdr1 = *headers_data[i]
    print, '------ data', i
    print, 'PARENT : ', fxpar(hdr1, 'PARENT', missing = '')
    print, 'EXTNAME : ', fxpar(hdr1, 'EXTNAME', missing = '')
    print, 'RESEXT : ', fxpar(hdr1, 'RESEXT', missing = '')
    print, 'DATAEXT : ', fxpar(hdr1, 'DATAEXT', missing = '')
    print, 'WGTEXT : ', fxpar(hdr1, 'WGTEXT', missing = '')
    print, 'INCLEXT : ', fxpar(hdr1, 'INCLEXT', missing = '')
    print, 'CONSTEXT : ', fxpar(hdr1, 'CONSTEXT', missing = '')
    print, 'RESIDEXT : ', fxpar(hdr1, 'RESIDEXT', missing = '')
    print, ''
    ; print, hdr1
    ; stop
    spice_xcfit_block, ana = ana[i]
  ENDFOR
END
