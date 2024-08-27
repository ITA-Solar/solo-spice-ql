PRO spice_test_coord

  f='/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-ras_20230117T153421_V04_167772348-000.fits' ; with binning
  ;f='/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T184932_V03_167772350-000.fits' ; no binning

  o=spice_data(f)

  nwin = o->get_number_windows()
  ccd_size = o->get_ccd_size()
  
  wave_extreme = fltarr(nwin, 2)
  wave_extreme_rel = fltarr(nwin, 2)

  for iwin=0,nwin-1 do begin
    xscale=o->get_lambda_vector(iwin,/full)
    wave_extreme[iwin, *] = [xscale[0], xscale[-1]]
    xscale_win=o->get_lambda_vector(iwin)
    pos = o->get_window_position(iwin,/debin,/idl, detector=detector)
    if detector eq 2 then begin
      pos[0:1] = pos[0:1] - ccd_size[0]
    endif
    wavemin = o->get_header_keyword('WAVEMIN', iwin)
    wavemax = o->get_header_keyword('WAVEMax', iwin)
    pos_wave = [xscale[pos[0]], xscale[pos[1]]]

    cdelt3 = o->get_header_keyword('CDElt3', iwin)
    wave_extreme_rel[iwin, *] = [xscale[0]/cdelt3, xscale[-1]/cdelt3] - wave_extreme_rel[0,*]
    
    print, ''
    print, '----------------------'
    print, 'Window number   : ', iwin
    print, 'Window positions: ', pos
    print, 'Wavemin, wavemax: ', wavemin, wavemax
    print, 'xscale window   : ', xscale_win[0], xscale_win[-1]
    print, 'diff            : ', xscale_win[0]-wavemin, xscale_win[-1]-wavemax
    print, 'diff/cdelt3     : ', (xscale_win[0]-wavemin)/cdelt3, (xscale_win[-1]-wavemax)/cdelt3
    
    print, ''
    print, 'xscale det[pos] : ', pos_wave
    print, 'dif             : ', pos_wave[0]-wavemin, pos_wave[1]-wavemax
    print, 'diff/cdelt3     : ', (pos_wave[0]-wavemin)/cdelt3, (pos_wave[1]-wavemax)/cdelt3

    print, ''
    print, 'cdelt3          : ', cdelt3

  endfor

  print, ''
  print, wave_extreme
  print, ''
  print, wave_extreme_rel
END
