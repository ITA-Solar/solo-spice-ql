PRO private_test_l3_sigma
  ; switches for creating l3 files
  create_l3 = 0 ; main switch
  create_all = 1

  ; switches for looking at l3 files
  chi2avg_overview = 0

  ; switch for both creating and looking at L3 files
  use_new_l3 = 1

  IF use_new_l3 THEN BEGIN
    l2_files = []
    l2_files = [l2_files, "$SPICE_DATA/level2/2025/02/08/solo_L2_spice-n-ras_20250208T061921_V04_301990207-000.fits"]
    l2_files = [l2_files, "$SPICE_DATA/level2/2025/03/14/solo_L2_spice-n-ras_20250314T140552_V06_318767164-003.fits"]
    l2_files = [l2_files, "$SPICE_DATA/level2/2025/03/24/solo_L2_spice-n-ras_20250324T230647_V03_318767223-005.fits"]
    l2_files = [l2_files, "$SPICE_DATA/level2/2025/03/30/solo_L2_spice-n-sit_20250330T113021_V08_318767276-000.fits"]
    l2_files = [l2_files, "$SPICE_DATA/level2/2025/04/26/solo_L2_spice-n-ras_20250426T203032_V02_318767456-000.fits"]
    l3_files = []
    l3_files = [l3_files, "/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20250208T061921_V01_301990207-000.fits"]
    l3_files = [l3_files, "/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20250314T140552_V01_318767164-003.fits"]
    l3_files = [l3_files, "/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20250324T230647_V01_318767223-005.fits"]
    l3_files = [l3_files, "/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-sit_20250330T113021_V01_318767276-000.fits"]
    l3_files = [l3_files, "/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20250426T203032_V01_318767456-000.fits"]
  ENDIF ELSE BEGIN
    l2_files = ['/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-ras_20231028T001206_V22_218104189-001.fits', $ ; raster
      '/Users/mawiesma/data/spice/level2/2023/10/28/solo_L2_spice-n-sit_20231028T032925_V22_218104192-000.fits', $ ; sit-and-stare
      '/Users/mawiesma/data/spice/level2/2023/01/17/solo_L2_spice-n-exp_20230117T151432_V02_167772346-000.fits', $ ; single exposure, small window
      '/Users/mawiesma/data/spice/level2/2024/01/01/solo_L2_spice-n-exp_20240101T180040_V02_234881025-000.fits' $ ; single exposure, whole detector
      ] ; list of L2 files
    l3_files = ['/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-ras_20231028T001206_V01_218104189-001.fits', $ ; raster
      '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-sit_20231028T032925_V01_218104192-000.fits', $ ; sit-and-stare
      '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-exp_20230117T151432_V01_167772346-000.fits', $ ; single exposure, small window
      '/Users/mawiesma/Documents/spice/tests/test_l3_files/solo_L3_spice-n-exp_20240101T180040_V01_234881025-000.fits'] ; single exposure, whole detector
  ENDELSE

  out_dir = '/Users/mawiesma/Documents/spice/tests/test_l3_files/' ; output directory

  IF create_l3 THEN BEGIN
    IF create_all THEN BEGIN
      no_xcfit_block = 1
      no_line_list = 0
      no_fitting = 0

      ; window_indices = 0 ; select the first two windows
      FOREACH l2_file, l2_files DO BEGIN
        o = spice_object(l2_file)

        l3_file = o.create_l3_file(window_indices, pipeline_dir = out_dir, no_xcfit_block = no_xcfit_block, no_line_list = no_line_list, no_fitting = no_fitting, $
          all_result_headers = all_result_headers, all_data_headers = all_data_headers)
      ENDFOREACH
    ENDIF ELSE BEGIN ; create_all

      no_xcfit_block = 0
      no_line_list = 0
      no_fitting = 0

      l2_files = l2_files[1]

      window_indices = 0 ; select the first two windows
      FOREACH l2_file, l2_files DO BEGIN
        o = spice_object(l2_file)
        sigma_l2 = spice_calc_sigma(o, window_indices, no_masking = no_masking, approximated_slit = approximated_slit)

        l3_file = o.create_l3_file(window_indices, pipeline_dir = out_dir, no_xcfit_block = no_xcfit_block, no_line_list = no_line_list, no_fitting = no_fitting, $
          all_result_headers = all_result_headers, all_data_headers = all_data_headers)

        data = o.get_window_data(window_indices, no_masking = no_masking, approximated_slit = approximated_slit)
        hdr_result = *all_result_headers[0]
        hdr_data = *all_data_headers[0]
        SIGMADAT = fxpar(hdr_result, 'SIGMADAT', missing = '')
        ; sigma_l3 = spice_calc_sigma(data, SIGMADAT = SIGMADAT, hdr_result = hdr_result, hdr_data = hdr_data)

        ; sigma_diff = sigma_l2 - sigma_l3
        ; mindiff = min(sigma_diff, max = maxdiff)
        ; print, 'Minimum difference in sigma: %f', mindiff
        ; print, 'Maximum difference in sigma: %f', maxdiff

        stop
      ENDFOREACH
    ENDELSE ; create_all
  ENDIF ELSE BEGIN ; create_l3
    ; ana = fits2ana(l3_files[0])

    spice_xcontrol_l23, l3_files[0]
    stop
    ana = fits2ana(l3_files[0], headers_data = headers_data)
    print, fxpar(*headers_data[4], 'EXTNAME')
    xcfit_block, ana = ana[4]

    FOREACH l3_file, l3_files, index DO BEGIN
      IF chi2avg_overview THEN BEGIN
        ana = fits2ana(l3_file, headers_results = headers_results, headers_data = headers_data, /headers_only)
        nwin = n_elements(headers_results)
        chisqavg = fltarr(nwin)
        xposure = fltarr(nwin)
        FOR iwin = 0, nwin - 1 DO BEGIN
          h = headfits(l3_file, ext = iwin)
          chisqavg[iwin] = fxpar(*headers_results[iwin], 'CHISQAVG', missing = -999)
          xposure[iwin] = fxpar(*headers_data[iwin], 'XPOSURE', missing = -999)
        ENDFOR
        print, 'Average chi-squared value for L3 file: ', l3_file, ' is: '
        FOR iwin = 0, nwin - 1 DO BEGIN
          print, 'Window ', iwin, ': ', chisqavg[iwin], ' (Exposure time: ', xposure[iwin], ')'
        ENDFOR
      ENDIF ELSE BEGIN ; chi2avg_overview
        ; ana = fits2ana(l3_file, headers_only = 1)
        ; IF ana EQ 0 THEN message, 'Error reading L3 file: ' + l3_file
        ; print, 'L3 file read successfully: ', l3_file
        d = readfits(l3_file, h, ext = 0)
        print, fxpar(h, 'CHISQAVG', missing = '-999')
        fitshead2wikitext, l3_file, extension = extension, output_file = '/Users/mawiesma/Documents/spice/tests/test_l3_files/fitshead2wikitext_l3.txt'
        fitshead2wikitext, l2_files[index], extension = extension, output_file = '/Users/mawiesma/Documents/spice/tests/test_l3_files/fitshead2wikitext_l2.txt'
        stop
        ; spice_xcontrol_l23, l3_file
        ; stop
      ENDELSE ; chi2avg_overview
    ENDFOREACH
    stop
  ENDELSE ; create_l3
END
