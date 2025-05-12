; Convert FITS header to wikitext format
; $Id: 2025-05-12 13:31 CEST $

PRO fitshead2wikitext, fitsfile, extension = extension
  level = 3
  outfile = '~/spice/fitshead2wikitext.txt'
  extension = '(STP122) Ly beta 1025 (Merged)'
  IF level EQ 2 THEN BEGIN
    file_l2 = 'solo_L2_spice-n-ras_20201118T103132_V22_33554583-000.fits'
    fitsfile = spice_find_file(file_l2, level = 2)
  ENDIF ELSE IF level EQ 3 THEN BEGIN
    file_l3 = 'solo_L3_spice-n-ras_20201118T103132_V06_33554583-000.fits'
    extension = extension + ' results'
    fitsfile = spice_find_file(file_l3, /user, level = 3)
  ENDIF ELSE return
  hdr = headfits(fitsfile, ext = extension)
  help, hdr
  hdr = '    ' + hdr + '  '
  help, hdr

  openw, unit, outfile, /get_lun
  FOR i = 0, n_elements(hdr) - 1 DO printf, unit, hdr[i]
  close, unit
  free_lun, unit
END
