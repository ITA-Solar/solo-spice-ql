; Convert FITS header to wikitext format
; $Id: 2025-08-20 13:38 CEST $

PRO fitshead2wikitext, fitsfile, extension = extension, output_file = output_file
  ptools.parcheck, fitsfile, 1, "fitsfile", 'string', 0, result = result
  IF result[0] NE '' THEN BEGIN
    level = 3
    output_file = '~/spice/fitshead2wikitext.txt'
    extension = '(STP122) Ly beta 1025 (Merged)'
    IF level EQ 2 THEN BEGIN
      file_l2 = 'solo_L2_spice-n-ras_20201118T103132_V22_33554583-000.fits'
      fitsfile = spice_find_file(file_l2, level = 2)
    ENDIF ELSE IF level EQ 3 THEN BEGIN
      file_l3 = 'solo_L3_spice-n-ras_20201118T103132_V06_33554583-000.fits'
      extension = extension + ' results'
      fitsfile = spice_find_file(file_l3, /user, level = 3)
    ENDIF ELSE return
  ENDIF
  ptools.parcheck, extension, 0, "extension", ['string', 'integers'], 0, default = 0
  ptools.parcheck, output_file, 0, "output_file", 'string', 0, default = 'fitshead2wikitext.txt'

  hdr = headfits(fitsfile, ext = extension)
  hdr = '    ' + hdr + '  '

  openw, unit, output_file, /get_lun
  FOR i = 0, n_elements(hdr) - 1 DO printf, unit, hdr[i]
  close, unit
  free_lun, unit
END
