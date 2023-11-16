pro test_new

  file = '/Users/mawiesma/data/spice/level2/2023/04/05/solo_L2_spice-n-ras_20230405T165232_V02_184549674-000.fits'
  d = readfits(file, h)
  
  HEADERS_INPUT_DATA = h
  
  mkhdr, hdr, indgen(3,3)
  a = ana2fitshdr_wcshdr(HDR, HEADERS_INPUT_DATA, XDIM1_TYPE='WAVE')
  
  print,HEADERS_INPUT_DATA
  
  a = fxpar(HEADERS_INPUT_DATA, 'PC*', missing='xxx')
  help,a
  print,a
  a = fxpar(HEADERS_INPUT_DATA, 'PC1_*', missing='xxx')
  help,a
  print,a
  a = fxpar(HEADERS_INPUT_DATA, 'PC2_*', missing='xxx')
  help,a
  print,a
  a = fxpar(HEADERS_INPUT_DATA, 'PC3_*', missing='xxx')
  help,a
  print,a
  a = fxpar(HEADERS_INPUT_DATA, 'PC4_*', missing='xxx')
  help,a
  print,a
  

end
