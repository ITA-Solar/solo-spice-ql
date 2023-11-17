pro test_new

  file = '/Users/mawiesma/data/spice/level2/2023/04/05/solo_L2_spice-n-ras_20230405T165232_V02_184549674-000.fits'
  d = readfits(file, h)
  
  HEADERS_INPUT_DATA = h
  
  mkhdr, hdr, indgen(3,3)
  result = ana2fitshdr_wcshdr(HDR, HEADERS_INPUT_DATA, XDIM1_TYPE='WAVE')
  
  print,HEADERS_INPUT_DATA
  
  a0 = fxpar(HEADERS_INPUT_DATA, 'PC*', missing='xxx')
  help,a0
  print,a0
  a1 = fxpar(HEADERS_INPUT_DATA, 'PC1_*', missing='xxx')
  help,a1
  print,a1
  a2 = fxpar(HEADERS_INPUT_DATA, 'PC2_*', missing='xxx')
  help,a2
  print,a2
  a3 = fxpar(HEADERS_INPUT_DATA, 'PC3_*', missing='xxx')
  help,a3
  print,a3
  a4 = fxpar(HEADERS_INPUT_DATA, 'PC4_*', missing='99', count=count)
  help,a4
  print,a4
  print,count
  
  print,result
  
  wcs = fitshead2wcs(HEADERS_INPUT_DATA)
  help,wcs
  print,wcs.proj_names
  print,wcs.proj_values
  help,wcs.time
  help,wcs.position
  help,wcs.spectrum
  
  new_hdr = wcs2fitshead(wcs)
  help,new_hdr
  print,new_hdr

end
