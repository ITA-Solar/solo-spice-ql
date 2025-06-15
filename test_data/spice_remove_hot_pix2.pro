PRO r
  COMMON spice_remove_hot_pix_original_input, data_in, object_in, ext_in, $
     res_earlier_in, lw_map, sw_map, header, xposure
  delvarx, data_in
END

FUNCTION percentile, data, pctile
  nbins = 10000
  h = histogram(data,nbins=nbins, omin=omin, omax=omax)
  x = total(h, /cumulative)
  x = x / max(x) * 
  ix = (where(h GT pctile, count))[0]
  IF count EQ 0 THEN message, "Can't do this percentile???"
  binsize = (omax - omin)*1.0/nbins
  value = omin + binsize*ix
  stop
END 

FUNCTION spice_remove_hot_pix, data, object, ext, res_earlier = res_earlier, limit=limit
  ptools.default, limit, 100
  COMMON spice_remove_hot_pix, hotpix_obj
  IF n_elements(hotpix_obj) EQ 0 THEN hotpix_obj = obj_new('hotpix')
  
  COMMON spice_remove_hot_pix_original_input, data_in, object_in, ext_in, $
     res_earlier_in, lw_map, sw_map, header, xposure
  
  IF n_elements(data_in) EQ 0 THEN BEGIN
     data_in = data
     object_in = object
     ext_in = ext
     hdr = object.get_header(ext)
     hotpix_obj.set, fxpar(hdr, 'DATE-BEG')
     hotpix_obj.darks, lw_map, sw_map ; Get "dark maps" for LW/SW detector
  END
  IF n_elements(data) EQ 0 THEN BEGIN
     data = data_in
     object = object_in
     ext = ext_in
  END
  
  hdr = object.get_header(ext)
  date_beg = fxpar(hdr, 'DATE-BEG')
  detector = fxpar(hdr, 'DETECTOR')
  xposure = fxpar(hdr, 'XPOSURE')
    

  data = float(data)
  
  pxbeg = [0] ; Dummies
  pxend = [0]
  FOR i=1, 4 DO BEGIN
     n = i.toString()
     pxbeg = [pxbeg, fxpar(hdr, 'PXBEG' + n)]
     pxend = [pxend, fxpar(hdr, 'PXEND' + n)]
  END
  pxbeg = pxbeg MOD 1024
  pxend = pxend MOD 1024
  sizes = abs(pxend - pxbeg) + 1
  XSIZE = sizes[1]
  YSIZE = sizes[2]
  DSIZE = sizes[3]
  TSIZE = sizes[4]
  
  
  map = detector EQ 'SW' ? sw_map : lw_map
  
  hotmap = map - fmedian(map, 3, 3)
  nhotmask = n_elements(hotmap)*1.0
  window, 0
  pih, hotmap
  badmask = hotmap GT limit
  badix = where(badmask, count)
  hotmap[badix] = !values.f_nan
  print, "Fraction: ", count/nhotmask
  
  hotmap_extract = transpose(hotmap(pxbeg[3]-1:pxend[3]-1,  pxbeg[2]-1:pxend[2]-1))
  help, hotmap_extract

  bin_dsize = dsize/fxpar(hdr, 'NBIN3')
  hotmap_extract = rebin(reform(hotmap_extract, 1, ysize, dsize, 1), xsize, ysize, bin_dsize, tsize)
  help, hotmap
  help, data
  
  
  window, 0
  pih,data[*,*,5], 
  
;  ratio_limit = 0.1
;  FOR i=0, n_elements(data[*, 0, 0])-1 DO BEGIN
;     exposure = reform(data[i, *, *])
;     hotdata = exposure - fmedian(exposure, 3, 3)
;     ratio = hotdata/exposure
;     cut_ix = where(ratio GE ratio_limit, count)
;     IF count EQ 0 THEN CONTINUE
;     exposure[cut_ix] = !values.f_nan
;     data[i, *, *] = exposure
;  END
  
  hotmap_extract_badix = where(hotmap_extract NE hotmap_extract, count)
  print, "Fraction: ", count*1.0/n_elements(hotmap_extract)
  data[hotmap_extract_badix] = !values.f_nan
  window, 1
  pih, data[*, *, 5], 0.01

  stop
  
  
  IF arg_present(res_earlier) THEN BEGIN
    file = spice_find_file("solo_L1_spice-n-ras_20250331T160031_V02_318767282-000.fits", /user, level = 1)
    file = file[0]
    res_earlier = readfits(file, h)
  ENDIF

  return, data
END

;test_new_sigma     ; initialize common block, takes time
;d = spice_remove_hot_pix(limit=1) ; use thereafter, quick
END
