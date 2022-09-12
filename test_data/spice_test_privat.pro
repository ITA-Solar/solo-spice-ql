PRO spice_test_privat, file_number
  COMPILE_OPT IDL2

  IF N_ELEMENTS(file_number) NE 1 then file_number=500
  have_proc = have_proc('spice_test', out=path)
  path = file_dirname(path, /mark_directory)

  ;for file_number=1,12 do begin
  CASE file_number of
    1: file = path+'solo_L1_spice-n-ras_20210314T135349751_V01.fits'
    2: file = path+'solo_L1_spice-n-sit-db_20210623T132924744_V01.fits'
    3: file = path+'solo_L1_spice-n-ras-int_20210314T135349751_V01.fits'
    4: file = path+'solo_LL01_spice-n-exp_0542902641_V202001130934I.fits'
    5: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083015_V202003091733C_12583153-000.fits'
    6: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083203_V202003091733C_12583154-000.fits'
    7: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083431_V202003091733C_12583155-000.fits'
    8: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0637083611_V202003091733C_12583156-000.fits'
    9: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637082835_V202003091733C_12583152-000.fits'
    10: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637083841_V202003091734C_12583157-000.fits'
    11: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-sit_0637084153_V202003091734C_12583158-000.fits'
    12: file = '/Users/mawiesma/spice/data/first_images/solo_L0_spice-n-exp_0640792768_V202004211913C_12583408-000.fits'
    13: file = '/Users/mawiesma/spice/data/level2/solo_L2_spice-n-exp_20200421T135817961_V01_12583408-000.fits'
    101: BEGIN
      files = spice_find_file('2020/04/21 16:04:00.000', /all, level=2)
      file = files[4]
      ;file = files[3]
      print,file
      ; level 1
      ;  0     solo_L1_spice-n-sit_20200421T103820_V01_12583344-000.fits
      ;  1     solo_L1_spice-n-exp_20200421T110035_V01_12583367-000.fits
      ;  2     solo_L1_spice-w-exp_20200421T114615_V01_12583372-000.fits
      ;  3     solo_L1_spice-n-ras_20200421T121522_V01_12583376-000.fits
      ;  4     solo_L1_spice-n-exp_20200421T130235_V01_12583398-000.fits
      ;  5     solo_L1_spice-w-exp_20200421T132524_V01_12583404-000.fits
      ;  6     solo_L1_spice-n-exp_20200421T135817_V01_12583408-000.fits
      ;  7     solo_L1_spice-n-exp_20200421T144652_V01_12583411-000.fits
      ; level 2
      ;  0     solo_L2_spice-n-sit_20200421T103820_V01_12583344-000.fits
      ;  1     solo_L2_spice-n-exp_20200421T104920_V01_12583360-000.fits
      ;  2     solo_L2_spice-w-exp_20200421T114615_V01_12583372-000.fits
      ;  3     solo_L2_spice-n-ras_20200421T121522_V01_12583376-000.fits
      ;  4     solo_L2_spice-n-exp_20200421T130439_V01_12583399-000.fits
      ;  5     solo_L2_spice-w-exp_20200421T132524_V01_12583404-000.fits
      ;  6     solo_L2_spice-n-exp_20200421T144652_V01_12583411-000.fits
    END
    102: BEGIN
      files = spice_find_file('2020/05/15 16:04:00.000', /all, level=2)
      file = files[0]
      print,file
      ; level 1
      ; level 2
      ;  0     solo_L2_spice-n-ras_20200515T120956_V01_12583521-000.fits
      ;  1     solo_L2_spice-w-ras_20200515T121630_V01_12583522-000.fits
      ;  2     solo_L2_spice-n-sit_20200515T124521_V01_12583536-000.fits
      ;  3     solo_L2_spice-n-exp_20200515T140417_V01_12583568-000.fits
    END
    103: BEGIN
      files = spice_find_file('2020/06/20 05:09:00.000', level=2)
      file = files[0]
      print,file
      ; level 1
      ; level 2
      ;  0     solo_L2_spice-n-exp_20200620T051001_V01_16777428-000.fits
    END
    ; for testing xcfit_block
    ; raster
    200: file='/Users/mawiesma/data/spice/level2/2021/03/22/solo_L2_spice-n-ras_20210322T112721_V05_50331869-001.fits'
    201: file='/Users/mawiesma/data/spice/level2/2021/03/22/solo_L2_spice-n-ras_20210322T110031_V05_50331869-000.fits'
    ; sit-and-stare
    210: file='/Users/mawiesma/data/spice/level2/2021/04/25/solo_L2_spice-n-sit_20210425T224556_V01_50331981-000.fits'

    ; bug fixing, raster with uneven heights of windows and some windows have binning 16 -> 1 pixel in lambda
    300: file='/Users/mawiesma/data/spice/level2/2020/06/02/solo_L2_spice-n-ras-db-int_20200602T125545_V10_12583761-000.fits'
    
    ; test binary table
    ;Single exposure observation with two binary table extensions,
    ;EXTNAME=
    ;LOST_TELEMETRY
    ;APRXPLNPIXLIST[Full LW 4:1 Focal Lossy]
    401: file = '/Users/mawiesma/data/spice/level2/2021/03/22/solo_L2_spice-n-exp_20210322T033925_V09_50331861-018.fits'
    ;Raster observation with 4(!) binary tables,
    ;EXTNAMES=
    ;VARIABLE_KEYWORDS
    ;LOST_TELEMETRY
    ;APRXPLNPIXLIST[C III 977 - Peak]
    ;LOSTPLNPIXLIST[C III 977 - Peak]
    402: file = '/Users/mawiesma/data/spice/level2/2021/09/14/solo_L2_spice-n-ras_20210914T043532_V04_67109161-000.fits'
    500: file = '/Users/mawiesma/data/spice/level2/2022/04/04/solo_L2_spice-n-ras_20220404T195533_V02_100664048-000.fits'
    501: file = '/Users/mawiesma/data/spice/level2/2022/04/12/solo_L2_spice-n-exp_20220412T080039_V01_117440593-000.fits' ; single exposure
    510: file = '/Users/mawiesma/data/spice/user/level2/2022/04/04/solo_L2_spice-n-exp_20220404T150410_V01_100664043-002.fits.gz' ; single exposure
    else: file = ''
  ENDCASE


  window_index = 5 ; for 500
  ;window_index = 0


  obj = spice_data(file)
  obj->show_lines
  d=obj->get_window_data(0)
  ;obj->help
  ;stop
  ;obj->transform_data_for_ana, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  ;  debug_plot=debug_plot, $
  ;  DATA=DATA, LAMBDA=LAMBDA, WEIGHTS=WEIGHTS, MISSING=MISSING
  ;help,data,lambda,weights,missing
  ;stop
  ;badix = where(data ne data, n_bad)
  ;IF n_bad GT 0 THEN data[badix] = missing
  ;ana = mk_analysis(LAMbda, DAta, WeighTS, adef, MISSing)
  ;xcfit_block,ana=ana
  ;help,ana
  
  ana = obj->mk_analysis(0, /init_all_cubes)
  
  stop

  ;ana = obj->xcfit_block(window_index)
  ;handle_value, ana.fit_h, fit
  ;help,fit
  ;stop
  ;help,ana

  ;l3_file = obj->create_l3_file([0,5], /no_widget);, /no_fitting)
  ;print,l3_file
  ;meta_data = { $
  ;  file:'L2-file', $
  ;  winno:0, $
  ;  extname:'TEST', $
  ;  category:0, $
  ;  l3_created:0b, $
  ;  l3_file:l3_file, $
  ;  image_small_created:0b, $
  ;  image_large_created:0b $
  ;}
  ;ana = fits2ana(l3_file)
  ;help,ana
  
  
  l3_file = '/Users/mawiesma/data/spice/level3/2022/04/04/solo_L3_spice-n-ras_20220404T195533_V02_100664048-000.fits'
  root_dir = '/mn/stornext/u3/mawiesma/spice/l3_images/'
  ;spice_create_l3_images, l3_file, root_dir+'test/'
  
  handle_value,ana.result_h,res
  help,res
  stop
  spice_create_level3_jpeg_presentation, meta_data
  stop

  ;print, obj->get_header(window_index)
  ana = fits2ana('/Users/mawiesma/data/spice/level3/2022/04/04/solo_L3_spice-n-ras_20220404T195533_V02_100664048-000.fits')
  handle_value, ana.result_h, result
  plot,result[1,20,*]
;  ind = where(result lt 0)
;  result[ind]=0
;  d = reform(result[0,*,*])
;  sd = size(d)
;  magnification = 64d/sd[2]
;  d = rot(d, 0, magnification, /interp)
;  factor = 255d/max(d)
;  d = d*factor
;  d = UINT(d)
;  dx = sd[1]*magnification/2
;  x0 = floor(sd[1]/2.0-dx)
;  x1 = ceil(sd[1]/2.0+dx)
;  y0 = floor(sd[2]/2.0-32)
;  y1 = ceil(sd[2]/2.0+32)-1
;  d = d[x0:x1, y0:y1]
;  print,min(d),max(d)
  
  image_data = reform(result[0,*,*])
  ind = where(image_data lt -999.9 AND image_data gt -1000.1, count, complement=gooddata)
  image_min = min(image_data[gooddata], max=image_max)
  image_min = image_min - (image_max-image_min)/10
  if count gt 0 then image_data[ind]=image_min
  size_image = size(image_data)
  magnification = 64d/size_image[2]
  image_small = rot(image_data, 0, magnification, /interp)
  factor = 255d/(image_max-image_min)
  image_small = (image_small - image_min) * factor
  image_small = UINT(image_small)
  dx = size_image[1]*magnification/2
  x0 = floor(size_image[1]/2.0-dx)
  x1 = ceil(size_image[1]/2.0+dx)
  y0 = floor(size_image[2]/2.0-32)
  y1 = ceil(size_image[2]/2.0+32)-1
  image_small = image_small[x0:x1, y0:y1]

  c = colortable(3)
  c1=reform(c[*,0])
  c2=reform(c[*,1])
  c3=reform(c[*,2])
  
  write_png, '~/Desktop/test.png', image_small, c1, c2, c3
  loadct, 0
  ;pih,d
  
  stop
  
  
  print,'ttypes'
  print,obj->get_bintable_ttypes()
  
  print,'all'
  data = obj->get_bintable_data()
  help,data
  help,data[0]
  
  print,'one, values_only'
  data = obj->get_bintable_data('MIRRPOS', /values_only)
  help,data

  
  stop

  obj->create_l3, window_index;, /no_widget, /no_fitting

  stop
  ;d=readfits(file,h)
  ;
  ;h=fitshead2struct(h)
  ;wcs = obj->get_wcs_coord(0)
  
  ;print,obj->get_xcen(), obj->get_ycen()
  ;print,obj->get_fovx(), obj->get_fovy()
  ;stop
  ;help, obj->get_missing_value()
  ;stop

  ;print, 'NAXIS1 : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2)
  ;print, 'NAXIS2 : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2)
  ;print, 'NAXIS3 : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2)
  ;print, 'NAXIS4 : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)

  for i=0,obj->get_number_windows()-1 do begin
    ;print,obj->get_window_position(i)
    print, strcompress(string(i)) + '  : ' + strtrim(string(obj->get_header_info('NAXIS1', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS2', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS3', window_index)),2) + $
      '  : ' + strtrim(string(obj->get_header_info('NAXIS4', window_index)),2)
  endfor
  ;print,obj->get_spatial_binning()
  ;print,obj->get_spectral_binning()

  ;print, obj->get_satellite_rotation()
  ;print, obj->get_window_position(window_index, detector=detectornr);, /idl_coord, /reverse_y, /reverse_x)
  help, obj->get_one_image(window_index, 0, /debin)
  ;help, obj->get_one_image(window_index, 1, /debin)
  ;stop



  spice_raster_browser, obj, /no_goes
  stop

  spice_xwhisker, obj, window_index
  ;stop

  ;d = spice_getwindata(obj, window_index)
  ;help,d

  spice_xdetector, obj, indgen(obj->get_number_windows())
  stop

  spice_xraster, obj, [window_index]
  stop

  spice_xmap, obj, linelist=window_index

  ;spice_xcontrol, obj
END
