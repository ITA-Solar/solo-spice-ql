pro test_new

  file = '/Users/mawiesma/data/spice/level2/2023/04/05/solo_L2_spice-n-ras_20230405T165232_V02_184549674-000.fits'
  file = '/Users/mawiesma/data/spice/level2/2023/10/05/solo_L2_spice-n-ras_20231005T011034_V01_218103890-000.fits'
;  d0 = readfits(file, h0)
;  help,d0
;  d1 = readfits(file, h1, ext=1)
;  help,d1
;  d2 = readfits(file, h2, ext=2)
;  help,d2

  ol2 = spice_data(file)
  ana = ol2->mk_analysis(0, /init_all_cubes)

  FILEPATH_OUT = '~/file.fits'
  N_WINDOWS = 3
  WINNO = 0
  DATA_ID = ['data_id_0', 'data_id_1', 'data_id_2']
  TYPE_XDIM1 = 'WAVE'
  IS_EXTENSION = 0
  LEVEL = 'L3'
  VERSION = 3
  HEADER_INPUT_DATA = ptrarr(3)
  HEADER_INPUT_DATA[0] = ptr_new(ol2->get_header(0))
  HEADER_INPUT_DATA[1] = ptr_new(ol2->get_header(1))
  HEADER_INPUT_DATA[2] = ptr_new(ol2->get_header(2))
  PROGENITOR_DATA = ptrarr(3)
  PROGENITOR_DATA[0] = ptr_new(ol2->get_window_data(0))
  PROGENITOR_DATA[1] = ptr_new(ol2->get_window_data(1))
  PROGENITOR_DATA[2] = ptr_new(ol2->get_window_data(2))


  SAVE_XDIM1 = 0
  NO_SAVE_DATA = 0
  PRINT_HEADERS = 1


  ol2 = spice_data(file)
  ana0 = ol2->mk_analysis(0, /init_all_cubes)
  ana1 = ol2->mk_analysis(1, /init_all_cubes)
  ana2 = ol2->mk_analysis(2, /init_all_cubes)
  ana = [ana0, ana1, ana2]
  

  ana2fits, ANA, FILEPATH_OUT=FILEPATH_OUT, $
    N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
    DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
    IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
    PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
    XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
    RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
    CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
    DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
    PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
    SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
    SAVE_NOT=SAVE_NOT, $
    DATA_ARRAY=DATA_ARRAY, $
    headers_results=headers_results, headers_data=headers_data, $
    headers_xdim1=headers_xdim1, headers_weights=headers_weights, $
    headers_include=headers_include, headers_constants=headers_constants








  return

  
;  HEADERS_INPUT_DATA = h
;  
;  ; Add time to DATE
;  caldat, systime(/julian), month, day, year, hour, minute, second
;  datetime = {CDS_EXT_TIME, $
;    year:year, $
;    month:month, $
;    day:day, $
;    hour:hour, $
;    minute:minute, $
;    second:second, $
;    millisecond:0}
;  datetime = anytim(datetime, /ccsds)
;
;  data_id = 'data_id'
;  extension_names = data_id + [ $
;    ' results', $
;    ' data', $
;    ' xdim1', $
;    ' weights', $
;    ' includes', $
;    ' constants']
;    
;    XDIM1_TYPE = 'WAVE'
;    
;    CONST = d
;
;  
;    wcs = fitshead2wcs(HEADERS_INPUT_DATA)
;    help,wcs
;    print,wcs.proj_names
;    print,wcs.proj_values
;    help,wcs.time
;    help,wcs.position
;    help,wcs.spectrum
;;    help,wcs.distortion
;;    help,wcs.distortion.dw1
;;    print,wcs.distortion.dw1.param
;;    print,wcs.distortion.dw1.value
;;    help,wcs.distortion.dw2
;;    print,wcs.distortion.dw2.param
;;    print,wcs.distortion.dw2.value
;;    print,wcs.distortion.associate
;;    print,wcs.distortion.apply
;  
;  
;  ;print,HEADERS_INPUT_DATA
;  
;  
;  wcs = ana_wcs_get_transform(XDIM1_TYPE, HEADERS_INPUT_DATA)
;
;  
;  
;  hdr_const = ana2fitshdr_const(DATETIME=DATETIME, EXTENSION_NAMES=EXTENSION_NAMES, CONST=CONST, WCS=WCS)
;    
;    
;  print, hdr_const
;  
;  ;const = !NULL
  
  print, ''
  print, ''
  print, '-----------------'
  print, ' ana2fitshdr   TEST'
  print ,''
  
  ol2 = spice_data(file)
  ana = ol2->mk_analysis(0, /init_all_cubes)
  
  handle_value,ana.history_h,history
  handle_value,ana.lambda_h,xdim1
  handle_value,ana.data_h,input_data
  handle_value,ana.weights_h,weights
  handle_value,ana.fit_h,fit
  handle_value,ana.result_h,result
  handle_value,ana.residual_h,residual
  handle_value,ana.include_h,include
  handle_value,ana.const_h,const
  handle_value,ana.origin_h,origin
  handle_value,ana.scale_h,scale
  handle_value,ana.phys_scale_h,phys_scale
  handle_value,ana.dimnames_h,dimnames
  filename_ana = ana.filename
  datasource = ana.datasource
  definition = ana.definition
  missing = ana.missing
  label = ana.label

  FILENAME_OUT = 'file.fits'
  N_WINDOWS = 1
  WINNO = 0
  DATA_ID = 'data_id'
  TYPE_XDIM1 = 'WAVE'
  IS_EXTENSION = 0
  LEVEL = 'L3'
  VERSION = 3
  HEADER_INPUT_DATA = h
  PROGENITOR_DATA = d
  ;RESULT = d
  ;FIT = {a:0}
  ;PROC_STEPS
  ;PROJ_KEYWORDS
  
  ;INPUT_DATA = d
  ;XDIM1 = d
  CONST[0] = 3
  INCLUDE[0] = 3
  WEIGHTS[0] = 3
  
  SAVE_XDIM1 = 0
  NO_SAVE_DATA = 0
  PRINT_HEADERS = 1

  hdrs = ana2fitshdr( FILENAME_OUT=FILENAME_OUT, $
  N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
  DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
  IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
  PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
  XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
  RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
  CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
  DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
  PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
  SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
  DATA_ARRAY=DATA_ARRAY)
  
  print,''
  print,'  without ANA '
  help,hdrs
  help,data_array
  return
  

  SAVE_XDIM1 = 1
  NO_SAVE_DATA = 1
  PRINT_HEADERS = 1

  hdrs = ana2fitshdr(ana,  FILENAME_OUT=FILENAME_OUT, $
    N_WINDOWS=N_WINDOWS, WINNO=WINNO, $
    DATA_ID=DATA_ID, TYPE_XDIM1=TYPE_XDIM1, $
    IS_EXTENSION=IS_EXTENSION, LEVEL=LEVEL, VERSION=VERSION, $
    PROC_STEPS=PROC_STEPS, PROJ_KEYWORDS=PROJ_KEYWORDS, $
    XDIM1=XDIM1, INPUT_DATA=INPUT_DATA, FIT=FIT, $
    RESULT=RESULT, RESIDUAL=RESIDUAL, WEIGHTS=WEIGHTS, INCLUDE=INCLUDE, $
    CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
    DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL, HISTORY=HISTORY, $
    PROGENITOR_DATA=PROGENITOR_DATA, HEADER_INPUT_DATA=HEADER_INPUT_DATA, $
    SAVE_XDIM1=SAVE_XDIM1, NO_SAVE_DATA=NO_SAVE_DATA, PRINT_HEADERS=PRINT_HEADERS, $
    DATA_ARRAY=DATA_ARRAY)


  print,''
  print,'  WITH ANA '
  help,hdrs
  help,data_array

  return
  
  
  
  
  mkhdr, hdr, indgen(3,3)
  ;result = ana2fitshdr_wcshdr(HDR, HEADERS_INPUT_DATA, XDIM1_TYPE='WAVE')

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
  
  print,''
  print,'OLD'
  print,wcs.pc
  new_pc = wcs.pc
  new_pc[0,*] = wcs.pc[2,*]
  new_pc[1:2,*] = wcs.pc[0:1,*]
  new_pc[3,*] = wcs.pc[3,*]

  print,''
  print,'NEW'
  print,new_pc
  new_new_pc = new_pc
  new_new_pc[*,0] = new_pc[*,2]
  new_new_pc[*,1:2] = new_pc[*,0:1]
  new_new_pc[*,3] = new_pc[*,3]

  print,''
  print,'NEW NEW'
  print,new_new_pc
  
  wcs.pc = new_new_pc
  
  new_hdr = wcs2fitshead(wcs)
  help,new_hdr
  print,new_hdr
  
  
  a = indgen(5)
  print, a
  print,''
  print, ana_wcs_transform_vector(a, 3, 0, 5)
  print,''
  print, ana_wcs_transform_vector(a, 3, 1, 5)
  print,''
  print, ana_wcs_transform_vector(a, 0, 3, 5)
  print,''
  print, ana_wcs_transform_vector(a, 0, 0, 5)
  print,''
  print,''

  a = indgen(4,4)
  print, a
  print,''
  print, ana_wcs_transform_array(a, 2, 0, 4)
  print,''
  print, ana_wcs_transform_array(a, 2, 1, 4)
  print,''
  print, ana_wcs_transform_array(a, 0, 2, 4)
  print,''
  print, ana_wcs_transform_array(a, 0, 0, 4)
  print,''

end


;PC1_1   =       0.997119168647 / Non-default value due to CROTA degrees S/C roll
;PC1_2   =     -0.0208210801654 / Contribution of dim 2 to coord 1 due to roll
;PC2_1   =       0.276323969269 / Contribution of dim 1 to coord 2 due to roll
;PC2_2   =       0.997119168647 / Non-default value due to CROTA degrees S/C roll
;PC3_3   =              1.00000 / Default value, no rotation
;PC4_4   =              1.00000 / Default value, no rotation
;PC4_1   =       -5.30000305176 / Contribution of dimension 1 to coordinate 4
