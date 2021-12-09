function fits2ana, fitsfile
fitsfile = '/Users/mawiesma/data/spice/level3/2020/11/19/solo_L3_spice-n-sit_20201119T102559_V03_33554593-000.fits'
  result = readfits(fitsfile, hdr)

  ;extract info from header
  filename = fxpar(hdr, 'ANAFILE', missing='')
  datasource = fxpar(hdr, 'ANADSRC', missing='')
  definition = fxpar(hdr, 'ANADEF', missing='')
  missing = fxpar(hdr, 'ANAMISS', missing=0)
  label = fxpar(hdr, 'ANALABEL', missing='')
  history = fxpar(hdr, 'ANAHISTO', missing='')
  history = strtrim(strsplit(history,';',/extract,count=count), 2)

  ; extract fit components
  tag_names = hash()
  fit_components_hash = orderedhash()
  n_components = fxpar(hdr, 'COMPCNT', missing=0)
  for icomp=0,n_components-1 do begin
    ; Get keywords for each fit component
    fitnr = fns('##', icomp)
    n_params = fxpar(hdr, 'PARCNT'+fitnr, missing=0)
    stc = mk_component_stc(n_params)
    stc.FUNC_NAME = fxpar(hdr, 'CMPTYP'+fitnr, missing='')
    stc.NAME = fxpar(hdr, 'CMPNAM'+fitnr, missing='')
    stc.FUNC_STRING = fxpar(hdr, 'CMPSTR'+fitnr, missing='')
    stc.description[*] = ''
    description = fxpar(hdr, 'CMPDES'+fitnr, missing='')
    stc.description = strtrim(strsplit(description,';',/extract,count=count), 2)
    stc.MULTIPLICATIVE = fxpar(hdr, 'CMPMUL'+fitnr, missing=0)
    stc.INCLUDE = fxpar(hdr, 'CMPINC'+fitnr, missing=0)

    for ipar=0,n_params-1 do begin
      ; Get keywords for each fit parameter
      parnr = string(byte(ipar+97))
      param = stc.param[ipar]
      param.name = fxpar(hdr, 'PRNAM'+fitnr+parnr, missing='')
      param.description[*] = ''
      description = fxpar(hdr, 'PRDES'+fitnr+parnr, missing='')
      param.description = strtrim(strsplit(description,';',/extract,count=count), 2)
      param.initial = fxpar(hdr, 'PRINI'+fitnr+parnr, missing='')
      param.value = fxpar(hdr, 'PRVAL'+fitnr+parnr, missing='')
      param.max_val = fxpar(hdr, 'PRMAX'+fitnr+parnr, missing='')
      param.min_val = fxpar(hdr, 'PRMIN'+fitnr+parnr, missing='')
      param.trans_a = fxpar(hdr, 'PRTRA'+fitnr+parnr, missing='')
      param.trans_b = fxpar(hdr, 'PRTRB'+fitnr+parnr, missing='')
      param.const = fxpar(hdr, 'PRCON'+fitnr+parnr, missing='')
      stc.param[ipar] = param
    endfor ; ipar=0,n_params-1

    ;create tag name
    case stc.FUNC_STRING of
      'g': tag_name = 'igauss'
      'b': tag_name = 'bgauss'
      'v': tag_name = 'voigt'
      'p0': tag_name = 'bg'
      else: tag_name = stc.FUNC_STRING + '_'
    endcase
    if tag_names.HasKey(tag_name) then begin
      tag_names[tag_name] = tag_names[tag_name]+1
    endif else begin
      tag_names[tag_name] = 1
    endelse
    if tag_name NE 'bg' || tag_names[tag_name] gt 1 then begin
      tag_name = tag_name + strtrim(string(tag_names[tag_name]), 2)
    endif
    fit_components_hash[tag_name] = stc

  endfor ; icomp=0,component_n-1

  fit = fit_components_hash.tostruct(/no_copy)

  data = readfits(fitsfile, hdr, ext=1)
  lambda = readfits(fitsfile, hdr, ext=2)
  residual = readfits(fitsfile, hdr, ext=3)
  weights = readfits(fitsfile, hdr, ext=4)
  include = readfits(fitsfile, hdr, ext=5)
  const = readfits(fitsfile, hdr, ext=6)

  IF NOT exist(ana) THEN ana = mk_analysis()

  ana.filename = filename
  ana.datasource = datasource
  ana.definition = definition
  ana.missing = missing
  ana.label = label

  handle_value,ana.history_h,history,/no_copy,/set
  handle_value,ana.lambda_h,lambda,/no_copy,/set
  handle_value,ana.data_h,data,/no_copy,/set
  handle_value,ana.weights_h,weights,/no_copy,/set
  handle_value,ana.fit_h,fit,/no_copy,/set
  handle_value,ana.result_h,result,/no_copy,/set
  handle_value,ana.residual_h,residual,/no_copy,/set
  handle_value,ana.include_h,include,/no_copy,/set
  handle_value,ana.const_h,const,/no_copy,/set
  handle_value,ana.origin_h,origin,/no_copy,/set
  handle_value,ana.scale_h,scale,/no_copy,/set
  handle_value,ana.phys_scale_h,phys_scale,/no_copy,/set
  handle_value,ana.dimnames_h,dimnames,/no_copy,/set

  return, ana
end