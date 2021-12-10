function fits2ana, fitsfile

  if N_ELEMENTS(fitsfile) eq 0 then fitsfile = '/Users/mawiesma/data/spice/level3/2020/11/19/solo_L3_spice-n-sit_20201119T102559_V03_33554593-000.fits'

  result = readfits(fitsfile, hdr)
  n_windows = fxpar(hdr, 'NWIN', missing=0)
  for iwin=0,n_windows-1 do begin
    extension = iwin*7
    if iwin gt 0 then result = readfits(fitsfile, hdr, ext=extension)

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
      stc.FUNC_NAME = strtrim(fxpar(hdr, 'CMPTYP'+fitnr, missing=''), 2)
      stc.NAME = strtrim(fxpar(hdr, 'CMPNAM'+fitnr, missing=''), 2)
      stc.FUNC_STRING = strtrim(fxpar(hdr, 'CMPSTR'+fitnr, missing=''), 2)
      stc.description[*] = ''
      description = fxpar(hdr, 'CMPDES'+fitnr, missing='')
      stc.description = strtrim(strsplit(description,';',/extract,count=count), 2)
      stc.MULTIPLICATIVE = fxpar(hdr, 'CMPMUL'+fitnr, missing=0)
      stc.INCLUDE = fxpar(hdr, 'CMPINC'+fitnr, missing=0)

      for ipar=0,n_params-1 do begin
        ; Get keywords for each fit parameter
        parnr = string(byte(ipar+97))
        param = stc.param[ipar]
        param.name = strtrim(fxpar(hdr, 'PRNAM'+fitnr+parnr, missing=''), 2)
        param.description[*] = ''
        description = fxpar(hdr, 'PRDES'+fitnr+parnr, missing='')
        param.description = strtrim(strsplit(description,';',/extract,count=count), 2)
        param.initial = fxpar(hdr, 'PRINI'+fitnr+parnr, missing=0)
        param.value = fxpar(hdr, 'PRVAL'+fitnr+parnr, missing=0)
        param.max_val = fxpar(hdr, 'PRMAX'+fitnr+parnr, missing=0)
        param.min_val = fxpar(hdr, 'PRMIN'+fitnr+parnr, missing=0)
        param.trans_a = fxpar(hdr, 'PRTRA'+fitnr+parnr, missing=0)
        param.trans_b = fxpar(hdr, 'PRTRB'+fitnr+parnr, missing=0)
        param.const = fxpar(hdr, 'PRCON'+fitnr+parnr, missing=0)
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

    data = readfits(fitsfile, hdr, ext=extension+1)
    lambda = readfits(fitsfile, hdr, ext=extension+2)
    residual = readfits(fitsfile, hdr, ext=extension+3)
    weights = readfits(fitsfile, hdr, ext=extension+4)
    include = readfits(fitsfile, hdr, ext=extension+5)
    const = readfits(fitsfile, hdr, ext=extension+6)

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

    if iwin eq 0 then ana_all = ana $
    else ana_all = [ana_all, ana]

  endfor ; iwin=0,n_windows-1

  return, ana_all
end
