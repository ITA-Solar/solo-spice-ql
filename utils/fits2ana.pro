;+
; NAME:
;     FITS2ANA
;
; PURPOSE:
;     fits2ana reads a FITS file and returns ana structure(s). The input FITS file must contain all the necessary
;     extensions and keywords. ANA2FITS creates the correct FITS files. An ana structure is used by e.g. xcfit_block,
;     it is created by e.g. mk_analysis.
;
; CATEGORY:
;     FITS -- utility
;
; CALLING SEQUENCE:
;     anas = fits2ana(fitsfile [ ,titles=titles])
;
; INPUTS:
;     fitsfile : name and path to a FITS file (e.g. SPICE level 3 file)
;
; OUTPUT:
;     Array of ana structure, number of elements is the same as number of windows in the FITS file.
;
; OPTIONAL OUTPUT:
;     titles: returns the keyword EXTNAML2 for each window in a string array
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; HISTORY:
;     23-Nov-2021: Martin Wiesmann
;-
; $Id: 2022-06-24 14:09 CEST $


function fits2ana, fitsfile, titles=titles

  if N_ELEMENTS(fitsfile) eq 0 then fitsfile = '/Users/mawiesma/data/spice/level3/2020/11/19/solo_L3_spice-n-sit_20201119T102559_V03_33554593-000.fits'

  result = readfits(fitsfile, hdr)
  n_windows = fxpar(hdr, 'NWIN', missing=0)
  titles = strarr(n_windows)
  for iwin=0,n_windows-1 do begin
    extension = iwin*7
    if iwin gt 0 then result = readfits(fitsfile, hdr, ext=extension)

    ;extract info from header
    filename = strtrim(fxpar(hdr, 'ANA_FILE', missing=''), 2)
    datasource = strtrim(fxpar(hdr, 'ANA_SRC', missing=''), 2)
    definition = strtrim(fxpar(hdr, 'ANA_DEF', missing=''), 2)
    missing = fxpar(hdr, 'ANA_MISS', missing=0)
    label = strtrim(fxpar(hdr, 'ANA_LABL', missing=''), 2)
    history = fxpar(hdr, 'ANA_HIST', missing='')
    history = strtrim(strsplit(history,';',/extract,count=count), 2)
    titles[iwin] = strtrim(fxpar(hdr, 'EXTNAML2', missing=''), 2)

    ; extract fit components
    tag_names = hash()
    fit_components_hash = orderedhash()
    n_components = fxpar(hdr, 'ANA_NCOMP', missing=0)
    for icomp=0,n_components-1 do begin
      ; Get keywords for each fit component
      fitnr = strtrim(string(icomp+1), 2)
      n_params = fxpar(hdr, 'CMP_NP'+fitnr, missing=0)
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
        param.name = strtrim(fxpar(hdr, 'PNAME'+fitnr+parnr, missing=''), 2)
        param.description[*] = ''
        description = fxpar(hdr, 'PDESC'+fitnr+parnr, missing='')
        param.description = strtrim(strsplit(description,';',/extract,count=count), 2)
        param.initial = fxpar(hdr, 'PINIT'+fitnr+parnr, missing=0)
        param.value = fxpar(hdr, 'PVAL'+fitnr+parnr, missing=0)
        param.max_val = fxpar(hdr, 'PMAX'+fitnr+parnr, missing=0)
        param.min_val = fxpar(hdr, 'PMIN'+fitnr+parnr, missing=0)
        param.trans_a = fxpar(hdr, 'PTRNA'+fitnr+parnr, missing=0)
        param.trans_b = fxpar(hdr, 'PTRNB'+fitnr+parnr, missing=0)
        param.const = fxpar(hdr, 'PCONS'+fitnr+parnr, missing=0)
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
