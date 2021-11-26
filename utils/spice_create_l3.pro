;+
; NAME:
;     SPICE_CREATE_L3
;
; PURPOSE:
;     spice_create_l3 creates a SPICE level 3 file from a level 2 file
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     The SPICE_CREATE_L3 procedure is
;                 spice_create_l3
;
; INPUTS:
;     file : path of a SPICE FITS file.
;
; OUTPUT:
;     Level 3 files
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
; $Id: 2021-11-26 14:30 CET $


pro spice_create_l3, spice_object, window_index
  COMPILE_OPT IDL2

  if typename(spice_object) NE 'SPICE_DATA' then begin
    print, 'Need a spice_object as input'
    return
  endif
  self = spice_object
  if N_ELEMENTS(window_index) eq 0 then window_index = indgen(self->get_number_windows())

  for iwindow=0,N_ELEMENTS(window_index)-1 do begin

    if ~self->get_number_exposures(window_index[iwindow]) then begin
      print, 'single exposure data, do not start xcfit_block'
      return
    endif

    data = self->get_window_data(window_index[iwindow], /load)
    ind = where(data ne data, count)
    print, 'data ne data', count
    if count gt 0 then data[ind] = -1000.0
    lambda = self->get_wcs_coord(window_index[iwindow], /lambda)

    size_data = size(data)
    if self->get_sit_and_stare() then begin
      print, 'sit_and_stare'
      lambda = transpose(lambda, [2, 0, 1, 3])
      data = transpose(data, [2, 0, 1, 3])
      weights = make_array(size_data[3], size_data[1], size_data[2], size_data[4], value=1.0)
    endif else begin
      print, 'not sit_and_stare'
      lambda = reform(lambda)
      lambda = transpose(lambda, [2, 0, 1])
      data = transpose(data, [2, 0, 1])
      weights = make_array(size_data[3], size_data[1], size_data[2], value=1.0)
    endelse
    type_data = size(data, /type)
    lambda = fix(lambda, type=type_data)
    miss = self->get_missing_value()
    miss = -1000.0d

    print, 'before'
    help, LAMbda, DAta, WeighTS, FIT, MISS, RESULT, RESIDual, INCLUDE, CONST

    adef = generate_adef(data, LAMbda)
    print,''
    print,'adef'
    help,adef

    ;ana = mk_analysis(LAMbda, DAta, WeighTS, FIT, MISS, RESULT, RESIDual, INCLUDE, CONST)
    ana = mk_analysis(LAMbda, DAta, WeighTS, adef, MISS, RESULT, RESIDual, INCLUDE, CONST)

    help,ana
    handle_value,ana.fit_h,fit
    help,fit
    ;stop

    print, ''
    print,'fitting window ' + strtrim(string(iwindow+1), 2) + ' of ' + strtrim(string(N_ELEMENTS(window_index)), 2)
    print, 'this may take a while'
    print, ''
    window,0
    handle_value,ana.result_h,result
    help,result
    ;stop
    ;pih, result[0,*,*]
    cfit_block, analysis=ana, quiet=quiet, /double, /x_face
    handle_value,ana.result_h,result
    help,result
    pih, result[0,0,*,*]

    if iwindow gt 0 then extension=1 else extension=0
    ;headers = ana2fitshdr(ana, extension=extension)
    ;help,headers

    h = spice_ana2fitshdr_results(ana, header_l2=self->get_header(window_index[iwindow], /string), $
      window_index=window_index[iwindow], extension=extension, filename_l3=filename_l3)
    help, h
    help, filename_l3
    ;return

    stop


  endfor ; iwindow=0,N_ELEMENTS(window_index)-1

end
