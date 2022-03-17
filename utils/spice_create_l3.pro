;+
; NAME:
;     SPICE_CREATE_L3
;
; PURPOSE:
;     spice_create_l3 creates a SPICE level 3 file from a level 2 file. The level 3 file is saved as a
;     FITS file and moved into the directory $SPICE_DATA/level3/ .
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     spice_create_l3, spice_object, window_index
;
; INPUTS:
;     spice_object : a SPICE_DATA object.
;
; OPTIONAL INPUTS:
;     window_index : One or more window indices of windows that should be included in level 3 file.
;                    If not provided, all windows will be included.
;
; KEYWORD PARAMETERS:
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;     no_fitting: If set, fitting won't be computed. This can still be done manually in xcfit_block.
;     no_widget: If set, xcfit_block will not be called
;
; OUTPUT:
;     Level 3 file, as FITS file, saved to directory $SPICE_DATA/level3/ .
;
; CALLS:
;     generate_adef, mk_analysis, cfit_block, SPICE_XCFIT_BLOCK, spice_ana2fitshdr, writefits, spice_ingest
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
; $Id: 2022-03-17 13:37 CET $


pro spice_create_l3, spice_object, window_index, approximated_slit=approximated_slit, no_fitting=no_fitting, $
  no_widget=no_widget
  COMPILE_OPT IDL2

  if typename(spice_object) NE 'SPICE_DATA' then begin
    print, 'Need a spice_object as input'
    return
  endif

  if N_ELEMENTS(window_index) eq 0 then window_index = indgen(spice_object->get_number_windows())

  for iwindow=0,N_ELEMENTS(window_index)-1 do begin

    ana = spice_object->xcfit_block(window_index[iwindow], approximated_slit=approximated_slit, no_fitting=no_fitting, $
      no_widget=no_widget)
    if size(ana, /type) NE 8 then continue

    if iwindow gt 0 then extension=1 else extension=0

    headers = spice_ana2fitshdr(ana, header_l2=spice_object->get_header(window_index[iwindow], /string), $
      extension=extension, filename_l3=filename_l3, n_windows=N_ELEMENTS(window_index), $
      HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
      FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
      CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
      DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)

    help, headers
    help, filename_l3
    help,result
    help,INPUT_DATA
    help,lambda
    help,residual
    help,weights
    help,include
    help,const
    help,fit

    if iwindow eq 0 then begin
      file = filepath(filename_l3, /tmp)
      help,file
      ;stop
    endif

    writefits, file, RESULT, *headers[0], append=extension
    writefits, file, INPUT_DATA, *headers[1], /append
    writefits, file, LAMBDA, *headers[2], /append
    writefits, file, RESIDUAL, *headers[3], /append
    writefits, file, WEIGHTS, *headers[4], /append
    writefits, file, INCLUDE, *headers[5], /append
    writefits, file, CONST, *headers[6], /append

    d= readfits(file,h)

  endfor ; iwindow=0,N_ELEMENTS(window_index)-1

  spice_ingest, file, destination=destination, file_moved=file_moved, files_found=files_found, /force
  print,files_found
  print,file_moved
  print,destination

end
