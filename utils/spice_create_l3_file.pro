;+
; NAME:
;     SPICE_CREATE_L3_FILE
;
; PURPOSE:
;     spice_create_l3_file creates a SPICE level 3 file from a level 2 file. The level 3 file is saved as a
;     FITS file and moved into the directory $SPICE_DATA/level3/ .
;
; CATEGORY:
;     Solar Orbiter - SPICE.
;
; CALLING SEQUENCE:
;     spice_create_l3_file, spice_object [, window_index] [, no_masking=no_masking] [, approximated_slit=approximated_slit]
;     [, no_fitting=no_fitting] [, no_widget=no_widget]
;
; INPUTS:
;     spice_object : a SPICE_DATA object.
;
; OPTIONAL INPUTS:
;     window_index : One or more window indices of windows that should be included in level 3 file.
;                    If not provided, all windows will be included.
;     VELOCITY : Set this equal to the initial velocity if you want
;                 the line position represented by the velocity
;                 relative to a lab wavelength - the lab wavelength
;                 is taken from the supplied POSITION, i.e., INT_POS_FWHM(1).
;                 This input is ignored if /POSITION is set.
;                 Default is zero.
;
; KEYWORD PARAMETERS:
;     no_masking: If set, then ::mask_regions_outside_slit will NOT be called on the data.
;                 This procedure masks any y regions in a narrow slit data cube that don't contain
;                 slit data, i.e. pixels with contributions from parts of the
;                 detector that lie above/below the dumbbells,
;                 in the gap between the slit ends and the dumbbells, and the
;                 dumbbell regions themselves. The keyword is ignored for wide-slit
;                 observations or if window_index corresponds to a regular
;                 dumbbell extension.
;     approximated_slit: If set, routine uses a fixed (conservative) value for the slit
;                 range, i.e. does not estimate the slit length based on the position of the dumbbells.
;     no_fitting: If set, fitting won't be computed. This can still be done manually in xcfit_block.
;     no_widget:  If set, xcfit_block will not be called
;     position: If set, then the line position is NOT represented by the velocity
;                 relative to a lab wavelength, but as the wavelength.
;
; OUTPUT:
;     The path and name of the Level 3 FITS file.
;     Level 3 file, as FITS file, saved to directory $SPICE_DATA/level3/ .
;
; CALLS:
;     generate_adef, mk_analysis, cfit_block, XCFIT_BLOCK, spice_ana2fitshdr, writefits, spice_ingest
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
; $Id: 2022-06-14 12:07 CEST $


FUNCTION spice_create_l3_file, spice_object, window_index, no_masking=no_masking, approximated_slit=approximated_slit, $
  no_fitting=no_fitting, no_widget=no_widget, position=position, velocity=velocity
  COMPILE_OPT IDL2

  if typename(spice_object) NE 'SPICE_DATA' then begin
    print, 'Need a spice_object as input'
    return
  endif

  if N_ELEMENTS(window_index) eq 0 then window_index = indgen(spice_object->get_number_windows())

  for iwindow=0,N_ELEMENTS(window_index)-1 do begin

    ana = spice_object->mk_analysis(window_index[iwindow], no_masking=no_masking, approximated_slit=approximated_slit, $
      /init_all_cubes, position=position, velocity=velocity)
    if size(ana, /type) NE 8 then continue
    
    if ~keyword_set(no_fitting) then begin
      print, '====================='
      print, 'fitting data'
      print, 'this may take a while'
      print, '====================='
      cfit_block, analysis=ana, quiet=quiet, /double, /x_face, smart=1
    endif

    if ~keyword_set(no_widget) then begin
      XCFIT_BLOCK, ana=ana
    endif

    if iwindow gt 0 then extension=1 else extension=0
    headers = spice_ana2fitshdr(ana, header_l2=spice_object->get_header(window_index[iwindow]), $
      extension=extension, filename_l3=filename_l3, n_windows=N_ELEMENTS(window_index), $
      HISTORY=HISTORY, LAMBDA=LAMBDA, INPUT_DATA=INPUT_DATA, WEIGHTS=WEIGHTS, $
      FIT=FIT, RESULT=RESULT, RESIDUAL=RESIDUAL, INCLUDE=INCLUDE, $
      CONST=CONST, FILENAME_ANA=FILENAME_ANA, DATASOURCE=DATASOURCE, $
      DEFINITION=DEFINITION, MISSING=MISSING, LABEL=LABEL)

    if iwindow eq 0 then file = filepath(filename_l3, /tmp)
    writefits, file, RESULT, *headers[0], append=extension
    writefits, file, INPUT_DATA, *headers[1], /append
    writefits, file, LAMBDA, *headers[2], /append
    writefits, file, RESIDUAL, *headers[3], /append
    writefits, file, WEIGHTS, *headers[4], /append
    writefits, file, INCLUDE, *headers[5], /append
    writefits, file, CONST, *headers[6], /append

  endfor ; iwindow=0,N_ELEMENTS(window_index)-1

  spice_ingest, file, destination=destination, file_moved=file_moved, files_found=files_found, /force
  print, 'Level 3 file saved to: ', destination
  return, destination

END
