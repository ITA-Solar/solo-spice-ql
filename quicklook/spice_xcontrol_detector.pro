;+
; NAME:
;       SPICE_XCONTROL_DETECTOR
;
; PURPOSE:
;       SPICE_XCONTROL_DETECTOR is a helper function for SPICE_XCONTROL. It returns
;       the detector views of the two detectors to be displayed. Size of output can
;       be given in either direction or in both, the direction which is not given is
;       calculated so that the aspect ratio of detector is the same as the original,
;       except if both directions are given. If no size is given, the images are 
;       returned in the original detector size.
;
; CATEGORY:
;       Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;       spice_xcontrol_detector, data [, detector2=detector2, xsize=xsize, ysize=ysize]
;
; INPUTS:
;       data: data object of type 'spice_data'
;
; OPTIONAL INPUTS:
;       xsize: the size the resulting images should have in x-direction. If not provided
;              this keyword returns the resulting size in x-direction, which is calculated with
;              size in y-direction and detector aspect ratio, or as original detector size.
;       xsize: the size the resulting images should have in y-direction. If not provided
;              this keyword returns the resulting size in y-direction, which is calculated with
;              size in x-direction and detector aspect ratio, or as original detector size.
;
; OPTIONAL OUTPUTs:
;       detector2: 2-D array containing the image of the second detector, with the windows
;              plotted in it
;
; OUTPUTS:
;       2-D array containing the image of the first detector, with the windows plotted in it
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;     17-Nov-2020: Martin Wiesmann, First version
;
; $Id: 2024-08-27 13:41 CEST $
;-
;


FUNCTION spice_xcontrol_detector, data, detector2=detector2, xsize=xsize, ysize=ysize

  nwin = data->get_number_windows()
  ccd_size = data->get_ccd_size()
  win_positions = intarr(nwin,4)
  clip_image = intarr(nwin,4)
  detector_nr = intarr(nwin)
  for i=0,nwin-1 do begin
    win_positions[i,*] = data->get_window_position(i, detector=detectornr, /idl_coord, /debin)
    detector_nr[i] = detectornr
    if detectornr eq 2 then begin
      win_positions[i,0:1] = win_positions[i,0:1] - ccd_size[0]
    endif
  endfor

;  ; data window size in level 2 does not correspond to
;  ; win_positions due to transformations
;  ; -> this 'fitting' should no longer be necessary, since we PXPOS
;  ;    to find correct position on detector for level 2
;  if data->get_level() eq 2 then begin
;    for i=0,nwin-1 do begin
;      sizey = data->get_header_keyword('NAXIS2', i) * data->get_spatial_binning(i)
;      dy = sizey - (win_positions[i,3]-win_positions[i,2]+1)
;      if dy ne 0 then begin
;        dy1 = fix(dy/2.0)
;        dy2 = dy-dy1
;        win_positions[i,2] = win_positions[i,2] - dy1
;        if win_positions[i,2] lt 0 then begin
;          clip_image[i,2] = -1 * win_positions[i,2]
;          win_positions[i,2] = 0
;        endif
;        win_positions[i,3] = win_positions[i,3] + dy2
;        if win_positions[i,3] ge ccd_size[1] then begin
;          clip_image[i,3] = win_positions[i,2] - (ccd_size[1]-1)
;          win_positions[i,3] = ccd_size[1]-1
;        endif
;      endif
;      sizel = data->get_header_keyword('NAXIS3', i) * data->get_spectral_binning(i)
;      dl = sizel - (win_positions[i,1]-win_positions[i,0]+1)
;      if dl ne 0 then begin
;        dl1 = fix(dl/2.0)
;        dl2 = dl-dl1
;        win_positions[i,0] = win_positions[i,0] - dl1
;        if win_positions[i,0] lt 0 then begin
;          clip_image[i,0] = -1 * win_positions[i,0]
;          win_positions[i,0] = 0
;        endif
;        win_positions[i,1] = win_positions[i,1] + dl2
;        if win_positions[i,1] ge ccd_size[0] then begin
;          clip_image[i,1] = win_positions[i,1] - (ccd_size[0]-1)
;          win_positions[i,1] = ccd_size[0]-1
;        endif
;      endif
;    endfor
;  endif

  ; initialize size of draw window (ccd display):
  detector = fltarr(ccd_size)
  aspect=float(ccd_size[0])/float(ccd_size[1])
  if N_ELEMENTS(xsize) eq 0 then begin
    if N_ELEMENTS(ysize) eq 0 then begin
      xsize=ccd_size[0]
      ysize=ccd_size[1]
    endif else begin ; N_ELEMENTS(ysize) eq 0
      xsize=fix(ysize*aspect)
    endelse ; N_ELEMENTS(ysize) eq 0
  endif else begin ; N_ELEMENTS(xsize) eq 0
    if N_ELEMENTS(ysize) eq 0 then begin
      ysize=fix(xsize/aspect)
    endif else begin ; N_ELEMENTS(ysize) eq 0
      ; xsize and ysize given
    endelse ; N_ELEMENTS(ysize) eq 0
  endelse

  ;build the initial images
  for idet=1,2 do begin
    detector[*] = !Values.F_NAN
    inddet = where(detector_nr eq idet, count)
    for i=0,count-1 do begin
      ind=inddet[i]
      window_image = data->get_one_image(ind, 0, /debin, /no_masking)
      if data->has_dumbbells(ind) then window_image = rotate(window_image, 5)
      size_image = size(window_image)
      detector[win_positions[ind,0]:win_positions[ind,1], win_positions[ind,2]:win_positions[ind,3]] = $
        window_image[clip_image[ind,0]:size_image[1]-1-clip_image[ind,1], clip_image[ind,2]:size_image[2]-1-clip_image[ind,3]]
    endfor
    case idet of
      1: detector1 = congrid(detector, xsize, ysize)
      2: detector2 = congrid(detector, xsize, ysize)
    endcase
  endfor

  return, detector1
END
