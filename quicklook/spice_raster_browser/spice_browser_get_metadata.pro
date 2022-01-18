;+
; NAME:
;     spice_browser_get_metadata
;
; PURPOSE:
;     Used internally in spice_raster_browser.
;     This extracts various bits of metadata from the data object.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_browser_get_metadata, data
;
; INPUTS:
;     input:  XXX
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     None.
;
; OUTPUT:
;     XXX
;
; EXAMPLE:
;
; INTERNAL ROUTINES:
;
; PROGRAMMING NOTES:
;     Assuming that x-, y- and time-values are the same for all windows !!!
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-
; $Id: 25.04.2020 21:57 CEST $


FUNCTION spice_browser_get_metadata, data

  xpos = data->get_instr_x_vector(0)
  ypos = data->get_instr_y_vector(0)

  nx = n_elements(xpos)
  ny = n_elements(ypos)

  scale = data->get_resolution(0)
  scale = scale[0:1]

  ; Coordinates of bottom-left of raster
  origin = [min(xpos),min(ypos)]

  ;
  ; sit_stare is 1 if it's a sit-and-stare observation and 0 otherwise.
  sit_stare = data->get_sit_and_stare()

  ;
  ; Gets the mid-point time of each exposure in '12:00:00' format. This
  ; is stored in wid_data.
  ;
  ; The mid-time is the time shown on the GUI by "Time:"
  ;
  mid_ti = data->get_time_vector(0)
  t_tai = anytim2tai(data->get_start_time()) + mid_ti
  utc = anytim2utc(t_tai)
  midtime=anytim2utc(t_tai,/ccsds,/time_only,/trunc)
  tmid_min=(t_tai-t_tai[0])/60.

  ;
  ; Modify things if it's a sit-and-stare observation
  ;  - for the scale I set it so that a square image corresponds to
  ;    about 60" in Y and 20 mins in time (hence factor
  ;    3=60/20). I may need to adjust based on experience with
  ;    sit-and-stare.
  ; 2019-12-10, Martin Wiesmann, disabled scaling for the moment
  IF sit_stare EQ 1 THEN BEGIN
    ;scale[0]=3.*(tmid_min[1]-tmid_min[0])
    scale[*] = 1
    origin[0]=0.0
  ENDIF

  ;
  ; Get raster direction by using the getdx value.
  ;   rast_direct=1  -> left-to-right (east-to-west)
  ;   rast_direct=0  -> right-to-left (west-to-east)
  ;
  dx = data->get_resolution(0, /x)
  IF dx[0] GE 0 THEN rast_direct=1 ELSE rast_direct=0

  IF sit_stare EQ 1 THEN BEGIN
    rast_direct=1
    obs_type='Sit-n-stare'
  ENDIF ELSE BEGIN
    obs_type='Raster'
    IF rast_direct EQ 1 THEN obs_type='Raster (L-to-R)' ELSE obs_type='Raster (R-to-L)'
  ENDELSE

  IF rast_direct EQ 0 THEN midtime=reverse(midtime)

  ;
  ; Get xcen and ycen
  ;
  xcen = data->get_header_info('CRVAL1', 0)
  ycen = data->get_header_info('CRVAL2', 0)

  ;
  ; Exposure times can vary between NUV and FUV, and also over time. The
  ; one constant is cadence with seems to be stored in the FITS keyword
  ; STEPT_AV.
  ;
  cadence=data->get_exposure_time(0)

  outstr={ origin: origin, $
    scale: scale, $
    midtime: midtime, $
    sit_stare: sit_stare, $
    xpos: xpos, $
    ypos: ypos, $
    tmid_min: tmid_min, $
    utc: utc, $
    stud_acr: data->get_header_info('SPIOBSID', 0), $
    date_obs: data->get_header_info('DATE-OBS', 0, ''), $
    l1p5_ver: 'NA', $
    obs_type: obs_type, $
    cadence: cadence, $
    xcen: xcen, $
    ycen: ycen, $
    rast_direct: rast_direct }

  return,outstr

END
