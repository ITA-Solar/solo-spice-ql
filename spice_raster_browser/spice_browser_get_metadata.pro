;+
; NAME:
;     spice_browser_get_metadata
;
; PURPOSE:
;     XXX
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
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


FUNCTION spice_browser_get_metadata, data
  ;
  ; This extracts various bits of metadata from the data object
  ;
  xpos=data->getxpos()
  ypos=data->getypos()

  ;
  ; Pick out critical roll angles (allowing 5 degree uncertainty)
  ;
  roll=data->getinfo('SAT_ROT')
  CASE 1 OF
    roll GE 85: rswtch=1
    roll LE -85: rswtch=-1
    ELSE: rswtch=0
  ENDCASE

  ;
  ; For roll angles -90 and +90, xpos and ypos get swapped by the object
  ; software. I am always going to display the data with the scan
  ; direction on the X-axis, so I need to swap xpos and ypos again. By
  ; comparing with AIA images, I find that I need to reverse ypos for
  ; -90 roll.
  ;
  ; Note: an example of a +90 data-set is 9-Jul-2014 08:00.
  ;       an example of a -90 data-set is 25-Feb-2014 00:05.
  ;       an example of a +45 data-set is 6-Sep-2014 11:24.
  ;
  IF n_elements(xpos) NE data->getinfo('NEXP') THEN BEGIN
    xtemp=xpos
    xpos=ypos
    ypos=xtemp
    ;
    IF rswtch EQ -1 THEN ypos=reverse(ypos)
    IF rswtch EQ 1 THEN BEGIN
      ypos=reverse(ypos)
      xpos=reverse(xpos)
    ENDIF
  ENDIF

  nx=n_elements(xpos)
  ny=n_elements(ypos)

  ;
  ; This is a bit of kluge for getting the pixel scale, but I
  ; couldn't find the object method to get this .
  ;
  xscale=median(xpos[1:nx-1]-xpos[0:nx-2])
  yscale=median(ypos[1:ny-1]-ypos[0:ny-2])
  scale=[xscale,yscale]

  ; Coordinates of bottom-left of raster
  origin=[min(xpos),min(ypos)]

  ;
  ; sit_stare is 1 if it's a sit-and-stare observation and 0 otherwise.
  ; 2-Nov-2016, PRY: The getsit_and_stare method has broken so I just
  ; check STEPS_AV now.
  ;
  steps_av=data->getinfo('STEPS_AV')
  sit_stare = steps_av EQ 0.
  ;sit_stare=data->getsit_AND_stare()

  ;
  ; Gets the mid-point time of each exposure in '12:00:00' format. This
  ; is stored in wid_data.
  ;
  ; The mid-time is the time shown on the GUI by "Time:"
  ;
  ; Since the FUV channel can have a different exposure time to the NUV
  ; channel, then I specifically request the times for FUV.
  ;
  iwin=data->getwindx(1402)
  IF iwin[0] EQ -1 THEN iwin=0 ELSE iwin=iwin[0]
  ;
  ti1=data->getti_1()
  ti2=data->getti_2()
  ti1=data->sec_from_obs_start(ti1)
  ti2=data->sec_from_obs_start(ti2)
  mid_ti=(ti2-ti1)/2.+ti1
  t_tai=data->ti2tai(mid_ti)
  utc=data->ti2utc(mid_ti)
  midtime=anytim2utc(t_tai,/ccsds,/time_only,/trunc)
  tmid_min=(t_tai-t_tai[0])/60.



  ;
  ; Modify things if it's a sit-and-stare observation
  ;  - for the scale I set it so that a square image corresponds to
  ;    about 60" in Y and 20 mins in time (hence factor
  ;    3=60/20). I may need to adjust based on experience with
  ;    sit-and-stare.
  ;
  IF sit_stare EQ 1 THEN BEGIN
    scale[0]=3.*(tmid_min[1]-tmid_min[0])
    origin[0]=0.0
  ENDIF


  ;
  ; Get raster direction by using the getdx value.
  ;   rast_direct=1  -> left-to-right (east-to-west)
  ;   rast_direct=0  -> right-to-left (west-to-east)
  ;
  dx=data->getdx()
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
  ; This gets the level 1.5 version (i.e., the version of the iris_prep
  ; used to derive the 1.5 file.
  ;
  history=data->getinfo('HISTORY')
  chck=strpos(history,'VERSION:')
  k=where(chck GE 0,nk)
  IF nk GT 0 THEN BEGIN
    i=k[0]
    str=history[i]
    l1p5_ver=trim(strmid(str,chck[k[0]]+8))
  ENDIF

  ;
  ; Get xcen and ycen
  ;
  xcen=data->getxcen()
  ycen=data->getycen()

  ;
  ; Exposure times can vary between NUV and FUV, and also over time. The
  ; one constant is cadence with seems to be stored in the FITS keyword
  ; STEPT_AV.
  ;
  cadence=data->getinfo('STEPT_AV')

  outstr={ origin: origin, $
    scale: scale, $
    midtime: midtime, $
    sit_stare: sit_stare, $
    xpos: xpos, $
    ypos: ypos, $
    tmid_min: tmid_min, $
    utc: utc, $
    stud_acr: data->getinfo('OBSID'), $
    date_obs: data->getinfo('DATE_OBS'), $
    l1p5_ver: l1p5_ver, $
    obs_type: obs_type, $
    cadence: cadence, $
    xcen: xcen, $
    ycen: ycen, $
    rast_direct: rast_direct }

  return,outstr

END
