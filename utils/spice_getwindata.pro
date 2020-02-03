;+
; NAME:
;       SPICE_GETWINDATA
;
; PURPOSE:
;       Returns the SPICE data structure for one spectral window. The
;       format is chosen to copy the Hinode/EIS routine
;       EIS_GETWINDATA.
;
; CALLING SEQUENCE:
;       d = spice_getwindata(file, iwin)
;
; INPUTS:
;       FILE: input SPICE fits file or data object. See restrictions.
;
; OPT. INPUT:
;       IWIN: scalar with the index of the desired windows. This can
;             also be a wavelength or as a string that
;             matches one of the line ids.
;       Wrange: A 2-element array specifying a wavelength range to be
;               returned. It is intended for use with full-CCD
;               data-sets for which only a specific wavelength range
;               is required. It can also significantly speed up
;               SPICE_GETWINDATA for large, full-CCD rasters.
;       Ixrange:  A 2-element array specifying a X-pixel range to be
;                 returned. This is principally intended for speeding
;                 up the routine when working with long sit-and-stare
;                 sequences.
;
; KEYWORDS:
;       KEEP_SAT:  If set, then saturated data are retained rather
;                  than set to the missing value. (This can be useful
;                  when making pretty pictures.)
;       CLEAN:     If set, then the NEW_SPIKE routine is used to clean
;                  the data from cosmic rays.
;       VERBOSE:   If set, then information about how long the routine
;                  takes to run is printed to the screen.
;       QUIET:     If set, then no information messages will be
;                  printed.
;       NORMALIZE: If set, then the intensity is divided by the
;                  exposure time.
;       CALIB:     If set, then intensities (and errors) will be
;                  returned in calibrated units of
;                  erg/cm2/s/sr/pixel.
;       PERANG:    If set, the intensities are returned in units of
;                  erg/cm2/s/sr/Angstrom. To be used in conjunction
;                  with /CALIB; ignored otherwise.
;
; OUTPUTS:
;       Structure with data and header information. The structure is
;       designed to mimic the structure created by the Hinode/EIS
;       routine EIS_GETWINDATA. An example of the tags for one
;       data-set is given below. Please see the 'Programming notes'
;       section for notable differences with the EIS version.
;
;  ** Structure <3156e08>, 27 tags, length=171829752, data length=171829684, refs=1:
;     FILENAME        STRING    'iris_l2_20131022_205938_3820259443_raster_t000_r00000.fits'
;     LINE_ID         STRING    'Si IV 1403'
;     INT             FLOAT     Array[306, 64, 1093]
;     ERR             FLOAT     Array[306, 64, 1093]
;     WVL             DOUBLE    Array[306]
;     DATA_QUALITY    BYTE      Array[306]
;     EXPOSURE_TIME   DOUBLE    Array[64]
;     TIME            DOUBLE    Array[64]
;     TIME_CCSDS      STRING    Array[64]
;     NL              LONG               306
;     NX              LONG                64
;     NY              LONG              1093
;     SCALE           DOUBLE    Array[2]
;     SOLAR_X         DOUBLE    Array[64]
;     SOLAR_Y         DOUBLE    Array[1093]
;     XCEN            DOUBLE          -172.95100
;     YCEN            DOUBLE           30.754500
;     UNITS           STRING    'Corrected DN'
;     MISSING         FLOAT          -200.000
;     IWIN            INT              4
;     SIT_AND_STARE   INT              0
;     WAVE_CORR_SET   INT              0
;     WAVE_CORR       DOUBLE    Array[64, 1093]
;     WAVE_CORR_TILT  DOUBLE    Array[1093]
;     WAVE_CORR_T     DOUBLE    Array[64]
;     HDR             STRUCT    -> <Anonymous> Array[1]
;     TIME_STAMP      STRING    'Wed Sep  9 13:58:30 2015'
;
; EXAMPLES:
;       Get window containing Si IV 1393 line:
;
;       IDL> wd = spice_getwindata(file,1393)
;
; PROGRAMMING NOTES:
;     - The level-2 IRIS files return intensities in "corrected DN"
;       units, compared to EIS for which the intensities are in
;       erg/cm2/s/sr/angstrom. By default spice_getwindata does not apply the
;       radiometric calibration, and so it returns the intensity array
;       in corrected DN units. For calibrated units, see the keywords
;       /CALIB and /PERANG.
;
;     - The WAVE_CORR tags are not used by IRIS, but are retained to
;       ensure compatibility with the EIS IDL routines.
;
;     - The WINDATA.HDR structure is just a copy of the IRIS header
;       structure, but a few extra tags have been added to ensure
;       compatibility with the EIS routines. Perhaps the most
;       significant is SLIT_IND, which takes a value of 4 (the EIS
;       slits had values between 0 and 3).
;
;     - The SOLAR_X and SOLAR_Y tags lose their meaning when the roll
;       angle is not 0 degrees. In this case Y is interpreted as
;       arcsec along the slit, with zero at the bottom of the slit. X
;       is interpreted as arcsec perpendicular to the slit, with the
;       initial position set to zero.
;
;     - The tag SIT_N_STARE has been added to flag sit-and-stare data
;       (0-no, 1-yes). This tag was not present for EIS.
;
;     - Missing data are set to -200 for IRIS, but for this routine we
;       consider any pixels with values of -10 or lower as being
;       missing (and thus set to -200).
;
;     - The tag 'data_quality' is just set to zeros.
;
;     - One difference from EIS is that different wavelength windows
;       can have different exposure times (specifically FUV can be
;       different from NUV) so it's important to pass the
;       window index to some of the methods.
;
; CALLS:
;       IRIS_GET_CALIB, IRIS_OBJ, READ_IRIS_L2, NEW_SPIKE
;
; MODIFICATION HISTORY:
;       Ver.1, 3-Feb-2020, Martin Wiesmann
;         modified from iris_getwindata
;-

FUNCTION spice_getwindata, input_file, input_iwin, keep_sat=keep_sat, $
  clean=clean, wrange=wrange, verbose=verbose, $
  ixrange=ixrange, normalize=normalize, quiet=quiet, $
  calib=calib, perang=perang



  IF n_params() EQ 0 THEN BEGIN
    print,'Use:  IDL> wd=spice_getwindata( filename, i)'
    print,'               where i is the index of the window'
    print,'Or:   IDL> wd=spice_getwindata( filename, wvl)'
    print,'               where wvl is the desired wavelength'
    print,''
    print,'Optional inputs:'
    print,'   /keep_sat - do not flag saturated data as missing'
    print,'   /clean    - clean window of cosmic rays with new_spike'
    print,'   wrange=   - specify a subset of wavelength range to load'
    print,'   ixrange=  - specify a subset of X (exposures) to load'
    print,'   calib=    - apply IRIS radiometric calibration'
    print,'   perang=   - (if /calib set) intensity given in per-Angstrom units'
    return,-1
  ENDIF

  t0=systime(1)

  ;
  ; Check if a filename or object is being input.
  ;
  IF datatype(input_file) EQ 'STR' THEN BEGIN
    swtch=0
    IF n_elements(input_file) GT 1 THEN BEGIN
      print,'% SPICE_GETWINDATA: only a single filename can be specified. Please check your inputs.'
      print,'                   Returning...'
      return,-1
    ENDIF
    d=iris_obj(input_file[0])
  ENDIF ELSE BEGIN
    swtch=1
    d=input_file
  ENDELSE


  ;
  ; If input_iwin wasn't specified then the following asks the
  ; user to manually select a window. Note that if the user inputs "1.3"
  ; then this will be interpreted as round(1.3)=1.
  ;
  IF n_params() EQ 1 THEN BEGIN
    print,'% SPICE_GETWINDATA: Please choose a window from the following:'
    nwin=d->get_number_windows()
    d->show_lines
    ans=''
    read,ans,prompt='Choose number from 0 to '+trim(nwin-1)+': '
    IF is_number(ans) THEN BEGIN
      ans=float(ans)
      IF ans GE 0 AND ans LE nwin-1 THEN input_iwin=round(ans) ELSE input_iwin=-1
    ENDIF ELSE BEGIN
      input_iwin=-1
    ENDELSE
    IF input_iwin EQ -1 THEN BEGIN
      print,'% SPICE_GETWINDATA: invalid input. Returning...'
      return,-1
    ENDIF
  ENDIF

  ;
  ; SPICE missing data are assigned value given in header keyword 'BLANK' in level 0
  ; and level 1. Level 2, NAN
  missing_val=d->get_missing_value()

  ;
  ; Get spatial binning factor
  ;
  ybin=d->getinfo('SUMSPAT')


  ;
  ; The object codes will actually change input_iwin from a wavelength
  ; to an index, so the line below defines the index iwin based on what
  ; the user has input. Therefore input_iwin will not be modified.
  ;
  IF input_iwin GT 1000 THEN iwin=d->getwindx(input_iwin) ELSE iwin=input_iwin

  IF iwin EQ -1 THEN BEGIN
    print,'% SPICE_GETWINDATA:  wavelength not found in data-set. Returning...'
    return,-1
  ENDIF

  ;
  ; Get the "label" for the chosen wavelength window (this is needed for
  ; read_iris_l2).
  ;
  lbl=d->getline_id(iwin)

  ;
  ; Extract the data array.
  ; 9-Sep-2015: I've switched to using read_iris_l2 as this is
  ; factors 2-3 quicker than the object method.
  ;
  ;wd=d->getvar(iwin,/load)
  read_iris_l2,input_file[0],index,wd,wave=lbl,/silent

  t1=systime(1)

  ;
  ; Get dimensions of array. Note that at this point X and Y are swapped
  ; in WD (this is fixed later).
  ;
  s=size(wd,/dim)
  nl=s[0]
  nx=s[2]
  ny=s[1]


  IF n_elements(ixrange) NE 0 THEN BEGIN
    ix0=ixrange[0]
    ix1=min([nx-1,ixrange[1]])
    wd=wd[*,*,ix0:ix1]
    nx=ix1-ix0+1
    IF NOT keyword_set(quiet) THEN print,'% SPICE_GETWINDATA: Warning - the windata.xcen tag does not take account of the sub-range selected by IXRANGE.'
  ENDIF ELSE BEGIN
    ix0=0
    ix1=nx-1
  ENDELSE


  IF nl GE 2048 AND (nx*ny) GE 1e5 AND n_elements(wrange) EQ 0 THEN BEGIN
    IF NOT keyword_set(quiet) THEN print,'% SPICE_GETWINDATA: this is a huge data-set! Please consider using WRANGE= to pick out a sub-range in the wavelength dimension.'
  ENDIF


  ;
  ; For computing the photons in an efficient way I need a 3D array of
  ; wavelengths that I call wvl_arr.
  ;
  ; I use this section to implement the WRANGE= keyword, which also
  ; modifies WD making the routine significantly quicker from this point
  ; on.
  ;
  lam=d->getlam(iwin)
  IF n_elements(wrange) NE 0 THEN BEGIN
    k=where(lam GE wrange[0] AND lam LE wrange[1],nk)
    IF nk NE 0 THEN BEGIN
      lam=lam[k]
      nl=nk
      wd=wd[k,*,*]
    ENDIF ELSE BEGIN
      print,'% SPICE_GETWINDATA: the input WRANGE is not consistent with the wavelength window. Returning...'
      return,-1
    ENDELSE
  ENDIF
  wvl_arr=fltarr(nl,nx,ny,/nozero)
  wvl_arr_2d=lam#(fltarr(ny)+1.)
  FOR i=0,nx-1 DO wvl_arr[*,i,*]=wvl_arr_2d



  ;
  ; Need to swap the X and Y dimensions in the array.
  ; For large files this can be very slow, but transpose is about a
  ; factor two quicker than rearrange (SSW routine).
  ;
  ;wd=rearrange(temporary(wd),[1,3,2])
  wd=transpose(temporary(wd),[0,2,1])


  IF keyword_set(clean) THEN BEGIN
    new_spike,wd,wdout,/neighbours,missing=missing_val
    wd=temporary(wdout)
  ENDIF

  ;
  ; Identify which wavelength region we have ('fuv' or 'nuv')
  ;
  reg=d->getregion(iwin)



  ;
  ; There are quite a few pixels that get a assigned a large negative
  ; value (between -199 and -10). I'm not sure how these come
  ; about but I'm going to set them to be missing. Note that a
  ; pixel with negative DN is not assigned a photon noise error: it will
  ; only get a read noise error.
  ;
  k=where(wd GT missing_val AND wd LT -10,nk)
  IF nk NE 0 THEN wd[k]=missing_val


  ;
  ; Saturated data seem to be set at 16183 DN so I will flag these
  ; values as missing
  ;
  IF NOT keyword_set(keep_sat) THEN BEGIN
    k=where(wd EQ 16183,nk)
    IF nk NE 0 THEN wd[k]=missing_val
  ENDIF



  ;
  ; The gain is the number of electrons released in the detector that
  ; yield 1 DN.
  ;
  ; Set the values for the gain (g) and dark current noise
  ; (dark_unc). The values have been taken from the IRIS instrument
  ; paper. Note that dark_unc is specified in DN.
  ;
  ; The quantum yield is the number of electrons released by a single
  ; incident photon on the detector. The theoretical yield is
  ; 12398.5/wvl/3.65 (wvl in angstroms) and this works well at EUV and
  ; X-ray wavelengths. J.P.Wuelser in a message from 29-Sep-2014 says
  ; that this formula can't be used at FUV wavelengths and it should be assumed
  ; that 1.5 electrons are generated for the entire FUV channel. For
  ; the NUV channel it is 1.0. Note that these are the numbers given in
  ; the IRIS instrument paper.
  ;
  IF trim(reg) EQ 'FUV' THEN BEGIN
    yield=1.5
    g=6.0
    dark_unc=3.1
  ENDIF ELSE BEGIN
    yield=1.0
    g=18.0
    dark_unc=1.2
  ENDELSE

  ;
  ; Compute the DN to photon conversion factor.
  ;
  ;
  dn_to_p=g/yield

  ;
  ; Compute dark current uncertainty in photons (rather than DN)
  ;
  dark_unc_p=dark_unc*dn_to_p


  ;
  ; Create the photon array, making sure to set missing pixels.
  ;
  wd_p=wd*dn_to_p
  k_miss=where(wd EQ missing_val,nk)


  ;
  ; Compute errors on photon counts by combining sqrt(N) photon
  ; statistics with a Gaussian distribution for the dark current
  ; uncertainty. Note that for wd_p<0 the uncertainty is only from the
  ; dark current.
  ;
  x=wd_p>0 + dark_unc_p^2
  err_p=sqrt(temporary(x))

  ;
  ; Now convert the photon errors to an error in DN
  ;
  err=temporary(err_p)/dn_to_p
  wd_p=0
  IF nk NE 0 THEN err[k_miss]=missing_val


  ;
  ; Below I extract time information.
  ;   - getti_1() seems to be the same as gettime() which is mentioned
  ;     in the IRIS user guide.
  ;   - ti2utc() rounds the time to the nearest second so it's
  ;     better to use ti2tai()
  ;   - ti2 is the shutter close time, but I don't actually put
  ;     it in the windata output.
  ;
  ti1=d->getti_1(iwin)
  ti2=d->getti_2(iwin)

  ti1=d->sec_from_obs_start(ti1)
  ti2=d->sec_from_obs_start(ti2)

  time=ti1

  ti1=d->ti2tai(ti1)
  ti2=d->ti2tai(ti2)

  ti1=anytim2utc(ti1,/ccsds)
  ti2=anytim2utc(ti2,/ccsds)

  ;
  ; The following adds some EIS tags to the header structure. Some are
  ; just set to zero, but others have real values that are used in the
  ; software.
  ;
  header=d->gethdr()
  hdr=fitshead2struct(header)
  hdr2=add_tag(hdr,0,'YWS')
  hdr=temporary(hdr2)
  hdr2=add_tag(hdr,0,'RAST_ID')
  hdr=temporary(hdr2)
  hdr2=add_tag(hdr,0,'NRASTER')
  hdr=temporary(hdr2)
  hdr.nraster=1-(d->getsit_AND_stare())
  hdr2=add_tag(hdr,0,'SLIT_IND')
  hdr=temporary(hdr2)
  hdr.slit_ind=4    ; note EIS slits are numbered 0-3
  hdr2=add_tag(hdr,ny,'YW')
  hdr=temporary(hdr2)

  ;
  ; The keyword 'nexp_prp' is not consistent with EIS, as a 100 exposure
  ; sit-and-stare will be set to nexp_prp=100 whereas for EIS it would
  ; be 1. It seems nexp_prp is always 1 for IRIS so I'm just
  ; going to set it to 1.
  ;
  hdr.nexp_prp=1


  ;
  ; Get satellite roll angle
  ;
  roll_angle=d->getinfo('SAT_ROT')

  ;
  ; The satellite roll angle potentially messes up xpos, ypos, etc. My
  ; procedure is to treat these as normal for a roll angle < 5 degrees
  ; (this is the EIS case). For other angles, I assume Y corresponds to
  ; distance along the slit, and X in the direction perpendicular to the
  ; slit, i.e., they are not heliocentric coordinates in this case.
  ;
  ; 6-Jun-2017: I've updated the case where roll_angle is more 5
  ; degrees to now take the X and Y step-sizes from CDELT3 and CDELT2,
  ; respectively. (Previously I was setting the step sizes to 1, which
  ; caused problems for other routines.)
  ;
  IF abs(roll_angle) LT 5.0 THEN BEGIN
    xpos=d->getxpos(iwin=iwin)
    ypos=d->getypos(iwin=iwin)
    IF d->getsit_AND_stare() EQ 1 THEN xscale=0.33 ELSE xscale=median(xpos[1:nx-1]-xpos[0:nx-2])
    yscale=median(ypos[1:ny-1]-ypos[0:ny-2])
    scale=[xscale,yscale]
    xpos=xpos[ix0:ix1]
  ENDIF ELSE BEGIN
    dx=d->getinfo('CDELT3',iwin)   ; perpendicular to slit
    dy=d->getinfo('CDELT2',iwin)   ; along slit
    IF d->getsit_AND_stare() EQ 1 THEN xpos=fltarr(nx)+0 ELSE xpos=findgen(nx)*dx
    ypos=findgen(ny)*dy
    scale=[dx,dy]
  ENDELSE


  exp_time=d->getexp(iwin=iwin)

  units=d->getinfo('bunit')


  ;
  ; If /normalize is set, then divide the intensity array by the
  ; exposure time. Don't do this if /calib has been set, though.
  ;
  IF keyword_set(normalize) AND NOT keyword_set(calib) THEN BEGIN
    FOR i=0,nx-1 DO BEGIN
      exp_img=wd[*,i,*]
      k=where(exp_img NE missing_val,nk)
      IF nk NE 0 THEN exp_img[k]=exp_img[k]/exp_time[ix0+i]
      wd[*,i,*]=temporary(exp_img)
    ENDFOR
    units=units+' s^-1'
  ENDIF


  t2=systime(1)

  ;
  ; Note for data-sets with very large windows (e.g., 2053x400x1093),
  ; simply creating the windata structure can take about 30secs.
  ;
  windata= { filename: file_basename(input_file[0]), $
    line_id: d->getline_id(iwin), $
    int: wd, $
    err: err, $
    wvl: lam, $
    data_quality: bytarr(nl), $
    exposure_time: exp_time[ix0:ix1], $
    time: time[ix0:ix1], $
    time_ccsds: ti1[ix0:ix1], $
    nl: nl, $
    nx: nx, $
    ny: ny, $
    scale: scale, $
    solar_x: xpos, $
    solar_y: ypos, $
    xcen: d->getinfo('xcen'), $
    ycen: d->getinfo('ycen'), $
    units: units, $
    missing: missing_val, $
    iwin: iwin, $
    sit_AND_stare: d->getsit_AND_stare(), $
    wave_corr_set: 0, $
    wave_corr: dblarr(nx,ny), $
    wave_corr_tilt: dblarr(ny), $
    wave_corr_t: dblarr(nx), $
    time_stamp: systime(), $
    hdr: hdr }

  IF swtch EQ 0 THEN obj_destroy,d


  IF keyword_set(calib) THEN BEGIN
    cal=iris_get_calib(windata.wvl,windata.hdr.date_obs,ybin=ybin,units=units, $
      perang=perang)
    ;
    id_y=make_array(windata.ny,value=1.)
    expt_y=(1./windata.exposure_time)#id_y
    ;
    cal_array=fltarr(nl,nx,ny)
    FOR i=0,nl-1 DO cal_array[i,*,*]=cal[i]*expt_y
    ;
    k=where(windata.int NE windata.missing)
    windata.int[k]=windata.int[k]*cal_array[k]
    windata.err[k]=cal_array[k]*windata.err[k]
    windata.units=units
    ;
    junk=temporary(cal_array)   ; tidy up
  ENDIF



  t3=systime(1)

  IF keyword_set(verbose) THEN BEGIN
    print,format='("  Time taken (s): ",f6.2)',t3-t0
    print,format='("       Load data: ",f6.2)',t1-t0
    print,format='("   Prepare arays: ",f6.2)',t2-t1
    print,format='("  Make structure: ",f6.2)',t3-t2
  ENDIF

  return,windata

END
