;+
; NAME:
;     SPICE_RASTER_BROWSER
;
; PURPOSE:
;     This routine is used to browse 3D IRIS data-cubes. It has been
;     adapted from the EIS_RASTER_BROWSER routine available in the
;     Hinode/EIS Solarsoft distribution.
;
;     Within the image display window, the mouse buttons are used as:
;        LEFT   Zoom in to image (factor 2).
;        MIDDLE Choose a new spatial pixel for spectrum display.
;        RIGHT  Zoom out from image (factor 2).
;
;     Similarly, for the spectrum display windows, the mouse buttons are
;     used as:
;        LEFT   Zoom in to spectrum (factor 2).
;        MIDDLE Choose a new spectrum pixel for image display.
;        RIGHT  Zoom out from spectrum (factor 2).
;
;     If you are using the *Mac trackpad*, then you can still use
;     spice_raster_browser. Go to X11->Preferences...->Input and select
;     the 'Emulate three button mouse' option. The GUI now accepts the
;     following inputs:
;
;        CLICK  Zoom in to image (factor 2).
;        OPTION+CLICK  Select a new spatial pixel
;        COMMAND+CLICK Zoom out from image.
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_raster_browser, File
;
; INPUTS:
;     File:  Can be either the name of an IRIS data file, or an IRIS data
;            object. Can also be an array of filenames if the files
;            belong to a raster sequence. In this case an additional
;            widget appears allowing the user to flick between
;            rasters.
;
; OPTIONAL INPUTS:
;     None.
;
; KEYWORDS:
;     QUIET:   If set, then do not print messages to the IDL command
;             window.
;
;     YOFFSETS: If set, then the three wavelength channels will be
;               adjusted for spatial offsets in Y. This should only be
;               used if you notice that there are offsets in the
;               images. Most data-sets should be
;               aligned. **There shouldn't be a need to use
;               this keyword any more.**
;
;     NO_SJI: By default spice_raster_browser looks for SJI files that
;             match FILE and displays the images in the 4th plot
;             column of the GUI. Setting /NO_SJI means that the 4th
;             column is used for displaying a fourth raster column
;             (this was the original behavior, before Feb-2015).
;
;     NO_HCR: The routine queries the Heliophysics Coverage Registry
;             (HCR) to retrieve some metadata, but sometimes this
;             service crashes. By setting this keyword you can bypass
;             the HCR.
;
;     CHUNK_SIZE: Only applicable for sit-and-stare data. It defines the
;                 size of the chunks (in number of exposures) to be
;                 displayed.
;
;     RETINA: The widget was too big for my MacBook Pro retina screen,
;             so I've added this keyword to shrink the fonts.
;
;     NO_GOES: This disables the GOES plot.
;
; EXAMPLE:
;     Find a raster file sequence:
;     IDL> file=iris_find_file('29-Mar-2014 17:00')
;
;     Start the browser:
;     IDL> spice_raster_browser, file
;
; INTERNAL ROUTINES:
;     spice_browser_goes_plot, spice_browser_oplot_line_ids,
;     spice_browser_plot_image, IRIS_BROWSER_PLOT_SPEC,
;     spice_browser_font, IRIS_BROWSER_EVENT, IRIS_BROWSER_WIDGET,
;     spice_browser_update_spectrum, spice_browser_update_image,
;     spice_browser_plot_sji, spice_browser_coltable,
;     spice_browser_wvl_list, IRB_GET_FLARE_TEXT
;
; PROGRAMMING NOTES:
;     Incorporating SJI images
;     ------------------------
;     I decided to use the read_iris_l2 routine to access SJI images
;     as it was quicker than using the iris_sji object. The SJI images
;     are loaded using the subroutine spice_browser_plot_sji.
;
;     Sit-and-stare chunking
;     ----------------------
;     Sit-and-stare studies can run continuously for long periods
;     (e.g., 10 hours) and so loading them into raster_browser can be
;     very slow. To get around this, I decided to break such studies
;     into chunks, and a new slider is introduced to select different
;     chunks. To handle this there are extra parameters in wid_data:
;       nxpos   The size of a chunk in numbers of exposures.
;       ixpos   Index of the start of a chunk.
;       jxpos   Index of the end of a chunk.
;       nchunk  Number of chunks.
;       ichunk  Index of current chunk.
;     nxpos is computed automatically by the routine.
;
; TO-DO LIST:
;     - display SJI images correctly when the roll angle is not 0 or 90
;       degrees.
;
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-


;---------------------
PRO spice_raster_browser, input, quiet=quiet, yoffsets=yoffsets, no_sji=no_sji, $
  chunk_size=chunk_size, retina=retina, no_hcr=no_hcr, $
  no_goes=no_goes
  
  
  input = '/Users/mawiesma/data/iris/level2/20180706_155939_3600008058/iris_l2_20180706_155939_3600008058_raster_t000_r00001.fits'


;  IF n_params() EQ 0 THEN BEGIN
;    print,'Use:  IDL> spice_raster_browser, obj'
;    print,' or:  IDL> spice_raster_browser, filename'
;    print,'         multiple filenames can be specified if they belong to the same raster sequence.'
;    return
;  ENDIF

  ;
  ; Below I check if INPUT is a string (i.e., a filename) or an object.
  ;
  IF datatype(input) EQ 'STR' THEN BEGIN
    data=iris_obj(input[0])
    file=input
    swtch=1
  ENDIF ELSE BEGIN
    data=input
    swtch=0
  ENDELSE


  ;
  ; The routine iris_sji_match looks for SJI files that match the raster
  ; file. If an object has been input, then the routine makes uses of
  ; iris_find_filea nd iris_sji_match to find the SJI files.
  ;
  IF NOT keyword_set(no_sji) AND swtch EQ 1 THEN BEGIN
    sji_file=iris_sji_match(file[0])
  ENDIF ELSE BEGIN
    date_obs=data->getinfo('DATE_OBS')
    file=iris_find_file(date_obs,count=count)
    IF count NE 0 THEN sji_file=iris_sji_match(file[0]) ELSE sji_file=''
  ENDELSE


  ;
  ; If multiple filenames are specified, then the following checks to
  ; make sure they belong to the same raster sequence. I then create the
  ; structure 'filestr' containing information about the sequence.
  ;
  nf=n_elements(file)
  IF nf GT 1 THEN BEGIN
    bname=file_basename(file)
    strchck=strmid(bname[0],0,41)
    chck=strpos(bname,strchck)
    k=where(chck LT 0,nk)
    IF nk GT 0 THEN BEGIN
      print,'% SPICE_RASTER_BROWSER: multiple filenames have been specified, but they do not all belong to the'
      print,'                       same raster sequence. Please check your inputs.'
      print,'                       All files should have the same base-name: '+strchck
      return
    ENDIF
    ;
    ; The data object was loaded earlier, so extract info.
    ;
    t0=data->getinfo('DATE_OBS')
    t1=data->getinfo('ENDOBS')
    ;
    ;; d=iris_obj(file[nf-1])
    ;; t1=d->getinfo('DATE_END')
    ;; obj_destroy,d
    ;
    filestr={nfiles: nf, filelist: file, current: 0, t0: t0, t1: t1, sji_file: sji_file}
  ENDIF ELSE BEGIN
    t0=data->getinfo('DATE_OBS')
    t1=data->getinfo('DATE_END')
    filestr={nfiles: 1, filelist: file, current: 0, t0: t0, t1: t1, sji_file: sji_file}
  ENDELSE


  ;
  ; Check to make sure the data are not from the SJI.
  ;
  instrume=data->getinfo('INSTRUME')
  IF trim(instrume) EQ 'SJI' THEN BEGIN
    print,'% SPICE_RASTER_BROWSER: this routine is not compatible with slit-jaw image data. Returning...'
    IF swtch EQ 1 THEN obj_destroy,data
    return
  ENDIF

  IF NOT keyword_set(quiet) THEN BEGIN
    print,''
    print,' SPICE_RASTER_BROWSER was written by Peter Young (GMU/GSFC).'
    print,' Please report any errors to pyoung9@gmu.edu.'
    print,''
  ENDIF


  ;
  ; Check if we have an internet connection.
  ;
  net_chck=have_network()

  ;
  ; Use the HCR to get the obstitle and AR number. Note the latter comes
  ; from the metadata input by the IRIS planner.
  ;
  IF net_chck EQ 1 AND NOT keyword_set(no_hcr) THEN hcr=iris_obs2hcr(t0,t1)
  IF n_tags(hcr) NE 0 THEN BEGIN
    obstitle=hcr.obstitle
    obs_desc=hcr.goal
    noaanum=trim(hcr.noaanum)
    iris_fov=[hcr.xfov,hcr.yfov]
    iris_xcen=hcr.xcen
    iris_ycen=hcr.ycen
  ENDIF ELSE BEGIN
    obstitle=''
    obs_desc=data->getinfo('OBS_DESC')
    noaanum=''
    hcr=-1
  ENDELSE

  ;
  ; This retrieves a list of GOES flare for the observing period.
  ;
  IF net_chck EQ 1 THEN BEGIN
    flare_data=iris_hek_swpc_flares(starttime=t0,endtime=t1)
  ENDIF ELSE BEGIN
    flare_data=-1
  ENDELSE


  IF net_chck EQ 1 THEN BEGIN
    sock_list,'http://pyoung.org/iris/iris_raster_browser_check.html',page
  ENDIF


  ;
  ; I'm thinking of adding a check to see if EIS was running. TBD...
  ;
  ;; IF have_proc('eis_obs_structure') THEN BEGIN
  ;;   x0=iris_xcen-iris_fov[0]/2.
  ;;   x1=iris_xcen+iris_fov[0]/2.
  ;;   y0=iris_ycen-iris_fov[1]/2.
  ;;   y1=iris_ycen+iris_fov[1]/2.
  ;;   chck=eis_obs_structure(t0,t1,/quiet,count=count)
  ;;   IF count NE 0 THEN BEGIN
  ;;     FOR i=0,count-1 DO BEGIN
  ;;       k=where(chck[i].xcen
  ;;     ENDFOR
  ;;   ENDIF
  ;; ENDIF


  IF NOT keyword_set(quiet) THEN BEGIN
    print,'  OBSTITLE: '+obstitle
    print,'  OBS_DESC: ',data->getinfo('OBS_DESC')
    print,'  DATE_OBS: ',t0
    print,'  DATE_END: ',t1
    print,'  NOAA_NUM: ',noaanum
    IF n_elements(iris_xcen) NE 0 THEN print,'      XCEN: ',string(format='(f6.1)',iris_xcen)
    IF n_elements(iris_ycen) NE 0 THEN print,'      YCEN: ',string(format='(f6.1)',iris_ycen)
    print,format='("  Raster: ",i4," of ",i4)',data->getinfo('RASRPT'), $
      data->getinfo('RASNRPT')
    print,format='("  Roll angle: ",f6.1)',data->getinfo('SAT_ROT')
    print,''
  ENDIF



  spice_browser_widget,data,yoffsets=yoffsets, filestr=filestr, chunk_size=chunk_size, $
    retina=retina, hcr=hcr, no_goes=no_goes, flare_data=flare_data

  ;
  ; Tidy up before exiting.
  ;
  IF swtch EQ 1 THEN BEGIN
    obj_destroy,data
  ENDIF


END
