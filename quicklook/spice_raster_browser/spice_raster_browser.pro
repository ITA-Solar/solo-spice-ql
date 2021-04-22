;+
; NAME:
;     SPICE_RASTER_BROWSER
;
; PURPOSE:
;     This routine is used to browse 3D SPICE data-cubes. It has been
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
;     If you are using the *Mac trackpad* on a *VNC screen*, we
;     recommend using BetterTouchTool with e.g. "TipTap Left" set to
;     trigger a middle click, which selects a new spatial pixel.
;
;
; CATEGORY:
;     Solar Orbiter - SPICE; QuickLook.
;
; CALLING SEQUENCE:
;     spice_raster_browser, input [, quiet=quiet, yoffsets=yoffsets, $
;       chunk_size=chunk_size, retina=retina, no_goes=no_goes]
;
; INPUTS:
;     INPUT:  Can be either the name and path of a SPICE data file, 
;             or a SPICE data object.
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
;     CHUNK_SIZE: Only applicable for sit-and-stare data. It defines the
;                 size of the chunks (in number of exposures) to be
;                 displayed.
;
;     RETINA: The widget was too big for my MacBook Pro retina screen,
;             so I've added this keyword to shrink the fonts.
;
;     NO_GOES: This disables the GOES plot.
;
; OUTPUT:
;     None
;
; EXAMPLE:
;     Start the browser:
;     IDL> spice_raster_browser, file
;
; DEPENDENCIES:
;     spice_object
;     have_network
;     iris_hek_swpc_flares
;     box_message
;     spice_browser_widget
;
;     spice_browser_goes_plot, spice_browser_oplot_line_ids,
;     spice_browser_plot_image, IRIS_BROWSER_PLOT_SPEC,
;     spice_browser_font, IRIS_BROWSER_EVENT, IRIS_BROWSER_WIDGET,
;     spice_browser_update_spectrum, spice_browser_update_image,
;     spice_browser_plot_sji, spice_browser_coltable,
;     spice_browser_wvl_list, IRB_GET_FLARE_TEXT
;
; PROGRAMMING NOTES:
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
; HISTORY:
;     Ver. 1, 22-Nov-2019, Martin Wiesmann
;       modified from iris_raster_browser.
;-
; $Id: 2021-04-22 10:01 CEST $


;---------------------
PRO spice_raster_browser, input_data, quiet=quiet, yoffsets=yoffsets, $
  chunk_size=chunk_size, retina=retina, no_goes=no_goes
  COMPILE_OPT IDL2

  IF n_params() EQ 0 THEN BEGIN
    print,'Use:  IDL> spice_raster_browser, obj'
    print,' or:  IDL> spice_raster_browser, filename'
    return
  ENDIF

  ;
  ; Below I check if INPUT is a string (i.e., a filename) or an object.
  ;
  data = spice_get_object(input_data, is_spice=is_spice, object_created=object_created)
  if ~is_spice then return

  ;
  ; Check if we have an internet connection.
  ;
  net_chck=have_network()

  ;
  ; This retrieves a list of GOES flare for the observing period.
  ; To be uncommented later. Commented it out for testing
  ;
  ;  IF net_chck EQ 1 THEN BEGIN
  ;    start_time = data->get_start_time()
  ;    end_time = data->get_end_time()
  ;    flare_data=iris_hek_swpc_flares(starttime=start_time, endtime=end_time)
  ;  ENDIF ELSE BEGIN
  flare_data=-1
  ;  ENDELSE

  ; something similar for spice_browser_raster?
  ;  IF net_chck EQ 1 THEN BEGIN
  ;    sock_list,'http://pyoung.org/iris/iris_raster_browser_check.html',page
  ;  ENDIF

  IF NOT keyword_set(quiet) THEN BEGIN
    box_message, ['FILENAME = ' + data->get_header_info('FILENAME', 0), $
      'EXTNAME  = ' + data->get_header_info('EXTNAME', 0), $
      'STUDYTYP = ' + data->get_header_info('STUDYTYP', 0), $
      'STUDYDES = ' + data->get_header_info('STUDYDES', 0, ''), $
      'STUDY    = ' + data->get_header_info('STUDY', 0, ''), $
      'OBS_TYPE = ' + data->get_header_info('OBS_TYPE', 0, ''), $
      'OBS_ID   = ' + data->get_header_info('OBS_ID', 0, ''), $
      'SPIOBSID = ' + strtrim(string(data->get_header_info('SPIOBSID', 0)), 2), $
      'PURPOSE  = ' + data->get_header_info('PURPOSE', 0, ''), $
      'SOOPNAME = ' + data->get_header_info('SOOPNAME', 0, '')]
  ENDIF

  spice_browser_widget, data, yoffsets=yoffsets, chunk_size=chunk_size, $
    retina=retina, no_goes=no_goes, flare_data=flare_data, quiet=quiet

  ;
  ; Tidy up before exiting.
  ;
  IF object_created EQ 1 THEN BEGIN
    obj_destroy, data
  ENDIF

END
