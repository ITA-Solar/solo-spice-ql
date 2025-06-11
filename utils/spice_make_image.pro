FUNCTION spice_make_image, file, wvl, image_only = image_only
  ;+
  ; NAME:
  ;     SPICE_MAKE_IMAGE
  ;
  ; PURPOSE:
  ;     Creates a 2D image from a SPICE wavelength window.
  ;
  ; CATEGORY:
  ;     SPICE; image.
  ;
  ; CALLING SEQUENCE:
  ;     Result = SPICE_MAKE_IMAGE( File, Wvl )
  ;
  ; INPUTS:
  ;     File:  The name of the SPICE file.
  ;     Wvl:   The wavelength of the emission line for which the image is
  ;            requested, in Angstrom.
  ;
  ; KEYWORD PARAMETERS:
  ;     IMAGE_ONLY:  If set, then only the 2D image is returned.
  ;
  ; OUTPUTS:
  ;     A IDL map structure containing the 2D image. If the observation was a
  ;     sit-and-stare, then the x-axis is time even though it will be
  ;     displayed as solar-x when plotting the image. The time assigned to
  ;     the raster is the mid-time of the raster.
  ;
  ;     The image is created by averaging the intensity over the instrumental
  ;     width of the line. The continuum is not subtracted.
  ;
  ;     If the requested wavelength is not found in the dataset, then a value
  ;     of -1 is returned.
  ;
  ; EXAMPLE:
  ;     IDL> file=spice_find_file('28-may-2020 16:05')
  ;     IDL> map=spice_make_image(file,1031.9)
  ;     IDL> p=plot_map_obj(map,rgb_table=3)
  ;
  ; MODIFICATION HISTORY:
  ;     Ver.1, 31-Oct-2024, Peter Young
  ;-

  ; $Id: 2024-11-06 10:25 EST $

  IF n_params() LT 2 THEN BEGIN
    print, 'Use:  IDL> result=spice_make_image( file, wvl [, /image_only ] )'
    return, -1
  ENDIF

  d = spice_object(file, is_spice = is_spice, object_created = object_created)
  IF ~is_spice THEN return, -1
  nwin = d.get_number_windows()

  sit_stare = d.get_sit_AND_stare()

  IF wvl LE 900. THEN instr_width = 0.79 ELSE instr_width = 0.96

  FOR i = 0, nwin - 1 DO BEGIN
    wvl_array = d.get_lambda_vector(i)
    IF wvl / 10. GE min(wvl_array) AND wvl / 10. LE max(wvl_array) THEN BEGIN
      getmin = min(abs((wvl / 10. - instr_width / 20.) - wvl_array), i1)
      getmin = min(abs((wvl / 10. + instr_width / 20.) - wvl_array), i2)
      wd = d.get_window_data(i)
      wd = reform(wd)
      IF keyword_set(sit_stare) THEN BEGIN
        wd = rearrange(wd, [2, 3, 1])
      ENDIF ELSE BEGIN
        wd = rearrange(wd, [3, 1, 2])
      ENDELSE
      img = average(wd[i1 : i2, *, *], 1)
      ;
      IF d.get_sit_AND_stare() THEN BEGIN
        solar_x = d.get_time_vector(i)
      ENDIF ELSE BEGIN
        solar_x = d.get_instr_x_vector(i, /auto_diff_rot)
      ENDELSE

      xcen = d.get_xcen(i)
      ycen = d.get_ycen(i)
      IF sit_stare THEN BEGIN
        dx = d.get_resolution(i, /time)
      ENDIF ELSE BEGIN
        dx = d.get_resolution(i, /x)
      ENDELSE
      dy = d.get_resolution(i, /y)
      time = d.get_header_keyword('DATE-AVG', 0)
      roll_angle = d.get_header_keyword('CROTA', 0)

      map = make_map(img, xc = xcen, yc = ycen, time = time, dx = dx, dy = dy, roll_angle = roll_angle)
      obj_destroy, d
      ;
      IF keyword_set(image_only) THEN map = map.data
      return, map
    ENDIF
  ENDFOR

  IF object_created THEN obj_destroy, d
  message, /info, /cont, 'The specified wavelength is not found in this dataset. Returning...'
  return, -1
END
