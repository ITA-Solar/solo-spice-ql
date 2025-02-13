;+
; NAME:
;      SPICE_CREATE_L3_IMAGES_SINGLE_EXP
;
; PURPOSE:
;      This procedure creates images from level 2 data of single exposure FITS files.
;      It is derived from spice_create_l3_images.pro, see documentation there for more details.
;
; CATEGORY:
;      Solar Orbiter - SPICE; Utility.
;
; CALLING SEQUENCE:
;      spice_create_l3_images_single_exp, data, l2_header, filename_base [, /show_plot] [, filename = filename ]
;
; INPUTS:
;      data: The data cube of the level 2 FITS file.
;      l2_header: The keyword header of the level 2 FITS file.
;      filename_base: The base filename for the output images. The filename should be
;             constructed similar as in spice_create_l3_images.pro. The filename_base
;             should not contain the file extension.
;
; KEYWORDS:
;     SHOW_PLOT: If set, then the image is shown on the screen and not saved into a file.
;
; OUTPUTS:
;      Writes jpeg and png files with images into out_dir.
;
; OPTIONAL OUTPUTS:
;      filename: The filename of the png file.
;
; CALLS:
;      fxpar, fitshead2wcs, prits_tools.write_image_real_size
;
; HISTORY:
;      Ver. 1,   10-Feb-2025, Martin Wiesmann
;
;-
; $Id: 2025-02-13 15:28 CET $

PRO spice_create_l3_images_single_exp, data, l2_header, filename_base, show_plot = show_plot, filename = filename
  oJpg = spice_jpg_exp()

  image_data = reform(data)

  wcs = fitshead2wcs(l2_header)

  ion = 'single'
  lam = '00nm00'
  ion = string(ion + '--------', format = '(A-8)')
  IF lam.strlen() EQ 6 THEN lam = '-' + lam

  winno = fxpar(l2_header, 'WINNO')
  filename_base2 = filename_base.replace('ql', 'ql-' + ion + lam + '-' + 'int') + fns('#', winno) + '-' + '1-int'
  filename = filename_base2 + '.jpg'

  colortable = 3
  color_center_value = !NULL
  reverse_colortable = 0
  startrow = 0
  endrow = (size(image_data))[1] - 1

  xtitle1 = 'Wavelength [nm]'
  ytitle1 = 'Solar Y [arcsec]'

  IF 1 THEN BEGIN
    oJpg.update, filename, image_data, wcs, remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, $
      fit_trend = fit_trend, value_max = value_max, value_min = value_min, colortable = colortable, reverse_colortable = reverse_colortable, $
      xtitle = xtitle1, ytitle = ytitle1, $
      startrow = startrow, endrow = endrow, l2_header = l2_header, l3_header = l2_header, show_plot = show_plot
    oJpg.plot, /clock
    oJpg.save
  ENDIF

  filename = filename_base2 + '-thumb.png'
  format = 'PNG'
  prits_tools.write_image_real_size, image_data, filename, $
    remove_horizontal_trend = this_remove_horizontal_trend, remove_vertical_trend = this_remove_vertical_trend, fit_trend = fit_trend, $
    value_max = value_max, value_min = value_min, colortable = colortable, format = format, $
    height = 64, border = 0, reverse_colortable = reverse_colortable, /no_axis, $
    color_center_value = color_center_value, show_plot = show_plot
END
