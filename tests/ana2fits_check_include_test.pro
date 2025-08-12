; $Id: 2025-08-12 10:44 CEST $

PRO ANA2FITS_CHECK_INCLUDE_test
  x = 4
  y = 5
  z = 6
  gauss1 = spice_mk_comp_gauss([100, 30, 7])
  gauss2 = spice_mk_comp_gauss([110, 32, 8])
  gauss3 = spice_mk_comp_gauss([120, 34, 9])
  gauss3.include = 0
  bg = mk_comp_poly([130, 36, 10])
  fit = {igauss2: gauss1, igauss3: gauss2, igauss4: gauss3, bg: bg}
  result = findgen(3 * 3 + 1 + 1, x, y, z)

  include = intarr(4, x, y, z)
  include[*] = 1
  include[0, 2, 3, 4] = 0
  include[0, 2, 4, 4] = 0
  include[1, *, *, 2] = 0

  pixels_not_included = 3 * (x * y * z + 1 + 1 + x * y)

  result = ana2fits_check_include(fit = fit, result = result, include = include)

  ind = where(~finite(result), n_infinite)
  IF n_infinite NE pixels_not_included THEN BEGIN
    print, 'Test failed: Expected ', pixels_not_included, ' infinite values, found ', n_infinite
  ENDIF

  ind = where(finite(result[0 : 2, 2, 3, 4]), n_finite)
  IF n_finite NE 0 THEN BEGIN
    print, 'Test failed'
    stop
  ENDIF

  ind = where(finite(result[0 : 2, 2, 4, 4]), n_finite)
  IF n_finite NE 0 THEN BEGIN
    print, 'Test failed'
    stop
  ENDIF

  ind = where(finite(result[3 : 5, *, *, 2]), n_finite)
  IF n_finite NE 0 THEN BEGIN
    print, 'Test failed'
    stop
  ENDIF

  ind = where(finite(result[6 : 8, *, *, *]), n_finite)
  IF n_finite NE 0 THEN BEGIN
    print, 'Test failed'
    stop
  ENDIF

  print, 'Test passed: All expected values are NAN.'
END
