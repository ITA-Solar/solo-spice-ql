; $Id: 2025-08-11 15:26 CEST $

PRO ANA2FITS_CHECK_INCLUDE_test
  gauss1 = spice_mk_comp_gauss([100, 30, 7])
  gauss2 = spice_mk_comp_gauss([110, 32, 8])
  gauss3 = spice_mk_comp_gauss([120, 34, 9])
  gauss3.include = 0
  bg = mk_comp_poly([130, 36, 10])
  fit = {igauss2: gauss1, igauss3: gauss2, igauss4: gauss3, bg: bg}
  result = findgen(11, 4, 5, 6)
  include = intarr(4, 4, 5, 6)
  include[*] = 1
  include[1, 2, 3, 4] = 0
  include[1, 2, 4, 4] = 0
  include[2, *, *, 2] = 0

  include_null = where(include EQ 0, n_null)
  print, 'Number of nulls in include array: ', n_null

  pixels_not_included = 3 * (4 * 5 * 6 + 1 + 1 + 4 * 5)

  result = ana2fits_check_include(fit = fit, result = result, include = include)
  ind = where(finite(result), n_finite)
  IF n_finite NE pixels_not_included THEN BEGIN
    print, 'Test failed: Expected ', pixels_not_included, ' finite values, found ', n_finite
  ENDIF
END
