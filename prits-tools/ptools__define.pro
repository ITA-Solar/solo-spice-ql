FUNCTION ptools::init, quiet = quiet
  COMMON ptools, first_pt
  IF n_elements(first_pt) EQ 0 THEN first_pt = self
  !NULL = self.IDL_Object::init()
  self.vso_addons_init, quiet = quiet
  self.gen_addons_init
  self.date_addons_init
  return, 1
END

PRO ptools::getproperty, au = au, rsun = rsun, deg2rad = deg2rad, rad2deg = rad2deg
  au = 1.49597e11 ; meters
  rsun = 6.957e8 ; meters
  deg2rad = !const.dtor
  rad2deg = !const.rtod
END

PRO ptools::cleanup
  ; message, /info, "Cleanup"
END

PRO ptools::static_test
  COMPILE_OPT STATIC
  print, "PTOOLS.STATIC_TEST succeeded"
END

PRO ptools__define
  vso = {ptools__vso_addons}
  gen = {ptools__gen_addons}
  date = {ptools__date_addons}
  !NULL = {ptools, INHERITS idl_object, vso: vso, gen: gen, date: date}
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
  ptools.static_test
END

END
