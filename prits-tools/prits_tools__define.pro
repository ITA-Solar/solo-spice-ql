FUNCTION prits_tools::init
  COMMON prits_tools, first_pt
  IF n_elements(first_pt) EQ 0 THEN first_pt = self
  dummy = self.IDL_Object::init()
  self.vso_addons_init
  self.gen_addons_init
  self.date_addons_init
  return, 1
END

PRO prits_tools::getproperty, au=au, rsun=rsun, deg2rad=deg2rad, rad2deg=rad2deg
  au = 1.49597e11 ; meters
  rsun = 6.957e8  ; meters
  deg2rad = !const.dtor
  rad2deg = !const.rtod
END

PRO prits_tools::cleanup
  ;message, /info, "Cleanup"
END 


PRO prits_tools::static_test
  compile_opt static
  print, "PRITS_TOOLS.STATIC_TEST succeeded"
END

  
PRO prits_tools__define
  vso = {prits_tools__vso_addons}
  gen = {prits_tools__gen_addons}
  date = {prits_tools__date_addons}
  struct = {prits_tools, INHERITS IDL_Object, vso:vso, gen:gen, date:date}  
END


IF getenv("USER") EQ "steinhh" THEN BEGIN
   prits_tools.static_test
END 

END
  
