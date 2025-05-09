FUNCTION ptools::singleton_instance
  COMPILE_OPT STATIC
  COMMON ptools_singleton, pt

  IF n_elements(pt) EQ 0 THEN pt = obj_new('ptools')
  return, pt
END
