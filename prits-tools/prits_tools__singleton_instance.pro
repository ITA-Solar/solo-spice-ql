FUNCTION prits_tools::singleton_instance
  COMPILE_OPT STATIC
  COMMON prits_tools_singleton, pt
  
  IF n_elements(pt) EQ 0 THEN pt = obj_new('prits_tools')
  return, pt
END
