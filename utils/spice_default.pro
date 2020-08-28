PRO spice_default, var, default
  compile_opt static
  IF n_elements(var) EQ 0 THEN var = default
END 
