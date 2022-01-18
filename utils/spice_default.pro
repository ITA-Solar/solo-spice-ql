; $Id: 2020-11-25 21:19 CET $
PRO spice_default, var, default
  compile_opt static
  IF n_elements(var) EQ 0 THEN var = default
END 
