; Returns physical path from a relative and/or symlinked path
FUNCTION ptools::physical_path, path
  COMPILE_OPT STATIC
  catch, error
  IF error NE 0 THEN BEGIN
    catch, /cancel
    message, "Cannot cd to " + path + " to get physical path"
  ENDIF
  cd, path, current = current
  catch, /cancel
  cd, current, current = physical_path
  return, physical_path
END
