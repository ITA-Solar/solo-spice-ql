FUNCTION prits_tools::have_python, raise_error = raise_error
  COMPILE_OPT STATIC
  COMMON prits_tools__add_pythonpaths, previous_result

  IF n_elements(previous_result) EQ 1 THEN BEGIN
    IF previous_result EQ -1 THEN message, "Python not available"
    return, previous_result
  ENDIF

  catch, python_error
  IF python_error THEN BEGIN
    catch, /cancel
    message, "Can't use python", /info
    IF keyword_set(raise_error) THEN BEGIN
      message, /reissue_last
    END
    return, 0
  END
  !null = python.import('numpy')
  return, 1
END
