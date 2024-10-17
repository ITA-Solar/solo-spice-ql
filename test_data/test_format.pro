;+
; :Tooltip:
;   do whatever
;
; :Description:
;   alsdfkj
;
; :Arguments:
;   new_data: bidirectional, required, Long
;     Placeholder docs for argument, keyword, or property
;
; :Keywords:
;   switch_me: bidirectional, optional, any
;     Placeholder docs for argument, keyword, or property
;
; :Something:
;   asldfkjasdfoijwrn
;
; :Privat:
;   that's mine
;
; :Author:
;   ich
;
;-
PRO test_Format, new_data, switch_me = switch_me
  COMPILE_OPT IDL2

  print, 'blabla'
  my_variable = 324
  IF n_elements(new_data) EQ 0 THEN new_data = '234'
  a = spice_data('file')
  a.show_lines

  print, my_variable
  print, 'asdlfkj'

  print, 'alskdjf'
  IF keyword_set(switch_me) THEN print, 'i am ''switch'''
  help, new_data
END

;+
; :Description:
;   Hello here we are
;
; :Returns: any
;
; :Arguments:
;   input1: in, required, str
;     Placeholder docs for argument, keyword, or property
;
; :Keywords:
;   asdf: bidirectional, optional, any
;     Placeholder docs for argument, keyword, or property
;
;-
FUNCTION test_format_func, input1, asdf = asdf
  COMPILE_OPT IDL2

  FOR i = 0, 9 DO print, i
  IF keyword_set(asdf) THEN help, input1

  a = 0
  FOR j = 0, n_elements(input1) - 1 DO BEGIN
    a += j
  ENDFOR

  test_Format, a, switch_me = 1

  result = i
  print, 'hallo "welt"'

  return, result
END