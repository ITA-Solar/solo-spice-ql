;+
; Name        :
;	PARCHECK
;
; Purpose     :
;	Routine to check user parameters to a procedure.
;
; Explanation :
;	This routine checks whether a parameter fulfills some criteria. It checks the data type
;	and the number of dimensions. Optionally, it can also check for minimum and/or maximum
;	allowed values, or for object and structure names.
;	If the parameter is undefined, then the DEFAULT value is returned if provided.
;	If one of the tests fails a message is printed and a RETALL issued.
;	These consequences can be suppressed by supplying the RESULT keyword.
;
; Use         :
;       prits_tools.parcheck, parameter, parnum, name, types, valid_ndims, default=default, $
;                             maxval=maxval,minval=minval, structure_name=structure_name, $
;                             object_name=object_name, disallow_subclasses=disallow_subclasses, result=result
;
;	EXAMPLE     :
;
;	IDL> parcheck, hdr, 2, 'FITS Image Header', 7, 1
;
;	This example checks whether the parameter 'hdr' is of type string (=7)
;	and is a vector (1 dimension).   If either of these tests fail, a
;	message will be printed
;		"Parameter 2 (FITS Image Header) is undefined"
;		"Valid dimensions are 1"
;		"Valid types are string"
;
; See prits_tools::parcheck_test for more examples.
;
; INPUTS      :
;	PARAMETER - Parameter passed to the routine.
;	PARNUM    - Integer parameter number.This information will be used
;             in a possible error message. If set to zero the parameter
;             is assumed to be a keyword.
;	NAME      - The name of the parameter. This information will be used
;	            in a possible error message.
;	TYPES     - Integer or string, scalar or vector of valid types:
;	   0 - undefined
;		 1 - byte        2 - int      3 - long
;		 4 - float       5 - double   6 - complex
;		 7 - string      8 - struct   9 - dcomplex
;		 12 - uint       13 - ulong   14 - long64
;		 15 - ulong64
;    10 - pointer
;    11 - objref (any object, including 'list', 'hash', 'dictionary' and 'orderedhash')
;    Additional valid string types, which signify a collection of valid types:
;    (in paranthesis are the valid type numbers that are included)
;    - unsigned (1, 12, 13, 15)
;    - signed (2, 3, 14)
;    - integers (unsigned + signed = 1, 2, 3, 12, 13, 14, 15)
;    - floats (4, 5)
;    - numeric (integers + floats = 1, 2, 3, 4, 5, 12, 13, 14, 15)
;    - multiplicative (numeric + 6, 9 = 1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 15)
;	VALID_NDIMS - Integer, scalar or vector, giving number of allowed dimensions.
;	              For scalar values, the number of dimensions is zero.
;
; Opt. Inputs :
;
; Outputs     :	None.
;
; Opt. Outputs:	None.
;
; Keywords    :	RESULT: Receives the error messages (string array).
;                       If present, no error message is printed out
;                       and procedure returns to caller without stopping.
;
;               MINVAL: Minimum value for the parameter. Checked
;                       against MIN([parameter]).
;
;               MAXVAL: Maximum value for the parameter. Checked
;                       against MAX([parameter]).
;
;               STRUCTURE_NAME: string, scalar or vector. If the input parameter
;                         is of type 8 (STRUCT), the name of the structure
;                         is checked against STRUCTURE_NAME.
;
;               OBJECT_NAME: string, scalar or vector. If the input parameter
;                         is of type 11 (OBJREF), the name of the object
;                         is checked against OBJECT_NAME, objects inheriting from the given OBJECT_NAME
;                         are also allowed by default, except if DISALLOW_SUBCLASS keyword
;                         is set.
;
;               DISALLOW_SUBCLASS: If set, then objects inheriting from given OBJECT_NAME are not
;                         allowed.
;
;               DEFAULT: If parameter is undefined, then DEFAULT will be returned.
;
; Calls       :	None.
;
; Common      :	None.
;
; Restrictions:
; LIST, HASH, DICTIONARY and ORDEREDHASH alway have dimension 1, except if they are empty.
; A 1-dimensional array of those will also have dimension 1.
;
; Side effects:
;	If an error in the parameter is found, a message is printed and a RETALL issued,
;	except if the keyword RESULT is present.
;
; Category    :	Utilities, Miscellaneous
;
; Prev. Hist. :
;       Taken from ZPARCHECK:
;	version 1  D. Lindler  Dec. 86
;	documentation updated.  M. Greason, May 1990.
;
;
; Written     :	D. Lindler, GSFC/HRS, December 1986
;
; Modified    :	Version 1 (ZPARCHECK), William Thompson, GSFC, 29 March 1994
;			Incorporated into CDS library
;               Version 2, Stein Vidar Haugan, UiO, October 1995
;               Version 3, Martin Wiesmann, UiO, September 2022
;                 Generell overhaul, bugfixing and introduced check for
;                 object name, i.e. new keywords STRUCTURE_NAME, OBJECT_NAME and DISALLOW_SUBCLASS,
;                 improved documentation
;
; Version     :	Version 3, September 2022
;
; $Id: 2022-09-30 14:31 CEST $
;-
;
;----------------------------------------------------------

PRO prits_tools::check_type, parameter, types_string, error, pt, $
  structure_name=structure_name, object_name=object_name, disallow_subclasses=disallow_subclasses
  error = ''
  par_type = size(parameter, /tname)
  IF (where(par_type EQ types_string))[0] EQ -1 THEN BEGIN
    error = "is an invalid data type: " + par_type
  ENDIF ELSE BEGIN
    IF par_type EQ 'STRUCT' && N_ELEMENTS(structure_name) GT 0 THEN BEGIN
      structure_name = STRUPCASE(structure_name)
      IF (where(typename(parameter[0]) EQ structure_name))[0] EQ -1 THEN BEGIN
        error = 'is an invalid structure type: ' + typename(parameter[0])
      ENDIF
    ENDIF
    IF par_type EQ 'OBJREF' && N_ELEMENTS(object_name) GT 0 THEN BEGIN
      error = ''
      pt.check_object_name, parameter, error, object_name, disallow_subclasses=disallow_subclasses
    ENDIF
  ENDELSE
END


PRO prits_tools::check_object_name, parameter, error, object_name, disallow_subclasses=disallow_subclasses
  object_name = STRUPCASE(object_name)
  IF keyword_set(disallow_subclasses) THEN BEGIN
    par_typename = typename(parameter)
    IF (where(par_typename EQ object_name))[0] EQ -1 THEN BEGIN
      IF par_typename NE 'LIST' && par_typename NE 'HASH' && $
        par_typename NE 'DICTIONARY' && par_typename NE 'ORDEREDHASH' THEN BEGIN
        IF (where(typename(parameter[0]) EQ object_name))[0] EQ -1 THEN BEGIN
          error = 'is an invalid object type: ' + typename(parameter)
        ENDIF
      ENDIF ELSE BEGIN
        error = 'is an invalid object type: ' + typename(parameter)
      ENDELSE
    ENDIF
  ENDIF ELSE BEGIN
    nomatch=1
    FOR i=0,N_ELEMENTS(object_name)-1 DO BEGIN
      IF (obj_isa(parameter, object_name[i]))[0] THEN BEGIN
        nomatch=0
        BREAK
      ENDIF
    ENDFOR
    IF nomatch THEN error = 'is an invalid object type: ' + typename(parameter)
  ENDELSE
END


PRO prits_tools::check_ndims, parameter, valid_ndims, error
  par_ndim = size(parameter, /n_dimensions)
  IF size(parameter, /type) EQ 8 && N_ELEMENTS(parameter) EQ 1 THEN par_ndim=0
  IF (where(par_ndim EQ valid_ndims))[0] EQ -1 THEN BEGIN
    error = "has wrong number of dimensions: " + trim(par_ndim)
  ENDIF ELSE BEGIN
    error = ''
  ENDELSE
END


PRO prits_tools::check_range, parameter, min, max, error
  error = ''
  IF n_elements(min) GT 1 THEN error = "MINVAL keyword of PRITS_TOOLS::PARCHECK must be scalar"
  IF n_elements(max) GT 1 THEN BEGIN
    error_temp = "MAXVAL keyword of PRITS_TOOLS::PARCHECK must be scalar"
    IF error NE '' THEN error = [error, error_temp] $
    ELSE error = error_temp
  ENDIF
  IF error[0] NE '' THEN return
  IF n_elements(min) EQ 1 THEN BEGIN
    IF (where(parameter LT min))[0] NE -1 THEN error = 'is smaller than minimum value ' + trim(min)
  ENDIF
  IF n_elements(max) EQ 1 THEN BEGIN
    IF (where(parameter GT max))[0] NE -1 THEN BEGIN
      error_temp = 'is larger than maximum value ' + trim(max)
      IF error NE '' THEN error = [error, error_temp] $
      ELSE error = error_temp
    ENDIF
  ENDIF
END


FUNCTION prits_tools::tnames_from_tnames, typenames
  unsigned = ['BYTE', 'UINT', 'ULONG', 'ULONG64']
  signed = ['INT', 'LONG', 'LONG64']
  integers = [unsigned, signed]
  floats = ['FLOAT', 'DOUBLE']
  numeric = [integers, floats]
  multiplicative = [numeric, 'COMPLEX', 'DCOMPLEX']
  new_typenames = []
  foreach typename, typenames DO BEGIN
    CASE typename OF
      'UNSIGNED': add = unsigned
      'SIGNED': add = signed
      'INTEGERS': add = integers
      'FLOATS': add = floats
      'NUMERIC': add = numeric
      'MULTIPLICATIVE': add = multiplicative
      ELSE: add = typename
    ENDCASE
    new_typenames = [new_typenames, add]
  ENDFOREACH
  return, new_typenames
END


FUNCTION prits_tools::typename_from_typecode, typecode, pt, error
  IF size(typecode, /tname) EQ 'STRING' THEN return, pt.tnames_from_tnames(STRUPCASE(typecode))
  CASE typecode OF
    0: return, 'UNDEFINED'
    1: return, 'BYTE'
    2: return, 'INT'
    3: return, 'LONG'
    4: return, 'FLOAT'
    5: return, 'DOUBLE'
    6: return, 'COMPLEX'
    7: return, 'STRING'
    8: return, 'STRUCT'
    9: return, 'DCOMPLEX'
    10: return, 'POINTER'
    11: return, 'OBJREF'
    12: return, 'UINT'
    13: return, 'ULONG'
    14: return, 'LONG64'
    15: return, 'ULONG64'
    ELSE: BEGIN
      error = 'TYPE CODE for PRITS_TOOLS::PARCHECK must be GE 0 and LE 15: ' + trim(typecode)
      return, 'UNKNOWN'
    END
  ENDCASE
END


PRO prits_tools::parcheck, parameter, parnum, name, types, valid_ndims, default=default, $
  maxval=maxval,minval=minval, structure_name=structure_name, object_name=object_name, $
  disallow_subclasses=disallow_subclasses, result=result
  compile_opt idl2, static

  pt = prits_tools()
  noerror = arg_present(result)
  result = ''
  errors = []
  error = ''
  types_string = []
  foreach type, types DO types_string = [types_string, pt.typename_from_typecode(type, pt, error)]

  valid_ndims_use = valid_ndims
  IF n_elements(parameter) EQ 0 THEN BEGIN
    IF n_elements(default) NE 0 THEN BEGIN
      parameter = default
      return
    ENDIF ELSE IF (where(types_string EQ 'UNDEFINED'))[0] EQ -1 THEN BEGIN
      errors = 'is undefined and no default has been specified'
      GOTO, ABORT
    ENDIF ELSE BEGIN
      valid_ndims_use = [0, valid_ndims] 
    ENDELSE
  ENDIF
  IF error NE '' THEN errors = [errors, error]

  IF N_params() LT 5 THEN BEGIN
    on_error, 2
    message, 'Use: PARCHECK, parameter, parnum, name, types, n_dimensions'
  ENDIF

  pt.check_ndims, parameter, valid_ndims_use, error
  IF error NE '' THEN errors = [errors, error]

  pt.check_type, parameter, types_string, error, pt, $
    structure_name=structure_name, object_name=object_name, disallow_subclasses=disallow_subclasses
  IF error[0] NE '' THEN errors = [errors, error]

  pt.check_range, parameter, minval, maxval, error
  IF error[0] NE '' THEN errors = [errors, error]

  IF n_elements(errors) EQ 0 THEN return

  ABORT:
  help,calls=callers
  caller=strupcase((str_sep(callers[1],' '))[0])

  IF parnum NE 0 THEN result = 'Parameter ' + trim(parnum) + ' ('+name+')' $
  ELSE                result = 'Keyword ' + name + ' '
  result = [result + ' of routine ' + STRUPCASE(caller) + ' ' + errors]

  dimension_strings = trim(valid_ndims)
  dimension_string = strjoin(dimension_strings, ', ')
  result = [result,'Valid number of dimensions are: '+dimension_string]

  stype = ''
  FOR i = 0, N_elements( types_string )-1 DO BEGIN
    stype += types_string[i]
    IF i LT N_elements( types_string )-1 THEN stype += ', '
  ENDFOR
  result = [result,'Valid types are: ' + stype]

  IF N_ELEMENTS(structure_name) GT 0 THEN BEGIN
    otype = ''
    FOR i = 0, N_elements( structure_name )-1 DO BEGIN
      otype += structure_name[i]
      IF i LT N_elements( structure_name )-1 THEN otype += ', '
    ENDFOR
    result = [result,'Valid structure names are: ' + otype]
  ENDIF

  IF N_ELEMENTS(object_name) GT 0 THEN BEGIN
    ctype = ''
    FOR i = 0, N_elements( object_name )-1 DO BEGIN
      ctype += object_name[i]
      IF i LT N_elements( object_name )-1 THEN ctype += ', '
    ENDFOR
    result_temp = 'Valid object names are: ' + ctype + ' (subclasses '
    IF keyword_set(disallow_subclasses) THEN result_temp += 'NOT '
    result_temp += 'allowed)'
    result = [result, result_temp]
  ENDIF

  IF Keyword_SET(noerror) THEN RETURN
  PRINT,''                     ; Blank line
  FOR i=0,N_elements(result)-1 DO Print,"! " + result[i]
  print

  on_error, 2
  MESSAGE,"STOPPING, returning to checkpoint in " + caller + string([13b, 10b])

END


PRO prits_tools::parcheck_test
  compile_opt static
  print,''
  print,'Test 1 should fail'
  prits_tools.parcheck, [5], 1, "test_01", ['BYTE'], [0, 5], result=result
  print, result, format='(a)'
  print,''
  print,'Test 2.1 should fail'
  prits_tools.parcheck, a, 2, "test_02.1", ['BYTE'], [0, 5], result=result
  print, result, format='(a)'
  print,''
  print,'Test 2.2 should be ok'
  prits_tools.parcheck, a, 2, "test_02.2", ['BYTE', 'undefined'], [0, 5], result=result
  print, result, format='(a)'
  print,''
  print,'Test 3 should be ok'
  prits_tools.parcheck, a, 3, "test_03", ['BYTE'], [0, 5], result=result, default=77
  print, result, format='(a)'
  print,''
  print,'Test 4 should be ok'
  prits_tools.parcheck, 4US, 4, "test_04", ['unSIgned'], 0, result=result, default=77
  print, result, format='(a)'
  print,''
  print,'Test 5.1 should fail'
  prits_tools.parcheck, [5], 1, "test_05.1", ['numeric'], [0, 1], result=result, minval=10, maxval=20
  print, result, format='(a)'
  print,''
  print,'Test 5.2 should fail'
  prits_tools.parcheck, indgen(20,20), 2, "test_05.2", ['integers'], [0, 1, 2], result=result, minval=10, maxval=20
  print, result, format='(a)'
  print,''
  print,'Test 5.3 should be ok'
  prits_tools.parcheck, [11,15,19], 3, "test_05.3", ['integers'], [0, 1, 2], result=result, minval=10, maxval=20
  print, result, format='(a)'

  print,''
  print,'Test 5.4 should fail'
  prits_tools.parcheck, [11,15,19], 3, "test_05.4", ['integers'], [0, 1, 2], result=result, minval=[10,11], maxval=20
  print, result, format='(a)'
  print,''
  print,'Test 5.5 should fail'
  prits_tools.parcheck, [11,15,19], 3, "test_05.5", ['integers'], [0, 1, 2], result=result, minval=10, maxval=[20,22]
  print, result, format='(a)'
  print,''
  print,'Test 5.6 should fail'
  prits_tools.parcheck, [11,15,19], 3, "test_05.6", ['integers'], [0, 1, 2], result=result, minval=[10,11], maxval=[20,22]
  print, result, format='(a)'
  print,''
  print,'Test 5.7 should fail'
  prits_tools.parcheck, [11,15,19], 3, "test_05.7", 22, [0, 1, 2], result=result
  print, result, format='(a)'

  print,''
  print,'Test 6 should be ok'
  st = {mystruct, a:0, b:'adf'}
  prits_tools.parcheck, st, 0, "test_06", 8, 0, result=result, structure_name='mystruct'
  print, result, format='(a)'
  print,''
  print,'Test 7 should fail'
  prits_tools.parcheck, st, 0, "test_07", 8, 0, result=result, structure_name=['anotherstruct','struc']
  print, result, format='(a)'
  print,''
  print,'Test 8 should be ok'
  prits_tools.parcheck, [st, st], 0, "test_08", 8, 1, result=result, structure_name='mystruct'
  print, result, format='(a)'

  print,''
  print,'Test 9.1 should be ok'
  obj = obj_new('IDL_Container')
  prits_tools.parcheck, obj, 0, "test_09.1", 11, 0, result=result, object_name=['IDL_Container','MyObject']
  print, result, format='(a)'
  print,''
  print,'Test 9.2 should fail'
  prits_tools.parcheck, obj, 0, "test_09.2", 11, 1, result=result, object_name=['IDL_Container','MyObject']
  print, result, format='(a)'
  print,''
  print,'Test 9.3 should be ok'
  prits_tools.parcheck, [obj, obj], 0, "test_09.3", 11, 1, result=result, object_name='IDL_Container'
  print, result, format='(a)'
  print,''
  print,'Test 9.4 should fail'
  prits_tools.parcheck, [obj, obj], 0, "test_09.4", 11, 0, result=result, object_name='IDL_Container'
  print, result, format='(a)'
  print,''
  print,'Test 9.5 should fail'
  prits_tools.parcheck, obj, 0, "test_09.5", 11, 0, result=result, object_name=['MyObject','AnotherObject']
  print, result, format='(a)'

  print,''
  print,'Test 10 should be ok'
  hash = HASH("one", 1.0, "blue", [255,0,0], "Pi", !DPI)
  prits_tools.parcheck, hash, 0, "test_10", 11, 1, result=result, object_name='hash'
  print, result, format='(a)'
  print,''
  print,'Test 11 should fail'
  prits_tools.parcheck, hash, 0, "test_11", 11, 0, result=result, object_name='list'
  print, result, format='(a)'
  print,''
  print,'Test 12.1 should be ok'
  prits_tools.parcheck, hash, 0, "test_12.1", 11, 1, result=result
  print, result, format='(a)'
  print,''
  print,'Test 12.2 should be ok'
  prits_tools.parcheck, [hash, hash], 0, "test_12.2", 11, 1, result=result, object_name='hash'
  print, result, format='(a)'

  print,''
  print,'Test 13.1 should be ok'
  list = LIST('one', 2.0, 3, 4l, PTR_NEW(5), {n:6}, COMPLEX(7,0))
  prits_tools.parcheck, list, 0, "test_13.1", 11, 1, result=result, object_name='list'
  print, result, format='(a)'
  print,''
  print,'Test 13.2 should be ok'
  prits_tools.parcheck, [list, list], 0, "test_13.2", 11, 1, result=result, object_name='list'
  print, result, format='(a)'

  print,''
  a = obj_new('idlitvisaxis')
  print,'Test 14.1 should be ok'
  prits_tools.parcheck, a, 0, "test_14.1", 11, 0, result=result, object_name='idlitvisaxis'
  print, result, format='(a)'
  print,''
  print,'Test 14.2 should be ok'
  prits_tools.parcheck, a, 0, "test_14.2", 11, 0, result=result, object_name='idlitvisualization'
  print, result, format='(a)'
  print,''
  print,'Test 14.3 should fail'
  prits_tools.parcheck, a, 0, "test_14.3", 11, 0, result=result, object_name='idlitvisualization', /disallow_subclasses
  print, result, format='(a)'
  print,''
  print,'Test 14.4 should be ok'
  prits_tools.parcheck, [a, a], 0, "test_14.4", 11, [0, 1], result=result, object_name='idlitvisualization'
  print, result, format='(a)'
END


IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
  IF getenv("USER") EQ "steinhh" THEN add_path, "$HOME/idl/solo-spice-ql", /expand
  prits_tools.parcheck_test
ENDIF

END
