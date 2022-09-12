;+
; Name        :	
;	PARCHECK
; Purpose     :	
;	Routine to check user parameters to a procedure
; Explanation :	
;	Routine to check user parameters to a procedure
; Use         :	
;       pt.parcheck, parameter, parnum, name,  types, valid_ndims, default=default, $
;                             maxval=maxval,minval=minval, $
;                             result=result
;
;	EXAMPLE:
;
;	IDL> parcheck, hdr, 2, 7, 1, 'FITS Image Header'
;
;	This example checks whether the parameter 'hdr' is of type string (=7)
;	and is a vector (1 dimension).   If either of these tests fail, a 
;	message will be printed
;		"Parameter 2 (FITS Image Header) is undefined"
;		"Valid dimensions are 1"
;		"Valid types are string"	
;
; Inputs      :	
;	parameter - parameter passed to the routine
;	parnum    - integer parameter number
;	types     - integer scalar or vector of valid types
;		 1 - byte        2 - integer  3 - int*4
;		 4 - real*4      5 - real*8   6 - complex
;		 7 - string      8 - structure
;	dimens   - integer scalar or vector giving number
;		      of allowed dimensions.
;
; Opt. Inputs :	
;
; Outputs     :	None.
;
; Opt. Outputs:	None.
;
; Keywords    :	RESULT: Receives the error messages (string array)
;                       if the keyword /NOERROR is set.
;               
;               NOERROR: Set to avoid error message (stopping)
;                 
;               MINVAL: Minimum value for the parameter. Checked
;                       agains MIN([parameter]).
;
;               MAXVAL: Maximum value for the parameter.
;
; Calls       :	None.
;
; Common      :	None.
;
; Restrictions:	None.
;
; Side effects:	
;	If an error in the parameter is a message is printed
;	a RETALL issued
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
;
; Version     :	Version 2, 11-October-1995
;-
;
;----------------------------------------------------------

PRO prits_tools::check_type, parameter, types, error, error_message
  compile_opt static, idl2
  IF typename(types) NE 'STRING' THEN BEGIN
     new_types = []
     foreach type, types DO new_types = [new_types, prits_tools.typename_from_typecode(type)]
  END
  par_type = size(parameter, /tname)
  valid = WHERE(par_type EQ types, Ngood)
  error = ''
  IF where(par_type EQ types) EQ -1 THEN BEGIN
     error = error_message
  ENDIF
END


PRO prits_tools::check_ndims, parameter, valid_ndims, error, error_message
  par_ndim = size(parameter, /n_dimensions)
  error = ''
  IF (where(par_ndim EQ valid_ndims))[0] EQ -1 THEN BEGIN
     error = error_message
  END
END 


PRO prits_tools::check_range, parameter, min, max, error
  IF n_elements(min) GT 1 THEN message, "MINVAL keyword of PRITS_TOOLS::PARCHECK must be scalar"
  IF n_elements(max) GT 1 THEN message, "MAXVAL keyword of PRITS_TOOLS::PARCHECK must be scalar"
  error = []
  IF n_elements(max) EQ 1 THEN BEGIN
     IF (where(parameter GT max))[0] NE -1 THEN error = 'is larger than maximum value ' + trim(max)
  END
  IF n_elements(min) EQ 1 THEN BEGIN
     IF (where(parameter LT min))[0] NE -1 THEN error = 'is smaller than minimum value ' + trim(min)
  END
END


FUNCTION prits_tools::tnames_from_tnames, typenames
  unsigned = ['BYTE', 'UINT', 'ULONG', 'ULONG64']
  integers = [unsigned, 'INT', 'LONG', 'LONG64']
  real = ['FLOAT', 'DOUBLE']
  numeric = [unsigned, integers, real]
  multiplicative = [integers, floats, 'COMPLEX']
  new_typenames = []
  foreach typename, typenames DO BEGIN
     add = typename
     CASE typename OF 
        'UNSIGNED': add = unsigned
        'INTEGERS': add = integers
        'DECIMAL': add = floats
        'NUMERIC': add = numeric
        'MULTIPLICATIVE': add = multiplicative
     END
     new_typenames = [new_typenames, add]
  END
  return, new_typenames
END


FUNCTION prits_tools::typename_from_typecode, typecode
  IF size(typecode, /tname) EQ 'STRING' THEN return, typecode
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

  END
END


PRO prits_tools::parcheck, parameter, parnum, name,  types, valid_ndims, default=default, $
                             maxval=maxval,minval=minval, $
                             result=result
  compile_opt idl2, static
  
  IF n_params() EQ 1 AND n_elements(default) NE 0 THEN BEGIN
     IF n_elements(parameter) EQ 0 THEN parameter = default
     return
  END
  
  IF n_elements(parameter) EQ 0 THEN BEGIN
     err = 'is undefined and no default has been specified'
     GOTO, ABORT
  ENDIF
   
  IF N_params() LT 5 THEN BEGIN
     on_error, 2
     message, 'Use: PARCHECK, parameter, parnum, name, types, dimensions'
  END
  
  noerror = arg_present(result)
  result = ''
  
  pt = prits_tools()
  
  errors = []
  pt.check_ndims, parameter, valid_ndims, err, "has wrong number of dimensions"
  IF err NE '' THEN errors = [errors, err]
   
  pt.check_type, parameter, types, err, "is an invalid data type"
  IF err NE '' THEN errors = [errors, err]
    
  pt.check_range, parameter, minval, maxval, err
  IF err NE '' THEN errors = [errors, err]
  
  IF n_elements(errors) EQ 0 THEN return
   
ABORT:
   help,calls=callers
   caller=strupcase((str_sep(callers[1],' '))[0])

   IF parnum NE 0 THEN result = 'Parameter ' + trim(parnum) + ' ('+name+')' $
   else                result = 'Keyword ' + name + ' '
   
   result = [result + ' of routine ' + STRUPCASE(caller) + ' ' + errors]
   
   dimension_strings = trim(valid_ndims)
   dimension_string = strjoin(dimension_strings, ', ')
   result = [result,'Valid number of dimensions are: '+dimension_string]
   
   stype = ''
   FOR i = 0, N_elements( types )-1 DO BEGIN
      stype += pt.typename_from_typecode(types)
   END
   
   result = [result,'Valid types are: ' + stype]
   
   IF Keyword_SET(noerror) THEN RETURN
   PRINT,''                     ; Blank line
   FOR i=0,N_elements(result)-1 DO Print,"! " + result[i]
   print
   
   on_error, 2
   MESSAGE,"STOPPING, returning to checkpoint in " + caller + string([13b, 10b])
   
END
   
PRO prits_tools::parcheck_test
  compile_opt static
  prits_tools.parcheck,[5],2,"test",['BYTE'],[0, 5], result = result
  print, result, format='(a)'
END

IF getenv("USER") EQ "steinhh" THEN BEGIN
   add_path, "$HOME/idl/solo-spice-ql", /expand
   prits_tools.parcheck_test
END


END
