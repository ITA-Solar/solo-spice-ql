;+
; Project     : PRITS-TOOLS
;
; Name        : PRITS_TOOLS.CHECK_EQUALITY
;
; Purpose     : Checks if a variable is equal to another variable. The variable may
;               be an array, in that case the output is also an array of same size of type byte.
;               This function can also perform the check if the reference variable
;               is not a finite number.
;               A tolerance value can be provided by which the variable may maximally differ from the
;               reference value and still be considered equal.
;
;               YOU MAY BE BETTER SERVED BY USING THE FUNCTIONS:
;               WHERE_MISSING, WHERE_NOT_MISSING, IS_MISSING, IS_NOT_MISSING
;               because those are already in the 'gen' branch of solarsoft. But those are less flexible.
;
; Use         : result = PRITS_TOOLS.CHECK_EQUALITY( VARIABLE, REFERENCE_VALUE [, /NANorINF] [, /SIGN] $
;                             [, TOLERANCE=TOLERANCE] )
;
; Inputs      : VARIABLE : The variable to be checked against the reference_value.
;               REFERENCE_VALUE : The reference value that the variable shold be checked against.
;                                 This may also be +/-NAN or +/-INF.
;
; Opt. Inputs : TOLERANCE : A value that the variable may maximally differ from the reference value and still
;                           be considered to be equal. This keyword is ignored if NANorINF is set.
;
; Outputs     : Boolean. True if the variable is the same, False otherwise. If input variable is an array
;                        the output is also an array of same size of type byte.
;
; Opt. Outputs: None.
;
; Keywords    : NANorINF : If set, then the output is only True if the variable is of the same
;                          infinite type (NAN or INF) than the reference variable.
;               SIGN : If set, then the output is only True if the variable has the same sign (+ or -)
;                      as the reference variable. This keyword is ignored if NANorINF is not set.
;
; Calls       : None.
;
; Common      : None.
;
; Restrictions: None.
;
; Side effects: None.
;
; Category    : Utility, Misc.
;
; Written     : Martin Wiesmann, 24.5.2034
;
; Modified    : Never
;
; $Id: 2024-11-21 11:37 CET $
;-

FUNCTION PRITS_TOOLS::CHECK_EQUALITY, variable, reference_value, tolerance = tolerance, NANorINF = NANorINF, sign = sign
  COMPILE_OPT STATIC

  IF n_params() LT 2 THEN message, "Use: result = PRITS_TOOLS.CHECK_EQUALITY( VARIABLE, REFERENCE_VALUE[, /NANorINF] [, /SIGN] )"
  prits_tools.parcheck, variable, 1, "VARIABLE", 'numeric', [0, 1, 2, 3, 4, 5, 6, 7, 8]
  prits_tools.parcheck, reference_value, 2, "REFERENCE_VALUE", 'numeric', 0
  prits_tools.parcheck, tolerance, 0, "TOLERANCE", 'numeric', 0, /optional

  IF finite(reference_value) THEN BEGIN
    result = fix(variable, type = 1)
    result[*] = 0
    IF keyword_set(tolerance) THEN BEGIN
      ind = where(variable GE reference_value - tolerance AND variable LE reference_value + tolerance, count)
    ENDIF ELSE BEGIN
      ind = where(variable EQ reference_value, count)
    ENDELSE
    IF count GT 0 THEN result[ind] = 1
  ENDIF ELSE BEGIN
    IF keyword_set(NANorINF) THEN BEGIN
      is_NAN = finite(reference_value, /NAN)
      is_INF = ~is_NAN
      IF keyword_set(sign) THEN BEGIN
        IF finite(reference_value, NAN = is_NAN, INFINITY = is_INF, SIGN = 1) THEN sign_value = 1 ELSE sign_value = -1
      ENDIF
      result = finite(variable, NAN = is_NAN, INFINITY = is_INF, SIGN = sign_value)
    ENDIF ELSE BEGIN
      result = ~finite(variable)
    ENDELSE
  ENDELSE

  return, result
END

PRO PRITS_TOOLS::CHECK_EQUALITY_test
  COMPILE_OPT STATIC

  result = prits_tools.check_equality(3, 3)
  IF ~result THEN print, 'test 1 failed'

  result = prits_tools.check_equality([3, 0, 9, 3], 3)
  IF total(result) NE 2 THEN print, 'test 2.1 failed'

  a = intarr(3, 5, 6, 2, 9)
  a[1, 2, 3, *, 2 : 4] = 3
  result = prits_tools.check_equality(a, 3)
  IF total(result) NE 6 THEN print, 'test 2.2 failed'

  result = prits_tools.check_equality([3.05, 0, 9, 3.1], 3, tolerance = 0.08)
  IF total(result) NE 1 THEN print, 'test 2.3 failed'

  a = fltarr(10)
  ; Set some values to +/-NaN and positive or negative Infinity:
  a[3] = !VALUES.f_nan
  a[4] = -!VALUES.F_NAN
  a[6] = !VALUES.f_infinity
  a[7] = -!VALUES.F_INFINITY
  result = prits_tools.check_equality(a, a[3])
  IF total(result) NE 4 THEN print, 'test 3 failed'

  result = prits_tools.check_equality(a, a[3], /NANorINF)
  IF total(result) NE 2 THEN print, 'test 4 failed'

  result = prits_tools.check_equality(a, a[6], /NANorINF)
  IF total(result) NE 2 THEN print, 'test 5 failed'

  result = prits_tools.check_equality(a, a[3], /NANorINF, /SIGN)
  IF result[3] NE 1 THEN print, 'test 6 failed'

  result = prits_tools.check_equality(a, a[4], /NANorINF, /SIGN)
  IF result[4] NE 1 THEN print, 'test 7 failed'

  result = prits_tools.check_equality(a, a[6], /NANorINF, /SIGN)
  IF result[6] NE 1 THEN print, 'test 8 failed'

  result = prits_tools.check_equality(a, a[7], /NANorINF, /SIGN)
  IF result[7] NE 1 THEN print, 'test 8 failed'
END

IF getenv("USER") EQ "steinhh" || getenv("USER") EQ "mawiesma" THEN BEGIN
  prits_tools.CHECK_EQUALITY_test
ENDIF

END