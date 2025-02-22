;+
; Project     : Hinode Science Data Centre Europe (Oslo SDC)
;
; Name        : REGEX_REPLACE
;
; Purpose     : Regexp replace function, global or not, with backreferences
;
; Explanation : Implements regular expression search and replace. Backreferences
;               are supported, i.e., $1 in the replacement string represents the
;               first subpattern, $2 is the 2nd, etc. Subpatterns are numbered
;               by counting opening parentheses from left to right.
;
;               The replacement is by default single-shot, but setting /global
;               makes it repeat until no replacements are left. It's possible to
;               end up with an infinite recursion if e.g. the pattern matches
;                the replacement.
;
; Use         : result = prits_tools.regex_replace(original,pattern,replacement)
;
; Inputs      : See "Use". Original may be an array of strings
;
; Opt. Inputs : None.
;
; Outputs     : Returns string with substitution(s) performed.
;
; Opt. Outputs: None.
;
; Keywords    : GLOBAL: repeat until no change
;
; Calls       : Only built-ins and itself
;
; Common      : None
;
; Restrictions: ?
;
; Side effects: None known.
;
; Categories  : STRING
;
; Prev. Hist. : None
;
; Written     : SVH Haugan, UiO, 2025-02-22
;
; History     : Version 1, 2025-02-22
;
; Contact     : prits-group@astro.uio.no
;-
FUNCTION prits_tools::regex_replace_arr, string_in, regex, replacement, global = global
  COMPILE_OPT STATIC
  result = string_in
  FOR i = 0, n_elements(string_in) - 1 DO BEGIN
    result[i] = prits_tools.regex_replace(string_in[i], regex, replacement, global = global)
  END
  return, result
END

FUNCTION prits_tools::regex_replace, string, regex, replacement, global = global
  COMPILE_OPT STATIC
  prits_tools.parcheck, string, 1, "string", 'STRing', 0
  prits_tools.parcheck, regex, 2, "regex", 'STRing', 0
  prits_tools.parcheck, replacement, 3, "replacement", 'STRing', 0

  IF size(string, /n_dim) GT 0 THEN $
    return, prits_tools.regex_replace_strarr(string, regex, replacement, global = global)

  replace_with = replacement
  match = [stregex(string, regex, /extract, /subexpr)]
  IF match[0] EQ "" THEN return, string

  FOR i = 1, n_elements(match) - 1 DO BEGIN
    back_reference = "$" + trim(i)
    replace_with = replace_with.replace(back_reference, match[i]) ; GLOBAL replace - ok
  END

  ; match[0] occurs AT LEAST ONCE, replace FIRST ONE ONLY (recurse if keyword_set(global))
  start_replace = strpos(string, match[0])
  start_string = strmid(string, 0, start_replace)
  ending = strmid(string, start_replace + strlen(match[0]), 10000000)
  final = start_string + replace_with + ending
  IF keyword_set(global) THEN BEGIN
    return, prits_tools.regex_replace(final, regex, replacement, global = global)
  END
  return, final
END

PRO prits_tools::regexp_test, in, expected
  COMPILE_OPT STATIC
  IF in NE expected THEN BEGIN
    print, in + " !!!!= " + expected
    print, "."
    stop
  END ELSE BEGIN
    print, "Ok: " + in
    print, "."
  END
END

prits_tools.regexp_test, prits_tools.regex_replace("abaa", "b.*", "-"), "a-"
prits_tools.regexp_test, prits_tools.regex_replace("aba", "a(b)a", "aa$1"), "aab"
prits_tools.regexp_test, prits_tools.regex_replace("abaaba", "a(b)a", "aa$1"), "aababa"
prits_tools.regexp_test, prits_tools.regex_replace("abaaba", "a(b)a", "aa$1", /global), "aaabba"
prits_tools.regexp_test, prits_tools.regex_replace("1234567", "(((1)(2)(3)))", "$5 $4 $3 $2 $1"), '3 2 1 123 1234567'
; Should never terminate:
print, "The final test will never terminate, you'll have to stop it manually."
prits_tools.regexp_test, prits_tools.regex_replace("1234567", "(((1)(2)(3)))", "$5 $4 $3 $2 $1", /global)
END
