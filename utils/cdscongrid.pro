; $Id: 2021-08-25 14:52 CEST $

;+
; NAME:
;	CDSCONGRID
;
; PURPOSE:
;       Shrink/expand the size of an array by an arbitrary amount.
;
; EXPLANATION:
;       This IDL procedure simulates the action of the VAX/VMS
;       CONGRID/CONGRIDI function.
;
;	This function is similar to "REBIN" in that it can resize a
;       one, two, or three dimensional array.   "REBIN", however,
;       requires that the new array size must be an integer multiple
;       of the original size.   CDSCONGRID will resize an array to any
;       arbitrary size (REBIN is somewhat faster, however).
;       REBIN averages multiple points when shrinking an array,
;       while CDSCONGRID just resamples the array.
;
; CATEGORY:
;       Array Manipulation.
;
; CALLING SEQUENCE:
;	array = CDSCONGRID(array, x, y, z)
;
; INPUTS:
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
; OPTIONAL INPUTS:
;       y:      The new Y dimension of the resized array.   If the original
;               array has only 1 dimension then y is ignored.   If the
;               original array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array.   If the original
;               array has only 1 or 2 dimensions then z is ignored.   If the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORD PARAMETERS:
;       INTERP: If set, causes linear interpolation to be used.
;               Otherwise, the nearest-neighbor method is used.
;
;	CUBIC:	If set, uses "Cubic convolution" interpolation.  A more
;		accurate, but more time-consuming, form of interpolation.
;		CUBIC has no effect when used with 3 dimensional arrays.
;
;       MINUS_ONE:
;               If set, will prevent CDSCONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   For example,
;               If the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               If MINUS_ONE is present (AND IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and (j-1)/(y-1).
;
; OUTPUTS:
;	The returned array has the same number of dimensions as the original
;       array and is of the same data type.   The returned array will have
;       the dimensions (x), (x, y), or (x, y, z) depending on how many
;       dimensions the input array had.
;
; PROCEDURE:
;       IF the input array has three dimensions, or if INTERP is set,
;       then the IDL interpolate function is used to interpolate the
;       data values.
;       If the input array has two dimensions, and INTERP is NOT set,
;       then the IDL POLY_2D function is used for nearest neighbor sampling.
;       If the input array has one dimension, and INTERP is NOT set,
;       then nearest neighbor sampling is used.
;
; EXAMPLE:
;       ; vol is a 3-D array with the dimensions (80, 100, 57)
;       ; Resize vol to be a (90, 90, 80) array
;       vol = CDSCONGRID(vol, 90, 90, 80)
;
; MODIFICATION HISTORY:
;       DMS, Sept. 1988.
;       DMS, Added the MINUS_ONE keyword, Sept. 1992.
; 	Daniel Carr. Re-wrote to handle one and three dimensional arrays
;                    using INTERPOLATE function.
; 	DMS, RSI, Nov, 1993.  Added CUBIC keyword.
;	SVHH, UiO, May 1994.   Included in CDS project with name 
;		changed from CONGRID. Also fixed the problem noted that
;		an interpolated image seemed shifted half a pixel if
;		interpolation was used.
;		Modified usage of CUBIC keyword in call to INTERPOLATE,
;		to enable use on IDL v 3.0
;-
FUNCTION cdsround,a
  return,fix(a+.5d)
END


FUNCTION cdsCongrid, arr, x, y,	z, Interp=int, Minus_One=m1, Cubic = cubic
  
  On_Error, 2		  ;Return to caller if error
  s = Size(arr)
  
  IF ((s(0) EQ 0) OR (s(0) GT 3)) THEN $
     Message, 'Array must have 1, 2, or 3 dimensions.'
  
;  Supply defaults = no interpolate, and no minus_one.
  IF N_elements(int) le	0 THEN int = 0 ELSE int	= Keyword_SET(int)
  IF N_elements(m1) le 0 THEN m1 = 0 ELSE m1 = Keyword_SET(m1)
  cub =	Keyword_SET(cubic)
  IF cub THEN int = 1	  ;Cubic implies interpolate
  
  
  CASE s(0) OF
      1: BEGIN				   ; *** ONE DIMENSIONAL ARRAY
	  srx =	float(s(1) - m1)/(x-m1)	* findgen(x)  ;subscripts
	  IF int THEN $
	     RETURN, INTERPOLATE(arr, srx, CUBIC = cub)	ELSE $
	     RETURN, arr(cdsROUND(srx))
      EndCASE
      2: BEGIN ; *** TWO DIMENSIONAL ARRAY
	  IF int THEN BEGIN
	      srx = float(s(1) - m1) / (x-m1) *	findgen(x) - .5	 ; CDS Hack SVHH
	      sry = float(s(2) - m1) / (y-m1) *	findgen(y) - .5	 ; 
	      IF strmid(!version.release,2,1) gt '0' THEN $
		   RETURN, INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cub)	$
	      ELSE RETURN, INTERPOLATE(arr, srx, sry, /GRID) ; Cubic's not allowed
	  EndIF	ELSE $
	    RETURN, POLY_2D(arr, $
		  [[0,0],[(s(1)-m1)/float(x-m1),0]], $ ;Use poly_2d
		  [[0,(s(2)-m1)/float(y-m1)],[0,0]],int,x,y)
	  
      EndCASE
      3: BEGIN ; *** THREE DIMENSIONAL ARRAY
	  srx =	float(s(1) - m1) / (x-m1) * findgen(x)
	  sry =	float(s(2) - m1) / (y-m1) * findgen(y)
	  srz =	float(s(3) - m1) / (z-m1) * findgen(z)
	  RETURN, interpolate(arr, srx,	sry, srz, /grid)
      EndCASE
  EndCASE
  
  RETURN, arr_r
END
