      FUNCTION FMEDIAN,ARRAY,NW1,NW2,MISSING=MISSING_IN,ONLY_MISSING=ONLY_MISSING
;+
; Project     : SOHO - CDS     
;                   
; Name        : FMEDIAN
;               
; Purpose     : Median filtering w/rectangular neighbourhood & MISSING
;               
; Explanation : Performs median filtering. Differs from MEDIAN in that the
;               median filter extends smoothly to the edge of the array, and
;               in that different widths can be set for the X and Y
;               directions.
;               
; Use         : Result = FMEDIAN(ARRAY  [, NW1  [, NW2 ]])
;    
; Inputs      : ARRAY : Array to filter.
;               
; Opt. Inputs : NW1 : Width of the median filter in the first (X)
;                     direction. Default is 3.
;                 
;               NW2 : Width of the median filter in the second (Y) direction.
;                     Default is NW1. Ignored if ARRAY has only one dimension.
;               
; Outputs     : Returns an array with the same size and type as the
;               input array.
;               
; Opt. Outputs: None.
;               
; Keywords    : MISSING : Value signifying missing data. Missing pixels will
;                         not be included when calculating the median of the
;                         neighbourhood. If no valid pixels are found in the
;                         neighbourhood of a pixel, the corresponding median
;                         value will be set to MISSING.
;
;               ONLY_MISSING: Only pixels flagged as MISSING have to be
;                             replaced with the median (to speed up the
;                             processing when e.g. filling in gaps). This
;                             keyword does *not* have any effect when using
;                             the call_external version, but has been
;                             implemented in fmedian_slow and in the fmedian
;                             DLM.
;
; Env. Vars.  :	SSW_EXTERNAL_F = Points to a sharable object file containing
;			       associated Fortran software callable by
;			       CALL_EXTERNAL.  If this environment variable
;			       exists, then the routine uses CALL_EXTERNAL to
;			       calculate the checksum.  Otherwise the checksum
;			       is calculated within IDL, which is slower.
;
;			       For backwards compatibility, the software will
;			       also look for the environment variable
;			       CDS_EXTERNAL if it doesn't find SSW_EXTERNAL_F
;
;		SSW_EXTERNAL_PREFACE = On some operating systems, such as older
;			       versions of SunOS, this needs to be set to the
;			       underscore character "_".  Otherwise, it doesn't
;			       need to be defined.
;
; Calls       : FMEDIAN_SLOW, CALL_EXTERNAL("$SSW_EXTERNAL_F")
;
; Common      : None.
;               
; Restrictions: ARRAY must be either one or two dimensional.  Will call
;               FMEDIAN_SLOW if $SSW_EXTERNAL_F library is not found, causing a
;               severe slow-down of the routine (factor of 3 - 100, depending
;               on the size of the filter).
;               
; Side effects: Loads call_external library pointed to by $SSW_EXTERNAL_F.  If
;               this library cannot be found, an IDL version called
;               FMEDIAN_SLOW is used instead.
;               
; Category    : Utilities, Arrays
;               
; Prev. Hist. : SERTS routine.
;
; Written     : William Thompson, August 1991.
;               
; Modified    : Version 2, S.V.H.Haugan, UiO, 9 October 1996
;                       Added MISSING keyword, added a few parameter range
;                       checks. Added calll to FMEDIAN_SLOW when no
;                       SSW_EXTERNAL_F library is available. Supplying
;                       workspace to the fortran routine. Fortran routine
;                       using a quicksort-based median finder to keep ahead
;                       of IDL's fast median routine.
;		Version 3, William Thompson, GSFC, 16 July 1998
;			Look for SSW_EXTERNAL evar before CDS_EXTERNAL
;			Check for SSW_EXTERNAL_PREFACE instead of !version.os
;               Modified, 14-Feb-07, Zarro (ADNET) - commented out !DEBUG
;               Version 4, S.V.H.Haugan, UiO, 9 January 2008
;                       Added ONLY_MISSING keyword, passed on to fmedian_slow
;-            
          
;       ON_ERROR,2
;       IF !DEBUG NE 0 THEN ON_ERROR,0
;
;  Check the number of dimensions.
;
        SZ = SIZE(ARRAY)
        N_DIM = SZ(0)
        IF N_DIM EQ 0 THEN BEGIN
                MESSAGE,'Variable ARRAY must be an array'
        END ELSE IF N_DIM GT 2 THEN BEGIN
                MESSAGE,'ARRAY must be one or two-dimensional'
        ENDIF
             
        IF N_ELEMENTS(MISSING_IN) EQ 1 THEN MISSING = MISSING_IN $
        ELSE                                MISSING = MIN(ARRAY)-1
        
;
;  Get the filter parameters NW1 and NW2.
;
        IF N_PARAMS() EQ 1 THEN NW1 = 3
        IF N_PARAMS() LT 3 THEN NW2 = NW1
        IF N_DIM EQ 1 THEN NW2 = 1
        
;
; Get array sizes
;
        DIM = SZ(1:N_DIM)
        IDIM1 = DIM(0)
        IF N_DIM EQ 1 THEN IDIM2 = 1 ELSE IDIM2 = DIM(1)
        
;
;  Check ranges of filter parameters
;
        IF NW1 GT IDIM1 OR NW1 LT 1 THEN BEGIN
                MESSAGE,'NW1 OUT OF RANGE'
        END
        
        IF NW2 GT IDIM2 OR NW2 LT 1 THEN BEGIN
                MESSAGE,'NW2 OUT OF RANGE'
        END
        
;
;  Use IDL version if call_external library not found
;
	XTERNAL_EVAR = "SSW_EXTERNAL_F"
        XTERNAL_FILE = getenv("SSW_EXTERNAL_F")
        IF XTERNAL_FILE EQ "" THEN BEGIN
	    XTERNAL_EVAR = "CDS_EXTERNAL"
	    XTERNAL_FILE = getenv("CDS_EXTERNAL")
	ENDIF
        
        IF XTERNAL_FILE EQ "" THEN $
            RETURN, FMEDIAN_SLOW(ARRAY,NW1,NW2,MISSING=MISSING,$
                                ONLY_MISSING=ONLY_MISSING)
        
;
;  Set up defining array for output variable.
;  (Done later, when the data type has been fixed)
;        F = MAKE_ARRAY(DIMENSION=DIM,/LONG)
        
;
;  Chose data type (at least INTEGER*4).  Depending on the data type, call
;  either L_MEDIAN (default), F_MEDIAN or D_MEDIAN.  If one of the integer
;  types, then pass LONG(ARRAY) rather than simply ARRAY.
;
        TYPE = SZ(SZ(0) + 1)
        CASE TYPE OF
                3: BEGIN
                        MISSING = LONG(MISSING)
                        F = MAKE_ARRAY(SIZE=SZ,/NOZERO)
                        ROUTINE_NAME = "L_MEDIAN"
                        ARRAY_NAME = "ARRAY"
                        END
                4: BEGIN
                        F = MAKE_ARRAY(SIZE=SZ,/NOZERO)
                        MISSING = FLOAT(MISSING)
                        ROUTINE_NAME = "F_MEDIAN"
                        ARRAY_NAME = "ARRAY"
                        END
                5: BEGIN
                        F = MAKE_ARRAY(SIZE=SZ,/NOZERO)
                        MISSING = DOUBLE(MISSING)
                        ROUTINE_NAME = "D_MEDIAN"
                        ARRAY_NAME = "ARRAY"
                        END
                6: MESSAGE,'Operation not supported for complex variables.'
                7: MESSAGE,'Operation not supported for string variables.'
                8: MESSAGE,'Operation not supported for structures.'
                ELSE: BEGIN
                        MISSING = LONG(MISSING)
                        ROUTINE_NAME = "L_MEDIAN"
                        ARRAY_NAME = "LONG(ARRAY)"
                        SZ(SZ(0) + 1) = 3 ;; Make it a LONG type
                        F = MAKE_ARRAY(SIZE=SZ,/NOZERO)
                        END
        ENDCASE
;
; Make a workspace for sub-arrays
;
        WORKSIZE = NW1*NW2
        WORKSPACE = MAKE_ARRAY(WORKSIZE,TYPE=SZ(SZ(0) + 1),/NOZERO)
;
;  If Unix, then the routine name will have the form "name_c" instead of
;  "NAME".  If the environment variable SSW_EXTERNAL_PREFACE is set to the "_"
;  character, then this will be prepended to the name.  This is required in
;  some situations, such as older versions of SunOS.
;
	IF !VERSION.OS NE "vms" THEN ROUTINE_NAME =	$
		STRLOWCASE(ROUTINE_NAME) + "_c"
	ROUTINE_NAME = GETENV("SSW_EXTERNAL_PREFACE") + ROUTINE_NAME
;
;  Form the name of the sharable object file.
;
        
        IF !VERSION.OS EQ "vms" THEN FILENAME = XTERNAL_EVAR ELSE	$
		FILENAME = XTERNAL_FILE
;
;  Perform the median filter.
;
        COMMAND = "TEST1=CALL_EXTERNAL(FILENAME,ROUTINE_NAME,"+ARRAY_NAME+ $
           ",F,LONG(IDIM1),LONG(IDIM2),LONG(NW1),LONG(NW2),MISSING,WORKSPACE)"
        TEST2 = EXECUTE(COMMAND)
;
;  If the input array was INTEGER*1 or INTEGER*2, then convert back to that 
;  type from the intermediate INTEGER*4 result.
;
        IF TYPE EQ 1 THEN BEGIN
                F = BYTE(F)
        END ELSE IF TYPE EQ 2 THEN BEGIN
                F = FIX(F)
        ENDIF
;
        RETURN, F
        END





