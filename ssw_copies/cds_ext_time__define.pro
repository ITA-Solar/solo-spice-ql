;+
; NAME:
;      CDS_EXT_TIME
;
; PURPOSE:
;      This is the definition of the CDS External time structure.
;      Referred to as "EXT" in any routine names.  A structure
;		   containing the elements, YEAR, MONTH, DAY, HOUR, MINUTE,
;		   SECOND, and MILLISECOND as shortword integers.
;
; CATEGORY:
;      GEN -- utility -- time
;
; HISTORY:
;      Ver. 1, 13-Nov-2024, Martin Wiesmann
;-
; $Id: 2024-11-26 13:50 CET $

PRO cds_ext_time__define
  ; idl-disable-next-line unused-var
  void = {cds_ext_time, $
    year: 0, $
    month: 0, $
    day: 0, $
    hour: 0, $
    minute: 0, $
    second: 0, $
    millisecond: 0}
END
