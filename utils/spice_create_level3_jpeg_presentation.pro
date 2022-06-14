;+
; NAME:
;      SPICE_CREATE_LEVEL3_JPEG
;
; PURPOSE:
;      This function creates images out of level 3 data.
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      spice_create_level3_jpeg
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; CALLS:
;
; HISTORY:
;      Ver. 1, 13-Jun-2022, Martin Wiesmann
;
;-
; $Id: 2022-06-14 12:16 CEST $


PRO spice_create_level3_jpeg_presentation

  meta_data_file = '/Users/mawiesma/Documents/spice/generate_l3_meta_data.sav'
  meta_data_template = { $
    file:'', $
    winno:0, $
    extname:'', $
    category:0, $
    l3_created:0b, $
    l3_file:'', $
    image_small_created:0b, $
    image_large_created:0b $
  }

  restore, meta_data_file
  ind = where(meta_data.l3_created, ndata)
  
  for idata=0,ndata-1 do begin
    ana = fits2ana(meta_data[ind[idata]].l3_file, titles=titles)
    handle_value,ana.data_h,data,/no_copy
    handle_value,ana.fit_h,fit,/no_copy
    help,data
    help,fit
    stop
  endfor ; idata=0,ndata-1
END
