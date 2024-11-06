

function spice_mask_spectrum, l1name, mask, $
                              sum=sum


;+
; NAME:
;     SPICE_MASK_SPECTRUM
;
; PURPOSE:
;     Creates a 1D SPICE spectrum that has been averaged over a pixel mask.
;
; CATEGORY:
;     SPICE; spectral fitting.
;
; CALLING SEQUENCE:
;     Result = SPICE_MASK_SPECTRUM( File, Mask )
;
; INPUTS:
;     File:  A SPICE level-2 filename.
;     Mask:  A 2D image of same size as the X-Y image of the SPICE raster
;            that contains 0's and 1's. A value of 1 indicates the pixel will
;            be included in the mask.
;
; KEYWORD PARAMETERS:
;     SUM:  If set, then the spectra will summed over the pixel mask rather
;           than averaged.
;
; OUTPUTS:
;     An IDL structure with the following tags:
;      .wvl  Wavelength array (Ang)
;      .int  Intensity array (erg/cm2/s/sr/Ang)
;      .err  Error array (erg/cm2/s/sr/Ang)
;      .qual Data quality array
;      .qualmax  Maximum value of data quality
;
;     Data quality gives the number of spatial pixels used in the averaging
;     for a particularly wavelength pixel. For example, if the mask contains
;     60 pixels then qualmax=60 and if a wavelength pixel has 10 missing
;     spatial pixels (due to cosmic rays, for example), then qual=50.
;
;     The structure contains a number of additional tags containing metadata.
;
; EXAMPLE:
;     IDL> file=spice_find_file('28-may-2020 16:05')
;     IDL> map=spice_make_image(file,1031.9)
;     IDL> mask=pixel_mask_gui(map)
;     IDL> spec=spice_mask_spectrum(file,mask)
;     IDL> spec_gauss_spice,spec    ; for fitting lines in spec
;
; MODIFICATION HISTORY:
;     Ver.1, 31-Oct-2024, Peter Young
;-

; $Id: 2024-11-06 10:25 EST $



IF n_params() LT 2 THEN BEGIN
  print,'Use:  IDL> result = spice_mask_spectrum( file, mask [, /sum ] )'
  return,-1
ENDIF


IF n_tags(mask) NE 0 THEN swtch=mask.image ELSE swtch=mask


d=spice_data(l1name)
nwin=d->get_number_windows()
date_obs=d->get_header_keyword('DATE-OBS',0)
slit_wid=d->get_header_keyword('SLIT_WID',0)
obj_destroy,d



chck=0
yip=-1
FOR i=0,nwin-1 DO BEGIN
 ;
  wd=spice_getwindata(l1name,i,/quiet)
 ;
  nl=wd.nl
  int=make_array(nl,/float,value=wd.missing)
  err=make_array(nl,/float,value=wd.missing)
  qual=make_array(nl,/int,value=0)
  FOR j=0,nl-1 DO BEGIN
    int_img=reform(wd.int[j,*,*])
    err_img=reform(wd.err[j,*,*])
   ;
    k=where(swtch EQ 1,nk)
    IF nk GT 0 THEN BEGIN
      qual[j]=nk
      IF keyword_set(sum) THEN BEGIN 
        int[j]=total(int_img[k])
        err[j]=sqrt( total( err_img[k]^2 ) )
      ENDIF ELSE BEGIN
        int[j]=average(int_img[k])
        err[j]=sqrt( total( err_img[k]^2 ) ) / float(nk)
      ENDELSE 
    ENDIF
  ENDFOR

  IF n_elements(all_int) EQ 0 THEN BEGIN
    all_wvl=wd.wvl
    all_int=int
    all_err=err
    all_qual=qual
  ENDIF ELSE BEGIN
    all_wvl=[all_wvl,wd.wvl]
    all_int=[all_int,int]
    all_err=[all_err,err]
    all_qual=[all_qual,qual]
  ENDELSE 
ENDFOR

;
; Sometimes the wavelength windows are not in wavelength order.
;
k=sort(all_wvl)
all_wvl=all_wvl[k]
all_int=all_int[k]
all_err=all_err[k]
all_qual=all_qual[k]

output={wvl: all_wvl, $
        int: all_int, $
        err: all_err, $
        qual: all_qual, $
        qual_max: nk, $
        wvl_units: 'Ang', $
        int_units: wd.units, $
        date_obs: date_obs, $
        slit_wid: slit_wid, $
        instrument: 'SPICE', $
        filename: file_basename(l1name), $
        missing: wd.missing, $
        time_stamp: systime() }

return,output

END
