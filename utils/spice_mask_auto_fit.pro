
FUNCTION spice_mask_auto_fit, windata, mask_spec, template, wvl_select=wvl_select, $
                              outfile=outfile, chi2=chi2, $
                              FUNCTION_name=FUNCTION_name

;+
; NAME:
;     SPICE_MASK_AUTO_FIT
;
; PURPOSE:
;     Runs spice_auto_fit on a "mask spectrum", i.e., one created with
;     the routine spice_mask_spectrum with the specified fit template.
;
; CATEGORY:
;     SPICE; Gaussian fitting.
;
; CALLING SEQUENCE:
;     Result = SPICE_MASK_AUTO_FIT( Windata, Mask_Spec, Template )
;
; INPUTS:
;     Windata:  A structure in the format produced by
;               spice_getwindata.pro containing a SPICE data window. 
;     Mask_Spec: A structure in the format produced by
;                spice_mask_spectrum.pro containing a SPICE spectrum that
;                has been averaged over a spatial area.
;     Template:  A structure in the format produced by
;                eis_fit_template.pro containing a template for
;                fitting the mask spectrum.
;
; OPTIONAL INPUTS:
;     Wvl_Select:  A structure in the format created by
;                  eis_wvl_select.pro containing a specification for
;                  which part of the spectrum should be fitted.
;     Function_Name:  String giving the name of a fitting function to
;                     be used in place of a Gaussian.
;     Outfile:   The new name of a text file to which the results are
;                written. If outfile already exists, then the results
;                will be appended to the file.
;
; KEYWORD PARAMETERS:
;     None.
;
; OUTPUTS:
;     A structure in the format created by read_line_fits.pro
;     containing the fit parameters for the emission lines in the mask
;     spectrum.
;
; OPTIONAL OUTPUTS:
;     Chi2:  The reduced chi^2 value for the fit.
;
; CALLS:
;     SPICE_MASK2WINDATA, SPICE_AUTO_FIT, READ_LINE_FITS
;
; EXAMPLE:
;     IDL> wd=spice_getwindata(filename)
;     IDL> eis_fit_template,wd,template   ; create template
;     IDL> map=spice_make_image(filename,770.4)
;     IDL> mask=pixel_mask_gui(map)     ; choose pixel mask
;     IDL> spec=spice_mask_spectrum(filename,mask)
;     IDL> s=spice_mask_auto_fit(wd,spec,template)
;
; MODIFICATION HISTORY:
;     Ver.1, 08-Nov-2024, Peter Young
;       Adapted from eis_mask_auto_fit.
;-


IF n_params() LT 3 THEN BEGIN
  print,'Use:  IDL> result=spice_mask_auto_fit(windata,mask_spec,template [,wvl_select=, outfile= '
  print,'                                        function_name= ])'
  return,-1
ENDIF 



IF n_tags(wvl_select) NE 0 THEN ws=wvl_select

wdout=spice_mask2windata(windata,mask_spec)

;
; eis_auto_fit needs to operate on an array but I restrict it to just
; a 2x2 array to save time.
;
eis_auto_fit,wdout,fit,template=template,xrange=[0,1],yrange=[0,1],/quiet, $
             wvl_select=wvl_select, FUNCTION_name=FUNCTION_name


chi2=reform(fit.chi2[0,0])
n=fit.ngauss


;
; The results of fit are written to 'temporary_file.txt' and then read
; back into IDL with read_line_fits.pro. The file is then deleted.
; If outfile is specified, then the temporary file is still written
; and deleted, but the results are also sent to outfile. If outfile
; already exists, then the new results are appended to outfile. This
; mimics how spec_gauss_widget works.
;
output_file='temporary_file.txt'
chck=file_info(output_file)
IF chck.exists EQ 1  THEN file_delete,output_file
;
IF n_elements(outfile) NE 0 THEN BEGIN
  chck=file_info(outfile)
  IF chck.exists THEN BEGIN
    message,/info,/cont,'OUTFILE already exists. The results will be appended to this file.'
  ENDIF 
  output_file=[output_file,outfile]
  no_delete=1
ENDIF ELSE BEGIN
  no_delete=0
ENDELSE 
  

const=2*sqrt(2*alog(2))
IF fit.nback EQ 1 THEN BEGIN
  bg1=fit.aa[3*n]
  sig_bg1=fit.sigmaa[3*n]
  bg2=bg1
  sig_bg2=sig_bg1
ENDIF ELSE BEGIN
  bg1=fit.aa[3*n]
  sig_bg1=fit.sigmaa[3*n]
  bg2=fit.aa[3*n+1]
  sig_bg2=fit.sigmaa[3*n+1]
ENDELSE

nf=n_elements(output_file)
FOR j=0,nf-1 DO BEGIN 
  openw,lout,output_file[j],/get_lun,/append
  FOR i=0,n-1 DO BEGIN
    printf,lout,format='(2f12.4,2e12.3,2f12.4,2e12.4,2f12.4,4e12.4)', $
           fit.aa[i*3+1,0,0],fit.sigmaa[i*3+1,0,0], $
           fit.aa[i*3,0,0],fit.sigmaa[i*3,0,0], $
           fit.aa[i*3+2,0,0]*const,fit.sigmaa[i*3+2,0,0]*const, $
           fit.int[i,0,0],fit.interr[i,0,0], $
           fit.x_bg1[0,0], fit.x_bg2[0,0], $
           bg1,sig_bg1,bg2,sig_bg2
  ENDFOR 
  free_lun,lout
ENDFOR

read_line_fits,output_file[0],output,/eis

file_delete,output_file[0]

return,output

END
