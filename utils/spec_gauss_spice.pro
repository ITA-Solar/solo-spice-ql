
PRO spec_gauss_spice, specstr

;+
; NAME:
;     SPEC_GAUSS_SPICE
;
; PURPOSE:
;     A GUI for fitting Gaussians to a 1D SPICE spectrum. This routine is
;     a wrapper for calling the general purpose routine spec_gauss_widget.
;
; CATEGORY:
;     SPICE; spectral fitting.
;
; CALLING SEQUENCE:
;     SPEC_GAUSS_SPICE, SPEC
;
; INPUTS:
;     Specstr:  A structure in the format returned by spice_mask_spectrum.
;
; OUTPUTS:
;     A GUI will appear that allows Gaussians to be fit to the spectrum.
;     The fitting results will be written to the text file
;     'spec_gauss_fits.txt' in the working directory.
;
; EXAMPLE:
;     IDL> file=spice_find_file('28-may-2020 16:05')
;     IDL> map=spice_make_image(file,1031.9)
;     IDL> mask=pixel_mask_gui(map)
;     IDL> spec=spice_mask_spectrum(file,mask)
;     IDL> spec_gauss_spice,spec
;
; MODIFICATION HISTORY:
;     Ver.1, 31-Oct-2024, Peter Young
;-

; $Id: 2024-11-06 10:25 EST $


xx=specstr.wvl
yy=specstr.int
ee=specstr.err
qq=specstr.qual
IF tag_exist(specstr,'qual_max') THEN qual_max=specstr.qual_max

;
; This is the default width assumed for all EIS emission lines. If the
; user does not specify an initial guess for the width of the line,
; then def_width will be used.
;
def_width=0.70   ; angstroms

;; fname=getenv('SSW')+'/hinode/eis/idl/atest/pyoung/eis_line_ids.txt'
;; IF file_exist(fname) EQ 1 THEN BEGIN
;;   line_list={fname: fname, width:def_width, shift: shift}
;; ENDIF



spec_gauss_widget,xx,yy,ee,qq, $
                  def_width=def_width, qqmax=qual_max, $
                  angpix=1,/qqset, $
                  set_width_range=0, width_range=[0.5,0.15], $
                  parinfo_wvl_step=0.005

END
