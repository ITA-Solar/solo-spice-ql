;+
; Name        :
; SPICE_LINE_LIST
;
; Purpose     :
; This function return a list of predefined lines that should be
; fitted in the spectrum data. The line list has been taken from
; 
; A. Fludra et al., A&A 656, A38 (2021)
; https://doi.org/10.1051/0004-6361/202141221
; Table 1, except where stated differently
;
; Use         :
;       line_list = spice_line_list()
;
; Outputs     :
; A HASH table, with the wavelength in nm as keys, and the
; corresponding names as values.
;
; OPTIONAL OUTPUTS:
;     version :   Returns the version number of this software.
;
; Category    : SPICE, Fitting
;
; Written     : Martin Wiesmann, UIO, November 2022
;
; $Id: 2024-06-14 11:43 CEST $
;-
;
;----------------------------------------------------------


function spice_line_list, version=version

  version = 1 ; PLEASE increase this number when editing the code

  line_list = HASH( $

    ; Short Wavelength Channel
    70.03, 'Ar VII + S III', $ ; from Fig. 1
    70.28, 'O III', $ ; from Chianti, the 70.23 line in Fludra is a much weaker line
    70.38, 'O III', $
    70.60, 'Mg IX', $
    71.09, 'S III', $ ; from Fig. 1
    71.27, 'S VI', $ ; from Fig. 1
    71.38, 'Ar VIII', $ ; from Fig. 1
    71.85, 'O II', $
    74.49, 'S IV', $ ; from Fig. 1
    74.84, 'S IV', $ ; from Fig. 1
    75.02, 'S IV', $
    75.37, 'S IV', $ ; form Fig. 1
    76.04, 'O V', $
    76.52, 'N IV', $
    77.04, 'Ne VIII', $
    77.27, 'Mg VIII', $
    77.45, 'O V', $ ; from Fig. 1
    77.61, 'N II + S X', $ ; from Fig. 1
    78.03, 'Ne VIII', $
    78.23, 'Mg VIII', $ ; from Fig. 1
    78.65, 'S V', $
    78.77, 'O IV', $
    79.01, 'O IV', $ ; from Fig. 1

    ; Long Wavelength Channel
    97.25, 'H Ly gamma', $
    97.70, 'C III', $
    98.14, 'Fe III', $ ; from Fig. 1
    98.87, 'O I + Na VI', $ ; from Fig. 1
    98.98, 'N III', $
    99.16, 'N III', $
    99.74, 'Si III', $ ; from Fig. 1
    99.94, 'Fe III + O I', $ ; from Fig. 1
    101.03, 'Ne VI', $
    101.77, 'Fe III', $ ; from Fig. 1
    102.57, 'H Ly beta', $
    102.74, 'O I', $
    103.19, 'O VI', $
    103.60, 'C II', $
    103.70, 'C II', $
    103.76, 'O VI', $
    103.92, 'O I', $ ; from Fig. 1
    104.09, 'O I' $ ; from Fig. 1
    )

  return, line_list
end
