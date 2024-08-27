;+
; Name        :
; SPICE_LINE_LIST
;
; Purpose     :
; This function return a list of predefined lines that should be
; fitted in the spectrum data. The line list (spice_get_lines_all) has 
; been taken from
; 
; A. Fludra et al., A&A 656, A38 (2021)
; https://doi.org/10.1051/0004-6361/202141221 :
;
; and
;
; A. Giunta  "Lines observable
; with SPICE in different solar conditions" presentation in Goettingen 2024.
;
; Where the wavelength differ I use Giunta's values most of the time, after
; checking with actual observations
;
;
; The updated and much shorter line list (spice_get_lines_strongest) has been made
; from a selection of lines based on actual fitted lines and Alessandra's line
; list.
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
; $Id: 2024-08-27 14:05 CEST $
;-
;
;----------------------------------------------------------


function spice_get_lines_all, version=version

  version = 2 ; PLEASE increase this number when editing the code

  line_list = HASH( $

    ; Short Wavelength Channel
    70.03, 'Ar VII + S III', $ 
    70.28, 'O III',          $ ; Giunta
    70.38, 'O III',          $ 
    70.60, 'Mg IX',          $ 
    71.85, 'O II',           $ 
    74.84, 'S IV',           $ 
    75.02, 'S IV',           $ 
    75.87, 'O V',            $ 
    76.03, 'O V',            $ 
    76.20, 'O V',            $ ; Giunta          
    76.51, 'N IV',           $ 
    77.04, 'Ne VIII',        $ 
    77.23, 'Mg VIII',        $ 
    77.45, 'O V',            $ 
    77.61, 'N II + S X',     $ 
    78.03, 'Ne VIII',        $ 
    78.23, 'Mg VIII',        $ 
    78.65, 'S V',            $ 
    78.77, 'O IV',           $ 

    ; Long Wavelength Channel
    97.25, 'H Ly gamma',   $  
    97.70, 'C III',        $  
    98.87, 'O I + Na VI',  $  
    98.98, 'N III',        $
    99.16, 'N III',        $
  ;;99.48, 'Si III',       $ ; Chianti. Too weak to include, but it's there!      
    99.74, 'Si III',       $ 
    99.94, 'Fe III + O I', $ 
    101.03, 'Ne VI',       $
    102.57, 'H Ly beta',   $
    102.74, 'O I',         $
    103.19, 'O VI',        $
    103.60, 'C II',        $
    103.70, 'C II',        $
    103.76, 'O VI',        $
    103.92, 'O I',         $ 
    104.09, 'O I'          $ 
    )

  return, line_list
end


FUNCTION spice_get_lines_strongest, version=version
  
  version = 20                   ; PLEASE increase this number when editing the code
  
  
  line_list = HASH( $
              ; Short Wavelength Channel
              70.38, 'O III',   $
              70.60, 'Mg IX',   $
              75.02, 'S IV',    $
              76.04, 'O V',     $
              76.52, 'N IV',    $
              77.04, 'Ne VIII', $
              78.65, 'S V',     $
              78.77, 'O IV',    $
              
              
              ; Long Wavelength Channel
              97.25,  'H Ly gamma',  $
              97.70,  'C III',       $
              98.98,  'N III',       $
              101.03, 'Ne VI',       $
              102.57, 'H Ly beta',   $
              103.19, 'O VI',        $
              103.76, 'O VI',        $
              104.09, 'O I'          $ 
    )

  return, line_list
  
END


FUNCTION spice_line_list, version=version, strongest_lines=strongest_lines

  default, strongest_lines, 0
  
  line_list = (strongest_lines) ? spice_get_lines_strongest(version=version) : spice_get_lines_all(version=version)
  
  return, line_list
end
