; $Id: 2020-11-25 21:19 CET $
FUNCTION spice_keyword_info_header
  text = spice_inline_text()
;NAXIS1  =                    1 /
;NAXIS2  =                    1 /
;NAXIS3  =                    1 /
;NAXIS4  =                    1 /
;OBT_BEG =        646012990.301 /
;LEVEL   = 'L2      '           / Data processing level                          
;FILENAME= 'solo_L2_spice-n-exp_20200621T000329_V01_16777432-000.fits' / FI      '
;DATE-BEG= '2020-06-21T00:05:39.083' / [UTC] Beginning of data acquisition  
;SPIOBSID=             16777434 / SPICE Observation ID 
;RASTERNO=                    0 / Raster number (starting at 0)                    
;STUDYTYP= 'Single Exposure'    / Sit-and-stare, Raster or Single Exposure
;MISOSTUD=                 1785 / Ground study ID used in MISO planning tool     
;STUDYDES= 'Standard dark for cruise phase' / Description of the study           
;STUDY   = 'CAL_DARK_FS_SL04_1.0S_FD' / SPICE Study name     
;OBS_DESC= 'Standard first light observations' / Observation description  
;PURPOSE = 'Calibration/Dark'   / Purpose of study (Science/Calibration/Checkout)
;XPOSURE =             0.600000 / [s] Total effective exposure time  
;
;CNAME1  = '            '       /  
;CRVAL1  =              1.00000 / 
;CDELT1  =              2.00000 /
;CUNIT1  = '            '       / 
;CTYPE1  = '            '       /
;                               /
;CNAME2  = '            '       /  
;CRVAL2  =              3.00000 / 
;CDELT2  =              4.00000 /
;CUNIT2  = '            '       / 
;CTYPE2  = '            '       /
;                               /
;AUTHOR  = 'Tim Grundy'         / Author of study  
;STP     =                  122 / SoLO Short-Term Plan number   
;DSUN_AU =       0.521442615073 / [AU] S/C distance from Sun    
;CROTA   =       -1.38515696166 / [deg] S/C counter-clockwise roll rel to Solar N
;OBS_ID  = 'SSPI_XXXX_000_000_oooo_001' / SOC Observation ID                     
;READMODE= 'Destructive'        / Destructive or non-destructive                                         
;SOOPNAME= 'None    '           / SOOP Campaign name(s)                          
;SOOPTYPE= '000     '           / SOOP Campaign name code(s)                     
;NRASTERS=                    1 / Number of planned rasters for this SPIOBSID    
;STUDY_ID=                   54 / On-board Study ID slot (0-63)       
;XSTART  =                    4 / [arcsec] Slit x offset rel. to S/C boresight   
;FOCUSPOS=                10601 / Focus position                                 
;NWIN    =                    2 / Total number of windows (incl. any dumbbells)  
;NWIN_PRF=                    2 / Number of windows not dumbbell or Intensity    
;NWIN_DUM=                    0 / Number of dumbbell windows                     
;NWIN_INT=                    0 / Number of Intensity-windows                    
;                                                                                
;
;STUDYFLG=                    0 / Study flags                                    
;NOSPECTR=                    0 / Applies only to dumbbells    
;DBLEXP  =                    0 / If set, double exposure is enabled             
;DBLEXPNO=                    0 / Applies only when DBLEXP=1                     
;DARKMAP =                    0 / If set, a dark map was subtracted on-board     
;BLACKLEV=                    0 / If set, a bias frame was subtracted on-board   
;CALMODE =                    0 / If set, file contains both un/decompressed data
;
;COMPRESS= 'Focal Uncompressed' / Compression description                        
;COMP_RAT=              1.00000 / Compression ratio decompressed/compressed      
;                        
;
;VERS_SW = '2101    '           / UiO SVN revision number of L2 pipeline         
;VERS_CAL= '2097    '           / UiO SVN revision number of calibration software
;VERSION = '01      '           / Incremental version number                     
;COMPLETE= 'C       '           / Complete data set                              
;
;SLIT_ID =                    2 / Slit ID (0-3)                                  
;SLIT_WID=                    4 / [arcsec] Slit width                            
;
;DATE    = '2020-07-24T12:28:08' / Date and time of FITS file creation           '
;                                                                                
;           ------------------------------------------------------               
;           | Other keywords valid for all Obs-HDUs in this file |               
;           ------------------------------------------------------               
;TELAPSE =       0.599999904633 / [s] Elapsed time between beg. and end of acqu. 
;CREATOR = 'SDP-SPICE'          / Name of pipeline                               
;                                                                                
;                                                                              
;           -----------------------------------------------                      
;           | Keywords valid for this HDU (WINDOW0_74.73) |                      
;           -----------------------------------------------                      
; 
; 
;COMPTYPE=                    4 / Compression type (0-7)                         
;COMPPARA=                    0 / Applies only to JPEG-compressed data           
;SHCFFTID=                    0 / Applies only to SHC-compressed data            
;  
;           ------------------                                                   
;           | Auxiliary data |                                                   
;           ------------------                                                   
;MIRRPOS =              65535.0 / [adu] Scan mirror position                     
;TN_FOCUS=              2109.00 / [adu]     SFM focus   temperature              
;TN_GRAT =              2119.00 / [adu]     SFM grating temperature              
;TN_SW   =              2793.00 / [adu]     HAS SW      temperature              
;TN_LW   =              2796.00 / [adu]     HAS LW      temperature              
;T_FOCUS =              9.89957 / [Celsius] SFM focus   temperature              
;T_GRAT  =              9.63802 / [Celsius] SFM grating temperature              
;T_SW    =             -20.3739 / [Celsius] HAS SW      temperature              
;T_LW    =             -20.4530 / [Celsius] HAS LW      temperature              
;TIMAQUTC= '2020-06-21T00:05:39.083' / [UTC] Start t. of data acquisition        
;                                                                                
;VN_MCPSW=                  549 / [adu] MCP SW voltage                           
;VN_MCPLW=                  547 / [adu] MCP LW voltage                           
;VN_GAPSW=                  551 / [adu] GAP SW voltage                           
;VN_GAPLW=                  548 / [adu] GAP LW voltage                           
;V_MCPSW =              17.5079 / [V]   MCP SW voltage                           
;V_MCPLW =              16.6819 / [V]   MCP LW voltage                           
;V_GAPSW =              58.7131 / [V]   GAP SW voltage                           
;V_GAPLW =              55.2639 / [V]   GAP LW voltage                           
;                                                                                
;                                                                                
;           ----------------------------------------------                       
;           | SOLARNET keywords, and additional keywords |                       
;           ----------------------------------------------                       
;PARENT  = 'solo_L1_spice-n-exp_20200621T000539_V01_16777434-000.fits' / L1 filen
;FILE_RAW= 'sc_2020_06_20.xml;sc_2020_06_21.xml' / Telemetry file                
;                                                                                
;           ----------------------------                                         
;           | Solar Ephemeris Keywords |                                         
;           ----------------------------                                         
                 
;SOLAR_B0=        6.53806062476 / [deg] Tilt angle of Solar North toward S/C     
;SOLAR_P0=        22.3339674130 / [deg] S/C Celestial North to Solar North angle 
;HGLT_OBS=        6.53806062476 / [deg] S/C Heliographic latitude (B0 angle)     
;HGLN_OBS=        73.8479461044 / [deg] S/C Heliographic longitude               
;  
;OBS_VR  =        3365.57054359 / [m/s] Radial velocity of S/C away from the Sun 
;  
;PRSTEP1 = 'DARK-SUBTRACTION'   / Type of processing, step 4                     
;PRPROC1 = 'spice_prep_dark_offset_correction.pro' / Name of procedure, step 4   
;PRPVER1 = '1.2     '           / Version of procedure, step 4                   
;PRPARA1 = 'dark_spiobsid=33554603' / Parameters for PRPROC4                     
;PRLIB1A = 'uio-spice-pipeline' / Software library containing PRPROC4 
;  
;PRSTEP2 = 'Y-BINNING'  / XML decoding, decompression if applicable, etc 
;PRPROC2 = 'spice_process_telemetry.pro' / Name of procedure, step 1             
;PRPVER2 = '02.01.01'           / Version of procedure, step 1      
;PRPARA2 = 'asdfasdfdsfdasf' / Parameters for PRPROC1 
;PRLIB2A = 'uio_spice_pipeline' / Software library containing PRPROC1
; 
;PRSTEP3 = 'LAMBDA-BINNING'  / XML decoding, decompression if applicable, etc 
;PRPROC3 = 'spice_process_telemetry.pro' / Name of procedure, step 1             
;PRPVER3 = '02.01.01'           / Version of procedure, step 1      
;PRPARA3 = 'asdfasdfdsfdasf' / Parameters for PRPROC1 
;PRLIB3A = 'uio_spice_pipeline' / Software library containing PRPROC1
;  
;PRSTEP4 = 'COMPRESSION'  / XML decoding, decompression if applicable, etc 
;PRPROC4 = 'spice_process_telemetry.pro' / Name of procedure, step 1             
;PRPVER4 = '02.01.01'           / Version of procedure, step 1 
;PRPARA4 = 'asdfasdfdsfdasf' / Parameters for PRPROC2 
;PRLIB4A = 'uio_spice_pipeline' / Software library containing PRPROC1
;  
;PRSTEP5 = 'TELEMETRY-PARSING'  / XML decoding, decompression if applicable, etc 
;PRPROC5 = 'spice_process_telemetry.pro' / Name of procedure, step 1             
;PRPVER5 = '02.01.01'           / Version of procedure, step 1 
;PRPARA5 = 'asdfasdfdsfdasf' / Parameters for PRPROC2 
;PRLIB5A = 'uio_spice_pipeline' / Software library containing PRPROC1                
;                                                                                
;PRSTEP6 = 'FLATFIELDING'       / Type of processing, step 5                     
;PRPROC6 = 'spice_prep_flat_field_correction.pro' / Name of procedure, step 5    
;PRPVER6 = '1.2     '           / Version of procedure, step 5                   
;PRPARA6 = 'ground-calibration flat field' / Parameters for PRPROC5              
;PRLIB6A = 'uio-spice-pipeline' / Software library containing PRPROC5            
;                                                                                
;PRSTEP7 = 'SPATIAL-SPECTRAL-DISTORTION-CORRECTION' / Type of processing, step 6 
;PRPROC7 = 'spice_prep_distortion_correction.pro' / Name of procedure, step 6    
;PRPVER7 = '2.1     '           / Version of procedure, step 6                   
;PRPARA7 = 'distortion correction matrix version: 2021-04-12' / Parameters for PR
;PRLIB7A = 'uio-spice-pipeline' / Software library containing PRPROC6            
;                                                                                
;PRSTEP8 = 'RADIOMETRIC-CALIBRATION' / Type of processing, step 7                
;PRPROC8 = 'spice_prep_radiometric_calibration.pro' / Name of procedure, step 7  
;PRPVER8 = '1.2     '           / Version of procedure, step 7                   
;PRPARA8 = 'based on comparison to QS SUMER spectrum' / Parameters for PRPROC7   
;PRLIB8A = 'uio-spice-pipeline' / Software library containing 
;  
;MIRRDELT=              0.00000 / Slope of linear fit to MIRRPOS                 
;SMIRRDEL=              0.00000 / Sigma of slope of linear fit to MIRRPOS 
;
;-
  text = text[0 : -2]
  text = strmid(text, 1, 1000)
  return, [text, '']
END

FUNCTION spice_keyword_get_info, header, keyword
     val = fxpar(header, keyword)
     datatype = size(val, /tname)
     CASE datatype OF
        "STRING" : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width: 15, type:"t", webcat_type:"t"}
        "INT"    : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  5, type:"i", webcat_type:"i"}
        "LONG"   : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  8, type:"i", webcat_type:"l"}
        "FLOAT"  : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  6, type:"i", webcat_type:"f"}
        "DOUBLE" : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  8, type:"i", webcat_type:"f"}
     END
     IF keyword EQ "FIRST_RASTER" OR keyword EQ "LEVEL" THEN BEGIN
        info.display_width = 2
        info.type = "t"
        info.webcat_type = "t"
     END
     IF keyword EQ "FILE_PATH" THEN BEGIN
        info.display_width = 15
        info.type = "t"
        info.webcat_type = "t"
     END
     IF keyword EQ "ICON_PATH" THEN BEGIN
        info.display_width = 15
        info.type = "t"
        info.webcat_type = "icon_base_path"
     END
     return, info
END

FUNCTION spice_keyword_info,requested_keywords,all=all
  header = spice_keyword_info_header()
  first_eight = (strmid(header, 0, 8)).trim()
  
  IF n_elements(requested_keywords) EQ 0 THEN BEGIN
     all_keywords = first_eight(where(first_eight NE ""))
     all_keywords = ['FIRST_RASTER', all_keywords]
     all_keywords = [all_keywords, 'FILE_PATH', 'ICON_PATH']
     requested_keywords = all_keywords
  END 
  
  keyword_info_hash = orderedhash()
  foreach keyword, requested_keywords DO BEGIN
     info = spice_keyword_get_info(header, keyword)
     keyword_info_hash[keyword] = info
  END
  
  return,keyword_info_hash
END

FUNCTION spice_keyword_info_as_json, requested_keywords, all=all
  infos = spice_keyword_info(requested_keywords, all=all)
  
  index = 0
  print, "{"
  foreach info, infos DO BEGIN
     print, '   "' + info.keyword + '" : ', format='(a,$)'
     print, ' { "type" : "' + info.webcat_type + '", ', format='(a,$)'
     print, '   "display_width" : ', format='(a,$)'
     print, info.display_width.toString() + ' }',  format='(a,$)'
     IF index++ LT n_elements(infos)-1 THEN print, ','
  END
  print
  print, "}"
END 

a = spice_keyword_info()
b = spice_keyword_info_as_json()
END
