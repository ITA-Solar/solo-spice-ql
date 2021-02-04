; $Id: 2020-11-25 21:19 CET $
FUNCTION spice_keyword_info_header
  text = spice_inline_text()
;OBT_BEG =        646012990.301 /
;LEVEL   = 'L2      '           / Data processing level                          
;FILENAME= 'solo_L2_spice-n-exp_20200621T000329_V01_16777432-000.fits' / FI      '
;DATE-BEG= '2020-06-21T00:05:39.083' / [UTC] Beginning of data acquisition   
;STUDYTYP= 'Single Exposure'    / Sit-and-stare, Raster or Single Exposure       
;STUDYDES= 'Standard dark for cruise phase' / Description of the study           
;STUDY   = 'CAL_DARK_FS_SL04_1.0S_FD' / SPICE Study name                         
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
;OBS_ID  = 'SSPI_XXXX_000_000_oooo_001' / SOC Observation ID                     
;READMODE= 'Destructive'        / Destructive or non-destructive                 
;TRIGGERD= 'None    '           / Event that triggered observation               
;TARGET  = 'None    '           / Planned type of target                         
;SOOPNAME= 'None    '           / SOOP Campaign name(s)                          
;SOOPTYPE= '000     '           / SOOP Campaign name code(s)                     
;NRASTERS=                    1 / Number of planned rasters for this SPIOBSID    
;RASTERNO=                    0 / Raster number (starting at 0)                  
;STUDY_ID=                   54 / On-board Study ID slot (0-63)                  
;XSTART  =                    4 / [arcsec] Slit x offset rel. to S/C boresight   
;FOCUSPOS=                10601 / Focus position                                 
;NWIN    =                    2 / Total number of windows (incl. any dumbbells)  
;NWIN_PRF=                    2 / Number of windows not dumbbell or Intensity    
;NWIN_DUM=                    0 / Number of dumbbell windows                     
;NWIN_INT=                    0 / Number of Intensity-windows                    
;                                                                                
;
;DBLEXP  =                    0 / If set, double exposure is enabled             
;DBLEXPNO=                    0 / Applies only when DBLEXP=1                     
;DARKMAP =                    0 / If set, a dark map was subtracted on-board     
;BLACKLEV=                    0 / If set, a bias frame was subtracted on-board   
;CALMODE =                    0 / If set, file contains both un/decompressed data
;
;COMPRESS= 'Focal Uncompressed' / Compression description                        
;COMP_RAT=              1.00000 / Compression ratio decompressed/compressed      
;
;NSEGMENT=                    1 / Number of segments per window                  
;OBS_TYPE= 'oooo    '           / Unique code for OBS_MODE                       
;SPIOBSID=             16777434 / SPICE Observation ID                           
;DARKSPID=                   -1 / SPIOBSID of dark subtracted on-board           
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
;CROTA   =       -1.38515696166 / [deg] S/C counter-clockwise roll rel to Solar N
;                                                                                
;                                                                                
;           -------------------------------------                                
;           | STUDYFLG and its derived keywords |                                
;           -------------------------------------                                
;STUDYFLG=                    0 / Study flags                                    
;NOSPECTR=                    0 / Applies only to dumbbells                      
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
;DSUN_AU =       0.521442615073 / [AU] S/C distance from Sun                     
;SOLAR_B0=        6.53806062476 / [deg] Tilt angle of Solar North toward S/C     
;SOLAR_P0=        22.3339674130 / [deg] S/C Celestial North to Solar North angle 
;HGLT_OBS=        6.53806062476 / [deg] S/C Heliographic latitude (B0 angle)     
;HGLN_OBS=        73.8479461044 / [deg] S/C Heliographic longitude               
;  
;OBS_VR  =        3365.57054359 / [m/s] Radial velocity of S/C away from the Sun 
;  
;PRPROC1 = 'spice_prep_dark_offset_correction' / Name of procedure performing PRS
;PRPVER1 = '0.9     '           / Version of procedure PRPROC1                   
;PRMODE1 = 'dark corrected'     / Processing mode of step 1                      
;                                                                                
;PRPROC2 = 'spice_prep_flat_field_correction' / Name of procedure performing PRST
;PRPVER2 = '0.9     '           / Version of procedure PRPROC2                   
;PRMODE2 = 'flafield corrected' / Processing mode of step 2                      
;                                                                                
;PRPROC3 = 'spice_prep_distortion_correction' / Name of procedure performing PRST
;PRPVER3 = '0.9     '           / Version of procedure PRPROC3                   
;PRPARA3 = '2020-05-20'         / Parameters for PRPROC3                         
;-
  text = text[0 : -2]
  text = strmid(text, 1, 1000)
  return, [text, '']
END

FUNCTION spice_keyword_info,keywords,all=all
  header = spice_keyword_info_header()
  first_eight = (strmid(header, 0, 8)).trim()
  all_keywords = first_eight(where(first_eight NE ""))
  all_keywords = ['FIRST_RASTER', all_keywords]
  list = []
  foreach keyword, all_keywords DO BEGIN
     val = fxpar(header, keyword)
     datatype = size(val, /tname)
     CASE datatype OF
        "STRING" : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width: 15, type:"t"}
        "INT"    : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  3, type:"i"}
        "LONG"   : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  4, type:"i"}
        "DOUBLE" : info = {SPICE_KEYWORD_INFO, keyword: keyword, display_width:  5, type:"i"}
     END
     IF keyword EQ "FIRST_RASTER" OR keyword EQ "LEVEL" THEN BEGIN
        info.display_width = 2
        info.type = "t"
     END
     list = [list, info]
  END
  
  IF keyword_set(all) THEN keywords = list[*].keyword
  spice_default, keywords, list[*].keyword
  
  keyword_info_hash = orderedhash()
  foreach keyword, keywords, index DO BEGIN
     info = reform(list[where(list[*].keyword EQ keyword,count)])
     IF count NE 1 THEN message,"Huh? This shouldn't happen! Should be one and only one match!"
     keyword_info_hash[keyword] = info
  END
  return,keyword_info_hash
END


a = spice_keyword_info()
END
