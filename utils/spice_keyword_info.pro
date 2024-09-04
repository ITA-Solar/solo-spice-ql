; $Id: 2024-09-04 15:41 CEST $
FUNCTION spice_keyword_info_header
  text = inline_text()
;NAXIS1  =                    1 /
;NAXIS2  =                    1 /
;NAXIS3  =                    1 /
;NAXIS4  =                    1 /
;LEVEL   = 'L2      '           / Data processing level                          
;FILENAME= 'solo_L2_spice-n-exp_20200621T000329_V01_16777432-000.fits' / FI      '
;DATE-BEG= '2020-06-21T00:05:39.083' / [UTC] Beginning of data acquisition  
;SPIOBSID=             16777434 / SPICE Observation ID 
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
;  
;
;CRDER1  =       0.163271873730 / [arcsec] Mean stddev of Solar X
;CRDER2  =       0.182844822694 / [arcsec] Mean stddev of Solar Y
;CWERR1  =        6.99129430802 / [arcsec] Max absolute distortion, Solar X
;CWERR2  =       0.467100419833 / [arcsec] Max absolute distortion, Solar Y

;
;WAVECOV = '70.01061-70.790746, 71.580633-72.068218, 75.57883-76.358966, 76.232'
;PXCOV3  = '46-109, 209-240, 626-689, 695-726, 752-783, 914-977, 1137-1168, 171'
;
;AUTHOR  = 'Tim Grundy'         / Author of study  
;STP     =                  122 / SoLO Short-Term Plan number   
;DSUN_AU =       0.521442615073 / [AU] S/C distance from Sun    
;CROTA   =       -1.38515696166 / [deg] S/C counter-clockwise roll rel to Solar N
;OBS_ID  = 'SSPI_XXXX_000_000_oooo_001' / SOC Observation ID                     
;READMODE= 'Destructive'        / Destructive or non-destructive               
;OBJECT  = 'Sun     '           / Type of object observed  
;TARGET  = 'on disk, disc centre' / Course human interpretable pointing info 
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
;NWIN_ORG=                   12 / Number of windows before merging adjacent win.
;                                                                                
;DATE-AVG= '2024-08-16T15:37:11.520' / [UTC] Data acquisition midpoint           
;DATE-END= '2024-08-16T15:37:41.320' / [UTC] End of data acquisition  
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
;PCT_CMPL=              100.000 / Completeness of data set, all windows combined 
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
;                                                                                                                                                 
;           ----------------------------------------------                       
;           | SOLARNET keywords, and additional keywords |                       
;           ----------------------------------------------                       
;PARENT  = 'solo_L1_spice-n-exp_20200621T000539_V01_16777434-000.fits' / L1 filen
;FILE_RAW= 'sc_2020_06_20.xml;sc_2020_06_21.xml' / Telemetry file                
;                                                                                
;                                     
;                 
;SOLAR_B0=        6.53806062476 / [deg] Tilt angle of Solar North toward S/C     
;SOLAR_P0=        22.3339674130 / [deg] S/C Celestial North to Solar North angle 
;HGLT_OBS=        6.53806062476 / [deg] S/C Heliographic latitude (B0 angle)     
;HGLN_OBS=        73.8479461044 / [deg] S/C Heliographic longitude               
;  
;OBS_VR  =        3365.57054359 / [m/s] Radial velocity of S/C away from the Sun 
;  
;  
;WIN_TYPE= 'Narrow-slit Spectral' / Description of window type                   
;DATAPROD= 'Narrow-slit Spectral Raster' / WIN_TYPE+STUDYTYP                                               
;WINSHIFT=                   -7 / [pixel] Win redshift rel to win 3228 base pos.                                                                             
;                                                                                
;BTYPE   = 'Spectral Radiance'  / Type of data                                           
;BUNIT   = 'W/m2/sr/nm'         / Physical units of calibrated data              
;RADCAL  =        580.340433669 / [DN/(W m-2 sr-1 nm-1)] Average calibration
;NTOTPIX =               970160 / Number of potentially usable pixels excl padded
;NDATAPIX=               970156 / Number of usable pixels excl padded/NaN/NSATPIX
;NSATPIX =                    4 / Number of pixels set to NaN due to saturation  
;NLOSTPIX=                    0 / Number of NaN pix excl. padded and saturated   
;NAPRXPIX=                    0 / Number of approx. pix. b.f. geo. corr., now NaN
;                                                                                
;PCT_DATA=              99.9996 / NDATAPIX/NTOTPIX*100                           
;PCT_SATP=          0.000412303 / NSATPIX/ NTOTPIX*100                           
;PCT_LOST=              0.00000 / NLOSTPIX/NTOTPIX*100                           
;PCT_APRX=              0.00000 / NAPRXPIX/NTOTPIX*100   
;  
;DATAMIN =             0.100467 / [W/m2/sr/nm] Minimum data value                
;DATAMAX =              9.02325 / [W/m2/sr/nm] Maximum data value                
;DATAMEAN=             0.299003 / [W/m2/sr/nm] Mean    data value                
;DATAMEDN=             0.302962 / [W/m2/sr/nm] Median  data value                
;DATAP01 =             0.220780 / [W/m2/sr/nm] 1st  percentile of data values    
;DATAP10 =             0.251163 / [W/m2/sr/nm] 10th percentile of data values    
;DATAP25 =             0.280052 / [W/m2/sr/nm] 25th percentile of data values    
;DATAP75 =             0.317071 / [W/m2/sr/nm] 75th percentile of data values    
;DATAP90 =             0.331278 / [W/m2/sr/nm] 90th percentile of data values    
;DATAP95 =             0.344359 / [W/m2/sr/nm] 95th percentile of data values    
;DATAP98 =             0.369050 / [W/m2/sr/nm] 98th percentile of data values    
;DATAP99 =             0.399471 / [W/m2/sr/nm] 99th percentile of data values    
;DATARMS =            0.0387133 / [W/m2/sr/nm] sqrt(sum((data-DATAMEAN)^2)/N)    
;DATANRMS=             0.129474 / Normalised RMS dev: DATARMS/DATAMEAN           
;DATAMAD =            0.0252133 / [W/m2/sr/nm] MeanAbsDev sum(abs(data-DATAMEAN))
;DATASKEW=              19.2785 / Data skewness                                  
;DATAKURT=              3206.85 / Data kurtosis                                  '                                                                                       
;
;OBT_BEG =        646012990.301 /
;RASTERNO=                    0 / Raster number (starting at 0)  
;POINT_ID= '00001730'           / SVO pointing ID                                
;MOSAICID= '        '           / Blank when study is not a mosaic               
;SVO_GRP = '00001730'           / SVO file group ID, =POINT_ID when not in a SOOP  
; 
;PXBEG2  =                  101 / [pixel] First read-out pixel in Y dimension    
;PXEND2  =                  868 / [pixel] Last  read-out pixel in Y dimension    
;PXBEG3  =                   82 / [pixel] First read-out pixel in dispersion dim.
;PXEND3  =                  113 / [pixel] Last  read-out pixel in dispersion dim.                           
;                                                                                                
;NBIN2   =                    1 / Binning factor in Y dimension                  
;NBIN3   =                    2 / Binning factor in dispersion dimension                       
;NBIN    =                    2 / Total binning factor  
;
;           ------------------                                                   
;           | Auxiliary data |                                                   
;           ------------------                                                   
;VAR_KEYS= 'VARIABLE_KEYWORDS;TIMAQOBT,MIRRPOS,TN_FOCUS,TN_GRAT,TN_SW,TN_LW,T_F'
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
;PRSTEP1 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC1 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER1 = '1.1     '           / Version of procedure, step 9                   
;PRPARA1 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF1  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB1A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER1A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;   
;PRSTEP2 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC2 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER2 = '1.1     '           / Version of procedure, step 9                   
;PRPARA2 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF2  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB2A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER2A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP3 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC3 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER3 = '1.1     '           / Version of procedure, step 9                   
;PRPARA3 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF3  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB3A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER3A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP4 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC4 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER4 = '1.1     '           / Version of procedure, step 9                   
;PRPARA4 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF4  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB4A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER4A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP5 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC5 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER5 = '1.1     '           / Version of procedure, step 9                   
;PRPARA5 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF5  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB5A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER5A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP6 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC6 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER6 = '1.1     '           / Version of procedure, step 9                   
;PRPARA6 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF6  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB6A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER6A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP7 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC7 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER7 = '1.1     '           / Version of procedure, step 9                   
;PRPARA7 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF7  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB7A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER7A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP8 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC8 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER8 = '1.1     '           / Version of procedure, step 9                   
;PRPARA8 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF8  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB8A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER8A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP9 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC9 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER9 = '1.1     '           / Version of procedure, step 9                   
;PRPARA9 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF9  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB9A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER9A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP10 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC10 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER10 = '1.1     '           / Version of procedure, step 9                   
;PRPARA10 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF10  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB10A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER10A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP11 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC11 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER11 = '1.1     '           / Version of procedure, step 9                   
;PRPARA11 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF11  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB11A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER11A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP12 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC12 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER12 = '1.1     '           / Version of procedure, step 9                   
;PRPARA12 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF12  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB12A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER12A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
;
;PRSTEP13 = 'BURN-IN-CORRECTION' / Type of processing, step 9                     
;PRPROC13 = 'spice_prep_burnin_correction.pro' / Name of procedure, step 9        
;PRPVER13 = '1.1     '           / Version of procedure, step 9                   
;PRPARA13 = 'burn-in correction data version: 2023-06-15' / Parameters for
;PRREF13  = 'burn_in_correction_data_version = "2023-11-09",                    '
;PRLIB13A = 'uio-spice-pipeline' / Software library containing PRPROC9      
;PRVER13A =                 4730 / UiO SVN revision number of PRLIB3 (2024-08-21) 
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

IF getenv("USER") EQ "steinhh" THEN BEGIN 
   a = spice_keyword_info()
   b = spice_keyword_info_as_json()
   print
   box_message, ['spice_keyword_info.json goes in sdc/roslo/vol/spice/fits/', $
                 'and is auto-generated by spice_gen_cat.pro']
END 
END
