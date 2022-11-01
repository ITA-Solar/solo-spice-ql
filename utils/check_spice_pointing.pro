;+
; Project     :	Solar Orbiter - SPICE
;
; Name        :	CHECK_SPICE_POINTING()
;
; Purpose     :	Checks if SPICE pointed at requested position
;
; Category    :	Database, Coordinates
;
; Explanation :	This routine searches the SPICE catalog to find observations
;               where the specified pointing falls within the SPICE
;               field-of-view at the specified date/time or range.
;
; Syntax      :	Result = CHECK_SPICE_POINTING(Date, SolarX, SolarY)
;
; Examples    :	Date = '2022-03-07T11:05'               ;Single date/time
;               Result = CHECK_SPICE_POINTING(Date, 2000.0, 0.0)
;
;               Date = ['2022-03-07T11', '2022-03-07T12']       ;One hour
;               Result = CHECK_SPICE_POINTING(Date, 2000.0, 0.0)
;
;               Date = ['2022-03-07', '2022-03-08']             ;One day
;               Result = CHECK_SPICE_POINTING(Date, 2000.0, 0.0)
;
; Inputs      :	DATE    = Date/time to search.  Can be a single value, or a
;                         range of values.  Note that if only the date is
;                         passed, the time is assumed to be '00:00:00'.  See
;                         the example above for how this affects searching over
;                         a range of dates.
;
;               SolarX, SolarY = Location in arcseconds of the feature to be
;                                searched for.
;
; Opt. Inputs :	None
;
; Outputs     :	The result of the function is a list of filenames matching the
;               search criteria.  If no matches are found, the null string is
;               passed.
;
; Opt. Outputs:	None
;
; Keywords    :	COUNT   = Returns the number of matching files found.
;
;               CATFILE = Name of the catalog file to read in.  Defaults to
;                         $SPICE_DATA/spice_catalog.csv.
;
;               MARGIN  = Margin in arcseconds to apply to the SPICE raster
;                         values.  Default is no margin.
;
;               CARRINGTON = If set, then the pointing parameters to check are
;                            Carrington longitudes and latitudes instead of
; 			     arcseconds.
;
;		STONYHURST = If set, then the pointing parameters to check are
; 			     heliographic longitude (relative to the central
; 			     meridian) and latitude.  Overrides /CARRINGTON.
;
;               EARTH      = If set, then the pointing parameters to check are
;                            arcseconds as seen from Earth.  (Uses Carrington
;                            option.)  Overrides /STONYHURST or /CARRINGTON.
;
;               Note that the /CARRINGTON, /STONYHURST, or /EARTH options can
;               be slow when searching over large time periods.
;
;               DATEREF    = Reference date/time for the STONYHURST and EARTH
;                            options.  If not passed, then the central time
;                            given by the DATE parameter is used.
;
;		ERRMSG  = If defined and passed, then any error messages 
;			  will be returned to the user in this parameter 
;			  rather than being handled by the IDL MESSAGE 
;			  utility.  If no errors are encountered, then a 
;			  null string is returned.  In order to use this 
;			  feature, the string ERRMSG must be defined 
;			  first, e.g.,
;
;			      ERRMSG = ''
;			      RESULT = CHECK_SPICE_POINTING(ERRMSG=ERRMSG, ... )
;			      IF ERRMSG NE '' THEN ...
;
; Calls       :	CHECK_SPICE_POINTING_EXTRACT, ANYTIM2TAI, CONCAT_DIR, UTC2TAI,
;               TAI2UTC
;
; Common      :	CHECK_SPICE_POINTING is used internally to store the catalog
;               information for future calls.
;
; Restrictions:	Requires the file $SPICE_DATA/spice_catalog.csv.  Only Level 2
;               files are returned.
;
;               The /CARRINGTON, /STONYHURST, and /EARTH options requires the
;               SunSPICE package, and the ephemeris software and data in the
;               $SSW/so/gen tree.
;
; Side effects:	There is a delay the first time this routine is called to read
;               in the catalog.  Subsequent calls will be much faster.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 20-Oct-2022, William Thompson, GSFC
;               Version 2, 21-Oct-2022, WTT, added CARRINGTON option
;               Version 3, 26-Oct-2022, WTT, add STONYHURST, EARTH, DATEREF,
;                       and MARGIN keywords.
;
; Contact     :	WTHOMPSON
;-
;
;------------------------------------------------------------------------------
;
pro check_spice_pointing_extract, name, variable, wgood, errmsg=errmsg
;
common check_spice_pointing, header, catalog
;
;  Find the requested variable NAME within HEADER.
;
w = where(header eq strupcase(name), count)
if count eq 0 then begin
    message = 'Variable ' + name + ' not found.'
    if n_elements(errmsg) eq 0 then message, message
    errmsg = message
    return
endif
;
;  Extract out the values for the requested variable.
;
variable = catalog.(w[0])
;
;  If WGOOD is passed, then extract those indices within the array.
;
if n_elements(wgood) gt 0 then variable = variable[wgood]
;
end
;
;------------------------------------------------------------------------------
;
function check_spice_pointing, date, solarx, solary, count=count, $
                               catfile=catfile, carrington=carrington, $
                               stonyhurst=stonyhurst, earth=earth, $
                               dateref=dateref, margin=k_margin, errmsg=errmsg
;
common check_spice_pointing, header, catalog
;
carrington_set = keyword_set(carrington)
stony_or_earth = keyword_set(stonyhurst) or keyword_set(earth)
;
;  Check the input parameters.
;
if n_params() ne 3 then begin
    message = 'Syntax: Result = CHECK_SPICE_POINTING(Date, SolarX, SolarY)'
    goto, handle_error
endif
;
if (n_elements(date) lt 1) or (n_elements(date) gt 2) then begin
    message = 'Date must have one or two values.'
    goto, handle_error
endif
;
if (n_elements(solarx) ne 1) or (n_elements(solary) ne 1) then begin
    message = 'SolarX and SolarY must be scalars.'
    goto, handle_error
endif
;
case n_elements(k_margin) of
    0: margin = 0
    1: margin = k_margin
    else: begin
        message = 'MARGIN must be a scalar'
        goto, handle_error
    end
endcase
;
if n_elements(dateref) gt 1 then begin
    message = 'DATEREF must be a scalar'
    goto, handle_error
endif
;
;  Convert the DATE(s) into TAI values.
;
message = ''
tai = anytim2tai(date, errmsg=message)
if message ne '' then goto, handle_error
taimin = min(tai, max=taimax)
;
;  If the SPICE catalog file has not yet been read, then read it.
;
if n_elements(header) eq 0 then begin
    if n_elements(catfile) ne 1 then begin
        spice_data = getenv('SPICE_DATA')
        if spice_data eq '' then begin
            message = 'Environment variable SPICE_DATA not defined'
            goto, handle_error
        endif
        catfile = concat_dir(spice_data, 'spice_catalog.csv')
    endif
;
    if not file_exist(catfile) then begin
        message = 'File $SPICE_DATA/spice_catalog.csv does not exist'
        goto, handle_error
    endif
;
    catalog = read_csv(catfile, header=header)
endif
;
;  Extract out DATE-BEG and LEVEL.  Filter out entries with missing date
;  values, and data levels other than L2.
;
message = ''
check_spice_pointing_extract, 'DATE-BEG', date_beg, errmsg=message
check_spice_pointing_extract, 'LEVEL', level, errmsg=message
if message ne '' then goto, handle_error
wgood = where((date_beg ne 'MISSING') and (level eq 'L2'))
date_beg = date_beg[wgood]
;
;  Convert DATE-BEG from UTC to TAI.
;
message = ''
tai_beg = utc2tai(date_beg, errmsg=message)
if message ne '' then goto, handle_error
;
;  Extract out TELAPSE, and use it to calculate an end time.
;
check_spice_pointing_extract, 'TELAPSE', telapse, wgood, errmsg=message
if message ne '' then goto, handle_error
tai_end = tai_beg + telapse
date_end = tai2utc(tai_end, /ccsds)
;
;  Find observations which fall within the requested date range.  If none
;  found, then return the null string.
;
w = where((taimax ge tai_beg) and (taimin le tai_end), count)
if count eq 0 then return, ''
;
;  Filter out entries which don't fall within the requested date range.
;
wgood = wgood[w]
tai_beg = tai_beg[w]
tai_end = tai_end[w]
date_beg = date_beg[w]
date_end = date_end[w]
;
;  Extract out the filenames and pointing values.
;
message = ''
check_spice_pointing_extract, 'FILENAME', filename, wgood, errmsg=message
check_spice_pointing_extract, 'CROTA', crota, wgood, errmsg=message
check_spice_pointing_extract, 'NAXIS1', naxis1, wgood, errmsg=message
check_spice_pointing_extract, 'NAXIS2', naxis2, wgood, errmsg=message
check_spice_pointing_extract, 'CRVAL1', crval1, wgood, errmsg=message
check_spice_pointing_extract, 'CRVAL2', crval2, wgood, errmsg=message
check_spice_pointing_extract, 'CDELT1', cdelt1, wgood, errmsg=message
check_spice_pointing_extract, 'CDELT2', cdelt2, wgood, errmsg=message
check_spice_pointing_extract, 'SLIT_WID', slit_wid, wgood, errmsg=message
if message ne '' then goto, handle_error
;
;  Calculate the corners of the SPICE field-of-view.
;
width1 = ((naxis1-1) * cdelt1 + slit_wid) / 2 + margin
width2 = (naxis2 * cdelt2) / 2 + margin
x0 = crval1 - width1
x1 = crval1 + width1
y0 = crval2 - width2
y1 = crval2 + width2
;
;  Start by assuming that the input parameters are HPC coordinates as seen from
;  Solar Orbiter, but preserve the possibility that they might be heliographic
;  coordinates.
;
hgln = solarx
hglt = solary
solx = solarx
soly = solary
;
;  If either the STONYHURST or EARTH keywords were set, then convert to
;  Carrington coordinates.
;
if stony_or_earth then begin
    message = ''
    if n_elements(dateref) eq 1 then begin
        utc = anytim2utc(dateref, errmsg=message)
        if message ne '' then goto, handle_error
    end else utc = tai2utc((taimin + taimax) / 2)
;
    lonlat = get_sunspice_lonlat(utc, 'Earth', system='Carrington', /degrees, $
                                 /meters, errmsg=message)
    if message ne '' then goto, handle_error
;
;  If the EARTH keyword was set, then convert to Stonyhurst coordinates.
;
    if keyword_set(earth) then begin
        wcs_conv_hpc_hg, solx/3600.d0, soly/3600.d0, hgln, hglt, $
                         dsun_obs=lonlat[0], date_obs=utc, /degrees
    endif
;
;  Convert from Stonyhurst to Carrington coordinates.
;
    hgln = (hgln + lonlat[1]) mod 360
    carrington_set = 1
endif
;
;  If any of the CARRINGTON, STONYHURST, or EARTH keywords were set, then
;  convert the Carrington coordinates into Helioprojective Cartesian
;  coordinates at the central times of the observations.
;
if carrington_set then begin
    rsun = wcs_rsun(units='km')
    tai = (tai_beg + tai_end) / 2
    solx = dblarr(n_elements(tai))
    soly = solx
;
    for i=0,n_elements(tai)-1 do begin
        coord = [rsun, hgln, hglt]
        convert_sunspice_lonlat, tai[i], coord, 'Carrington', 'HPC', /degrees, $
                                 spacecraft='Solar Orbiter'
        solx[i] = coord[1] * 3600
        soly[i] = coord[2] * 3600
    endfor
endif
;
;  Rotate the requested position into the SPICE orientation.
;
crota = crota * !dpi / 180.d0
cosa = cos(crota)
sina = sin(crota)
xx = solx * cosa + soly * sina
yy = soly * cosa - solx * sina
;
;  Determine where the requested position falls within the SPICE
;  field-of-view.  If none are found, then return the null string.
;
w = where((xx ge x0) and (xx le x1) and (yy ge y0) and (yy le y1), count)
if count eq 0 then return, ''
;
;  Return the filenames.
;
return, filename[w]
;
;  Error handling point.
;
handle_error:
if n_elements(errmsg) eq 0 then message, message
errmsg = message
count = 0
return, ''
;
end
