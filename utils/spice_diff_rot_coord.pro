;+
; Project     :	Solar Orbiter - SPICE
;
; Name        :	SPICE_DIFF_ROT_COORD
;
; Purpose     :	Apply differential rotation to SPICE coordinates to account for
;               differential rotation.
;
; Category    :	Coordinates
;
; Explanation :	This routine uses the times for a SPICE raster to correct the
;               image coordinates for solar rotation as if all the data were
;               taken at the same time.
;
; Syntax      :	SPICE_DIFF_ROT_COORD
;
; Examples    :	Image = READFITS( Filename, Header, EXTEN_NO=EXTEN_NO)
;               WCS = FITSHEAD2WCS(Header)
;               Coord = WCS_GET_COORD(WCS)
;               SPICE_DIFF_ROT_COORD, WCS, Coord
;
; Inputs      :	WCS     = World Coordinate System structure derived from the
;                         FITS header.
;
; Opt. Inputs :	None
;
; Outputs     :	The first two dimensions of the variable COORD is modified to
;               take solar rotation into account.  The remaining dimensions are
;               unchanged.
;
; Opt. Outputs:	None
;
; Keywords    :	TARGET_WCS = WCS structure of an image that the SPICE data
;                            should be matched to, such as EUI or AIA
;
;               TRACKING   = If the spacecraft was performing feature tracking
;                            during the observation, then only the difference
;                            between the TARGET_TIME and DATE-AVG is used for
;                            calculating the differential rotation.
;
;               Also accepts keywords to the routine DIFF_ROT regarding which
;               rotation model to use.
;
; Calls       :	TAG_EXIST, ANYTIM2TAI, WCS_CONVERT_FROM_COORD, DIFF_ROT,
;               WCS_CONVERT_TO_COORD
;
; Common      :	None
;
; Restrictions:	When TARGET_WCS is used, it is recommended that the two
;               observations be close together in time.
;
; Side effects:	None
;
; Prev. Hist. :	None
;
; History     :	Version 1, 08-Oct-2024, William Thompson, GSFC
;               Version 2, 09-Oct-2024, William Thompson, GSFC
;                       Corrected sign of correction. Added TARGET_WCS keyword.
;               Version 3, 15-Oct-2024, William Thompson, GSFC
;                       Modified algorithm to use complete time array.
;
; Contact     :	WTHOMPSON
;-
;
PRO spice_diff_rot_coord, wcs, coord, tracking = tracking, $
  target_wcs = target_wcs, _extra = _extra
  ;
  ; Get the reference date.
  ;
  IF tag_exist(wcs.time, 'DATEREF') THEN dateref = wcs.time.dateref ELSE $
    dateref = wcs.time.observ_date
  ;
  ; If the target date was unspecified, then use the reference date.
  ;
  IF n_elements(target_wcs) NE 1 THEN target_time = wcs.time.observ_avg ELSE BEGIN
    date_avg = wcs_get_time(target_wcs, /avg, _extra = _extra)
    IF date_avg NE '' THEN target_time = date_avg
  ENDELSE
  ;
  ; Calculate the TAI times, and the change due to the spacecraft orbit,
  ; depending on whether /TRACKING is set or not.
  ;
  tai0 = anytim2tai(wcs.time.observ_avg)
  IF keyword_set(tracking) THEN BEGIN
    tai = tai0
  END ELSE BEGIN
    tai = anytim2tai(dateref) + reform(coord[3, *, *, *, *])
    utc0 = tai2utc(tai0)
    lonlat0 = get_sunspice_lonlat(utc0, 'solo', system = 'Carrington', /degrees)
    sz = size(tai)
    dlon = make_array(size = sz, /nozero)
    dlat = make_array(size = sz, /nozero)
    tai1 = all_vals(tai)
    utc1 = tai2utc(tai1)
    FOR i = 0, n_elements(utc1) - 1 DO BEGIN
      lonlat1 = get_sunspice_lonlat(utc1[i], 'solo', system = 'Carrington', $
        /degrees)
      w = where(tai1[i] EQ tai)
      dlon[w] = lonlat1[1, w] - lonlat0[1]
      dlat[w] = lonlat1[2, w] - lonlat0[2]
    ENDFOR
  ENDELSE
  ;
  ; Calculate the time difference in days.  If this would result in no change,
  ; then return.
  ;
  dd = (anytim2tai(target_time) - tai) / 86400.d0
  IF (n_elements(dd) EQ 1) AND (dd[0] EQ 0) THEN return
  ;
  ; Convert the HPC coordinates into Carrington longitude and latitude.
  ;
  wcs_convert_from_coord, wcs, coord, 'HG', lon, lat, /carrington
  ;
  ; Apply the differential rotation and the spacecraft motion.
  ;
  drot = diff_rot(dd, lat, /carrington, rigid = 0, synodic = 0, _extra = _extra)
  lon = lon + dlon + drot
  lat = lat + dlat
  ;
  ; Correct any latitudes that go beyond -90 to +90.
  ;
  w = where(lat GT 90, count)
  IF count GT 0 THEN BEGIN
    lat[w] = 180 - lat[w]
    lon[w] = lon[w] + 180
  ENDIF
  ;
  w = where(lat LT (-90), count)
  IF count GT 0 THEN BEGIN
    lat[w] = -180 - lat[w]
    lon[w] = lon[w] + 180
  ENDIF
  ;
  ; Convert the longitude and latitude back into HPC coordinates.  Only update
  ; coordinates that are on the disk.
  ;
  IF n_elements(target_wcs) EQ 1 THEN wcsout = target_wcs ELSE wcsout = wcs
  wcs_convert_to_coord, wcsout, coord1, 'HG', lon, lat, /carrington
  sz = size(coord)
  dim = sz[1 : sz[0]]
  nn = product(dim[1 : *])
  coord = reform(coord, sz[1], nn, /overwrite)
  sz1 = size(coord1)
  coord1 = reform(coord1, sz1[1], nn, /overwrite)
  w = where(finite(coord1[0, *]))
  coord[0, w] = coord1[0, w]
  coord[1, w] = coord1[1, w]
  coord = reform(coord, dim, /overwrite)
  ;
END
