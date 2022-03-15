;+
; NAME:
;      VECTOR2POLYGON
;
; PURPOSE:
;      This function computes the convex hull, a polygon, that encloses all input points.
;      The QHULL procedure is used for this purpose (https://www.l3harrisgeospatial.com/docs/qhull.html).
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      polygon = VECTOR2POLYGON(x [, y, xpolygon=xpolygon, ypolygon=ypoligon] )
;
; INPUTS:
;      x: Either a 1-dimensional vector giving the x-coordinates of all points that should be in the polygon.
;         Or a 2-dimensional vector of size (2 x npoints) giving the coordinates of all points.
;      y: A 1-dimensional vector giving the y-coordinates of all points that should be in the polygon.
;         Will be ignored, if x is 2-dimensional
;
; KEYWORDS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;      A 2-dimensional vector of size (2 x nvertices), containing all the coordinates of the vertices of the polygon.
;      The vertices are ordered in pairs that form an edge of the polygon, i.e. [*,i] and [*,i+1] is such a pair,
;      where i is divisible by 2. The edges of the polygon are unordered.
;
; OPTIONAL OUTPUTS:
;      xpolygon: A 1-dimensional vector with the x-components of the vertices of the polygon.
;      ypolygon: A 1-dimensional vector with the y-components of the vertices of the polygon.
; 
; EXAMPLE USAGE:
;      points = fix(99*randomu(seed, 2, 33))
;      vector = vector2polygon(points, xpolygon=xpolygon, ypolygon=ypolygon)
;      plot, points[0,*], points[1,*], psym=7, color=230, xstyle=2, ystyle=2
;      for i=0,(size(vector))[2]/2-1 do begin
;        xvertices = [vector[0,2*i], vector[0,2*i+1]]
;        yvertices = [vector[1,2*i], vector[1,2*i+1]]
;        oplot, xvertices, yvertices, color=180
;      endfor
;
; HISTORY:
;      Ver. 1, 10-Mar-2022, Martin Wiesmann
;-
; $Id: 2022-03-15 11:34 CET $


FUNCTION vector2polygon, x_in, y_in, xpolygon=xpolygon, ypolygon=ypolygon

  size_x = size(x_in)
  size_y = size(y_in)
  IF size_x[0] eq 1 THEN BEGIN
    IF size_y[0] ne 1 THEN RETURN, -1   ; If x is 1-dimensional, y must also be 1-dimensional
    IF size_x[1] NE size_y[1] THEN RETURN, -1   ; x- and y-vector must be of same length
    x = x_in
    y = y_in
  ENDIF ELSE IF size_x[0] eq 2 THEN BEGIN
    IF size_x[1] NE 2 THEN RETURN, -1   ; If x is 2-dimensional, first dimension must be of size 2
    x = x_in[0,*]
    y = x_in[1,*]
  ENDIF ELSE BEGIN
    RETURN, -1   ; x must be 1- or 2-dimensional
  ENDELSE
  
  QHULL, x, y, hull
  
  size_hull = size(hull)
  xpolygon = intarr(size_hull[2]*2)
  ypolygon = xpolygon
  FOR ihull=0,size_hull[2]-1 DO BEGIN
    xpolygon[ihull*2] = x[hull[0, ihull]]
    ypolygon[ihull*2] = y[hull[0, ihull]]
    xpolygon[ihull*2+1] = x[hull[1, ihull]]
    ypolygon[ihull*2+1] = y[hull[1, ihull]]
  ENDFOR
  
  RETURN, transpose([[xpolygon], [ypolygon]])

END
