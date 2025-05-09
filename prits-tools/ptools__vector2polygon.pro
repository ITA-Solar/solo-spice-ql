;+
; NAME:
;      VECTOR2POLYGON
;
; PURPOSE:
;      This function computes the convex hull, a polygon, that encloses all input points.
;      The QHULL procedure is used for this purpose (https://www.nv5geospatialsoftware.com/docs/qhull.html).
;
; CATEGORY:
;      SPICE -- utility
;
; CALLING SEQUENCE:
;      polygon = PTOOLS.VECTOR2POLYGON(x [, y] [, xpolygon=xpolygon] [, ypolygon=ypoligon] )
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
;      The vertices are ordered so that consecutive vertices are connected. The last vertex is connected to the first one.
;
; OPTIONAL OUTPUTS:
;      xpolygon: A 1-dimensional vector with the x-components of the vertices of the polygon.
;      ypolygon: A 1-dimensional vector with the y-components of the vertices of the polygon.
;
; EXAMPLE USAGE:
;      points = fix(99*randomu(seed, 2, 33))
;      vector = ptools.vector2polygon(points, xpolygon=xpolygon, ypolygon=ypolygon)
;
;      ; Plot method A, plotting each line
;      window, 0
;      plot, points[0,*], points[1,*], psym=7, color=230, xstyle=2, ystyle=2
;      for i=0,(size(vector))[2]-1 do begin
;        if i eq (size(vector))[2]-1 then ind=0 $
;        else ind=i+1
;        xvert = [vector[0,i], vector[0,ind]]
;        yvert = [vector[1,i], vector[1,ind]]
;        oplot, xvert, yvert, color=180
;      endfor
;
;      ; Plot method B, computing a mask
;      window, 1
;      obj = obj_new('IDLanROI', vector)
;      mask = obj->ComputeMask(dimensions=[100,100])
;      pih, mask
;      print, obj->ContainsPoints([[points], [102,103]])
;
;      ; Plot method C, filling the polygon
;      window, 2
;      polyfill, vector, color=100
;      oplot, points[0,*], points[1,*], psym=7, color=230
;
;
; HISTORY:
;      Ver. 1, 10-Mar-2022, Martin Wiesmann
;-
; $Id: 2025-05-09 13:28 CEST $

FUNCTION ptools::vector2polygon, x_in, y_in, xpolygon = xpolygon, ypolygon = ypolygon
  COMPILE_OPT IDL2, STATIC

  size_x = size(x_in)
  size_y = size(y_in)
  IF size_x[0] EQ 1 THEN BEGIN
    IF size_y[0] NE 1 THEN return, -1 ; If x is 1-dimensional, y must also be 1-dimensional
    IF size_x[1] NE size_y[1] THEN return, -1 ; x- and y-vector must be of same length
    x = x_in
    y = y_in
  ENDIF ELSE IF size_x[0] EQ 2 THEN BEGIN
    IF size_x[1] NE 2 THEN return, -1 ; If x is 2-dimensional, first dimension must be of size 2
    x = x_in[0, *]
    y = x_in[1, *]
  ENDIF ELSE BEGIN
    return, -1 ; x must be 1- or 2-dimensional
  ENDELSE

  qhull, x, y, hull

  size_hull = size(hull)
  xpolygon = intarr(size_hull[2])
  ypolygon = xpolygon
  xpolygon[0] = x[hull[0, 0]]
  ypolygon[0] = y[hull[0, 0]]
  ind = 0
  FOR i = 1, size_hull[2] - 1 DO BEGIN
    xpolygon[i] = x[hull[1, ind]]
    ypolygon[i] = y[hull[1, ind]]
    ind_new = where(hull[0, *] EQ hull[1, ind])
    ind = ind_new[0]
  ENDFOR

  return, transpose([[xpolygon], [ypolygon]])
END
