# solo-spice-ql
Quicklook software for Solar Orbiter SPICE

## Includes:

- spice object 'spice_data'
```
    obj = spice_object(spice_file)
```
- spice_raster_browser
```
    spice_raster_browser, spice_file/spice_object [, quiet=quiet, yoffsets=yoffsets, $
        chunk_size=chunk_size, retina=retina, no_goes=no_goes]
```
- spice_xhwisker
```
    spice_xwhisker, spice_object, line [, group_leader=group_leader, ncolors = ncolors]
```
- spice_getwindata
```
    spice_getwindata, input_file, input_iwin [, keep_sat=keep_sat, clean=clean,
        wrange=wrange, ixrange=ixrange, normalize=normalize,
        calib=calib, perang=perang, verbose=verbose, quiet=quiet]
```

## TODO: 

- make it possible to view movies using ximovie (e.g. in xwhisker and/or directly in object)
- implement calibration in object and/or getwindata
- get some numbers for gain, yield and dark current for error calculation in getwindata
