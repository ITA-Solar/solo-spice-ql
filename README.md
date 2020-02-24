# SPICE Quicklook and Data Analysis Software
## Currently planned software
The plan is to provide approximately the same functionality for SPICE data as what is already available for IRIS/EIS data. Much of this is achieved by ensuring that SPICE data are stored and handled in IDL in a similar way to data from IRIS/EIS. Nevertheless, significant changes to the existing software may be necessary to account for differences in instrument data.
Below is a top-level listing of functionalities, in a rough and preliminary prioritized order, with reference to corresponding heritage routines in parentheses (where appropriate). Note that not all software will necessarily be available for the first remote sensing window, hence the prioritization is important.
In general, all functionalities in the heritage routines will be preserved as far as technically possible.

* SPICE object (iris_data)
    * Implemented, continuous improvement
    ```
    obj = obj_new('spice_data', spice_file)
    obj->help
    ```
* Routine to read SPICE FITS file into an object (iris_obj)
    * Done
    ```
    obj = spice_object(spice_file)
    obj->help
    ```
* A 3D data-cube viewer (iris_raster_browser)
    * Initial working version
    * Dumbbells are shown
    ```
    spice_raster_browser, spice_file/spice_object [, quiet=quiet, yoffsets=yoffsets, $
        chunk_size=chunk_size, retina=retina, no_goes=no_goes]
    ```
* Whisker plot viewer (iris_xwhisker)
    * Initial working version
    * Animation with ximovie doesn’t work yet, might be a bigger change
    ```
    spice_xwhisker, spice_object, line [, group_leader=group_leader, ncolors = ncolors]
    ```
* Detector data extraction (iris_getwindata)
    * Initial working version
    * Keywords _keep_sat_, _calib_ and _perang_ are ignored for now
    ```
    data = spice_getwindata, spice_file/spice_object, window_index [, keep_sat=keep_sat, $
        clean=clean, wrange=wrange, ixrange=ixrange, normalize=normalize, $
        calib=calib, perang=perang, verbose=verbose, quiet=quiet]
    ```
* Detector viewer (iris_xdetector)
    * Initial working version
    * Animation with ximovie doesn’t work yet, might be a bigger change
    * Some bugs to fix:
        * crashes with file 3 windows 4 and higher (data 1D, i.e. Intensity window)
        * crashes with file 2, binned window, and dumbbells flipped x-axis?
    ```
    spice_xdetector, spice_object, window_indices [, group_leader=group_leader, ncolors = ncolors]
    ```
* Spectroheliogram viewer (iris_xraster)
* Intensity map viewer (iris_xmap)
* Interactive line fitting (xcfit)
* “Masking” - averaging spectra over spatial regions (eis_mask_spectrum/pixel_mask_gui)
* S/W assisting in organization of SPICE files on user’s computer (iris_ingest, iris_find_file)
* GUI for selection of files and quicklook tools (iris_xfiles, iris_xcontrol)
* Display EUI/PHI images similar to IRIS SJI images (iris_raster_browser)
* Display STIX data similar to GOES data (iris_raster_browser)

## Level-3 data products
Level-2 data for SPICE will be fully-calibrated data-sets, with instrumental effects removed. Level-3 data products will be physical quantities derived from integrated emission line intensities. For example, temperature, density and FIP bias maps. The first step in generating such maps will be automatic Gaussian fitting of the observed emission lines. This will be done with the CFIT software in Solarsoft.
Level 3 data products will be produced in the standard data pipeline, and the set of products will be determined by the SPICE consortium. It will be possible for users to run the pipeline, starting at level 1, with modified parameters, flatfields, etc. when desired.
The output format for Level 3 data will be fits files, with individual data products stored in separate extensions that may be read using standard fits software. Direct products from line fits (intensity/velocity/widths) will be viewable and modifiable using XCFIT.
Specialised routines for displaying secondary derived products such as temperature maps, etc., are not planned at this point.


## TODO: 

- make it possible to view movies using ximovie (e.g. in xwhisker, xdetector and/or directly in object)
- implement calibration in object and/or getwindata
- get some numbers for gain, yield and dark current for error calculation in getwindata
- check position keywords for axis, when those are displayed in pixels (PXBEGx, PXENDx)
- check for windows with binned data, including Intensity windows

## For Developers
This repository includes a pre-commit git hook, that updates a specific line of each modified file with the current date and time. The line with this format will be edited:
```
; $Id: 24.02.2020 20:37 CET $
```
If the file you modified, does not contain this line yet, please add it, preferably append it to the procedure description at the beginning of the file. 

To make git aware of this hook, run this command after cloning the repository:
```
cd path_of_repository
git config --local core.hooksPath .githooks/
chmod +x .githooks/*
```
Git will then run the script _./githooks/pre-commit_ every time you commit something. This script will check each modified file whether there is a line with the above format, and if yes, updates date and time and adds these changes to the commit.
