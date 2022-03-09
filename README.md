# SPICE Quicklook and Data Analysis Software

Find detailed information in our wikipage:   
https://github.com/ITA-Solar/solo-spice-ql/wiki

If you find any bugs, please report them to the PRITS group at the Institute of Theoretical Astrophysics:
Preferably by raising a new issue here: https://github.com/ITA-Solar/solo-spice-ql/issues

or else by mail to:
martin.wiesmann@astro.uio.no  
s.v.h.haugan@astro.uio.no  
terje.fredvik@astro.uio.no


## Table of Content

- [Quicklook Software](#quicklook-software)
- [Data Analysis Software](#data-analysis-software)
- [General Utilities](#general-utilities)
- [SPICE Data Object](#spice-data-object)
- [File Management Utilities](#file-management-utilities)
- [Currently Planned Software](#currently-planned-software)
- [Level 3 Data Products](#level-3-data-products)
- [For Developers](#for-developers)


## Quicklook Software

All quicklook software can be accessed through the GUI SPICE_XFILES. You can call it using this command:
```
IDL> spice_xfiles
```
This will open a window, which lets you search your local hard disk for spice files. Selecting a file will call SPICE_XCONTROL, which in turn opens a new window. In this window you'll get an overview of the content of the selected FITS file, and you can access the different quicklook software, which are:

* SPICE_XDETECTOR: This tool displays the windows superimposed on the whole SPICE detector.
* SPICE_RASTER_BROWSER: This routine is used to browse 3D SPICE data-cubes.
* SPICE_XRASTER: This tool displays all exposures of the selected windows in a row.
* SPICE_XWHISKER: This tool is used to display 2-D spectroscopic data as whisker plots (images), i.e. Intensity[wavelength, y]. FITS file must contain more than one exposure.
* SPICE_XMAP: This tool is used to display 2-D (or higher) data. FITS file must contain more than one exposure.


## Data Analysis Software

* SPICE wrapper for xcfit (Interactive line fitting)
    * Initial working version
    ```
    ana = spice_object->xcfit_block(window_index)
    ```
Coming soon:
* “Masking” - averaging spectra over spatial regions (à la eis_mask_spectrum/pixel_mask_gui)


## General Utilities

* SPICE_GETWINDATA: This routine returns the SPICE data structure for one spectral window. The format is chosen to copy the Hinode/EIS routine EIS_GETWINDATA.


## SPICE Data Object

We defined a SPICE data object, called SPICE_DATA, which can be initiated with a SPICE FITS file. This object contains many methods to handle the data and the header contained in the given FITS file. It can be created using this command:
```
IDL> obj = obj_new('spice_data', spice_file)
IDL> obj->help
```
The second line will print out all available methods of the object.

Alternatively, the object can be created using the function SPICE_OBJECT:
```
IDL> obj = spice_object(spice_file)
IDL> obj->help
```


## File Management Utilities

There are several useful tools to manage SPICE files:
* SPICE_INGEST: This tool will move your downloaded SPICE FITS files to the appropriate location in your $SPICE_DATA path.
* SPICE_INGEST_REORDER: This routine can be used to reorder your SPICE files.
* SPICE_FIND_FILE: This program is also used by SPICE_XFILES. It is a very flexible tool to search for files on your local hard drive.
* SPICE_CAT: This program allows filtering/sorting/selection of the contents in spice_catalog.txt which contains information about all available (not necessarily locally) SPICE FITS files.
* SPICE_GEN_CAT: This program creates a file called spice_catalog.txt in the $SPICE_DATA/ directory (but other paths can be specified), with various information on the content of the files found in the directory hierarchy below that path.
* SPICE_FILE2INFO: This routine takes as input a SPICE filename (or list of files) and extracts information from the filename.
* SPICE_REMOVE_OLD_FILES: Remove earlier versions of SPICE fits files.


## Currently Planned Software

The plan is to provide approximately the same functionality for SPICE data as what is already available for IRIS/EIS data. Much of this is achieved by ensuring that SPICE data are stored and handled in IDL in a similar way to data from IRIS/EIS. Nevertheless, significant changes to the existing software may be necessary to account for differences in instrument data.  
Below is a top-level listing of functionalities, in a rough and preliminary prioritised order, with reference to corresponding heritage routines in parentheses (where appropriate). Note that not all software will necessarily be available for the first remote sensing window, hence the prioritisation is important.  
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
    spice_xwhisker, spice_file/spice_object, line [, group_leader=group_leader, ncolors = ncolors]
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
    * binned images including intensity (1D) images expanded correctly
    * Dumbbells shown correctly
    * Animation with ximovie doesn’t work yet, might be a bigger change
    ```
    spice_xdetector, spice_file/spice_object, window_indices [, group_leader=group_leader, $
        ncolors = ncolors]
    ```
* Spectroheliogram viewer (iris_xraster)
    * Initial working version
    * Animation with ximovie doesn’t work yet, might be a bigger change
    ```
    spice_xraster, spice_file/spice_object, window_indices [, group_leader=group_leader, $
        ncolors = ncolors]
    ```
* Intensity map viewer (iris_xmap)
    * Initial working version
    ```
    spice_xmap, spice_file/spice_object [, linelist = window_index, group_leader=group_leader, $
        ncolors = ncolors]
    ```
* Interactive line fitting (xcfit)
    * Initial working version
    ```
    ana = spice_object->xcfit_block(window_index)
    ```
* “Masking” - averaging spectra over spatial regions (eis_mask_spectrum/pixel_mask_gui)
* S/W assisting in organization of SPICE files on user’s computer (iris_ingest, iris_find_file)
    * Implemented
    ```
    spice_ingest, Filename [, force=force, index=index, help=help]
    Result = spice_find_file(Time [, LEVEL=LEVEL, TOP_DIR=TOP_DIR, COUNT=COUNT, /SEQUENCE, /ALL, /QUIET ] )
    ```
* GUI for selection of files and quicklook tools (iris_xfiles, iris_xcontrol)
    * Implemented
    * spice_xcontrol will be called by spice_xfiles when user selects a file
    ```
    spice_xfiles
    ```
* Display EUI/PHI images similar to IRIS SJI images (iris_raster_browser)
* Display STIX data similar to GOES data (iris_raster_browser)


## Level 3 Data Products

Level 2 data for SPICE will be fully-calibrated data-sets, with instrumental effects removed. Level 3 data products will be physical quantities derived from integrated emission line intensities. For example, temperature, density and FIP bias maps. The first step in generating such maps will be automatic Gaussian fitting of the observed emission lines. This will be done with the CFIT software in Solarsoft.
Level 3 data products will be produced in the standard data pipeline, and the set of products will be determined by the SPICE consortium. It will be possible for users to run the pipeline, starting at level 1, with modified parameters, flatfields, etc. when desired.
The output format for Level 3 data will be fits files, with individual data products stored in separate extensions that may be read using standard fits software. Direct products from line fits (intensity/velocity/widths) will be viewable and modifiable using XCFIT.
Specialised routines for displaying secondary derived products such as temperature maps, etc., are not planned at this point.


## For Developers

This repository includes a pre-commit git hook, that updates a specific line of each modified file with the current date and time. The line with this format will be edited:
```
; $Id: 2022-03-09 13:26 CET $
```
If the file you modified, does not contain this line yet, please add it, preferably append it to the procedure description at the beginning of the file. 

To make git aware of this hook, run this command after cloning the repository:
```
cd path_of_repository
git config --local core.hooksPath .githooks/
chmod +x .githooks/*
```
Git will then run the script _./githooks/pre-commit_ every time you commit something. This script will check each modified and staged file whether there is a line with the above format, and if yes, updates date and time and adds these changes to the commit.
