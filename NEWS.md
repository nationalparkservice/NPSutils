# NPSutils 1.0.1 (development version)
## 2025-05-16
  * Add a vignette going over basic functions and how to use NPSutils
## 2025-05-08
  * add unit tests for all functions. Add packages necessary for unit tests to Suggests in DESCRIPTION file.

## 2025-03-25
  * fix bug that caused some functions to fail to detect certain .csv files
  
## 2025-03-12
  * Update to MIT license
## 2025-02-25
  * Update `CONTRIBUTING.md`
  * Update readme to remove mention of "borked" functions
  
## 2025-02-22
  * Add `CONTRIBUTING.md` file.

# NPSutils 1.0.0 
## 2025-01-22
  * Update license to CC0.
## 2025-01-19
  * remove `get_data_packages_deprecated()` is a breaking change resulting in release of v.1.0.0.  
  * update documentation for `get_unit_code()`, `get_park_code()`, and `get_unit_code_info()`

## 2024-12-19
  * remove `validate_data_package()` as this function was listed as "still under construction" is mostly obsolete given other functions and functions in the DPchecker package.
  * remove `load_domains()` as this function was not working properly and was conceived of before the data package specifications were properly set.
## 2024-12-19
  * updated `load_pkg_metadata` to be simpler and essentially call `DPchecker::load_metadata` but with a preset default directory structure that works well with the default settings for `get_data_package`.
  * Add meta-analysis functions for finding and producing summary statistics multiple data packages including `get_ref_list`, `get_refs_info()`, and `summarize_packages`.
## 2024-10-24
  * fix how `get_data_package` aliases `get_data_packages`, specifically now allows users to adjust parameters to non-default settings.
## 2024-10-21
  * Bug fixes to `load_data_package()`
  * Bug fixes to `.get_authors()`
  * `get_authors` now adds a period (.) after given names with a single character and can handle an unlimited number of given names.
  * Moved sf, leaflet, and stringr to from imports to suggests.
  * Enable `.get_contacts()` to handle cases when there is only one contact.
  
# NPSutils 0.3.2 "Lost Coast"
  * Add new functions, `load_data_packages()` and `load_data_package()`, which can load data packages (EML in .xml and data in .csv) similarly to the deprecated `load_data_package_deprecated()` function but also allows the data types in the tibbles loaded to be specified based on the information in the metadata.
  * Deprecate `load_data_package()` and rename it to `load_data_package_deprecated()`.
  * Update readme to us pak for package installation instead of devtools.
  * Update _pkgdown.yml to use bootstrap 5
  * added helper functions for API requests and user input to facilitate unit testing.
  * refactored `get_data_packages()` to take advantage of new helper functions.
  * added `get_data_package()` which aliases `get_data_packages()` mostly because many people will want to load one data package and forget that the function is plural.
  * renamed `load_EML_df()` to `load_core_metadata()`.

# NPSutils 0.3.1 "Ooh Aah Point"
  * added private functions `.get_authors()` and `.get_contacts()` to retrieve authors and contacts (and emails) from EML
  * added `load_EML_df()`, which retrieves commonly available metadata items from an EML-formatted R object and returns them as a single dataframe (for loading into Power BI)

# NPSutils 0.3.0

  * updated all datastore api requests from v4/v5 to v6 (units service remains at v2)
  * add global variables for base datastore api urls and helper functions to access them in utils.R
  * added new functionality to `get_data_packages()`: it will now check to see if a DataStore reference ID is invalid or not. It will also check whether the reference is a data package or not. Substantial feedback is reported to the user if the flag force is set to FALSE.
  * added a new function `rm_local_packages()` which can delete one or more (or all) packages within a folder called "data" (where your packages should be if you downloaded them with `get_data_packages()`). This only deletes local copies and does not delete any data from DataStore.
  * changed function name `load_metadata()` is now `load_pkg_metadata()` so as not to conflict with `DPchecker::load_metadata()`
  * updated all function names to be snake_case
  * updated all file names to match function names
  * updated `get_data_packages()` to (when `force = FALSE`) check for newer versions of requested data packages and  prompt the user to download the newest version if they so choose.
  * updated `get_data_packages()` to:
  * include a force option. Force defaults to false for a verbose & interactive function. Setting force = TRUE removes the interactive portions and eliminates all informative messages except critical errors.
  * `get_data_packages()` now inspects the location the data packages are being written to. If a folder with the data package reference id already exists, the function will prompt the user asking if they want to re-download and overwrite the existing data package (when force = FALSE)
  * `get_data_packages()` now returns the full path to the data package folders, including the "/data" directory they are all in.
  * update `get_park_taxon_refs()` to hit v5/rest services
  * update documentation: make it clear that the taxon_code parameter in `get_park_taxon_refs()` is the IRMA taxon code, not the ITIS TSN.
  * update documentation: make it explicit when `get_park_taxon_refs()` and `get_park_taxon_citations()` are hitting secure servers and warn users that the results may also need to be restricted.

# NPSutils 0.2.0.3
  * added `map_wkt()` function to map points, polygons, or both from Well Known Text coordinates (WKT). WKT is used in place to GPS coordinates when sensitive species locations have been "fuzzed". In this case, providing a polygon rather than the an exact (albeit fuzzed) is preferable as it is clear that the location is not exact. WKT is an efficient way to store geographic shapes such as polygons in flat files such as .csv.

# NPSutils 0.2.0.1
  
  * updated get_data_package to retrieve 1 or more files from a given reference, if for instance a data package has multiple files associated with it. get_data_package is file extension agnostic.
  * get_data_package can now take a list of reference IDs from data store. It will write a separate folder for each reference (within a /data folder) and put the data files in the relevant folder.
  * get_data_package can now specify the directory that the /data folder and all child data package folders and data files are saved to. Defaults to the working directory.
  * get_data_package now returns the directory that the /data directory (where all sub-directories and data files are) to enable piping with downstream functions.
  * get_data_package informs the user of the file names being downloaded and where they are being saved; informs the user if a .zip is extracted and informs the user when the original .zip file is deleted.
  * get_data_package informs the user if a download failed due to not specifying secure=TRUE.

# NPSutils 0.2.0.0

Facelift to the entire package:

* Functions and parameters have snake_case names
* Tidyverse styling via styler
* Added bare-bones pkgdown site

# NPSutils 0.1.0.0

* Added a `NEWS.md` file to track changes to the package.
