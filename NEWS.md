# NPSutils 0.2.0.1
  
  * updated get_data_package to retrieve 1 or more files from a given reference, if for instance a data package has multiple files associated with it. get_data_package is file extension agnostic.
  * get_data_package can now take a list of reference IDs from data store. It will write a separate folder for each reference (within a /data folder) and put the data files in the relevant folder.
  * get_data_package can now specify the directory that the /data folder and all child data package folders and data files are saved to. Defaults to the working directory.
  * get_data_package now returns the directory that the /data directory (where all sub-directories and data files are) to enable piping with downstream functions.
  * get_data_package informs the user of the file names being downloaded and where they are being saved; informs the user if a .zip is extracted and informs the user when the original .zip file is deleted.
  * get_data_package informs the user if a download failed due to not being on the VPN or not specifying secure=TRUE.

# NPSutils 0.2.0.0

Facelift to the entire package:

* Functions and parameters have snake_case names
* Tidyverse styling via styler
* Added bare-bones pkgdown site

# NPSutils 0.1.0.0

* Added a `NEWS.md` file to track changes to the package.
