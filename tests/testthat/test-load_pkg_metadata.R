# ---- load_pkg_metadata ----
#minimal testing here since this is essentially a wrapper function for DPchecker::load_metadata. Reason for separate function is 1) exposure to more public facing/data consuming NPSutils users and 2) better integration with default NPSutils data package download functions (but less flexibility).


# These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests

# httptest::with_mock_dir("load_pkg_metadata_dir", {
#   test_that("load_pkg_metadata loads valid metadata", {
#     # setup temp download directory
#     temp_dir <- withr::local_tempdir()
#     # download a data package
#     get_data_packages(reference_id = 2295255,
#                       path = temp_dir,
#                       force = TRUE,
#                       dev = FALSE)
#     # load the data pacakge metadata
#     meta <- load_pkg_metadata(reference_id = 2295255,
#                               path = file.path(temp_dir, "data"))
#     # test that loaded metadata is valid EML metadata
#     expect_true(EML::eml_validate(meta))
#   })
# })