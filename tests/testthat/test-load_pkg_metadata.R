# ---- load_pkg_metadata ----
#minimal testing here since this is essentially a wrapper function for DPchecker::load_metadata. Reason for separate function is 1) exposure to more public facing/data consuming NPSutils users and 2) better integration with default NPSutils data package download functions (but less flexibility).

httptest::with_mock_dir("load_pkg_metadata_dir", {
  test_that("load_pkg_metadata loads valid metadata", {
    
    local <- test_path()
    get_data_packages(2295255,
                      path = local,
                      force = TRUE,
                      dev = FALSE)
    
    meta <- load_pkg_metadata(2295255,
                              directory = "data")
    
    expect_true(EML::eml_validate(meta))
  })
})