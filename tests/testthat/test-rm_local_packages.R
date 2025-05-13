httptest::with_mock_dir("rm_local_packages_dir", {
  test_that("rm_local_packages removes local packages", {
    
    temp_dir <- withr::local_tempdir()
    #local <- test_path()
    get_data_packages(2295255,
                      path = temp_dir,
                      force = TRUE,
                      dev = FALSE)
    
    rm_local_packages(2295255,
                      all = TRUE,
                      path = temp_dir,
                      force = FALSE)
    
    expect_true(!dir.exists(file.path(temp_dir, "data", "2295255")))
    
  })
})