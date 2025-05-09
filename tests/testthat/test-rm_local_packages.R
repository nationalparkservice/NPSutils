httptest::with_mock_dir("rm_local_packages_dir", {
  test_that("rm_local_packages removes local packages", {
    
    local <- test_path()
    get_data_packages(2295255,
                      path = local,
                      force = TRUE,
                      dev = FALSE)
    
    rm_local_packages(2295255,
                      all = TRUE,
                      path = local,
                      force = FALSE)
    
    expect_true(!dir.exists(file.path(local, "data", "2295255")))
    
  })
})