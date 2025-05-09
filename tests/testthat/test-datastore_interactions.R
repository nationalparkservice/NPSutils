# ---- check_ref_exists ----

httptest::with_mock_dir("check_ref_exists_dir", {
  test_that("check_ref_exists returns TRUE if a reference exists", {
    
    local <- test_path()
    x <- check_ref_exists(reference_id = 2295255,
                      secure = FALSE,
                      dev = FALSE)
    expect_true(x)
  })  
})

httptest::with_mock_dir("check_ref_doesnt_exist_dir", {
  test_that("check_ref_exists returns FALSE if a reference doesn't exist", {
    
    local <- test_path()
    x <- check_ref_exists(reference_id = 00000000,
                          secure = FALSE,
                          dev = FALSE)
    expect_true(!x)
  })  
})

# ---- check_is_data_package ----

httptest::with_mock_dir("check_is_data_package_dir", {
  test_that("check_is_data_package returns TRUE if reference is a data package", {
    
    local <- test_path()
    x <- check_is_data_package(reference_id = 2295255,
                          secure = FALSE,
                          dev = FALSE)
    expect_true(x)
  })  
})

httptest::with_mock_dir("check_is_NOT_data_package_dir", {
  test_that("check_is_data_package returns FALSE if reference is not data package", {
    
    local <- test_path()
    x <- check_is_data_package(reference_id = 2306391, #not a data package
                               secure = FALSE,
                               dev = FALSE)
    expect_true(!x)
  })  
})

# ---- check_new_version ----

httptest::with_mock_dir("check_new_version_TRUE_dir", {
  test_that("check_new_version returns TRUE if newer version exists", {
    
    local <- test_path()
    x <- check_new_version(reference_id = 2301647, #has a newer version
                               secure = FALSE,
                               dev = FALSE)
    expect_true(x)
  })  
})

### kind of hard to test for when this is false as that "fact" could change

# ---- get_new_version_id ----

httptest::with_mock_dir("get_get_new_version_dir", {
  test_that("get_new_version returns TRUE if newer version exists", {
    
    local <- test_path()
    x <- get_new_version_id(reference_id = 2301647, #has a newer version
                           secure = FALSE)
    expect_true(!is.null(x))
  })  
})

#again, hard to test for something that doesn't have a version as it could be versioned at any time.
