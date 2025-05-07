
httptest::with_mock_dir("get_ref_list_dir", {
  test_that("get_ref_list returns a vector of 7-digit reference IDs", {
    
    local <- test_path()
    x <- get_ref_list(reference_type = "dataPackage",
                      no_of_entries = 100,
                      secure = FALSE)
    
    
    # 100 items returned:
    expect_equal(length(x), 100)
    
    # each item is 7 digits long:
    correct_length <- TRUE
    for (i in 1:length(x)) { 
      if (nchar(x[i]) != 7) {
        correct_length <- FALSE
      }
    }
    expect_true(correct_length)
    
  })  
})