
# ---- get_ref_list ----
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

# ---- get_refs_info ----

httptest::with_mock_dir("get_refs_info_dir", {
  test_that("get_refs_info returns expected values", {
    #list of 5 test references (all data packages)
    
    x <- get_refs_info(reference_type = "dataPackage",
                      no_of_entries = 5,
                      secure = FALSE)
    
    #returns info no 5 references:
    expect_equal(nrow(x), 5)
    
    #returns expected number of columns:
    expect_equal(ncol(x), 14)
    
    #returns expected columns:
    expect_equal(names(x), c("referenceId", "referenceType", "dateOfIssue",
                             "visibility", "fileCount", "fileAccess",
                             "title", "citation", "referenceUrl",
                             "referenceGroupType", "typeName", "isDOI",
                             "newVersion", "mostRecentVersion"))
  })  
})

# ---- summarize_packages ----

httptest::with_mock_dir("summarize_packages_dir", {
  test_that("get_ref_info returns expected values", {
    #list of 5 test references (all data packages)
    x <- get_refs_info(reference_type = "dataPackage",
                       no_of_entries = 5,
                       secure = FALSE)
    
    #returns info no 5 references:
    expect_equal(nrow(x), 5)
    
    #returns expected number of columns:
    expect_equal(ncol(x), 14)
    
    #returns expected columns:
    expect_equal(names(x), c("referenceId", "referenceType", "dateOfIssue",
                             "visibility", "fileCount", "fileAccess",
                             "title", "citation", "referenceUrl",
                             "referenceGroupType", "typeName", "isDOI",
                             "newVersion", "mostRecentVersion"))
  })  
})
