
# ---- get_park_taxon_refs ----

httptest::with_mock_dir("get_park_taxon_refs_dir", {
  test_that("get_park_taxon_refs returns a valid tibble", {
    
    local <- test_path()
    x <- get_park_taxon_refs(park_code = "APIS",
                        taxon_code = 126749)
    
    #valid dataframe is returned:
    expect_true(is.data.frame(x))
    
    #dataframe has expected number of columns:
    expect_equal(ncol(x), 13)
    
    #dataframe columns have expected names:
    expect_equal(names(x), c("referenceId", "referenceType", "dateOfIssue",
                             "lifecycle", "visibility", "fileCount",
                             "fileAccess", "title", "citation",
                             "referenceUrl", "referenceGroupType", "typeName",
                             "isDOI"))
  })  
  
test_that("get_park_taxon_url returns a valid tibble", {
    
    local <- test_path()
    x <- get_park_taxon_url(park_code = "APIS",
                             taxon_code = 126749)
    
    #character type object is returned
    expect_true(is.character(x))
    
    #character is formatted as a valid irma url
    expect_true(grepl("https://irma.nps.gov/DataStore/Reference/Profile/", x[1]))
    
  })  
})