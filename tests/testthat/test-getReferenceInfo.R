
# ---- get_park_taxon_refs ----

httptest::with_mock_dir("get_park_taxon_refs_dir", {
  test_that("get_park_taxon_refs returns a valid tibble", {
    
    local <- test_path()
    x <- get_park_taxon_refs(park_code = "APIS",
                        taxon_code = 126749)
    
    #valid dataframe is returned:
    expect_true(is.data.frame(x))
    
    # DataStore endpoint needs work, uncomment when back up and running
    # dataframe has expected number of columns:
    #expect_equal(ncol(x), 13)

    # DataStore endpoint needs work, uncomment when back up and running    
    # dataframe columns have expected names:
    # expect_equal(names(x), c("referenceId", "referenceType", "dateOfIssue",
    #                          "lifecycle", "visibility", "fileCount",
    #                          "fileAccess", "title", "citation",
    #                          "referenceUrl", "referenceGroupType", "typeName",
    #                          "isDOI"))
  })  
  
  test_that("get_park_taxon_url returns a valid tibble", {
    
    local <- test_path()
    x <- get_park_taxon_url(park_code = "APIS",
                             taxon_code = 126749)
    
    # character type object is returned
    expect_true(is.character(x))
    
    # DataStore endpoint needs work, uncomment when back up and running    
    # character is formatted as a valid irma url
    # expect_true(grepl("https://irma.nps.gov/DataStore/Reference/Profile/", x[1]))
    
  })  

  test_that("get_ref_info returns a valid title", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                          field = "Title")
    #title
    expect_contains(x, "Fish Inventory at Cape Hatteras National Seashore in 2003 - Open Format Dataset")
  
    })  

  test_that("get_ref_info returns a valid abstract", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                    field = "Abstract")
    #abstract
    expect_true(grepl("The Southeast Coast Network is comprised of 20 parks", x))
  }) 

  test_that("get_ref_info returns a valid Citation", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                    field = "Citation")
    #abstract
    expect_true(grepl("Quevedo I", x))
  }) 

  test_that("get_ref_info returns a valid Keywords", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                    field = "Keywords")
    #abstract
    expect_true(grepl("biodiversity", x[1]))
  }) 

  test_that("get_ref_info returns a valid Visibility", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                    field = "Visibility")
    #abstract
    expect_equal(x, "Public")
  }) 

  test_that("get_ref_info returns a valid Visibility", {
  
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                    field = "Visibility")
    #abstract
    expect_equal(x, "Public")
  }) 

  test_that("get_ref_info returns a valid Lifecycle", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "Lifecycle")
    #abstract
    expect_equal(x, "Active")
  }) 
  
  test_that("get_ref_info returns a valid IssuedYear", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "IssuedYear")
    #abstract
    expect_equal(x, 2023)
  })
  
  test_that("get_ref_info returns a valid Units", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "Units")
    #abstract
    expect_true(is.null(x))
  })
  
  test_that("get_ref_info returns a valid Versions", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "Versions")
    #abstract
    expect_true(is.data.frame(x))
  })
  
  test_that("get_ref_info returns a valid CreatedBy", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "CreatedBy")
    #abstract
    expect_equal(x, "iquevedo@nps.gov")
  })
  
  test_that("get_ref_info returns a valid ContentBeginYear", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "ContentBeginYear")
    #abstract
    expect_equal(x, 2003)
  })
  
  test_that("get_ref_info returns a valid ContentEndYear", {
    
    local <- test_path()
    x <- get_ref_info(reference_id = 2295255,
                      field = "ContentEndYear")
    #abstract
    expect_equal(x, 2003)
  })
  
})