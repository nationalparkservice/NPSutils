# ---- get_unit_code ----

httptest::with_mock_dir("get_unit_code_dir", {
  test_that("get_unit_code returns data frame", {
    
    local <- test_path()
    x <- get_unit_code("Rocky")

    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term in the full unit name:
    expect_true(grepl("Rocky", x[2], ignore.case=TRUE))
  })
  
  test_that("get_park_code returns data frame", {
    
    local <- test_path()
    x <- get_park_code("Rocky")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term in the full unit name:
    expect_true(grepl("Rocky", x[2], ignore.case=TRUE))
  })
  
  test_that("get_unit_code_info returns data frame", {
    local <- test_path()
    x <- get_unit_code_info("ROMO")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data contains the search term
    expect_true(grepl("ROMO", x[1], ignore.case=TRUE))
  })
  
  test_that("get_unit_info works returns data frame - code", {
    local <- test_path()
    x <- get_unit_info(code = "SFCN")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("SFCN", x[1], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - park", {
    local <- test_path()
    x <- get_unit_info(park = "Rocky")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("Rocky", x[2], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - life_cycle", {
    local <- test_path()
    x <- get_unit_info(life_cycle = "Active")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("Active", x[8], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - network_code", {
    local <- test_path()
    x <- get_unit_info(network_code = "ROMN")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("ROMN", x[9], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - net_name", {
    local <- test_path()
    x <- get_unit_info(net_name = "Mountain")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("Mountain", x[10], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - region_abb", {
    local <- test_path()
    x <- get_unit_info(region_abb = "IMR")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("IMR", x[11], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - region", {
    local <- test_path()
    x <- get_unit_info(region = "Intermountain")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("Intermountain", x[12], ignore.case=TRUE))
    
  })
  
  test_that("get_unit_info works returns data frame - state", {
    local <- test_path()
    x <- get_unit_info(state = "CO")
    
    #a data frame is returned
    expect_true(is.data.frame(x))
    
    #the returned data frame contains the search term
    expect_true(grepl("CO", x[13], ignore.case=TRUE))
    
  })
  
})