# a couple of house-keeping functions for mockr interactivity:
return_val_2 <- function() {2}
return_val_1 <- function() {1}

### ---- test get_data_package

# httptest::with_mock_dir("get_data_packages_test_dir", {
#   test_that("get_data_packages creates the expected directory structure", {
#     
#     local <- test_path()
#     get_data_packages(2295255,
#                            path = local,
#                            force = TRUE,
#                            dev = FALSE)
#     
#     #expected directory structure is created:
#     expect_true(dir.exists(file.path(local, "data", "2295255")))
#     
    # These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests
    
    #expected file numbers are in the correct directory
    # expect_equal(length(list.files(file.path(local, "data", "2295255"))), 2)
    # 
    # #expected data file is present
    # expect_true(file.exists(file.path(local,
    #                                   "data",
    #                                   "2295255",
    #                                   "CAHA_Fish_Cleaned_PUBLIC.csv")))
    # 
    # #expected metadata file is present
    # expect_true(file.exists(file.path(local,
    #                                   "data",
    #                                   "2295255",
    #                                   "CAHA_Fish_metadata.xml")))
#     })  
# })

# These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests


# httptest::with_mock_dir("get_data_packages_test_files", {
#   test_that("get_data_packages downloads expected files", {
#     temp_path <- test_path()
#     get_data_packages(2295255,
#                            path = temp_path,
#                            force = TRUE,
#                            dev = FALSE)
#     expect_true(file.exists(file.path(temp_path,
#                                    "data",
#                                    "2295255",
#                                    "CAHA_Fish_Cleaned_PUBLIC.csv")))
# 
#     expect_true(file.exists(file.path(temp_path,
#                                      "data",
#                                      "2295255",
#                                      "CAHA_Fish_metadata.xml")))
#     })
# })


# httptest::with_mock_dir("get_data_packages_not_dp", {
#   #return_val_1 <- function() {1}
#   test_that("get_data_package downloads even if not a data package", {
#     #return_val_1 <- function() {1}
#     local({mockr::local_mock(.get_user_input = return_val_1)
#       
#       local_path <- test_path()
#       #temp_path <- withr::local_tempdir()
#       get_data_packages(2306391, #not a data package
#                            path = local_path,
#                            force = TRUE,
#                            dev = FALSE)
#       
#       #expected directory structure is created:
#       expect_true(dir.exists(file.path(local_path, "data", "2306391")))
#     
#       #expected file number are in the correct directory
#       expect_equal(length(list.files(file.path(local_path,
#                                                "data",
#                                                "2306391"))), 1)
#       
#       #expected data file is present
#       expect_true(file.exists(file.path(local_path,
#                                         "data",
#                                         "2306391",
#                                         "Checklist_for_data_package_review2024_10_07.docx")))
#     }) 
#   })
# })


