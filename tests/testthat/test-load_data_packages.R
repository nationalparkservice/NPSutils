# ---- test load_data_packages

# These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests

# httptest::with_mock_dir("load_data_packages_test_dir", {
#   test_that("load_data_packages loads a package from default directory structure", {
#     
#     local <- test_path()
#     get_data_packages(2295255,
#                       path = local,
#                       force = TRUE,
#                       dev = FALSE)
#     
#     CAHA <- load_data_packages(2295255,
#                        directory = here::here("data"),
#                        assign_attributes = FALSE,
#                        simplify = TRUE)
#     
#     #tibble exists:
#     expect_true(!is.null(CAHA))
#     
#     #tibble has correct name:
#     expect_equal(names(CAHA), "pkg_2295255.CAHA_Fish_Cleaned_PUBLIC")
#     
#     #tibble has correct dimensions:
#     expect_equal(nrow(CAHA[[1]]), 179)
#     expect_equal(ncol(CAHA[[1]]), 27)
#     
#   })  
# })

# These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests

# httptest::with_mock_dir("load_data_packages_test_attributes", {
#   test_that("load_data_packages correctly assigns attributes from metadata", {
#     local <- test_path()
#     get_data_packages(2295255,
#                       path = local,
#                       force = TRUE,
#                       dev = FALSE)
#     
#     CAHA <- load_data_packages(2295255,
#                                directory = here::here("data"),
#                                assign_attributes = FALSE,
#                                simplify = TRUE)
#     CAHA2 <- load_data_packages(2295255,
#                                directory = here::here("data"),
#                                assign_attributes = TRUE,
#                                simplify = TRUE)
#     
#     # test that all data are coming through; nothing incorrectly turned to NA:
#     expect_equal(sum(is.na(CAHA)), sum(is.na(CAHA2)))
#     
#     # test that a subset of data correctly assigned data type of factor:
#     expect_true(is.factor(CAHA2[[1]]$locality))
#     expect_true(is.factor(CAHA2[[1]]$type))
#     expect_true(is.factor(CAHA2[[1]]$eventDate_flag))
#   })
# })
