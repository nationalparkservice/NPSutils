# These tests are commented out because httptest does provide good support for httr::write_disk functionality. Planned improvements for future versions of the package include updating the functions and tests

# httptest::with_mock_dir("load_core_metadata_dir", {
#   test_that("load_core_metadata returns a valid dataframe", {
#     
#     local <- test_path()
#     get_data_packages(2295255,
#                       path = local,
#                       force = TRUE,
#                       dev = FALSE)
#     
#     meta <- load_core_metadata(2295255,
#                                path = here::here("data"))
#     
#     #valid dataframe is returned:
#     expect_true(is.data.frame(meta))
#     
#     #dataframe has expected number of columns:
#     expect_equal(ncol(meta), 3)
#     
#     #dataframe has expected number of rows:
#     expect_equal(nrow(meta), 15)
#   })
# })