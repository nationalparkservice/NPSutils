test_that("map_wkt returns list object type", {
  
  #create dataframe with random WKT polygons:
  x <- data.frame(name = c("Test1", "Test2", "Test3"),
                   wkt = randgeo::wkt_polygon(), 3)
  map<- map_wkt(df = x,
                wellknowntext = "wkt",
                type = "all",
                remove.duplicates = TRUE)
  
  expect_true(is.list(map))
})
